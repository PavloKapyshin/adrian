from . import astlib, layers, errors, defs
from .context import context
from .type_lib import infer_type, types_are_equal, is_super_type
from .utils import A


def depends_on_self(expr):
    if expr in A(astlib.Name):
        return True if expr == defs.SELF else False
    elif expr in A(astlib.StructPath):
        return any([depends_on_self(p) for p in expr.path])
    elif expr in A(astlib.FuncCall):
        return any([depends_on_self(a) for a in expr.args])
    return False


def complete_init_method(method_decl, struct_decl):
    self_decl = astlib.VarDecl(
        astlib.Name(defs.SELF), struct_decl.name,
        astlib.InstanceValue(struct_decl.name, {}))
    return_self = astlib.Return(astlib.Name(defs.SELF))
    return astlib.MethodDecl(
        astlib.Name(defs.SPEC_METHOD_INIT), method_decl.args, struct_decl.name,
        [self_decl] + method_decl.body + [return_self])


def default_init_method(struct_decl):
    self_decl = astlib.VarDecl(
        astlib.Name(defs.SELF), struct_decl.name,
        astlib.InstanceValue(struct_decl.name, {}))
    field_decls = filter(
        lambda x: x is not None,
        [stmt if stmt in A(astlib.FieldDecl) else None
        for stmt in struct_decl.body])
    field_inits = []
    args = []
    for field_decl in field_decls:
        args.append((field_decl.name, field_decl.type_))
        field_inits.append(
            astlib.Assignment(
                astlib.StructPath([astlib.Name(defs.SELF), field_decl.name]),
                "=", field_decl.name))
    return_self = astlib.Return(astlib.Name(defs.SELF))
    return astlib.MethodDecl(
        astlib.Name(defs.SPEC_METHOD_INIT), args, struct_decl.name,
        [self_decl] + field_inits + [return_self])


class Interpreter(layers.Layer):

    @layers.register(astlib.LetDecl)
    def let_declaration(self, decl):
        self._declaration(decl)

    @layers.register(astlib.VarDecl)
    def var_declaration(self, decl):
        if decl.expr in A(astlib.Empty):
            errors.later()
        self._declaration(decl)

    @layers.register(astlib.FuncDecl)
    def func_declaration(self, decl):
        context.env[decl.name] = {
            "node_type": astlib.NodeT.func,
            "args": decl.args,
            "rettype": decl.rettype,
            "body": decl.body
        }

    @layers.register(astlib.MethodDecl)
    def method_declaration(self, decl):
        assert(context.parent_struct is not None)
        assert(context.parent_struct in context.env)
        context.env[context.parent_struct]["methods"][decl.name] = {
            "args": decl.args,
            "rettype": decl.rettype,
            "body": decl.body
        }

    @layers.register(astlib.StructDecl)
    def struct_declaration(self, decl):
        context.env[decl.name] = {
            "node_type": astlib.NodeT.struct,
            "parameters": decl.parameters,
            "implemented_protocols": decl.implemented_protocols,
            "fields": {},
            "methods": {}
        }
        context.env.add_scope()
        context.parent_struct = decl.name
        body = decl.body
        has_init = False
        new_body = []
        for stmt in body:
            if (stmt in A(astlib.MethodDecl) and
                    stmt.name == defs.SPEC_METHOD_INIT):
                has_init = True
                new_body.append(complete_init_method(stmt, decl))
                break
            new_body.append(stmt)
        if not has_init:
            new_body = [default_init_method(decl)] + new_body
        self.b(new_body)
        context.parent_struct = None
        context.env.remove_scope()

    @layers.register(astlib.ExtensionDecl)
    def extension_declaration(self, decl):
        def _update_implemented_protocols(name, impled_protocols):
            old = context.env[name]["implemented_protocols"]
            to_add = []
            for protocol_name in impled_protocols:
                if protocol_name not in old:
                    to_add.append(protocol_name)
            context.env[name]["implemented_protocols"].extend(to_add)

        assert(decl.name in context.env)
        _update_implemented_protocols(decl.name, decl.implemented_protocols)
        context.env.add_scope()
        context.parent_struct = decl.name
        self.b(decl.body)
        context.parent_struct = None
        context.env.remove_scope()

    @layers.register(astlib.ProtocolDecl)
    def protocol_declaration(self, decl):
        context.env[decl.name] = {
            "node_type": astlib.NodeT.protocol,
            "parameters": decl.parameters,
            "implemented_protocols": decl.implemented_protocols,
            "fields": {},
            "methods": {},
        }
        context.env.add_scope()
        context.parent_struct = decl.name
        self.b(decl.body)
        context.parent_struct = None
        context.env.remove_scope()

    @layers.register(astlib.FuncProtoDecl)
    def func_proto_declaration(self, decl):
        assert(context.parent_struct is not None)
        assert(context.parent_struct in context.env)
        assert(context.env[context.parent_struct]["node_type"] == astlib.NodeT.protocol)
        context.env[context.parent_struct]["methods"][decl.name] = {
            "args": decl.args,
            "rettype": decl.rettype
        }

    @layers.register(astlib.FieldDecl)
    def field_declaration(self, decl):
        assert(context.parent_struct is not None)
        assert(context.parent_struct in context.env)
        context.env[context.parent_struct]["fields"][decl.name] = {
            "type": decl.type_
        }

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        expr = stmt.right
        if stmt.op != defs.EQ:
            expr = astlib.Expr(
                stmt.left, defs.ASSIGNMENT_OP_TO_EXPR_OP[stmt.op], expr)
        expr = self.eval(self._inline_references_of_name(expr, stmt.left))

        if stmt.left in A(astlib.Subscript):
            method_name = defs.SPEC_METHOD_SETITEM
            base, args = stmt.left.base, [stmt.left.index, expr]
            type_ = infer_type(self.eval(base))
            if type_ in A(astlib.PyType):
                return self.py_method_call(
                    base, astlib.FuncCall(method_name, args))
            return self.method_call(base, astlib.FuncCall(method_name, args))
        elif stmt.left in A(astlib.Name):
            if context.env[stmt.left]["node_type"] != astlib.NodeT.var:
                errors.cant_reassign(stmt.left)
            context.env[stmt.left]["expr"] = expr
        else:
            assert(stmt.left in A(astlib.StructPath))
            self._update_value_through_struct_path(stmt.left.path, expr)

    @layers.register(astlib.Cond)
    def cond(self, stmt):
        if_expr = self.eval(stmt.if_stmt.expr)
        context.env.add_scope()
        if if_expr:
            context.env.remove_scope()
            return self.b(stmt.if_stmt.body)
        for elif_stmt in stmt.elifs:
            elif_expr = self.eval(elif_stmt.expr)
            if elif_expr:
                context.env.remove_scope()
                return self.b(elif_stmt.body)
        if stmt.else_stmt is not None:
            context.env.remove_scope()
            return self.b(stmt.else_stmt.body)
        context.env.remove_scope()

    @layers.register(astlib.While)
    def while_stmt(self, stmt):
        expr = self.eval(stmt.expr)
        context.env.add_scope()
        while expr:
            result = self.b(stmt.body)
            if result in A(astlib.BreakEvent):
                break
            if result is not None:
                context.env.remove_scope()
                return result
            expr = self.eval(stmt.expr)
        context.env.remove_scope()

    @layers.register(astlib.For)
    def for_stmt(self, stmt):
        def register_name(name, expr):
            evaluated = self.eval(expr)
            context.env[name] = {
                "node_type": astlib.NodeT.var,
                "type": infer_type(evaluated),
                "expr": evaluated
            }
        container = self.eval(stmt.container)
        context.env.add_scope()
        names = stmt.names
        for elem in container:
            if len(names) > 1:
                for name, pair_elem in zip(names, elem):
                    register_name(name, pair_elem)
            else:
                register_name(names[0], elem)
            result = self.b(stmt.body)
            if result in A(astlib.BreakEvent):
                break
            if result is not None:
                context.env.remove_scope()
                return result
        context.env.remove_scope()

    @layers.register(astlib.Return)
    def return_stmt(self, stmt):
        return self.eval(stmt.expr)

    @layers.register(astlib.BreakEvent)
    def break_event(self, stmt):
        return stmt

    @layers.register(astlib.FuncCall)
    def func_call(self, func_call):
        info = context.env[func_call.name]
        if info["node_type"] == astlib.NodeT.struct:
            info = info["methods"][defs.SPEC_METHOD_INIT]
        elif info["node_type"] == astlib.NodeT.protocol:
            return astlib.GenericType(func_call.name, func_call.args)
        context.env.add_scope()
        for (name, type_), expr in zip(info["args"], func_call.args):
            context.env[name] = {
                "node_type": astlib.NodeT.var,
                "type": type_,
                "expr": self.eval(expr)
            }
        return_val = self.b(info["body"])
        context.env.remove_scope()
        return return_val

    def method_call(self, base, func_call):
        converted_base = self.eval(base)
        type_ = infer_type(converted_base)
        if depends_on_self(base):
            args = [converted_base]
        else:
            args = [base]
        args += [self.eval(arg) for arg in func_call.args]
        assert(type_ in A(astlib.Name))
        method_info = context.env[type_]["methods"][func_call.name]
        body = method_info["body"]
        context.env.add_scope()
        for (name, type_), expr in zip(
                [(astlib.Name(defs.SELF), type_)] + method_info["args"], args):
            context.env[name] = {
                "node_type": astlib.NodeT.var,
                "type": type_,
                "expr": expr
            }
        result = self.b(body)
        context.env.remove_scope()
        return result

    @layers.register(astlib.StructPath)
    def struct_path(self, struct_path):
        self.eval(struct_path)

    @layers.register(astlib.PyFuncCall)
    def py_call(self, func_call):
        def translate_iterable(iterable):
            return [self.eval(element) for element in iterable]

        if func_call.name == defs.TYPE_INT:
            return int(func_call.args[0].literal)
        elif func_call.name == defs.TYPE_STR:
            return func_call.args[0].literal
        elif func_call.name == defs.TYPE_LIST:
            return translate_iterable(
                func_call.args[0].literal)
        elif func_call.name == defs.TYPE_SET:
            return set(translate_iterable(
                func_call.args[0].literal))
        elif func_call.name == defs.TYPE_DICT:
            literal = func_call.args[0].literal
            if literal in A(set):
                # Parser parses `{}` as a set, and not as a dictionary, so...
                return {}
            return {
                self.eval(key): self.eval(val)
                for key, val in literal.items()}
        elif func_call.name == defs.FUNC_PRINT:
            for arg in func_call.args:
                arg = self.eval(arg)
                print(arg, end="")
            print()
        elif func_call.name == defs.FUNC_TO_STR:
            return str(self.eval(func_call.args[0]))
        elif func_call.name == defs.FUNC_TO_INT:
            return int(self.eval(func_call.args[0]))
        elif func_call.name == defs.FUNC_TO_SET:
            return set(self.eval(func_call.args[0]))
        elif func_call.name == defs.FUNC_TO_LIST:
            return list(self.eval(func_call.args[0]))
        elif func_call.name == defs.FUNC_LEN:
            return len(self.eval(func_call.args[0]))
        elif func_call.name == defs.FUNC_ZIP:
            return list(zip(*[self.eval(arg) for arg in func_call.args]))
        elif func_call.name == defs.FUNC_READ_FILE:
            file_path = self.eval(func_call.args[0])
            with open(file_path, "r") as file:
                contents = file.read()
            return contents
        elif func_call.name == defs.FUNC_WRITE_FILE:
            file_path = self.eval(func_call.args[0])
            contents = self.eval(func_call.args[1])
            with open(file_path, "w") as file:
                file.write(contents)
        else:
            # support other funcs
            errors.later()

    def py_method_call(self, base, func_call):
        converted_base = self.eval(base)
        type_ = infer_type(converted_base)
        args = [self.eval(arg) for arg in func_call.args]
        if func_call.name == defs.METHOD_SPLIT:
            return converted_base.split(args[0])
        elif func_call.name == defs.METHOD_VALUES:
            return list(converted_base.values())
        elif func_call.name == defs.METHOD_KEYS:
            return list(converted_base.keys())
        elif func_call.name == defs.METHOD_ITEMS:
            return list(converted_base.items())
        elif func_call.name == defs.SPEC_METHOD_GETITEM:
            if converted_base in A(list, str):
                if args[0] < len(converted_base):
                    return converted_base[args[0]]
                return None
            else:
                return converted_base.get(args[0])
        elif func_call.name == defs.SPEC_METHOD_SETITEM:
            expr = converted_base
            expr[args[0]] = args[1]
            self.assignment(astlib.Assignment(base, "=", expr))
        elif func_call.name == defs.SPEC_METHOD_CONTAINS:
            return args[0] in converted_base
        elif func_call.name == defs.SPEC_METHOD_EQ:
            return converted_base == args[0]
        elif func_call.name == defs.SPEC_METHOD_NEQ:
            return converted_base != args[0]
        elif func_call.name == defs.SPEC_METHOD_LT:
            return converted_base < args[0]
        elif func_call.name == defs.SPEC_METHOD_GT:
            return converted_base > args[0]
        elif func_call.name == defs.SPEC_METHOD_LTEQ:
            return converted_base <= args[0]
        elif func_call.name == defs.SPEC_METHOD_GTEQ:
            return converted_base >= args[0]
        elif func_call.name == defs.SPEC_METHOD_ADD:
            if type_.name == defs.TYPE_SET:
                return converted_base.union(args[0])
            elif type_.name == defs.TYPE_DICT:
                result = converted_base
                for key, val in args[0].items():
                    result[key] = val
                return result
            return converted_base + args[0]
        elif func_call.name == defs.SPEC_METHOD_SUB:
            return converted_base - args[0]
        elif func_call.name == defs.SPEC_METHOD_MUL:
            return converted_base * args[0]
        elif func_call.name == defs.SPEC_METHOD_DIV:
            result = converted_base / args[0]
            up, down = result.as_integer_ratio()
            # When result is an integer we don't need .0 printed at the end.
            if down == 1:
                return up
            return result
        else:
            # support other methods
            errors.later()

    def b(self, body):
        """Executes bodies of functions, methods, cycles, etc."""
        def transform_node(node, *, registry):
            node_func = registry.get(type(node))
            if node_func is not None:
                return node_func(node)

        reg = Interpreter().get_registry()
        for stmt in body:
            value = transform_node(stmt, registry=reg)
            if value is not None:
                return value

    def eval(self, expr):
        """Executes expressions."""
        if expr in A(astlib.Name):
            if expr in (defs.CONSTANT_TRUE, defs.CONSTANT_FALSE):
                return (True if expr == defs.CONSTANT_TRUE else False)
            info = context.env[expr]
            if info["node_type"] in (astlib.NodeT.var, astlib.NodeT.let):
                return self.eval(context.env[expr]["expr"])
            return expr
        elif expr in A(astlib.FuncCall):
            return self.func_call(expr)
        elif expr in A(astlib.PyFuncCall, astlib.PyTypeCall):
            return self.py_call(expr)
        elif expr in A(astlib.StructPath):
            root, tail = expr.path[0], expr.path[1:]
            root_expr_raw = root
            root_expr = self.eval(root)
            root_type = infer_type(root_expr)
            for elem in tail:
                if elem in A(astlib.FuncCall):
                    if root_type in A(astlib.PyType):
                        root_expr = self.py_method_call(root_expr, elem)
                        root_expr_raw = root_expr
                        root_type = infer_type(root_expr)
                    else:
                        root_expr = self.method_call(root_expr_raw, elem)
                        root_expr_raw = root_expr
                        root_type = infer_type(root_expr)
                else:
                    assert(root_expr in A(astlib.InstanceValue))
                    root_expr = root_expr.value[elem]
                    if root_expr_raw in A(astlib.StructPath):
                        root_expr_raw.path += [elem]
                    else:
                        root_expr_raw = astlib.StructPath(
                            [root_expr_raw, elem])
                    root_type = infer_type(root_expr)
            return root_expr
        elif expr in A(astlib.Expr):
            if expr.op == defs.IS:
                return self.eval(astlib.Is(expr.left, expr.right))
            elif expr.op == defs.NEQ:
                return not self.eval(
                    astlib.Expr(expr.left, defs.EQEQ, expr.right))
            elif expr.op == defs.LTEQ:
                return (self.eval(
                    astlib.Expr(expr.left, defs.EQEQ, expr.right)) or
                    self.eval(astlib.Expr(expr.left, defs.LT, expr.right)))
            elif expr.op == defs.GTEQ:
                return (self.eval(
                    astlib.Expr(expr.left, defs.EQEQ, expr.right)) or
                    self.eval(astlib.Expr(expr.left, defs.GT, expr.right)))
            elif expr.op == defs.AND:
                return self.eval(expr.left) and self.eval(expr.right)
            elif expr.op == defs.OR:
                return self.eval(expr.left) or self.eval(expr.right)
            method_name = defs.OPERATOR_TO_METHOD[expr.op]
            base, args = expr.left, [expr.right]
            if method_name == defs.SPEC_METHOD_CONTAINS:
                base, args = expr.right, [expr.left]
            type_ = infer_type(self.eval(base))
            if type_ in A(astlib.PyType):
                return self.py_method_call(
                    base, astlib.FuncCall(method_name, args))
            return self.method_call(base, astlib.FuncCall(method_name, args))
        elif expr in A(astlib.Literal):
            if expr.type_ == astlib.LiteralT.number:
                return int(expr.literal)
            elif expr.type_ == astlib.LiteralT.vector:
                return [self.eval(elem) for elem in expr.literal]
            elif expr.type_ == astlib.LiteralT.set_:
                return {self.eval(elem) for elem in expr.literal}
            elif expr.type_ == astlib.LiteralT.dict_:
                return {
                    self.eval(key): self.eval(val)
                    for key, val in expr.literal.items()}
            return expr.literal
        elif expr in A(astlib.Subscript):
            method_name = defs.SPEC_METHOD_GETITEM
            base, args = expr.base, [expr.index]
            type_ = infer_type(self.eval(base))
            if type_ in A(astlib.PyType):
                return self.py_method_call(
                    base, astlib.FuncCall(method_name, args))
            return self.method_call(base, astlib.FuncCall(method_name, args))
        elif expr in A(astlib.Not):
            return not self.eval(expr.expr)
        elif expr in A(astlib.Is):
            sub_expr = self.eval(expr.sub_expr)
            super_expr = self.eval(expr.super_expr)
            sub_type = infer_type(sub_expr)
            if sub_type in A(astlib.Name) and sub_type == defs.TYPE_VOID:
                return True
            super_type = infer_type(super_expr)
            if super_type is None:
                return sub_expr is None
            return (types_are_equal(sub_type, super_type) or
                is_super_type(sub_type, super_type))
        elif expr in A(
                int, str, list, set, dict, bool, astlib.InstanceValue,
                astlib.GenericType, astlib.PyType):
            return expr
        elif expr is None:
            return None
        else:
            # support other exprs
            errors.later()

    def _declaration(self, decl):
        expr = self.eval(decl.expr)
        context.env[decl.name] = {
            "node_type": (astlib.NodeT.var
                    if decl in A(astlib.VarDecl)
                    else astlib.NodeT.let),
            "type": (decl.type_
                if decl.type_ not in A(astlib.Empty)
                else infer_type(expr)),
            "expr": expr
        }

    def _update_value_through_struct_path(self, path, expr):
        root, inner_body, leaf = path[0], path[1:-1], path[-1]
        assert(root in A(astlib.Name))
        raw_root_expr = context.env[root]["expr"]
        root_expr = self.eval(root)
        old_instance_values = [root_expr]
        for elem in inner_body:
            assert(root_expr in A(astlib.InstanceValue))
            old_instance_values.append(root_expr)
            root_expr = root_expr.value[elem]
        new_expr = expr
        for elem, old_instance_value in zip(
                reversed(inner_body + [leaf]), reversed(old_instance_values)):
            new_expr = astlib.InstanceValue(
                old_instance_value.type_,
                {**old_instance_value.value, **{elem: new_expr}})
        context.env[root]["expr"] = new_expr
        if raw_root_expr in A(astlib.Name):
            assert(root == defs.SELF)
            context.env[raw_root_expr]["expr"] = new_expr

    def _inline_references_of_name(self, expr, name):
        if expr in A(astlib.Name):
            return context.env[expr]["expr"] if expr == name else expr
        elif expr in A(astlib.Expr):
            return astlib.Expr(
                self._inline_references_of_name(expr.left, name), expr.op,
                self._inline_references_of_name(expr.right, name))
        elif expr in A(astlib.StructPath):
            return astlib.StructPath(
                [self._inline_references_of_name(
                    expr.path[0], name)] + expr.path[1:])
        elif expr in A(astlib.PyTypeCall):
            return expr
        elif expr in A(astlib.FuncCall):
            return astlib.FuncCall(
                expr.name,
                [self._inline_references_of_name(arg, name)
                for arg in expr.args])
        elif expr in A(astlib.Subscript):
            return self.eval(expr)
        elif expr in A(int, str, list, dict, set):
            return expr
        else:
            # support other exprs
            errors.later()
