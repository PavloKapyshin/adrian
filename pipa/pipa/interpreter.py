from . import astlib, layers, errors, defs
from .context import context
from .inference import infer_type
from .utils import A


def inline_references_of_name(expr, name):
    if expr in A(astlib.Name):
        return context.env[expr]["expr"] if expr == name else expr
    elif expr in A(astlib.Expr):
        return astlib.Expr(
            inline_references_of_name(expr.left, name), expr.op,
            inline_references_of_name(expr.right, name))
    elif expr in A(astlib.StructPath):
        return astlib.StructPath(
            [inline_references_of_name(expr.path[0], name)] + expr.path[1:])
    elif expr in A(astlib.PyTypeCall):
        return expr
    elif expr in A(astlib.FuncCall):
        return astlib.FuncCall(
            expr.name,
            [inline_references_of_name(arg, name) for arg in expr.args])
    else:
        # support other exprs
        errors.later()


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
        self.declaration(decl)

    @layers.register(astlib.VarDecl)
    def var_declaration(self, decl):
        if decl.expr in A(astlib.Empty):
            errors.later()
        self.declaration(decl)

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
            "fields": {},
            "methods": {}
        }
        context.env.add_scope()
        context.parent_struct = decl.name
        body = decl.body
        has_init = False
        for stmt in body:
            if not has_init and stmt in A(astlib.MethodDecl):
                has_init = (stmt.name == defs.SPEC_METHOD_INIT)
        if not has_init:
            body = [default_init_method(decl)] + body
        self.b(body)
        context.parent_struct = None
        context.env.remove_scope()

    @layers.register(astlib.FieldDecl)
    def field_declaration(self, decl):
        assert(context.parent_struct is not None)
        context.env[context.parent_struct]["fields"][decl.name] = {
            "type": decl.type_
        }

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        if stmt.left not in A(astlib.Name, astlib.StructPath):
            errors.later()
        # TODO:
        # Left must be declarated only as variable.
        # Type of right must be equal to type of left.
        if stmt.op == defs.EQ:
            expr = inline_references_of_name(stmt.right, stmt.left)
        else:
            expr = inline_references_of_name(
                astlib.Expr(
                    stmt.left, defs.ASSIGNMENT_OP_TO_EXPR_OP[stmt.op],
                    stmt.right),
                stmt.left)
        if stmt.left in A(astlib.Name):
            context.env[stmt.left]["expr"] = self.eval(expr)
        else:
            root = stmt.left.path[0]
            assert(root in A(astlib.Name))
            root_expr_raw = context.env[root]["expr"]
            root_expr = self.eval(root)
            old_dicts = [(root_expr.type_, root_expr.value)]
            for elem in stmt.left.path[1:-1]:
                old_dicts.append((root_expr.type_, root_expr.value))
                root_expr = root_expr.value[elem]
            new_value = self.eval(expr)
            for elem, (type_, dict_) in zip(
                    reversed(stmt.left.path[1:]),
                    reversed(old_dicts)):
                new_value = astlib.InstanceValue(
                    type_, {**dict_, **{elem: new_value}})
            context.env[root]["expr"] = new_value
            if root_expr_raw in A(astlib.Name):
                context.env[root_expr_raw]["expr"] = new_value

    @layers.register(astlib.For)
    def for_stmt(self, stmt):
        def register_name(name, expr):
            evaluated = self.eval(expr)
            context.env[name] = {
                "node_type": astlib.NodeT.var,
                "type": infer_type(evaluated),
                "expr": evaluated
            }
        context.env.add_scope()
        container = self.eval(stmt.container)
        names = stmt.names
        for elem in container:
            if len(names) > 1:
                for name, pair_elem in zip(names, elem):
                    register_name(name, pair_elem)
            else:
                register_name(names[0], elem)
            result = self.b(stmt.body)
            if result is not None:
                context.env.remove_scope()
                return result
        context.env.remove_scope()

    @layers.register(astlib.Return)
    def return_stmt(self, stmt):
        return self.eval(stmt.expr)

    @layers.register(astlib.FuncCall)
    def func_call(self, func_call):
        info = context.env[func_call.name]
        if info["node_type"] == astlib.NodeT.struct:
            info = info["methods"][defs.SPEC_METHOD_INIT]
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

    def method_call(self, base, func_call):
        # TODO: add support for changing fields
        converted_base = self.eval(base)
        type_ = infer_type(converted_base)
        args = [base] + [self.eval(arg) for arg in func_call.args]
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
        if expr in A(astlib.Name):
            return self.eval(context.env[expr]["expr"])
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
            method_name = defs.OPERATOR_TO_METHOD[expr.op]
            return self.py_method_call(
                expr.left, astlib.FuncCall(method_name, [expr.right]))
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
        elif expr in A(int, str, list, set, dict, astlib.InstanceValue):
            return expr
        else:
            # support other exprs
            errors.later()

    def declaration(self, decl):
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
