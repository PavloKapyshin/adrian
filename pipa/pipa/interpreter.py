from . import astlib, layers, errors, defs
from .context import context
from .inference import infer_type
from .utils import A


def is_py_type(type_):
    return type_ in A(astlib.PyType)


class Interpreter(layers.Layer):

    def adr_to_py(self, expr):
        if expr in A(astlib.FuncCall):
            return self.func_call(expr)
        elif expr in A(astlib.Name):
            return self.adr_to_py(context.env[expr]["expr"])
        elif expr in A(astlib.StructPath):
            expr = expr.path
            if len(expr) > 2:
                errors.later()
            type_ = infer_type(expr[0])
            if expr[1] in A(astlib.FuncCall):
                if is_py_type(type_):
                    return self.py_method_call(expr[0], expr[1])
                else:
                    # TODO: think about methods
                    errors.later()
            else:
                # TODO: think about struct fields
                errors.later()
        elif expr in A(astlib.PyFuncCall, astlib.PyTypeCall):
            return self.py_call(expr)
        elif expr in A(astlib.Expr):
            method_name = defs.OPERATOR_TO_METHOD[expr.op]
            return self.py_method_call(
                expr.left, astlib.FuncCall(method_name, [expr.right]))
        elif expr in A(astlib.Literal):
            # Already translated, nothing to do.
            # Just checking in containers...
            if expr.type_ == astlib.LiteralT.number:
                return int(expr.literal)
            elif expr.type_ == astlib.LiteralT.vector:
                return [self.adr_to_py(elem) for elem in expr.literal]
            elif expr.type_ == astlib.LiteralT.set_:
                return {self.adr_to_py(elem) for elem in expr.literal}
            elif expr.type_ == astlib.LiteralT.dict_:
                return {
                    self.adr_to_py(key): self.adr_to_py(val)
                    for key, val in expr.literal.items()}
            return expr.literal
        elif expr in A(int, str, list, set, dict):
            return expr
        else:
            # support other exprs
            errors.later()

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

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        if stmt.left not in A(astlib.Name):
            errors.later()
        def unlazy(expr, name):
            if expr in A(astlib.Name):
                return (context.env[expr]["expr"] if expr == name else expr)
            elif expr in A(astlib.Expr):
                return astlib.Expr(
                    unlazy(expr.left, name), expr.op, unlazy(expr.right, name))
            elif expr in A(astlib.PyTypeCall):
                return expr
            else:
                errors.later()

        # left must be declarated as variable
        # type of right must be equal to type of left
        if stmt.op == defs.EQ:
            expr = unlazy(stmt.right, stmt.left)
        else:
            expr = unlazy(
                astlib.Expr(
                    stmt.left, defs.ASSIGNMENT_OP_TO_EXPR_OP[stmt.op],
                    stmt.right),
                stmt.left)
        context.env[stmt.left]["expr"] = expr

    @layers.register(astlib.For)
    def for_stmt(self, stmt):
        def register_name(name, expr):
            context.env[name] = {
                "node_type": astlib.NodeT.var,
                "type": infer_type(expr),
                "expr": expr
            }
        context.env.add_scope()
        container = self.adr_to_py(stmt.container)
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
        return self.adr_to_py(stmt.expr)

    @layers.register(astlib.FuncCall)
    def func_call(self, func_call):
        info = context.env[func_call.name]
        context.env.add_scope()
        for (name, type_), expr in zip(info["args"], func_call.args):
            context.env[name] = {
                "node_type": astlib.NodeT.var,
                "type": type_,
                "expr": expr
            }
        return_val = self.b(info["body"])
        context.env.remove_scope()
        return return_val

    @layers.register(astlib.StructPath)
    def struct_path(self, struct_path):
        return self.adr_to_py(struct_path)

    @layers.register(astlib.PyFuncCall)
    def py_call(self, func_call):
        def translate_iterable(iterable):
            return [self.adr_to_py(element) for element in iterable]

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
                self.adr_to_py(key): self.adr_to_py(val)
                for key, val in literal.items()}
        elif func_call.name == defs.FUNC_PRINT:
            for arg in func_call.args:
                arg = self.adr_to_py(arg)
                print(arg, end="")
            print()
        elif func_call.name == defs.FUNC_TO_STR:
            return str(self.adr_to_py(func_call.args[0]))
        elif func_call.name == defs.FUNC_TO_INT:
            return int(self.adr_to_py(func_call.args[0]))
        elif func_call.name == defs.FUNC_TO_SET:
            return set(self.adr_to_py(func_call.args[0]))
        elif func_call.name == defs.FUNC_TO_LIST:
            return list(self.adr_to_py(func_call.args[0]))
        elif func_call.name == defs.FUNC_LEN:
            return len(self.adr_to_py(func_call.args[0]))
        elif func_call.name == defs.FUNC_READ_FILE:
            file_path = self.adr_to_py(func_call.args[0])
            with open(file_path, "r") as file:
                contents = file.read()
            return contents
        elif func_call.name == defs.FUNC_WRITE_FILE:
            file_path = self.adr_to_py(func_call.args[0])
            contents = self.adr_to_py(func_call.args[1])
            with open(file_path, "w") as file:
                file.write(contents)
        else:
            # support other funcs
            errors.later()

    def py_method_call(self, base, func_call):
        type_ = infer_type(base)
        converted_base = self.adr_to_py(base)
        args = [self.adr_to_py(arg) for arg in func_call.args]
        if func_call.name == defs.METHOD_SPLIT:
            return converted_base.split(args[0])
        elif func_call.name == defs.METHOD_VALUES:
            return list(converted_base.values())
        elif func_call.name == defs.METHOD_KEYS:
            return list(converted_base.keys())
        elif func_call.name == defs.METHOD_ITEMS:
            return converted_base.items()
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

    def declaration(self, decl):
        context.env[decl.name] = {
            "node_type": (astlib.NodeT.var
                    if decl in A(astlib.VarDecl)
                    else astlib.NodeT.let),
            "type": (decl.type_
                if decl.type_ not in A(astlib.Empty)
                else infer_type(decl.expr)),
            "expr": decl.expr
        }
