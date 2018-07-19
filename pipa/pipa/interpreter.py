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
        elif expr in A(list):
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
        else:
            # support other exprs
            errors.later()

    @layers.register(astlib.LetDecl)
    def let_declaration(self, declaration):
        context.env[declaration.name] = {
            "node_type": astlib.NodeT.let,
            "type": (declaration.type_
                if declaration.type_ not in A(astlib.Empty)
                else infer_type(declaration.expr)),
            "expr": declaration.expr
        }

    @layers.register(astlib.FuncCall)
    def func_call(self, func_call):
        errors.later()

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
            if len(func_call.args) == 0:
                print()
            for arg in func_call.args:
                arg = self.adr_to_py(arg)
                if arg in A(set) and len(arg) == 0:
                    # I just don't like `set()` to be printed.
                    print("{}")
                else:
                    print(arg)
        else:
            # support other funcs
            errors.later()

    def py_method_call(self, base, func_call):
        type_ = infer_type(base)
        base = self.adr_to_py(base)
        args = [self.adr_to_py(arg) for arg in func_call.args]
        if func_call.name == defs.METHOD_SPLIT:
            return base.split(args[0])
        elif func_call.name == defs.METHOD_ADD:
            if type_.name == defs.TYPE_SET:
                return base.union(args[0])
            elif type_.name == defs.TYPE_DICT:
                result = base
                for key, val in args[0].items():
                    result[key] = val
                return result
            return base + args[0]
        elif func_call.name == defs.METHOD_SUB:
            return base - args[0]
        elif func_call.name == defs.METHOD_MUL:
            return base * args[0]
        elif func_call.name == defs.METHOD_DIV:
            result = base / args[0]
            up, down = result.as_integer_ratio()
            if down == 1:
                return up
            return result
        else:
            # support other methods
            errors.later()
