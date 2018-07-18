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
            if len(expr) > 2 or expr[0] not in A(astlib.Name):
                errors.later()
            info = context.env[expr[0]]
            if expr[1] in A(astlib.FuncCall):
                if is_py_type(info["type"]):
                    return self.py_method_call(expr[0], expr[1])
                else:
                    # TODO: think about methods
                    errors.later()
            else:
                # TODO: think about struct fields
                errors.later()
        elif expr in A(astlib.PyFuncCall, astlib.PyTypeCall):
            return self.py_call(expr)
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
        if func_call.name == defs.METHOD_SPLIT:
            expr = self.adr_to_py(base)
            return expr.split(self.adr_to_py(func_call.args[0]))
        else:
            # support other methods
            errors.later()
