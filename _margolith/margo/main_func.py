"""Collects some global statements to main function."""

from . import layers, astlib
from adrian import cgen


class MainFunc(layers.Layer):

    @layers.register(astlib.AST)
    def main(self, ast_, registry):
        main_func_body = []
        for node in ast_:
            if isinstance(node, (
                    cgen.Decl, cgen.FuncCall, cgen.Assignment, cgen.While)):
                main_func_body.append(node)
            else:
                yield node

        return_0 = cgen.Return(cgen.Val("0", type_=cgen.CTypes.int))
        main_func_body.append(return_0)

        yield cgen.make_main0(*main_func_body)