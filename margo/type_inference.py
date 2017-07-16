"""Inferences types where needed."""

from . import layers, astlib, errors, inferencelib
from .context import context


class TypeInference(layers.Layer):

    @layers.preregister(astlib.Decl)
    def _decl(self, decl):
        if isinstance(decl.type_, astlib.Empty):
            yield layers.create_with(
                decl, type_=inferencelib.get_type_from_expr(decl.expr))
        else:
            yield decl

    @layers.preregister(astlib.FuncDecl)
    def _func_decl(self, func_decl):
        # TODO: inference function's return type from return statements.
        # For now we have only empty functions and we dont have None type.
        yield func_decl
