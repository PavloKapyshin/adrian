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

    def _body_stmt(self, stmt, registry):
        # HARDCORE! FIXME!
        return layers.transform_ast(
            [stmt], registry=registry)[0]

    def _body(self, body, registry):
        if isinstance(body, astlib.Empty):
            return astlib.Empty()
        return astlib.Body(
            self._body_stmt(body.stmt, registry),
            self._body(body.rest, registry))

    @layers.preregister(astlib.FuncDecl)
    def _func_decl(self, func_decl):
        # TODO: inference function's return type from return statements.
        # For now we have only empty functions and we dont have None type.
        registry = TypeInference().get_registry()
        yield layers.create_with(func_decl, body=self._body(func_decl.body, registry))
