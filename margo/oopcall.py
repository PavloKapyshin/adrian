"""Translates method calls into function calls."""

from . import layers, astlib, errors
from .context import context


class OOPCall(layers.Layer):

    @layers.register(astlib.MethodCall)
    def method_call(self, call):
        errors.not_implemented("method calls are not supported")
