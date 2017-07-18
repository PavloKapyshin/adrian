"""Mangles names."""

from . import cdefs, layers, astlib, errors
from .context import context


def mangle_name(name):
    return context.file_hash + str(name)


class NameMangling(layers.Layer):

    @layers.preregister(astlib.VariableName)
    def _var_name(self, name):
        yield layers.create_with(name, data=mangle_name(name))

    @layers.preregister(astlib.FunctionName)
    def _func_name(self, name):
        yield layers.create_with(name, data=mangle_name(name))

    @layers.preregister(astlib.TypeName)
    def _type_name(self, name):
        yield layers.create_with(name, data=mangle_name(name))

    # @layers.preregister(astlib.ModuleMember)
    # def _module_member(self, module):
    #     if module.name == cdefs.CMODULE_NAME:
    #         yield module
    #     else:
    #         errors.not_implemented()
