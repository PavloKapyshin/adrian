"""Checks naming conventions."""

from . import defs, layers, astlib, errors
from .context import context


def _matches_maker(regex, error):
    def inner(name):
        if not regex.fullmatch(str(name)):
            error(context.position, context.exit_on_error, name)
        return name
    return inner


var_name = _matches_maker(
    defs.VAR_NAME_REGEX, errors.bad_name_for_var)
type_name = _matches_maker(
    defs.TYPE_NAME_REGEX, errors.bad_name_for_type)
module_name = _matches_maker(
    defs.MODULE_NAME_REGEX, errors.bad_name_for_module)


class NamingRules(layers.Layer):

    @layers.preregister(astlib.VariableName)
    def _var_name(self, name):
        yield var_name(name)

    @layers.preregister(astlib.TypeName)
    def _type_name(self, name):
        yield type_name(name)

    @layers.preregister(astlib.ModuleName)
    def _module_name(self, name):
        yield module_name(name)
