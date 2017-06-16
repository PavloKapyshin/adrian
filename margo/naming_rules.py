"""Checks naming conventions."""

from . import defs, layers, astlib, errors


# def _matches_maker(regex, error):
#     def inner(name):
#         if regex.fullmatch(str(name)):
#             return name
#         error(context.position, context.exit_on_error, name)
#     return inner


class NamingRules(layers.Layer):

    @layers.preregister(astlib.VariableName)
    def _var_name(self, name):
        if not defs.VAR_NAME_REGEX.fullmatch(str(name)):
            errors.bad_name_for_var(
                context.position, context.exit_on_error, name)
        yield name

    @layers.preregister(astlib.TypeName)
    def _type_name(self, name):
        if not defs.TYPE_NAME_REGEX.fullmatch(str(name)):
            errors.bad_name_for_type(
                context.position, context.exit_on_error, name)
        yield name

    @layers.preregister(astlib.ModuleName)
    def _module_name(self, name):
        if not defs.MODULE_NAME_REGEX.fullmatch(str(name)):
            errors.bad_name_for_module(
                context.position, context.exit_on_error, name)
        yield name

    #
    # _var_name = _matches_maker(
    #     defs.VAR_NAME_REGEX, errors.bad_name_for_var)
    # _type_name = _matches_maker(
    #     defs.TYPE_NAME_REGEX, errors.bad_name_for_type)
    # _module_name = _matches_maker(
    #     defs.MODULE_NAME_REGEX, errors.bad_name_for_module)

    # layers.preregister(_var_name, astlib.VariableName)
    # layers.preregister(_type_name, astlib.TypeName)
    # layers.preregister(_module_name, astlib.ModuleName)
