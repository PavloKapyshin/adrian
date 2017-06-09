"""Defines error messages and functions."""

import sys


# Error messages.
_SYNTAX_ERROR = "syntax error"
_ILLEGAL_CHAR = "illegal character '{char}'"

_BAD_NAME_FOR_VARIABLE = "bad name '{name}' for variable"
_BAD_NAME_FOR_CONSTANT = "bad name '{name}' for constant"
_BAD_NAME_FOR_FUNCTION = "bad name '{name}' for function"
_BAD_NAME_FOR_MODULE = "bad name '{name}' for module"
_BAD_NAME_FOR_TYPE = "bad name '{name}' for type"
_BAD_NAME_IN_EXPR = "bad name '{name}' in expression"

_NON_EXISTING_NAME = "non existing name '{name}'"
_NON_EXISTING_TYPE = "non existing type '{type_}'"
_NON_EXISTING_MODULE = "non existing module '{module}'"
_CANT_REASSIGN_BUILTIN = "can't reassign builtin '{name}'"
_CANT_FIND_NAME_IN_MODULE = "can't find name '{name}' in module '{module_name}'"

_TYPES_ARE_NOT_EQUAL = "type '{type1}' and type '{type2}' are not equal"

_WRONG_NUMBER_OF_ARGS = "wrong number of arguments: expected {expected}, got {got}"

_NOT_IMPLEMENTED = "not implemented"


class CompilationError(Exception):

    def __init__(self, message):
        self.message = message


def syntax_error(position, exit_on_error):
    _error(position, exit_on_error, _SYNTAX_ERROR)


def illegal_char(position, exit_on_error, char):
    _error(position, exit_on_error, _ILLEGAL_CHAR, char=char)


def bad_name_for_variable(position, exit_on_error, name):
    _error(position, exit_on_error, _BAD_NAME_FOR_VARIABLE, name=name)


def bad_name_for_function(position, exit_on_error, name):
    _error(position, exit_on_error, _BAD_NAME_FOR_FUNCTION, name=name)


def bad_name_for_constant(position, exit_on_error, name):
    _error(position, exit_on_error, _BAD_NAME_FOR_CONSTANT, name=name)


def bad_name_for_module(position, exit_on_error, name):
    _error(position, exit_on_error, _BAD_NAME_FOR_MODULE, name=name)


def bad_name_for_type(position, exit_on_error, name):
    _error(position, exit_on_error, _BAD_NAME_FOR_TYPE, name=name)


def bad_name_in_expr(position, exit_on_error, name):
    _error(position, exit_on_error, _BAD_NAME_IN_EXPR, name=name)


def non_existing_name(position, exit_on_error, name):
    _error(position, exit_on_error, _NON_EXISTING_NAME, name=name)


def non_existing_type(position, exit_on_error, type_):
    _error(position, exit_on_error, _NON_EXISTING_TYPE, type_=type_)


def non_existing_module(position, exit_on_error, module):
    _error(position, exit_on_error, _NON_EXISTING_MODULE, module=module)


def cant_reassign_builtin(position, exit_on_error, name):
    _error(position, exit_on_error, _CANT_REASSIGN_BUILTIN, name=name)


def cant_find_name_in_module(position, exit_on_error, name, module_name):
    _error(
        position, exit_on_error,
        _CANT_FIND_NAME_IN_MODULE, name=name, module_name=module_name)


def types_are_not_equal(position, exit_on_error, type1, type2):
    _error(
        position, exit_on_error, _TYPES_ARE_NOT_EQUAL,
        type1=type1, type2=type2)


def not_implemented(position, exit_on_error):
    _error(position, exit_on_error, _NOT_IMPLEMENTED)


def wrong_number_of_args(position, exit_on_error, expected, got):
    _error(position, exit_on_error, _WRONG_NUMBER_OF_ARGS, expected=expected, got=got)


def _error(position, exit_on_error, msg, **keywords):
    base_msg = "Error (line: {line}; column: {column}): {{0}}.".format(
        line=position.line, column=position.column)
    if position.line <= 0:
        base_msg = "Error: {0}."
    message = base_msg.format(msg.format_map(keywords))
    if exit_on_error:
        print(message, file=sys.stderr)
        sys.exit(1)
    raise CompilationError(message)
