"""Defines error messages and functions."""

import sys


# Error messages.
_SYNTAX_ERROR = "syntax error on line {line}"
_ILLEGAL_CHAR = "illegal character '{char}'"

_BAD_NAME_FOR_VAR = "bad name '{name}' for variable"
_BAD_NAME_FOR_CONST = "bad name '{name}' for constant"
_BAD_NAME_FOR_FUNC = "bad name '{name}' for function"
_BAD_NAME_FOR_METHOD = "bad name '{name}' for method"
_BAD_NAME_FOR_MODULE = "bad name '{name}' for module"
_BAD_NAME_FOR_TYPE = "bad name '{name}' for type"
_BAD_NAME_IN_EXPR = "bad name '{name}' in expression"

_NON_EXISTING_NAME = "non existing name '{name}'"
_NON_EXISTING_TYPE = "non existing type '{type_}'"
_NON_EXISTING_MODULE = "non existing module '{module}'"
_CANT_REASSIGN = "can't reassign '{name}'"
_CANT_FIND_NAME_IN_MODULE = "can't find name '{name}' in module '{module_name}'"

_TYPES_ARE_NOT_EQUAL = "type '{type1}' and type '{type2}' are not equal"

_WRONG_NUMBER_OF_ARGS = "wrong number of arguments: expected {expected}, got {got}"

_NOT_IMPLEMENTED = "not implemented:"


class CompileTimeError(Exception):

    def __init__(self, message):
        self.message = message


def syntax_error(exit_on_error, line):
    _error(exit_on_error, _SYNTAX_ERROR, line=line)


def illegal_char(exit_on_error, char):
    _error(exit_on_error, _ILLEGAL_CHAR, char=char)


def bad_name_for_var(exit_on_error, name):
    _error(exit_on_error, _BAD_NAME_FOR_VAR, name=name)


def bad_name_for_func(exit_on_error, name):
    _error(exit_on_error, _BAD_NAME_FOR_FUNC, name=name)


def bad_name_for_const(exit_on_error, name):
    _error(exit_on_error, _BAD_NAME_FOR_CONST, name=name)


def bad_name_for_module(exit_on_error, name):
    _error(exit_on_error, _BAD_NAME_FOR_MODULE, name=name)


def bad_name_for_method(exit_on_error, name):
    _error(exit_on_error, _BAD_NAME_FOR_METHOD, name=name)


def bad_name_for_type(exit_on_error, name):
    _error(exit_on_error, _BAD_NAME_FOR_TYPE, name=name)


def bad_name_in_expr(exit_on_error, name):
    _error(exit_on_error, _BAD_NAME_IN_EXPR, name=name)


def non_existing_name(exit_on_error, name):
    _error(exit_on_error, _NON_EXISTING_NAME, name=name)


def non_existing_type(exit_on_error, type_):
    _error(exit_on_error, _NON_EXISTING_TYPE, type_=type_)


def non_existing_module(exit_on_error, module):
    _error(exit_on_error, _NON_EXISTING_MODULE, module=module)


def cant_reassign(exit_on_error, name):
    _error(exit_on_error, _CANT_REASSIGN, name=name)


def cant_find_name_in_module(exit_on_error, name, module_name):
    _error(
        exit_on_error,
        _CANT_FIND_NAME_IN_MODULE, name=name, module_name=module_name)


def types_are_not_equal(exit_on_error, type1, type2):
    _error(
        exit_on_error, _TYPES_ARE_NOT_EQUAL,
        type1=type1, type2=type2)


def not_implemented(message):
    _error(True, "{} {}".format(_NOT_IMPLEMENTED, message))


def wrong_number_of_args(exit_on_error, expected, got):
    _error(
        exit_on_error, _WRONG_NUMBER_OF_ARGS,
        expected=expected, got=got)


def _error(exit_on_error, msg, **keywords):
    message = "Error: {0}.".format(msg.format_map(keywords))
    if exit_on_error:
        print(message, file=sys.stderr)
        sys.exit(1)
    raise CompileTimeError(message)
