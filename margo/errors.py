"""Defines error messages and functions."""

import sys


# Error messages.
_SYNTAX_ERROR = "syntax error"
_ILLEGAL_CHAR = "illegal character '{char}'"

_BAD_NAME_FOR_VAR = "bad name '{name}' for variable"
_BAD_NAME_FOR_CONST = "bad name '{name}' for constant"
_BAD_NAME_FOR_FUNC = "bad name '{name}' for function"
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


def syntax_error(line, exit_on_error):
    _error(line, exit_on_error, _SYNTAX_ERROR)


def illegal_char(line, exit_on_error, char):
    _error(line, exit_on_error, _ILLEGAL_CHAR, char=char)


def bad_name_for_var(line, exit_on_error, name):
    _error(line, exit_on_error, _BAD_NAME_FOR_VAR, name=name)


def bad_name_for_func(line, exit_on_error, name):
    _error(line, exit_on_error, _BAD_NAME_FOR_FUNC, name=name)


def bad_name_for_const(line, exit_on_error, name):
    _error(line, exit_on_error, _BAD_NAME_FOR_CONST, name=name)


def bad_name_for_module(line, exit_on_error, name):
    _error(line, exit_on_error, _BAD_NAME_FOR_MODULE, name=name)


def bad_name_for_type(line, exit_on_error, name):
    _error(line, exit_on_error, _BAD_NAME_FOR_TYPE, name=name)


def bad_name_in_expr(line, exit_on_error, name):
    _error(line, exit_on_error, _BAD_NAME_IN_EXPR, name=name)


def non_existing_name(line, exit_on_error, name):
    _error(line, exit_on_error, _NON_EXISTING_NAME, name=name)


def non_existing_type(line, exit_on_error, type_):
    _error(line, exit_on_error, _NON_EXISTING_TYPE, type_=type_)


def non_existing_module(line, exit_on_error, module):
    _error(line, exit_on_error, _NON_EXISTING_MODULE, module=module)


def cant_reassign_builtin(line, exit_on_error, name):
    _error(line, exit_on_error, _CANT_REASSIGN_BUILTIN, name=name)


def cant_find_name_in_module(line, exit_on_error, name, module_name):
    _error(
        line, exit_on_error,
        _CANT_FIND_NAME_IN_MODULE, name=name, module_name=module_name)


def types_are_not_equal(line, exit_on_error, type1, type2):
    _error(
        line, exit_on_error, _TYPES_ARE_NOT_EQUAL,
        type1=type1, type2=type2)


def not_implemented():
    _error(0, True, _NOT_IMPLEMENTED)


def wrong_number_of_args(line, exit_on_error, expected, got):
    _error(
        line, exit_on_error, _WRONG_NUMBER_OF_ARGS,
        expected=expected, got=got)


def _error(line, exit_on_error, msg, **keywords):
    base_msg = "Error (line: {line}): {{0}}.".format(line=line)
    if line <= 0:
        base_msg = "Error: {0}."
    message = base_msg.format(msg.format_map(keywords))
    if exit_on_error:
        print(message, file=sys.stderr)
        sys.exit(1)
    raise CompilationError(message)
