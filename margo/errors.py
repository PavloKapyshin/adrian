"""Defines error messages and functions."""

import sys


# Error messages.
_SYNTAX_ERROR = "syntax error"
_ILLEGAL_CHAR = "illegal character '{char}'"

_BAD_NAME_FOR_VARIABLE = "bad name '{name}' for variable"
_BAD_NAME_FOR_CONSTANT = "bad name '{name}' for constant"
_BAD_NAME_FOR_MODULE = "bad name '{name}' for module"
_BAD_NAME_FOR_TYPE = "bad name '{name}' for type"
_BAD_NAME_IN_EXPR = "bad name '{name}' in expression"

_NON_EXISTING_NAME = "non existing name '{name}'"
_NON_EXISTING_MODULE = "non existing module '{module}'"
_CANT_REASSIGN_BUILTIN = "can't reassign builtin '{name}'"
_CANT_FIND_NAME_IN_MODULE = "can't find name '{name}' in module '{module_name}'"

_TYPE_OF_NAME_AND_TYPE_OF_VALUE_ARE_NOT_EQUAL = (
    "type ('{type_of_name}') of name '{name}' and "
    "type ('{type_of_expr}') of expression are not equal")

_NOT_IMPLEMENTED = "not implemented"


class CompilationError(Exception):

    def __init__(self, message):
        self.message = message


def syntax_error(line, exit_on_error):
    _error(line, exit_on_error, _SYNTAX_ERROR)


def illegal_char(line, exit_on_error, char):
    _error(line, exit_on_error, _ILLEGAL_CHAR, char=char)


def bad_name_for_variable(line, exit_on_error, name):
    _error(line, exit_on_error, _BAD_NAME_FOR_VARIABLE, name=name)


def bad_name_for_constant(line, exit_on_error, name):
    _error(line, exit_on_error, _BAD_NAME_FOR_CONSTANT, name=name)


def bad_name_for_module(line, exit_on_error, name):
    _error(line, exit_on_error, _BAD_NAME_FOR_MODULE, name=name)


def bad_name_for_type(line, exit_on_error, name):
    _error(line, exit_on_error, _BAD_NAME_FOR_TYPE, name=name)


def bad_name_in_expr(line, exit_on_error, name):
    _error(line, exit_on_error, _BAD_NAME_IN_EXPR, name=name)


def non_existing_name(line, exit_on_error, name):
    _error(line, exit_on_error, _NON_EXISTING_NAME, name=name)


def non_existing_module(line, exit_on_error, module):
    _error(line, exit_on_error, _NON_EXISTING_MODULE, module=module)


def cant_reassign_builtin(line, exit_on_error, name):
    _error(line, exit_on_error, _CANT_REASSIGN_BUILTIN, name=name)


def cant_find_name_in_module(line, exit_on_error, name, module_name):
    _error(
        line, exit_on_error,
        _CANT_FIND_NAME_IN_MODULE, name=name, module_name=module_name)


def type_of_name_and_type_of_expr_are_not_equal(
        line, exit_on_error, name, type_of_name, type_of_expr):
    _error(
        line, exit_on_error, _TYPE_OF_NAME_AND_TYPE_OF_VALUE_ARE_NOT_EQUAL,
        name=name, type_of_name=type_of_name, type_of_expr=type_of_expr)


def not_implemented(line, exit_on_error):
    _error(line, exit_on_error, _NOT_IMPLEMENTED)


def _error(line, exit_on_error, msg, **keywords):
    base_msg = "Error on line {line}: {{0}}.".format(line=line)
    if line <= 0:
        base_msg = "Error: {0}."
    message = base_msg.format(msg.format_map(keywords))
    if exit_on_error:
        print(message, file=sys.stderr)
        sys.exit(1)
    raise CompilationError(message)
