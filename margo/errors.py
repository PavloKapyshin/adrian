"""Defines error messages and functions."""

import sys


# Error messages.
_SYNTAX_ERROR = "syntax error"
_ILLEGAL_CHAR = "illegal character '{char}'"

_NON_EXISTING_NAME = "non existing name '{name}'"
_CANT_REASSIGN_BUILTIN = "can't reassign builtin '{name}'"

_TYPE_OF_NAME_AND_TYPE_OF_VALUE_ARE_NOT_EQUAL = (
    "type ('{type_of_name}') of name '{name}' and type of "
    "value ('{type_of_value}') are not equal")

_NOT_IMPLEMENTED = "not implemented"


def syntax_error(line, exit_on_error):
    _error(line, exit_on_error, _SYNTAX_ERROR)


def illegal_char(line, exit_on_error, char):
    _error(line, exit_on_error, _ILLEGAL_CHAR, char=char)


def non_existing_name(line, exit_on_error, name):
    _error(line, exit_on_error, _NON_EXISTING_NAME, name=name)


def cant_reassign_builtin(line, exit_on_error, name):
    _error(line, exit_on_error, _CANT_REASSIGN_BUILTIN, name=name)


def type_of_name_and_type_of_value_are_not_equal(
        line, exit_on_error, name, type_of_name, type_of_value):
    _error(
        line, exit_on_error, _TYPE_OF_NAME_AND_TYPE_OF_VALUE_ARE_NOT_EQUAL,
        name=name, type_of_name=type_of_name, type_of_value=type_of_value)


def not_implemented(line, exit_on_error):
    _error(line, exit_on_error, _NOT_IMPLEMENTED)


def _error(line, exit_on_error, msg, **keywords):
    base_msg = "Error on line {line}: {{0}}.".format(line=line)
    if line <= 0:
        base_msg = "Error: {0}."
    message = base_msg.format(msg.format_map(keywords))
    print(message, file=sys.stderr)
    if exit_on_error:
        sys.exit(1)
