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


def syntax_error(line):
    _error(line, _SYNTAX_ERROR)


def illegal_char(line, char):
    _error(line, _ILLEGAL_CHAR, char=char)


def non_existing_name(line, name):
    _error(line, _NON_EXISTING_NAME, name=name)


def cant_reassign_builtin(line, name):
    _error(line, _CANT_REASSIGN_BUILTIN, name=name)


def type_of_name_and_type_of_value_are_not_equal(
        line, name, type_of_name, type_of_value):
    _error(
        line, _TYPE_OF_NAME_AND_TYPE_OF_VALUE_ARE_NOT_EQUAL,
        name=name, type_of_name=type_of_name, type_of_value=type_of_value)


def not_implemented(line):
    _error(line, _NOT_IMPLEMENTED)


def _error(line, msg, exit=True, **keywords):
    base_msg = "Error on line {line}: {{0}}.".format(line=line)
    if line <= 0:
        base_msg = "Error: {0}."
    message = base_msg.format(msg.format_map(keywords))
    if exit:
        print(message, file=sys.stderr)
        sys.exit(1)
    # TODO: raise an exception.
