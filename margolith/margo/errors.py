"""Defines error messages and functions."""

import sys

from .context import context


# Error messages.
_SYNTAX_ERROR = "syntax error on line {line}"
_ILLEGAL_CHAR = "illegal character '{char}'"

_NON_EXISTING_NAME = "non existing name '{name}'"
_CANT_REASSIGN = "can't reassign '{name}'"
_CANT_FIND_NAME_IN_MODULE = "can't find name '{name}' in module '{module_name}'"

_TYPES_ARE_NOT_EQUAL = "type '{type1}' and type '{type2}' are not equal"

_WRONG_NUMBER_OF_ARGS = "wrong number of arguments: expected {expected}, got {got}"

_NOT_IMPLEMENTED = "not implemented:"


class CompileTimeError(Exception):

    def __init__(self, message):
        self.message = message


def syntax_error(line):
    _error(_SYNTAX_ERROR, line=line)


def illegal_char(char):
    _error(_ILLEGAL_CHAR, char=char)


def non_existing_name(name):
    _error(_NON_EXISTING_NAME, name=name)


def cant_reassign(name):
    _error(_CANT_REASSIGN, name=name)


def cant_find_name_in_module(name, module_name):
    _error(_CANT_FIND_NAME_IN_MODULE, name=name, module_name=module_name)


def types_are_not_equal(type1, type2):
    _error(_TYPES_ARE_NOT_EQUAL, type1=type1, type2=type2)


def not_implemented(message):
    _error("{} {}".format(_NOT_IMPLEMENTED, message))


def wrong_number_of_args(expected, got):
    _error(
        _WRONG_NUMBER_OF_ARGS, expected=expected, got=got)


def _error(msg, **keywords):
    message = "Error: {0}.".format(msg.format_map(keywords))
    if context.exit_on_error:
        print(message, file=sys.stderr)
        sys.exit(1)
    raise CompileTimeError(message)
