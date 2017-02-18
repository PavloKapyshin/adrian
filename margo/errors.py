"""Defines error messages and functions."""

import sys


# Error messages.
_SYNTAX_ERROR = "syntax error"
_ILLEGAL_CHAR = "illegal character '{char}'"

_NON_EXISTING_NAME = "non existing name '{name}'"
_CANT_REASSIGN_BUILTIN = "can't reassign builtin '{name}'"


def syntax_error(line):
    _error(line, _SYNTAX_ERROR)


def illegal_char(line, char):
    _error(line, _ILLEGAL_CHAR, char=char)


def non_existing_name(line, name):
    _error(line, _NON_EXISTING_NAME, name=name)


def cant_reassign_builtin(line, name):
    _error(line, _CANT_REASSIGN_BUILTIN, name=name)


def _error(line, msg, exit=True, **keywords):
    base_msg = "Error on line {line}: {{0}}.".format(line=line)
    if line <= 0:
        base_msg = "Error: {0}."
    message = base_msg.format(msg.format_map(keywords))
    if exit:
        print(message, file=sys.stderr)
        sys.exit(1)
    # TODO: raise an exception.
