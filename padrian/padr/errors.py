import sys


_SYNTAX_ERROR = "syntax error on line {line}"
_ILLEGAL_CHAR = "illegal character '{char}'"


class CompileTimeError(Exception):

    def __init__(self, message):
        self.message = message


def syntax_error(exit_on_error, line):
    _error(exit_on_error, _SYNTAX_ERROR, line=line)


def illegal_char(exit_on_error, char):
    _error(exit_on_error, _ILLEGAL_CHAR, char=char)


def _error(exit_on_error, msg, **keywords):
    message = "Error: {0}.".format(msg.format_map(keywords))
    if exit_on_error:
        print(message, file=sys.stderr)
        sys.exit(1)
    raise CompileTimeError(message)