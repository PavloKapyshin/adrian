import sys

from .context import context

_SYNTAX_ERROR = "syntax error on line {line}"
_ILLEGAL_CHAR = "illegal character '{char}'"
_NOT_NOW = "try that out in later versions, key = {key}"
_WRONG_N_ARGS = "wrong number of arguments, got {got}, expected {expected}"

MODULE = "unsupported_module"
CUSTOM_OBJMETHOD = "custom_object_method"
TYPE_INFERENCE = "type_inference"
EXPR_INFERENCE = "expr_inference"
BAD = "bad things happened"
STRANGE_STMT = "strange statement"
LATER = "just wait.."


class CompileTimeError(Exception):

    def __init__(self, message):
        self.message = message


def syntax_error(line):
    _error(_SYNTAX_ERROR, line=line)

def illegal_char(char):
    _error(_ILLEGAL_CHAR, char=char)

def not_now(key):
    _error(_NOT_NOW, key=key)

def wrong_n_args(got, expected):
    _error(_WRONG_N_ARGS, got=got, expected=expected)


def _error(msg, **keywords):
    message = "Error: {0}.".format(msg.format_map(keywords))
    if context.exit_on_error:
        print(message, file=sys.stderr)
        sys.exit(1)
    raise CompileTimeError(message)
