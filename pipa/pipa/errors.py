import sys

from .context import context


class CompileTimeError(Exception):
    def __init__(self, message):
        self.message = message


def _error(msg, **keywords):
    message = "Error:\n  {0}.".format(msg.format_map(keywords))
    if context.exit_on_error:
        print(message, file=sys.stderr)
        sys.exit(1)
    raise CompileTimeError(message)


def _error_maker(message, *args_names):
    def helper(*args):
        _error(
            message,
            **{name: value for name, value in zip(args_names, args)})
    return helper


_UNKNOWN_NAME = "unknown name {name}"
_LATER = ":P just wait"
_FATAL = "FATAL ERROR: {message}"
_SYNTAX_ERROR = "syntax error on {line}"

unknown_name = _error_maker(_UNKNOWN_NAME, "name")
later = _error_maker(_LATER)
fatal = _error_maker(_FATAL, "message")
syntax_error = _error_maker(_SYNTAX_ERROR, "line")
