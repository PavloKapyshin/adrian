import enum
import sys

from .context import context


class CompileTimeError(Exception):
    def __init__(self, message):
        self.message = message


@enum.unique
class Version(enum.Enum):
    v0m5 = "0.5"
    v0m9 = "0.9"
    v1m0 = "1.0"
    unknown = "unknown"


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
_NO_SUCH_METHOD = "no method {method} in struct {struct}"
_NO_SUCH_FIELD = "no field {field} in struct {struct}"
_TYPE_MISMATCH = "type mismatch, typeA {type1}, typeB {type2}"
_ARGS_NUMBER_MISMATCH = "wrong number of args, got {got}, expected {expected}"

_WRONG_NODETYPE = (
    "found name {name}, but it has non acceptable nodetype {nodetype}")
_CANNOT_GET_INFO = "cannot find info for {request} ({func})"
_CANNOT_INFER_TYPE = "cannot infer type"
_CANNOT_INFER_EXPR = "cannot infer expression"
_CANNOT_FIND_FILE = "cannot find file {file_name}"
_LATER = "not implemented, wait for v{version}"
_FATAL = "FATAL ERROR: {message}"


unknown_name = _error_maker(_UNKNOWN_NAME, "name")
no_such_method = _error_maker(_NO_SUCH_METHOD, "struct", "method")
no_such_field = _error_maker(_NO_SUCH_FIELD, "struct", "field")
type_mismatch = _error_maker(_TYPE_MISMATCH, "type1", "type2")
args_number_mismatch = _error_maker(_ARGS_NUMBER_MISMATCH, "got", "expected")

wrong_nodetype = _error_maker(_WRONG_NODETYPE, "request", "nodetype")
cannot_get_info = _error_maker(_CANNOT_GET_INFO, "node", "func")
cannot_infer_type = _error_maker(_CANNOT_INFER_TYPE)
cannot_infer_expr = _error_maker(_CANNOT_INFER_EXPR)

cannot_find_file = _error_maker(_CANNOT_FIND_FILE, "file_name")

later = _error_maker(_LATER, "version")
fatal = _error_maker(_FATAL, "message")
