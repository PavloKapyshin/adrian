import sys

from .context import context

_SYNTAX_ERROR = "syntax error on line {line}"
_ILLEGAL_CHAR = "illegal character '{char}'"
_NOT_NOW = "try that out in later versions, key = {key}"
_WRONG_N_ARGS = "wrong number of arguments, got {got}, expected {expected}"
_CANNOT_FIND_FILE = "cannot find file {file}"
_UNKNOWN_STMT = "unknown statement"

_NO_SUCH_FIELD = "name '{parent}' of type '{parent_type}' have no field '{field}'"
_NO_SUCH_METHOD = "struct '{struct}' have no method '{method}'"
_UNKNOWN_NAME = "unknown name {name}"
_WRONG_NODE_TYPE = "found name {name}, but its nodetype {node_type} is not acceptable"
_INFER_TYPE = """cannot infer type from expression
(ast equivalent is {ast_node})"""
_INFER_EXPR = """cannot infer expression from type
(ast equivalent is {ast_node})"""
_NOT_IMPLEMENTED = "not implemented (func {func}): {msg}"

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


def no_such_field(parent, parent_type, field):
    _error(
        _NO_SUCH_FIELD, parent=parent,
        parent_type=parent_type, field=field)


def no_such_method(struct, method):
    _error(_NO_SUCH_METHOD, struct=struct, method=method)


def unknown_name(name):
    _error(_UNKNOWN_NAME, name=name)


def wrong_node_type(name, node_type):
    _error(_WRONG_NODE_TYPE, name=name, node_type=node_type)


def infer_type(ast_node):
    _error(_INFER_TYPE, ast_node=ast_node)


def infer_expr(ast_node):
    _error(_INFER_EXPR, ast_node=ast_node)


def not_implemented(msg, func):
    _error(_NOT_IMPLEMENTED, msg=msg, func=func)


def syntax_error(line):
    _error(_SYNTAX_ERROR, line=line)


def illegal_char(char):
    _error(_ILLEGAL_CHAR, char=char)


def not_now(key):
    _error(_NOT_NOW, key=key)


def wrong_n_args(got, expected):
    _error(_WRONG_N_ARGS, got=got, expected=expected)


def cannot_find_file(file_name):
    _error(_CANNOT_FIND_FILE, file=file_name)


def unknown_stmt():
    _error(_UNKNOWN_STMT)


def _error(msg, **keywords):
    message = "Warning: {0}.".format(msg.format_map(keywords))
    if context.exit_on_error:
        print(message, file=sys.stderr)
        sys.exit(1)
    raise CompileTimeError(message)
