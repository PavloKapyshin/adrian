import re
import copy
from functools import partial

from . import astlib, env


PY_MODULE = "py"
PRINT = "print"

APPEND = "append"

LEN = "length"
INT = "Int"
STR = "Str"
LIST = "List"


RESERVED_WORDS = {
    keyword: keyword.upper()
    for keyword in (
        "var", "let", "struct", "return",
        "fun", "adt", "protocol", "if",
        "else", "elif", "not", "and",
        "or", "while"
    )
}

NAME_REGEX = re.compile(r"[a-z][a-zA-Z0-9]*")
FUNC_REGEX = NAME_REGEX
TYPE_REGEX = re.compile(r"[A-Z][a-zA-Z0-9]*")
MODULE_REGEX = re.compile(r"[a-z][a-z_0-9]*")
METHOD_REGEX = re.compile(
    r"((__)[a-z][a-zA-Z0-9]*(__))|([a-z][a-zA-Z0-9]*)")
COMMON_REGEX = "".join([
    "(",
    ")|(".join([regex.pattern
        for regex in (NAME_REGEX, FUNC_REGEX,
        TYPE_REGEX, MODULE_REGEX, METHOD_REGEX)]),
    ")"])
T_STRING = "t"
F_STRING = "f"
U_STRING = "u"

INIT_METHOD = "__init__"
COPY_METHOD = "__copy__"
ADD_METHOD = "__add__"
SUB_METHOD = "__sub__"
MUL_METHOD = "__mul__"
DIV_METHOD = "__div__"
EQ_METHOD = "__eq__"
NEQ_METHOD = "__neq__"
GT_METHOD = "__gt__"
LT_METHOD = "__lt__"
LTE_METHOD = "__lte__"
GTE_METHOD = "__gte__"
AND_METHOD = "__and__"
OR_METHOD = "__or__"
NOT_METHOD = "__not__"
SELF = "self"
REF = "ref"
BOOL = "Bool"
TRUE = "True"
FALSE = "False"
OP_TO_METHOD = {
    "+": ADD_METHOD,
    "-": SUB_METHOD,
    "*": MUL_METHOD,
    "/": DIV_METHOD,
    "==": EQ_METHOD,
    "!=": NEQ_METHOD,
    ">=": GTE_METHOD,
    "<=": LTE_METHOD,
    "<": LT_METHOD,
    ">": GT_METHOD,
    "and": AND_METHOD,
    "or": OR_METHOD,
}


ENV = env.Env()
DEFAULT_MODULE_PATHS = ["library/"]
DEFAULT_CONTEXT_ARGUMENTS = {
    "env": copy.deepcopy(ENV),
    "exit_on_error": False,
    "module_paths": DEFAULT_MODULE_PATHS,
}
