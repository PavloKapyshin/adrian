import re
import copy
from functools import partial

from adrian.cgen import libc
from . import astlib, env


# Compiler defs.
# First 50 types are reserved for compiler.
TYPE_TAG_START = 51
TYPE_TAG_TYPE = astlib.LiteralType(astlib.LiteralT.uint_fast64_t)
TYPE_TAG_NAME = "type_tag"
ENV = env.Env()
DEFAULT_MODULE_PATHS = ["library/"]
DEFAULT_CONTEXT_ARGUMENTS = {
    "env": copy.deepcopy(ENV),
    "exit_on_error": False,
    "module_paths": DEFAULT_MODULE_PATHS,
    "clibs_includes": None,
    "i_count": 0,
}

RESERVED_WORDS = {
    keyword: keyword.upper()
    for keyword in (
    "var", "let", "struct",
    "return", "fun", "adt",
    "protocol", "if", "else", "elif", "not", "and", "or", "while"
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

# Adrian lang defs
INIT_METHOD = "__init__"
DEINIT_METHOD = "__deinit__"
COPY_METHOD = "__copy__"
REF = "ref"
BOOL = "Bool"
BOOL_TRANSLATION = astlib.DataMember(astlib.DataT.module, "c",
    astlib.Name("IntFast8"))
TRUE = "True"
TRUE_TRANSLATION = astlib.Callable(
    astlib.CallableT.struct, astlib.Empty,
    BOOL_TRANSLATION, [astlib.Literal(astlib.LiteralT.integer, "1")])
FALSE = "False"
FALSE_TRANSLATION = astlib.Callable(
    astlib.CallableT.struct, astlib.Empty,
    BOOL_TRANSLATION, [astlib.Literal(astlib.LiteralT.integer, "0")])
T_STRING = "t"
I_STRING = "i"
F_STRING = "f"
U_STRING = "u"

NOT_METHOD = "__not__"
OP_TO_METHOD = {
    "+": "__add__",
    "-": "__sub__",
    "*": "__mul__",
    "/": "__div__",
    "==": "__eq__",
    "!=": "__neq__",
    ">=": "__gte__",
    "<=": "__lte__",
    "<": "__lt__",
    ">": "__gt__",
    "and": "__and__",
    "or": "__or__",
}

# Adrian's c module defs
CMODULE = "c"
CMODULE_FILE = "adrian_c"
FREE_FUNC = "free"
MALLOC_FUNC = "malloc"
SIZEOF = "sizeof"
CFUNCS = (
    FREE_FUNC,
    MALLOC_FUNC,
)

MALLOC_FUNC_DESCR = libc.malloc
FREE_FUNC_DESCR = libc.free

_c_module_member = partial(
    astlib.DataMember, astlib.DataT.module, astlib.Name(CMODULE))


def _add_cnumeric_type(tname):
    member = _c_module_member(astlib.Name(tname))
    ENV[member] = {
        "node_type": astlib.NodeT.struct,
        "params": [],
        "fields": {},
        "methods": {
            "__init__": {
                "type_": member,
                "args": [
                    ("literal", astlib.LiteralType(astlib.LiteralT.integer))
                ],
                "body": []
            },
            "__deinit__": {
                "type_": astlib.Void(),
                "args": [
                    ("self", member)
                ],
                "body": []
            },
            "__copy__": {
                "type_": member,
                "args": [
                    ("self", member)
                ],
                "body": []
            },
            "__add__": {
                "type_": member,
                "args": [
                    ("self", member), ("other", member)
                ],
                "body": []
            },
            "__sub__": {
                "type_": member,
                "args": [
                    ("self", member), ("other", member)
                ],
                "body": []
            },
            "__mul__": {
                "type_": member,
                "args": [
                    ("self", member), ("other", member)
                ],
                "body": []
            },
            "__div__": {
                "type_": member,
                "args": [
                    ("self", member), ("other", member)
                ],
                "body": []
            },
            "__eq__": {
                "type_": BOOL_TRANSLATION,
                "args": [
                    ("self", member), ("other", member)
                ],
                "body": []
            },
            "__neq__": {
                "type_": BOOL_TRANSLATION,
                "args": [
                    ("self", member), ("other", member)
                ],
                "body": []
            },
            "__gte__": {
                "type_": BOOL_TRANSLATION,
                "args": [
                    ("self", member), ("other", member)
                ],
                "body": []
            },
            "__lte__": {
                "type_": BOOL_TRANSLATION,
                "args": [
                    ("self", member), ("other", member)
                ],
                "body": []
            },
            "__gt__": {
                "type_": BOOL_TRANSLATION,
                "args": [
                    ("self", member), ("other", member)
                ],
                "body": []
            },
            "__lt__": {
                "type_": BOOL_TRANSLATION,
                "args": [
                    ("self", member), ("other", member)
                ],
                "body": []
            },
            "__and__": {
                "type_": BOOL_TRANSLATION,
                "args": [
                    ("self", member), ("other", member)
                ],
                "body": []
            },
            "__or__": {
                "type_": BOOL_TRANSLATION,
                "args": [
                    ("self", member), ("other", member)
                ],
                "body": []
            },
            "__not__": {
                "type_": BOOL_TRANSLATION,
                "args": [
                    ("self", member)
                ],
                "body": []
            },
        }
    }


_add_cnumeric_type("IntFast8")
_add_cnumeric_type("IntFast16")
_add_cnumeric_type("IntFast32")
_add_cnumeric_type("IntFast64")
_add_cnumeric_type("UIntFast8")
_add_cnumeric_type("UIntFast16")
_add_cnumeric_type("UIntFast32")
_add_cnumeric_type("UIntFast64")
