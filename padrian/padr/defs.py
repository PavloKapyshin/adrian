import re
from functools import partial

from adrian.cgen import libc

from . import env, astlib


# Compiler defs.
ENV = env.Env()
DEFAULT_MODULE_PATHS = []

RESERVED_WORDS = {
    keyword: keyword.upper()
    for keyword in (
        "var", "let", "struct",
        "return", "fun", "adt",
        "protocol",
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
T_STRING = "t"

OP_TO_METHOD = {
    "+": "__add__",
    "-": "__sub__",
    "*": "__mul__",
    "/": "__div__",
}

# Adrian's c module defs
CMODULE = "c"
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
                ]
            },
            "__deinit__": {
                "type_": astlib.Void(),
                "args": [
                    ("self", member)
                ]
            },
            "__copy__": {
                "type_": member,
                "args": [
                    ("self", member)
                ]
            },
            "__add__": {
                "type_": member,
                "args": [
                    ("self", member), ("other", member)
                ]
            },
            "__sub__": {
                "type_": member,
                "args": [
                    ("self", member), ("other", member)
                ]
            },
            "__mul__": {
                "type_": member,
                "args": [
                    ("self", member), ("other", member)
                ]
            },
            "__div__": {
                "type_": member,
                "args": [
                    ("self", member), ("other", member)
                ]
            }
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
