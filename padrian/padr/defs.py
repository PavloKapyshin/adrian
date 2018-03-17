import re
from functools import partial

from . import env, astlib

# Compiler defs.
T_STRING = "t"
ENV = env.Env()

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

OP_TO_METHOD = {
    "+": "__add__",
    "-": "__sub__",
    "*": "__mul__",
    "/": "__div__",
}

# Adrian's c module defs
CMODULE = "c"

_c_module_member = partial(
    astlib.DataMember, astlib.DataT.module, astlib.Name(CMODULE))

def _add_cnumeric_type(tname):
    ENV[_c_module_member(astlib.Name(tname))] = {
        "node_type": astlib.NodeT.struct,
        "params": [],
        "fields": {
            "literal": astlib.LiteralType(astlib.LiteralT.integer)
        },
        "methods": {
            "__init__": {
                "rettype": _c_module_member(astlib.Name(tname)),
                "args": [
                    ("literal", astlib.LiteralType(astlib.LiteralT.integer))
                ]
            },
            "__deinit__": {
                "rettype": astlib.Void(),
                "args": [
                    ("self", _c_module_member(astlib.Name(tname)))
                ]
            },
            "__copy__": {
                "rettype": _c_module_member(astlib.Name(tname)),
                "args": [
                    ("self", _c_module_member(astlib.Name(tname)))
                ]
            },
            "__add__": {
                "rettype": _c_module_member(astlib.Name(tname)),
                "args": [
                    ("self", _c_module_member(astlib.Name(tname))),
                    ("other", _c_module_member(astlib.Name(tname)))
                ]
            },
            "__sub__": {
                "rettype": _c_module_member(astlib.Name(tname)),
                "args": [
                    ("self", _c_module_member(astlib.Name(tname))),
                    ("other", _c_module_member(astlib.Name(tname)))
                ]
            },
            "__mul__": {
                "rettype": _c_module_member(astlib.Name(tname)),
                "args": [
                    ("self", _c_module_member(astlib.Name(tname))),
                    ("other", _c_module_member(astlib.Name(tname)))
                ]
            },
            "__div__": {
                "rettype": _c_module_member(astlib.Name(tname)),
                "args": [
                    ("self", _c_module_member(astlib.Name(tname))),
                    ("other", _c_module_member(astlib.Name(tname)))
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
