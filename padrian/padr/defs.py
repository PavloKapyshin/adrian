import re
import copy
from functools import partial

from adrian.cgen import libc
from . import astlib, env


# First 50 types are reserved for compiler.
TYPE_TAG_START = 51
TYPE_TAG_TYPE = astlib.LiteralType(astlib.LiteralT.uint_fast64_t)
TYPE_TAG_NAME = "type_tag"

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
T_STRING = "t"
I_STRING = "i"
F_STRING = "f"
U_STRING = "u"

INIT_METHOD = "__init__"
DEINIT_METHOD = "__deinit__"
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



ENV = env.Env()
_c_module_member = partial(
    astlib.DataMember, astlib.DataT.module, astlib.Name(CMODULE))
def _add_cnumeric_type(tname):
    member = _c_module_member(astlib.Name(tname))
    _self_arg = ("self", member)
    _other_arg = ("other", member)
    def _init():
        return {
            "type_": member,
            "args": [("literal", astlib.LiteralType(astlib.LiteralT.integer))],
            "body": []
        }
    def _deinit():
        return {
            "type_": astlib.Void(),
            "args": [_self_arg],
            "body": []
        }
    def _copy():
        return {
            "type_": member,
            "args": [_self_arg],
            "body": []
        }
    def _math_method():
        return {
            "type_": member,
            "args": [_self_arg, _other_arg],
            "body": []
        }
    def _logic_method():
        return {
            "type_": BOOL_TRANSLATION,
            "args": [_self_arg, _other_arg],
            "body": []
        }
    def _not():
        return {
            "type_": BOOL_TRANSLATION,
            "args": [_self_arg],
            "body": []
        }
    ENV[member] = {
        "node_type": astlib.NodeT.struct,
        "params": [],
        "fields": {},
        "methods": {
            INIT_METHOD: _init(),
            DEINIT_METHOD: _deinit(),
            COPY_METHOD: _copy(),
            ADD_METHOD: _math_method(),
            SUB_METHOD: _math_method(),
            MUL_METHOD: _math_method(),
            DIV_METHOD: _math_method(),
            EQ_METHOD: _logic_method(),
            NEQ_METHOD: _logic_method(),
            GT_METHOD: _logic_method(),
            LT_METHOD: _logic_method(),
            LTE_METHOD: _logic_method(),
            GTE_METHOD: _logic_method(),
            AND_METHOD: _logic_method(),
            OR_METHOD: _logic_method(),
            NOT_METHOD: _not(),
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


DEFAULT_MODULE_PATHS = ["library/"]
DEFAULT_CONTEXT_ARGUMENTS = {
    "env": copy.deepcopy(ENV),
    "exit_on_error": False,
    "module_paths": DEFAULT_MODULE_PATHS,
    "clibs_includes": None,
    "i_count": 0,
}
