import re
import copy
from functools import partial

from . import astlib, env

ADRIAN_FILE_EXTENSION = "adr"

MANGLING_LENGTH = 6

TMP_FMT_STRING = "___tmp{}"


MODULE_PRELUDE = "prelude"
CONSTANT_TRUE = "True"
CONSTANT_FALSE = "False"
TYPE_NUMBER = "Number"
TYPE_STRING = "String"
TYPE_BOOL = "Bool"
TYPE_VECTOR = "Vector"
TYPE_SET = "Set"
TYPE_DICT = "Dict"
TYPE_SOME = "Some"
TYPE_NONE = "None"
TYPE_MAYBE = "Maybe"
TYPE_VOID = "Void"
PRELUDE_TYPES = (
    TYPE_NUMBER, TYPE_STRING, TYPE_BOOL, TYPE_VECTOR,
    TYPE_SET, TYPE_DICT, TYPE_SOME, TYPE_NONE, TYPE_MAYBE)
FUNC_PRINT = "print"
FUNC_LENGTH = "length"
FUNC_TO_STRING = "toString"
FUNC_TO_VECTOR = "toVector"
FUNC_TO_NUMBER = "toNumber"
FUNC_TO_SET = "toSet"
FUNC_TO_DICT = "toDict"
FUNC_ALL = "all"
FUNC_ANY = "any"
FUNC_ASSERT = "assert"
PRELUDE_FUNCS = (
    FUNC_PRINT, FUNC_LENGTH, FUNC_TO_STRING, FUNC_TO_SET, FUNC_TO_DICT,
    FUNC_TO_NUMBER, FUNC_TO_VECTOR, FUNC_ALL, FUNC_ANY, FUNC_ASSERT)
PROTOCOL_CONVERTIBLE_TO_STRING = "ConvertibleToString"
PROTOCOL_CONVERTIBLE_TO_NUMBER = "ConvertibleToNumber"
PROTOCOL_CONVERTIBLE_TO_VECTOR = "ConvertibleToVector"
PROTOCOL_CONVERTIBLE_TO_SET = "ConvertibleToSet"
PROTOCOL_CONVERTIBLE_TO_DICT = "ConvertibleToDict"
PROTOCOL_ADD = "Addable"
PROTOCOL_SUB = "Subtractable"
PROTOCOL_MUL = "Mutipliable"
PROTOCOL_DIV = "Divisible"
PROTOCOL_EQ = "Eq"
PROTOCOL_LT = "Lt"
PROTOCOL_GT = "Gt"
PROTOCOL_CONTAINER = "Container"
PROTOCOL_PRINT = "Printable"
PROTOCOL_LENGTH = "Lengthable"
PROTOCOL_ITERABLE = "Iterable"
PROTOCOL_GET_ITEM = "GetItemable"
PROTOCOL_SET_ITEM = "SetItemable"
PRELUDE_PROTOCOLS = (
    PROTOCOL_CONVERTIBLE_TO_DICT, PROTOCOL_CONVERTIBLE_TO_SET,
    PROTOCOL_CONVERTIBLE_TO_VECTOR, PROTOCOL_CONVERTIBLE_TO_NUMBER,
    PROTOCOL_CONVERTIBLE_TO_STRING, PROTOCOL_GET_ITEM, PROTOCOL_SET_ITEM,
    PROTOCOL_ADD, PROTOCOL_SUB, PROTOCOL_DIV, PROTOCOL_MUL, PROTOCOL_ITERABLE,
    PROTOCOL_LENGTH, PROTOCOL_PRINT, PROTOCOL_EQ, PROTOCOL_LT, PROTOCOL_GT,
    PROTOCOL_CONTAINER)
PRELUDE_OBJS = PRELUDE_PROTOCOLS + PRELUDE_FUNCS + PRELUDE_TYPES


MODULE_PY = "py"
TYPE_INT = "Int"
TYPE_STR = "Str"
TYPE_LIST = "List"
FUNC_TO_INT = "toInt"
FUNC_TO_STR = "toStr"
FUNC_TO_SET = "toSet"
FUNC_TO_LIST = "toList"
FUNC_LEN = "len"
FUNC_ZIP = "zip"
FUNC_READ_FILE = "readFile"
FUNC_WRITE_FILE = "writeFile"
CONSTANT_ARGV = "argv"
METHOD_SPLIT = "split"
METHOD_VALUES = "values"
METHOD_KEYS = "keys"
METHOD_ITEMS = "items"

PY_FUNCS = (
    FUNC_PRINT, FUNC_LEN, FUNC_ZIP, FUNC_TO_INT, FUNC_TO_STR, FUNC_TO_SET,
    FUNC_TO_LIST, FUNC_READ_FILE, FUNC_WRITE_FILE, FUNC_ASSERT)


EQ = "="
EQEQ = "=="
NEQ = "!="
LTEQ = "<="
GTEQ = ">="
LT = "<"
GT = ">"
IN = "in"
AND = "and"
OR = "or"
NOT = "not"
IS = "is"
ASSIGNMENT_OP_TO_EXPR_OP = {
    "+=": "+",
    "-=": "-",
    "*=": "*",
    "/=": "/"
}
RESERVED_WORDS = {
    keyword: keyword.upper()
    for keyword in (
        "var", "let", "for", "in", "fun", "return", "struct",
        "if", "elif", "else", "while", "extension", "protocol", "is",
        "and", "or", "not", "break"
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


SELF = "self"
SPEC_METHOD_GETITEM = "__getItem__"
SPEC_METHOD_SETITEM = "__setItem__"
SPEC_METHOD_INIT = "__init__"
SPEC_METHOD_ADD = "__add__"
SPEC_METHOD_SUB = "__sub__"
SPEC_METHOD_MUL = "__mul__"
SPEC_METHOD_DIV = "__div__"
SPEC_METHOD_CONTAINS = "__contains__"
SPEC_METHOD_EQ = "__eq__"
SPEC_METHOD_NEQ = "__neq__"
SPEC_METHOD_LTEQ = "__lteq__"
SPEC_METHOD_GTEQ = "__gteq__"
SPEC_METHOD_LT = "__lt__"
SPEC_METHOD_GT = "__gt__"
SPEC_METHOD_NEXT = "__next__"
OPERATOR_TO_METHOD = {
    "+": SPEC_METHOD_ADD,
    "-": SPEC_METHOD_SUB,
    "*": SPEC_METHOD_MUL,
    "/": SPEC_METHOD_DIV,
    "==": SPEC_METHOD_EQ,
    "!=": SPEC_METHOD_NEQ,
    "<=": SPEC_METHOD_LTEQ,
    ">=": SPEC_METHOD_GTEQ,
    "<": SPEC_METHOD_LT,
    ">": SPEC_METHOD_GT,
    "in": SPEC_METHOD_CONTAINS,
}


ENV = env.Env()
DEFAULT_MODULE_PATHS = ["library/"]
DEFAULT_CONTEXT_ARGUMENTS = {
    "env": copy.deepcopy(ENV),
    "exit_on_error": False,
}
