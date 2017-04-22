"""Contains many useful definitions."""

import re
import enum

from . import ast


RESERVED_WORDS = (
    "var",
    "fun",
    "struct",
    "return",
)


@enum.unique
class NodeType(enum.Enum):
    variable = 1
    constant = 2


CALLABLE_ATOMS = (
    ast.Name,
)

ATOM_TYPES = (
    ast.Integer,
    ast.String,
    ast.Name,
)

STD_FUNCS = (
    "print",
)

_STD_TYPES = (
    ast.Integer,
    ast.String
)
STD_TYPE_NAMES = set(type_.to_string() for type_ in _STD_TYPES)

TYPE_REGEX = re.compile(r"[A-Z_][a-zA-Z0-9]*")
VARIABLE_REGEX = re.compile(r"[a-z_][a-zA-Z0-9]*")
MODULE_REGEX = re.compile(r"[a-z_][a-z_0-9]*")
CONSTANT_REGEX = re.compile(r"[A-Z_][A-Z_0-9]*")

C_MODULE_NAME = "c"
C_INT32 = "Int32"
C_INT64 = "Int64"
C_CSTRING = "CString"
C_CHAR = "Char"

STD_MODULE_NAMES = ("std_types")
STD_MODULES_PATH = "std_modules/"
ADRIAN_FILE_EXTENSION = ".adr"