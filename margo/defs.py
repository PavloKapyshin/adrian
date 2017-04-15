"""Contains many useful definitions."""

import re
import enum

from . import ast


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

STANDARD_FUNC_NAMES = (
    "print",
)

_STANDARD_TYPES = (
    ast.Integer,
    ast.String
)
STANDARD_TYPE_NAMES = set(type_.to_string() for type_ in _STANDARD_TYPES)

TYPE_REGEX = re.compile(r"[A-Z_][a-zA-Z0-9]*")
VARIABLE_REGEX = re.compile(r"[a-z_][a-zA-Z0-9]*")
CONSTANT_REGEX = re.compile(r"[A-Z_][A-Z_0-9]*")

CTYPES_MODULE_NAME = "ctypes"
CTYPES_INT32_STRING = "Int32"
CTYPES_INT64_STRING = "Int64"
CTYPES_CHAR_STRING = "Char"
