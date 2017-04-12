"""Contains many useful definitions."""

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
