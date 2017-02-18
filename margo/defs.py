"""Contains many useful definitions."""

from . import ast


CALLABLE_ATOMS = (
    ast.VariableName,
)

NAME_TYPES = (
    ast.VariableName,
)

ATOM_TYPES = (
    ast.Integer,
    ast.String,
) + NAME_TYPES

STANDARD_FUNCS = (
    "print",
)

STANDARD_TYPES = (
    "Integer",
    "String",
)
