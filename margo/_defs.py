"""Contains many useful definitions."""

import re, enum

from . import ast


TYPE_REGEX = re.compile(r"[A-Z_][a-zA-Z0-9]*")
VARIABLE_REGEX = re.compile(r"[a-z_][a-zA-Z0-9]*")
FUNCTION_REGEX = re.compile(r"[a-z_][a-zA-Z0-9]*")
MODULE_REGEX = re.compile(r"[a-z_][a-z_0-9]*")
CONSTANT_REGEX = re.compile(r"[A-Z_][A-Z_0-9]*")
SPEC_STRUCT_ELEM_REGEX = re.compile(r"[_][_][a-zA-Z0-9]*[_][_]")

RESERVED_WORDS = (
    "var",
#    "cst",
#    "fun",
#    "sct",
#    "ret",
#    "iff",
#    "els",
)

CALLABLE_ATOMS = (
    ast.Name,
)


@enum.unique
class NodeType(enum.Enum):
    variable = 1
    constant = 2


STD_TYPES_MODULE_NAME = "std_types"
STD_TYPES = (
    ast.Integer,
    ast.String
)
STD_TYPES_NAMES = set(type_.to_type() for type_ in STD_TYPES)
# TODO: use cgen.CFuncDescr
STD_TYPES_FUNC_SIGNATURES = {
    "__init__Integer": {
        "rettype": ast.ModuleMember(
            name=ast.ModuleName(STD_TYPES_MODULE_NAME),
            member=ast.TypeName("Integer")),
        "args": [ast.CString.to_type()]
    },
    "__free__Integer": {
        "rettype": ast.CVoid(),
        "args": [ast.ModuleMember(
            name=ast.ModuleName(STD_TYPES_MODULE_NAME),
            member=ast.TypeName("Integer"))]
    },
    "__copy__Integer": {
        "rettype": ast.ModuleMember(
            name=ast.ModuleName(STD_TYPES_MODULE_NAME),
            member=ast.TypeName("Integer")),
        "args": [ast.ModuleMember(
            name=ast.ModuleName(STD_TYPES_MODULE_NAME),
            member=ast.TypeName("Integer"))]
    },
    "__add__Integer": {
        "rettype": ast.ModuleMember(
            name=ast.ModuleName(STD_TYPES_MODULE_NAME),
            member=ast.TypeName("Integer")),
        "args": [
            ast.ModuleMember(
                name=ast.ModuleName(STD_TYPES_MODULE_NAME),
                member=ast.TypeName("Integer")),
            ast.ModuleMember(
                name=ast.ModuleName(STD_TYPES_MODULE_NAME),
                member=ast.TypeName("Integer"))
        ]
    },
    "__sub__Integer": {
        "rettype": ast.ModuleMember(
            name=ast.ModuleName(STD_TYPES_MODULE_NAME),
            member=ast.TypeName("Integer")),
        "args": [
            ast.ModuleMember(
                name=ast.ModuleName(STD_TYPES_MODULE_NAME),
                member=ast.TypeName("Integer")),
            ast.ModuleMember(
                name=ast.ModuleName(STD_TYPES_MODULE_NAME),
                member=ast.TypeName("Integer"))
        ]
    },
    "__mul__Integer": {
        "rettype": ast.ModuleMember(
            name=ast.ModuleName(STD_TYPES_MODULE_NAME),
            member=ast.TypeName("Integer")),
        "args": [
            ast.ModuleMember(
                name=ast.ModuleName(STD_TYPES_MODULE_NAME),
                member=ast.TypeName("Integer")),
            ast.ModuleMember(
                name=ast.ModuleName(STD_TYPES_MODULE_NAME),
                member=ast.TypeName("Integer"))
        ]
    },
    "__div__Integer": {
        "rettype": ast.ModuleMember(
            name=ast.ModuleName(STD_TYPES_MODULE_NAME),
            member=ast.TypeName("Integer")),
        "args": [
            ast.ModuleMember(
                name=ast.ModuleName(STD_TYPES_MODULE_NAME),
                member=ast.TypeName("Integer")),
            ast.ModuleMember(
                name=ast.ModuleName(STD_TYPES_MODULE_NAME),
                member=ast.TypeName("Integer"))
        ]
    }
}

# C_FUNC_SIGNATURES = {
#     "__init__IntFast32": {
#         "rettype": ast.ModuleMember(
#             name=ast.Name(C_MODULE_NAME),
#             member=ast.Name("IntFast32")),
#     },
#     "__init__UIntFast32": {
#         "rettype": ast.ModuleMember(
#             name=ast.Name(C_MODULE_NAME),
#             member=ast.Name("UIntFast32")),
#     },
#     "__init__IntFast8": {
#         "rettype": ast.ModuleMember(
#             name=ast.Name(C_MODULE_NAME),
#             member=ast.Name("IntFast8")),
#     },
#     "__init__UIntFast8": {
#         "rettype": ast.ModuleMember(
#             name=ast.Name(C_MODULE_NAME),
#             member=ast.Name("UIntFast8")),
#     }
# }

STD_FUNCS = (
    "print",
)
STD_MODULE_NAMES = (STD_TYPES_MODULE_NAME, )
STD_MODULES_PATH = "std_modules/"

OP_TO_FUNC = {
    "+": "__add__",
    "-": "__sub__",
    "*": "__mul__",
    "/": "__div__"
}

ADRIAN_FILE_EXTENSION = ".adr"