from . import astlib, env_api
from .context import context
from .utils import A


def _types_are_equal_mismatch(type1, type2) -> bool:
    if type1 in A(astlib.Name):
        if type2 in A(astlib.GenericType):
            return types_are_equal(type1, type2.base)
        return False
    elif type1 in A(astlib.LiteralType, astlib.Empty, astlib.PyType):
        return False
    return _types_are_equal_mismatch(type2, type1)


def types_are_equal(type1, type2) -> bool:
    if type1 in A(astlib.Name) and type2 in A(astlib.Name):
        return type2 == type1
    elif type1 in A(astlib.GenericType) and type2 in A(astlib.GenericType):
        return types_are_equal(
            type1.base,
            type2.base)
    elif type1 in A(astlib.LiteralType) and type2 in A(astlib.LiteralType):
        return type1.type_ == type2.type_
    elif type1 in A(astlib.PyType) and type2 in A(astlib.PyType):
        return type1.type_ == type2.type_
    return _types_are_equal_mismatch(type1, type2)


def is_adt_member(adt_info, member_type):
    return any(
        types_are_equal(member_type, info["type_"]) for _, info in
            adt_info["fields"].items())


def implements_protocol(type_, protocol):
    type_info = env_api.type_info(type_)
    return protocol in type_info["protocols"]


def is_supertype(supertype, of) -> bool:
    while supertype in A(astlib.GenericType):
        supertype = supertype.base
    if supertype in A(astlib.Name):
        supertype_info = env_api.type_info(supertype)
        if env_api.is_adt(supertype_info["node_type"]):
            return is_adt_member(supertype_info, of)
        elif env_api.is_protocol(supertype_info["node_type"]):
            return implements_protocol(of, protocol=supertype)
    return False


def is_subtype(type, of) -> bool:
    return is_supertype(of, type)
