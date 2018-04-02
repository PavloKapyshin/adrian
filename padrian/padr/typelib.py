from . import astlib, env_api
from .context import context
from .utils import A


def _types_are_equal_mismatch(type1, type2) -> bool:
    if type1 in A(astlib.Name):
        return types_are_equal(type1, type2.base)
    elif type1 in A(astlib.DataMember):
        return False
    return _types_are_equal_mismatch(type2, type1)


def types_are_equal(type1, type2) -> bool:
    if type1 in A(astlib.Name) and type2 in A(astlib.Name):
        return type2 == type1
    elif type1 in A(astlib.GenericType) and type2 in A(astlib.GenericType):
        return types_are_equal(
            type1.base,
            type2.base)
    elif type1 in A(astlib.DataMember) and type2 in A(astlib.DataMember):
        return (type1.parent == type2.parent and
            types_are_equal(type1.member, type2.member))
    return _types_are_equal_mismatch(type1, type2)


def is_adt_member(adt_info, member_type):
    return any(
        types_are_equal(member_type, type_) for _, type_ in
            adt_info["fields"].items())


def is_supertype(supertype, of) -> bool:
    if supertype in A(astlib.Name):
        supertype_info = env_api.type_info(supertype)
        if env_api.is_adt(supertype_info["node_type"]):
            return is_adt_member(supertype_info, of)
    return False


def is_subtype(type, of) -> bool:
    return is_supertype(of, type)
