from typing import Optional, Type, Union, cast

from . import astlib
from .context import context
from .utils import A


AdrianType = Union[Type[astlib.Name], Type[astlib.GenericType]]


def _types_are_equal_mismatch(type1: AdrianType, type2: AdrianType) -> bool:
    if type1 in A(astlib.Name):
        return types_are_equal(type1, cast(astlib.GenericType, type2).base)
    elif type1 in A(astlib.DataMember):
        return False
    return _types_are_equal_mismatch(type2, type1)


def types_are_equal(type1: AdrianType, type2: AdrianType) -> bool:
    if type1 in A(astlib.Name) and type2 in A(astlib.Name):
        return type2 == type1
    elif type1 in A(astlib.GenericType) and type2 in A(astlib.GenericType):
        return types_are_equal(
            cast(astlib.GenericType, type1).base,
            cast(astlib.GenericType, type2).base)
    elif type1 in A(astlib.DataMember) and type2 in A(astlib.DataMember):
        return (type1.parent == type2.parent and
            types_are_equal(type1.member, type2.member))
    return _types_are_equal_mismatch(type1, type2)


def is_adt_member(adt_info, member_type: AdrianType):
    return any(
        types_are_equal(member_type, type_) for _, type_ in
            adt_info["fields"].items())


def is_supertype(supertype: AdrianType, of: AdrianType) -> Optional[bool]:
    if supertype in A(astlib.Name):
        supertype_info = context.env.get_type_info(supertype)
        if context.env.is_adt(supertype_info["node_type"]):
            return is_adt_member(supertype_info, of)
    return None


def is_subtype(type: AdrianType, of: AdrianType) -> bool:
    return is_supertype(of, type)
