import os

from . import astlib, errors
from .context import (context, get_node_type, NodeType)
from .patterns import A


def _unknown_stmt_in_struct_body():
    errors.not_implemented("unknown statement in struct body")


def is_ctype(type_):
    return type_ in A(astlib.CType)

def is_adt(type_):
    return get_node_type(type_) == NodeType.adt

def is_struct(type_):
    return get_node_type(type_) == NodeType.struct

def is_user_type(type_):
    return get_node_type(type_) in (NodeType.struct, NodeType.adt)



def split_struct_body(body):
    fields, methods = [], []
    for stmt in body:
        if stmt in A(astlib.FieldDecl):
            fields.append(stmt)
        elif stmt in A(astlib.MethodDecl):
            methods.append(stmt)
        else:
            _unknown_stmt_in_struct_body()
    return fields, methods

def find_file(file_name, dirs):
    for dir_ in dirs:
        for entity in os.listdir(dir_):
            full_path = os.path.join(dir_, entity)
            if (os.path.isfile(full_path) and
                    entity == file_name):
                return full_path
    errors.cannot_find_file(file_name)
