"""Translates parser AST to object-oriented AST."""

import sys

from . import astlib, parser_astlib
from .patterns import A


def decl_args(args):
    if args == []:
        return []
    elif len(args) == 1:
        return [astlib.Arg(args[0][0], args[0][1])]
    else:
        return [astlib.Arg(args[0][0], args[0][1])] + args[1:]


def is_linked_list(node):
    return node[0] in (
        parser_astlib.BODY, parser_astlib.TYPES,
        parser_astlib.VAR_TYPES, parser_astlib.ARG_LIST,
        parser_astlib.EMPTY)


def is_args(node):
    return node[0] == parser_astlib.ARGS


def as_list(llist):
    if llist in A(astlib.Empty):
        return []
    elif llist.rest in A(astlib.Empty):
        return llist.as_list()
    return [llist.value] + llist.rest


def translate(node):
    args = []
    for subnode in node[1:]:
        if isinstance(subnode, list):
            args.extend(translate(subnode))
        else:
            args.append(subnode)
    result = getattr(astlib, node[0])(*args)
    if is_linked_list(node):
        yield as_list(result)
    elif is_args(node):
        yield decl_args(as_list(result))
    else:
        yield result


def main(ast_):
    new_ast = []
    for node in ast_:
        new_ast.extend(translate(node))
    return new_ast
