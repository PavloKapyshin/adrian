"""Translates parser's AST to object-oriented AST."""

from . import astlib
from .utils import A


def translate(node):
    args = []
    for subnode in node[1:]:
        if isinstance(subnode, list):
            args.extend(translate(subnode))
        else:
            args.append(subnode)
    result = getattr(astlib, node[0])(*args)
    if result in A(astlib.LinkedListNode, astlib.Args):
        yield from result.as_list()
    else:
        yield result


def main(ast_):
    new_ast = []
    for node in ast_:
        new_ast.extend(translate(node))
    return new_ast
