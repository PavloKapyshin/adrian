"""Translates parser AST to object-oriented AST."""

from . import astlib


def translate(node):
    args = []
    for subnode in node[1:]:
        if isinstance(subnode, list):
            args.extend(translate(subnode))
        else:
            args.append(subnode)
    yield getattr(astlib, node[0])(*args)


def main(ast_):
    new_ast = []
    for node in ast_:
        new_ast.extend(translate(node))
    return new_ast
