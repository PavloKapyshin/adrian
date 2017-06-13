"""Translates parser AST to object-oriented AST."""

from . import astlib


def translate(node):
    args = []
    for subnode in node[1:]:
        if isinstance(subnode, list):
            args.extend(main([subnode]))
        else:
            args.append(subnode)
    yield getattr(astlib, node[0])(*args)


def main(ast_):
    new_ast = []
    for node in ast_:
        for new_node in translate(node):
            new_ast.append(new_node)
    return new_ast
