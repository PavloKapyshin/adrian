"""Translates parser AST to object-oriented AST."""

from . import astlib, parser_astlib


def is_linked_list(node):
    return node[0] in (
        parser_astlib.BODY, parser_astlib.ARGS,
        parser_astlib.TYPES, parser_astlib.VAR_TYPES,
        parser_astlib.ARG_LIST)


def translate(node):
    args = []
    for subnode in node[1:]:
        if isinstance(subnode, list):
            args.extend(translate(subnode))
        else:
            args.append(subnode)
    result = getattr(astlib, node[0])(*args)
    if is_linked_list(node):
        yield result.as_list()
    else:
        yield result


def main(ast_):
    new_ast = []
    for node in ast_:
        new_ast.extend(translate(node))
    return new_ast
