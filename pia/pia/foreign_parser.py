import os
import itertools

from . import astlib, defs, errors
from .context import context
from .utils import A


def translate(node):
    args = []
    for subnode in node[1:]:
        if isinstance(subnode, list):
            args.extend(translate(subnode))
        elif subnode is None:
            args.append([])
        else:
            args.append(subnode)
    result = getattr(astlib, node[0])(*args)
    if result in A(astlib.LinkedListNode, astlib.Args):
        yield list(itertools.chain.from_iterable(list(result)))
    else:
        yield result


def main(ast_):
    new_ast = []
    for node in ast_:
        new_ast.extend(translate(node))
    return new_ast
