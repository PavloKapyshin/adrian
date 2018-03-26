"""Translates parser's AST to object-oriented AST."""

import os
import itertools

from . import astlib, defs, errors
from .context import context
from .utils import A


def find_file(file_name, module_paths):
    for dir_ in module_paths:
        for entity in os.listdir(dir_):
            full_path = os.path.join(dir_, entity)
            if (os.path.isfile(full_path) and
                    entity == file_name):
                return full_path
    errors.cannot_find_file(file_name)


def add_found_module(module_name, file_name=None, type_="adr"):
    if file_name and type_ == "adr":
        found_source = find_file(
            ".".join([file_name, "c"]), context.module_paths)
        found_header = find_file(
            ".".join([file_name, "h"]), context.module_paths)
        if module_name not in context.clibs_includes:
            context.clibs_includes.update({
                module_name: {
                    "source": found_source,
                    "header": file_name + ".h",
                    "type_": "c"
                }
            })
    else:
        if module_name not in context.clibs_includes:
            context.clibs_includes.update({
                module_name: {
                    "header": module_name + ".h",
                    "type_": type_
                }
            })


def add_to_clibs(module_elem):
    if module_elem.parent not in context.clibs_includes:
        if module_elem.parent == defs.CMODULE:
            add_found_module("stdint", type_="c")
            add_found_module(defs.CMODULE, file_name=defs.CMODULE_FILE)
        else:
            errors.not_now(errors.MODULES)


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
        if result in A(astlib.DataMember):
            if result.datatype == astlib.DataT.module:
                add_to_clibs(result)
        elif result in A(astlib.Name):
            if result == defs.BOOL:
                add_to_clibs(defs.BOOL_TRANSLATION)
        yield result


def main(ast_):
    new_ast = []
    for node in ast_:
        new_ast.extend(translate(node))
    return new_ast
