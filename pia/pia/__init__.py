import sys
from copy import deepcopy

from . import (
    analyzer, context, defs, foreign_parser, layers, object_protocol, parser,
    linker, interpreter, type_inference)

LAYERS = (
    (object_protocol.ObjectProtocol, "transform_ast"),
    (analyzer.Analyzer, "transform_ast"),
    (type_inference.TypeInference, "transform_ast"),
    #(interpreter.Main, "proceed")
)


def compile_(current_ast, layers_, default_context_args):
    context_arguments = default_context_args
    for layer_cls, method_name in layers_:
        with context.new_context(**context_arguments):
            layer = layer_cls()
            got = getattr(layers, method_name)(
                current_ast, registry=layer.get_registry()
            )
            if got is not None:
                current_ast = list(got)
        context_arguments = _update_context_args()
    print(current_ast)


def _update_context_args():
    # You should always pass default environment to
    # layer to avoid possible bugs.
    return {**context.modified_context_args(), **{"env": deepcopy(defs.ENV)}}


def link(input_code):
    with context.new_context(**defs.DEFAULT_CONTEXT_ARGUMENTS):
        current_ast = linker.unwrap_module_usage(input_code)
        context_args = _update_context_args()
    return current_ast, context_args


def compile_from_string(input_code):
    current_ast, context_args = link(input_code)
    compile_(current_ast, LAYERS, context_args)


def _read_file(file_name):
    with open(file_name, mode="r", encoding="utf-8") as file:
        contents = file.read()
    return contents


def compile_from_file(in_file):
    compile_from_string(_read_file(in_file))
