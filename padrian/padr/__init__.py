import sys
from copy import deepcopy

from adrian import cgen
from . import (
    analyzer, arc, ccopts, context, copying, defs, foreign_parser, inlining,
    layers, object_protocol, parser, tac, tocgen, linker, checker)

LAYERS = (
    (object_protocol.ObjectProtocol, "transform_ast"),
    # (analyzer.Analyzer, "transform_ast"),
    # (checker.Checker, "transform_ast"),
    # (tac.TAC, "transform_ast"),
    # (copying.Copying, "transform_ast"),
    # (arc.ARC, "expand_ast"),
    # (inlining.Inlining, "transform_ast"),
    # (tocgen.ToCgen, "expand_ast")
)


def compile_(current_ast, layers_, default_context_args):
    context_arguments = default_context_args
    for layer_cls, method_name in layers_:
        with context.new_context(**context_arguments):
            layer = layer_cls()
            before = deepcopy(defs.ENV)
            current_ast = list(getattr(layers, method_name)(
                current_ast, registry=layer.get_registry()
            ))
            assert(before.space == defs.ENV.space)
        context_arguments = _update_context_args()
    return current_ast


def _update_context_args():
    # You should always pass default environment to
    # layer to avoid possible bugs.
    return {**context.modified_context_args(), **{"env": deepcopy(defs.ENV)}}


def link(input_code):
    with context.new_context(**defs.DEFAULT_CONTEXT_ARGUMENTS):
        current_ast = linker.unwrap_module_usage(input_code)
        context_args = _update_context_args()
    return current_ast, context_args

def compile_repl(input_code):
    layers_ = LAYERS
    if layers_[-1][0] is tocgen.ToCgen:
        layers_ = layers_[:-1]
    current_ast, context_args = link(input_code)
    return compile_(current_ast, layers_, context_args)


def compile_from_string(input_code, out_file, cc):
    current_ast, context_args = link(input_code)
    generator = cgen.Generator()
    generator.add_ast(
        compile_(current_ast, LAYERS, context_args))
    return {
        "code": "\n".join(generator.generate()),
        "cc_opts": ccopts.make(cc, defs.DEFAULT_MODULE_PATHS, out_file)
    }


def _read_file(file_name):
    with open(file_name, mode="r", encoding="utf-8") as file:
        contents = file.read()
    return contents


def compile_from_file(in_file, out_file, cc="clang"):
    result = compile_from_string(
        _read_file(in_file), out_file=out_file, cc=cc)
    print(result["cc_opts"], file=sys.stdout)
    return result["code"]
