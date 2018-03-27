import sys

from adrian import cgen
from . import (analyzer, arc, ccopts, context, copying, debug_formatter, defs,
               env, foreign_parser, inlining, layers, object_protocol, parser,
               tac, tocgen, linker)


LAYERS = (
    (object_protocol.ObjectProtocol, "transform_ast"),
    (analyzer.Analyzer, "transform_ast"),
    (tac.TAC, "transform_ast"),
    (copying.Copying, "transform_ast"),
    (arc.ARC, "expand_ast"),
    (inlining.Inlining, "transform_ast"),
    (tocgen.ToCgen, "expand_ast")
)


def compile_(current_ast, layers_, default_context_args):
    context_arguments = default_context_args
    for layer_cls, method_name in layers_:
        with context.new_context(**context_arguments):
            layer = layer_cls()
            current_ast = list(getattr(layers, method_name)(
                current_ast, registry=layer.get_registry()
            ))
            context_arguments = context.modified_context_args()
            # You should always pass default environment to
            # layer to avoid possible bugs.
            context_arguments["env"] = defs.ENV
    return current_ast


def compile_repl(input_code):
    layers_ = LAYERS
    if layers_[-1][0] is tocgen.ToCgen:
        layers_ = layers_[:-1]
    with context.new_context(**defs.DEFAULT_CONTEXT_ARGUMENTS):
        current_ast = linker.unwrap_module_usage(input_code)
        context_args = context.modified_context_args()
    return compile_(current_ast, layers_, context_args)


def compile_from_string(input_code, out_file, cc):
    with context.new_context(**defs.DEFAULT_CONTEXT_ARGUMENTS):
        current_ast = linker.unwrap_module_usage(input_code)
        context_args = context.modified_context_args()
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
