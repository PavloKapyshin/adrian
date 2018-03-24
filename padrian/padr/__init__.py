import sys

from adrian import cgen

from . import (analyzer, arc, ccopts, context, copying, debug_formatter, defs,
               foreign_parser, inlining, layers, object_protocol, parser, tac,
               tocgen, )


LAYERS = (
    (parser.Parser, "parse"),
    (object_protocol.ObjectProtocol, "transform_ast"),
    (analyzer.Analyzer, "transform_ast"),
    (tac.TAC, "transform_ast"),
    (copying.Copying, "transform_ast"),
    (arc.ARC, "expand_ast"),
    (inlining.Inlining, "transform_ast"),
    (tocgen.ToCgen, "expand_ast")
)


def compile_(input_code, layers_, default_context_args):
    context_arguments = default_context_args
    current_ast = []
    for layer_cls, method_name in layers_:
        with context.new_context(**context_arguments):
            layer = layer_cls()
            # TODO: replace parser by more high-level layer
            # (which will combine _parser and foreign_parser).
            if method_name == "parse":
                current_ast = foreign_parser.main(layer.parse(input_code))
            else:
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
    return compile_(input_code, layers_, defs.DEFAULT_CONTEXT_ARGUMENTS)


def compile_from_string(input_code, out_file, cc):
    generator = cgen.Generator()
    generator.add_ast(
        compile_(input_code, LAYERS, defs.DEFAULT_CONTEXT_ARGUMENTS))
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
