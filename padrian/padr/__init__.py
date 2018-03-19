import sys
from collections import OrderedDict

from adrian import cgen

from . import (
    parser, foreign_parser, analyzer,
    object_protocol, tac, copying,
    context, defs, layers, arc,
    debug_formatter, tocgen, ccopts, inlining)


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


def compile_(inp):
    context_kargs = {
        "env": defs.ENV,
        "exit_on_error": False,
        "module_paths": defs.DEFAULT_MODULE_PATHS,
        "clibs_includes": None
    }
    clibs_includes = OrderedDict()
    layers_ = LAYERS
    if layers_[-1][0] is tocgen.ToCgen:
        layers_ = layers_[:-1]
    for layer_cls, method_name in layers_:
        with context.new_context(**context_kargs):
            layer = layer_cls()
            context.context.clibs_includes = clibs_includes
            if method_name == "parse":
                current_ast = foreign_parser.main(layer.parse(inp))
            else:
                current_ast = list(getattr(layers, method_name)(
                    current_ast, registry=layer.get_registry()))
            clibs_includes = context.context.clibs_includes
    # generator = cgen.Generator()
    # generator.add_ast(current_ast)
    # return "\n".join(generator.generate())
    return current_ast


def compile_from_string(inp, out_file, cc):
    context_kargs = {
        "env": defs.ENV,
        "exit_on_error": True,
        "module_paths": defs.DEFAULT_MODULE_PATHS,
        "clibs_includes": None
    }
    clibs_includes = OrderedDict()
    for layer_cls, method_name in LAYERS:
        with context.new_context(**context_kargs):
            layer = layer_cls()
            context.context.clibs_includes = clibs_includes
            if method_name == "parse":
                current_ast = foreign_parser.main(layer.parse(inp))
            else:
                current_ast = list(getattr(layers, method_name)(
                    current_ast, registry=layer.get_registry()))
            clibs_includes = context.context.clibs_includes
    generator = cgen.Generator()
    generator.add_ast(current_ast)
    return {
        "code": "\n".join(generator.generate()),
        "cc_opts": ccopts.make(cc, ["library/"], out_file)
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
