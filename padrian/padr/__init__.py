import copy

from adrian import cgen

from . import (
    parser, foreign_parser, analyzer,
    object_protocol, tac, copying,
    context, defs, layers, arc,
    debug_formatter, tocgen)


LAYERS = (
    (parser.Parser, "parse"),
    (object_protocol.ObjectProtocol, "transform_ast"),
    (analyzer.Analyzer, "transform_ast"),
    (tac.TAC, "transform_ast"),
    (copying.Copying, "transform_ast"),
    (arc.ARC, "expand_ast"),
    # inlining
    (tocgen.ToCgen, "expand_ast")
)


def compile_(inp):
    context_kargs = {
        "env": defs.ENV,
        "exit_on_error": False,
        "module_paths": defs.DEFAULT_MODULE_PATHS,
    }
    for layer_cls, method_name in LAYERS:
        with context.new_context(**context_kargs):
            layer = layer_cls()
            if method_name == "parse":
                current_ast = foreign_parser.main(
                    parser.Parser().parse(inp))
            else:
                current_ast = list(getattr(layers, method_name)(
                    current_ast, registry=layer.get_registry()))
    generator = cgen.Generator()
    generator.add_ast(current_ast)
    return "\n".join(generator.generate())
    # return current_ast
