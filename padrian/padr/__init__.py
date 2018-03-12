import copy

from . import parser, foreign_parser, analyzer, object_protocol, tac
from . import context, defs, layers

LAYERS = (
    (parser.Parser, "parse"),
    (analyzer.Analyzer, "transform_ast"),
    (object_protocol.ObjectProtocol, "transform_ast"),
    (tac.TAC, "transform_ast"),
    # copying
    # arc, expand_ast
    # inlining
    # name_spacing
    # tocgen
    # main_func, expand_ast
)


def compile_(inp):
    for layer_cls, method_name in LAYERS:
        with context.new_context(env=defs.ENV, exit_on_error=False):
            layer = layer_cls()
            if method_name == "parse":
                current_ast = foreign_parser.main(
                    parser.Parser().parse(inp))
            else:
                current_ast = list(getattr(layers, method_name)(
                    current_ast, registry=layer.get_registry()))
    return current_ast
