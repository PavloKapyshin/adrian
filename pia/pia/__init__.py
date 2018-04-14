from copy import deepcopy

from . import (
    analyzer, context, defs, foreing_parser, layers, object_protocol,
    parser, interpreter, type_inference)


LAYERS = (
    (parser.Parse, "parse"),
    (syntax_sugar.SyntaxSugar, "transform_ast"),
    (object_protocol.ObjectProtocol, "transform_ast"),
    (analyzer.Analyzer, "transform_ast"),
    (type_inference.TypeInference, "transform_ast"),
    (interpreter.Main, "proceed")
)


def compile_(current_ast, layers_, default_context_args):
    context_args = default_context_args
    for layer_cls, method_name in layers_:
        with context.new_context(**context_args):
            layer = layer_cls()
            if method_name == "parse":
                current_ast = layer.parse(current_ast)
            else:
                got = getattr(layers, method_name)(
                    current_ast, registry=layer.get_registry())
                if got is not None:
                    current_ast = list(got)
        context_args = _update_context_args()


def _update_context_args():
    # You should always pass default environment to
    # layer to avoid possible bugs.
    return {
        **context.modified_context_args(),
        **{"env": deepcopy(defs.ENV)}}


def compile_from_string(input_code):
    compile_(input_code, LAYERS, defs.DEFAULT_CONTEXT_ARGUMENTS)


def _read_file(file_name):
    with open(file_name, mode="r", encoding="utf-8") as file:
        contents = file.read()
    return contents


def compile_from_file(in_file):
    compile_from_string(_read_file(in_file))
