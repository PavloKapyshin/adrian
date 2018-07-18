from . import context, defs, layers, utils, parser, loader, interpreter


LAYERS = (
    (parser.Parser, "parse"),
    (loader.Loader, "expand_ast"),
    (interpreter.Interpreter, "proceed")
)


def _update_context_args():
    # You should always pass default environment to
    # layer to avoid possible bugs.
    return {**context.modified_context_args(), **{"env": defs.ENV}}


def compile_from_string(input_code, *, stop_before, stop_after):
    context_args = defs.DEFAULT_CONTEXT_ARGUMENTS
    context_args["main_file_hash"] = utils.get_hash(input_code)
    current_ast = input_code
    for layer_cls, method_name in LAYERS:
        if stop_before == layer_cls:
            return current_ast
        with context.new_context(**context_args):
            layer = layer_cls()
            if method_name == "parse":
                current_ast = layer.parse(current_ast)
            else:
                got = getattr(layers, method_name)(
                    current_ast, registry=layer.get_registry())
                if got is not None:
                    current_ast = list(got)
        if stop_after == layer_cls:
            return current_ast
        context_args = _update_context_args()


def _read_file(file_name):
    with open(file_name, mode="r", encoding="utf-8") as file:
        contents = file.read()
    return contents


def compile_from_file(
        in_file, *, stop_before=None, stop_after=None, print_ast=False):
    result = compile_from_string(
        _read_file(in_file), stop_before=stop_before, stop_after=stop_after)
    if print_ast:
        print(result)
    return result
