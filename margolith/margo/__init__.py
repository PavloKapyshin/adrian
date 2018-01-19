import sys
import copy

from adrian import cgen as adr_cgen

from . import parser, foreign_parser, analyzer
from . import object_proto, inlining, tac, copying, arc
from . import name_spacing, tocgen, main_func
from . import env, context, layers, ccopts


REPL_FILE_HASH = "mangled"

LAYERS = (
    (parser.Parser, "parse"),
    (object_proto.ObjectProto, "transform_ast"),
    (analyzer.Analyzer, "transform_ast"),
    (tac.TAC, "transform_ast"),
    (copying.Copying, "transform_ast"),
    (arc.ARC, "expand_ast"),
    # # (inlining.Inlining, "transform_ast"),
    (name_spacing.NameSpacing, "transform_ast"),
    (tocgen.ToCGen, "transform_ast"),
    (main_func.MainFunc, "expand_ast")
)


def compile_repl(inp, *, contexts):
    tmp_count = 0
    for layer_cls, method_name in LAYERS:
        with context.new_context(**copy.deepcopy(contexts[layer_cls])):
            layer = layer_cls()
            context.context.tmp_count = tmp_count
            if method_name == "parse":
                current_ast = foreign_parser.main(
                    layer.parse(inp))
            else:
                current_ast = list(getattr(layers, method_name)(
                    current_ast, registry=layer.get_registry()))
            tmp_count = context.context.tmp_count
    generator = adr_cgen.Generator()
    generator.add_ast(current_ast)
    return "\n".join(generator.generate())
    #return current_ast


def compile_from_string(inp, file_hash, libs=None, out_file="OUT_FILE", cc="clang"):
    contexts = {
        layer: {
            "env": env.Env(),
            "exit_on_error": True,
            "file_hash": file_hash,
            "module_paths": ["library/"],
            "tmp_count": 0}
        for layer, _ in LAYERS
    }
    tmp_count = 0
    clibs_inc = []
    clibs_cinc = []
    for layer_cls, method_name in LAYERS:
        with context.new_context(**contexts[layer_cls]):
            layer = layer_cls()
            context.context.tmp_count = tmp_count
            context.context.clibs_inc = clibs_inc
            context.context.clibs_cinc = clibs_cinc
            if not method_name == "parse":
                current_ast = list(getattr(layers, method_name)(
                    current_ast, registry=layer.get_registry()))
            else:
                current_ast = foreign_parser.main(
                    layer.parse(inp))
            contexts[layer_cls]["tmp_count"] = context.context.tmp_count
            clibs_inc = context.context.clibs_inc
            clibs_cinc = context.context.clibs_cinc
            tmp_count = context.context.tmp_count
    generator = adr_cgen.Generator()
    generator.add_ast(current_ast)
    return {
        # "code": current_ast,
        "code": "\n".join(generator.generate()),
        "ccopts": ccopts.make(cc, ["library/"], out_file),
        }


def _read_file(file_name, encoding):
    with open(file_name, mode="r", encoding=encoding) as file:
        contents = file.read()
    return contents


def compile_from_file(in_file, file_hash, libs=None, out_file="OUT_FILE", cc="clang"):
    result = compile_from_string(
        _read_file(in_file, "utf-8"), file_hash=file_hash, libs=libs, out_file=out_file, cc=cc)
    print(result["ccopts"], file=sys.stdout)
    return result["code"]
