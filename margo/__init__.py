from adrian import cgen as acgen

from . import parser
from . import foreign_parser
from . import analyzer
from . import oopdef
from . import oopcall
from . import name_spacing
from . import simpex
from . import arc
from . import cgen
from . import main_func
from . import structs
from . import context
from . import layers


def compile_repl(inp, *, ns, ts, fs, exit_on_error):
    with context.new_context(
            ns=ns, ts=ts, fs=fs, exit_on_error=exit_on_error,
            file_hash="mangled_"):
        current_ast = foreign_parser.main(parser.main(inp))
        for layer_cls, method_name in (
                (analyzer.Analyzer, "transform_ast"),
                (oopdef.OOPDef, "transform_ast"),
                (oopcall.OOPCall, "transform_ast"),
                (name_spacing.NameSpacing, "transform_ast"),
                (simpex.SimpEx, "transform_ast"),
                (arc.ARC, "expand_ast"),
                (cgen.CGen, "transform_ast"),
                (main_func.MainFunc, "expand_ast")):
            layer = layer_cls()
            current_ast = list(getattr(layers, method_name)(
                current_ast, registry=layer.get_registry()))
    generator = acgen.Generator()
    generator.add_ast(current_ast)
    return list(generator.generate())
    # return current_ast


def compile_from_string(inp, file_hash):
    current_ast = foreign_parser.main(parser.main(inp))
    for layer_cls, method_name in (
            (analyzer.Analyzer, "transform_ast"),
            (oopdef.OOPDef, "transform_ast"),
            (oopcall.OOPCall, "transform_ast"),
            (name_spacing.NameSpacing, "transform_ast"),
            (simpex.SimpEx, "transform_ast"),
            (arc.ARC, "expand_ast"),
            (cgen.CGen, "transform_ast"),
            (main_func.MainFunc, "expand_ast")):
        with context.new_context(
                ns=structs.Namespace(), ts=structs.Namespace(),
                fs=structs.Namespace(), exit_on_error=True,
                file_hash=file_hash):
            layer = layer_cls()
            current_ast = list(getattr(layers, method_name)(
                current_ast, registry=layer.get_registry()))
    generator = acgen.Generator()
    generator.add_ast(current_ast)
    return "\n".join(generator.generate())


def _read_file(file_name, encoding):
    with open(file_name, mode="r", encoding=encoding) as file:
        contents = file.read()
    return contents


def compile_from_file(in_file, file_hash):
    return compile_from_string(
        _read_file(in_file, "utf-8"), file_hash=file_hash)
