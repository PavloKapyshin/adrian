from vendor.adrian import cgen as acgen

from . import parser
from . import foreign_parser
from . import analyzer
#from . import naming_rules
#from . import oop
# from . import name_existence
#from . import type_inference
# from . import type_checking
#from . import name_mangling
#from . import cgen
from . import structs
from . import context
from . import layers


# def _get_file_hash(file_name):
#     obj = hashlib.sha1()
#     with open(file_name, mode="rb") as file:
#         obj.update(file.read())
#     return obj.hexdigest()


# def _read_file(file_name, encoding):
#     with open(file_name, mode="r", encoding=encoding) as file:
#         contents = file.read()
#     return contents.splitlines()


def compile_repl(inp, *, ns, ts, fs, exit_on_error):
    with context.new_context(
            ns=ns, ts=ts, fs=fs, exit_on_error=exit_on_error,
            file_hash="mangled_"):
        current_ast = foreign_parser.main(parser.main(inp))
        # NameExistence layer must be after NamingRules layer.
        # TypeInference layer must be after NameExistence layer.
        # TypeChecking layer must be after TypeInference layer.
        for layer_cls in (
                analyzer.Analyzer,
                #naming_rules.NamingRules,
                #oop.OOP,
                #type_inference.TypeInference,
                #name_mangling.NameMangling,
                #cgen.CGen
                ):
            layer = layer_cls()
            current_ast = list(layers.transform_ast(
                current_ast, registry=layer.get_registry()))
    # generator = acgen.Generator()
    # generator.add_ast(current_ast)
    # return list(generator.generate())
    return current_ast


# def compile_repl(text, contexts, file_hash=""):
    # print("Stage 1: parsing code.")
    # lp_ast = lex_parse.main(
    #     text, exit_on_error=contexts["exit_on_error"])
    # fp_ast = foreign_parser.ForeignParser().main(
    #     lp_ast, exit_on_error=contexts["exit_on_error"])
    # print("Stage 2: checking naming rules and analyzing names.")
    # nr_ast = naming_rules.NamingRules(contexts["naming_rules"]).main(
    #     fp_ast, exit_on_error=contexts["exit_on_error"])
    # print("Stage 3: analyzing code and getting more data from context.")
    # an_ast = analyzer.Analyzer(contexts["analyzer"]).main(
    #     nr_ast, exit_on_error=contexts["exit_on_error"])
    # print("Stage 4: doing type inference where needed.")
    # ti_ast = type_inference.TypeInference(
    #     contexts["type_inference"]).main(
    #     an_ast, exit_on_error=contexts["exit_on_error"])
    # print("Stage 5: adding default values where needed.")
    # dv_ast = default_value.DefaultValue(
    #     contexts["default_value"]).main(
    #     ti_ast, exit_on_error=contexts["exit_on_error"])
    # print("Stage 6: translating standard aliases.")
    # sa_ast = std_alias.StdAlias(
    #     contexts["std_alias"]).main(
    #     dv_ast, exit_on_error=contexts["exit_on_error"])
    # print("Stage 7: translating Adrian structs to C structs.")
    # oop_ast = oop.OOP(contexts["oop"]).main(
    #     sa_ast, exit_on_error=contexts["exit_on_error"])
    # print("Stage 8: simplifying Adrian expressions.")
    # se_ast = simple_expr.SimpleExpr(
    #     contexts["simple_expr"]).main(
    #     oop_ast, exit_on_error=contexts["exit_on_error"])
    # print("Stage 9: checking name existence.")
    # ne_ast = name_existence.NameExistence(
    #    contexts["name_existence"]).main(
    #    se_ast, exit_on_error=contexts["exit_on_error"])
    # print("Stage 10: checking types.")
    # tc_ast = type_checking.TypeChecking(
    #     contexts["type_checking"]).main(
    #     ne_ast, exit_on_error=contexts["exit_on_error"])
    # print("Stage 11: doing automatic reference counting.")
    # arc_ast = arc.ARC(
    #     contexts["arc"]).main(
    #     tc_ast, exit_on_error=contexts["exit_on_error"])
    # # print("Stage 12: translating to C-like ast.")
    # # cgen_ast = cgen.CGen(
    # #     contexts["cgen"]).main(arc_ast)
    # print("Compiled!")
    # # return cgen_ast
    # return arc_ast


# def compile(text, context, file_hash=""):
    # print("Stage 1: parsing code.")
    # lp_ast = lex_parse.main(text, exit_on_error=context.exit_on_error)
    # print("Stage 2: analyzing code and getting more data from context.")
    # an_ast = analyzer.Analyzer(context.copy()).main(lp_ast)
    # print("Stage 3: checking naming rules.")
    # nr_ast = naming_rules.NamingRules(context.copy()).main(an_ast)
    # print("Stage 4: doing type inference where needed.")
    # ti_ast = type_inference.TypeInference(context.copy()).main(nr_ast)
    # print("Stage 5: adding default values where needed.")
    # dv_ast = default_value.DefaultValue(context.copy()).main(ti_ast)
    # print("Stage 6: translating standard aliases.")
    # sa_ast = std_alias.StdAlias(context.copy()).main(dv_ast)
    # print("Stage 7: translating Adrian structs to C structs.")
    # oop_ast = oop.OOP(context.copy()).main(sa_ast)
    # print("Stage 8: simplifying Adrian expressions.")
    # se_ast = simple_expr.SimpleExpr(context.copy()).main(oop_ast)
    # print("Stage 9: checking name existence.")
    # ne_ast = name_existence.NameExistence(context.copy()).main(se_ast)
    # print("Stage 10: checking types.")
    # tc_ast = type_checking.TypeChecking(context.copy()).main(ne_ast)
    # print("Stage 11: doing automatic reference counting.")
    # arc_ast = arc.ARC(context.copy()).main(tc_ast)
    # print("Stage 12: translating to C-like ast.")
    # cgen_ast = cgen.CGen(context.copy()).main(arc_ast)
    # print("Compiled!")
    # return cgen_ast


# def compile_file(
#         file_name, exit_on_error=True,
#         encoding="utf-8"):
#     return "\n".join([
#         compile(
#             line, exit_on_error=exit_on_error,
#             file_hash=_get_file_hash(file_name))
#         for line in _read_file(file_name, encoding)
#     ])
