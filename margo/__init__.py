import hashlib

from . import lex_parse
from . import analyzer
from . import naming_rules
from . import name_existence
from . import type_inference
from . import type_checking
from . import default_value
from . import std_alias
from . import oop
from . import simple_expr
from . import arc
# from . import name_mangling


def _get_file_hash(file_name):
    obj = hashlib.sha1()
    with open(file_name, mode="rb") as file:
        obj.update(file.read())
    return obj.hexdigest()


def _read_file(file_name, encoding):
    with open(file_name, mode="r", encoding=encoding) as file:
        contents = file.read()
    return contents.splitlines()


def compile_repl(text, contexts, mangle_names=False, file_hash=""):
    print("Stage 1: parsing code.")
    lp_ast = lex_parse.main(
        text, exit_on_error=contexts["exit_on_error"])
    print("Stage 2: analyzing code and getting more data from context.")
    an_ast = analyzer.Analyzer(contexts["analyzer"]).main(lp_ast)
    print("Stage 3: checking naming rules.")
    nr_ast = naming_rules.NamingRules(
        contexts["naming_rules"]).main(an_ast)
    print("Stage 4: doing type inference where needed.")
    ti_ast = type_inference.TypeInference(
        contexts["type_inference"]).main(nr_ast)
    print("Stage 5: adding default values where needed.")
    dv_ast = default_value.DefaultValue(
        contexts["default_value"]).main(ti_ast)
    print("Stage 6: translating standard aliases.")
    sa_ast = std_alias.StdAlias(
        contexts["std_alias"]).main(dv_ast)
    print("Stage 7: translating Adrian structs to C structs.")
    oop_ast = oop.OOP(contexts["oop"]).main(sa_ast)
    print("Stage 8: simplifying Adrian expressions.")
    se_ast = simple_expr.SimpleExpr(
        contexts["simple_expr"]).main(oop_ast)
    print("Stage 9: checking name existence.")
    ne_ast = name_existence.NameExistence(
        contexts["name_existence"]).main(se_ast)
    print("Stage 10: checking types.")
    tc_ast = type_checking.TypeChecking(
        contexts["type_checking"]).main(ne_ast)
    # print("Stage 11: doing automatic reference counting.")
    # arc_ast = arc.ARC(context).main(tc_ast)
    print("Compiled!")
    return tc_ast


def compile(text, context, mangle_names=False, file_hash=""):
    print("Stage 1: parsing code.")
    lp_ast = lex_parse.main(text, exit_on_error=context.exit_on_error)
    print("Stage 2: analyzing code and getting more data from context.")
    an_ast = analyzer.Analyzer(context.copy()).main(lp_ast)
    print("Stage 3: checking naming rules.")
    nr_ast = naming_rules.NamingRules(context.copy()).main(an_ast)
    print("Stage 4: doing type inference where needed.")
    ti_ast = type_inference.TypeInference(context.copy()).main(nr_ast)
    print("Stage 5: adding default values where needed.")
    dv_ast = default_value.DefaultValue(context.copy()).main(ti_ast)
    print("Stage 6: translating standard aliases.")
    sa_ast = std_alias.StdAlias(context.copy()).main(dv_ast)
    print("Stage 7: translating Adrian structs to C structs.")
    oop_ast = oop.OOP(context.copy()).main(sa_ast)
    print("Stage 8: simplifying Adrian expressions.")
    se_ast = simple_expr.SimpleExpr(context.copy()).main(oop_ast)
    print("Stage 9: checking name existence.")
    ne_ast = name_existence.NameExistence(context.copy()).main(se_ast)
    print("Stage 10: checking types.")
    tc_ast = type_checking.TypeChecking(context.copy()).main(ne_ast)
    print("Stage 11: doing automatic reference counting.")
    arc_ast = arc.ARC(context.copy()).main(tc_ast)
    print("Compiled!")
    return arc_ast


def compile_file(
        file_name, exit_on_error=True,
        mangle_names=False, encoding="utf-8"):
    return "\n".join([
        compile(
            line, exit_on_error=exit_on_error,
            mangle_names=mangle_names,
            file_hash=_get_file_hash(file_name))
        for line in _read_file(file_name, encoding)
    ])
