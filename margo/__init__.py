import hashlib

from . import lex_parse
from . import naming_rules
from . import name_existence
from . import type_inference
from . import type_checking
from . import default_value
from . import std_alias
from . import oop
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


def compile(text, context, mangle_names=False, file_hash=""):
    print("Stage 1: parsing code.")
    lp_ast = lex_parse.main(text, exit_on_error=context.exit_on_error)
    print("Stage 2: checking naming rules.")
    nr_ast = naming_rules.NamingRules(context).main(lp_ast)
    print("Stage 3: checking name existence.")
    ne_ast = name_existence.NameExistence(context).main(nr_ast)
    print("Stage 4: doing type inference where needed.")
    ti_ast = type_inference.TypeInference(context).main(ne_ast)
    print("Stage 5: checking types.")
    tc_ast = type_checking.TypeChecking(context).main(ti_ast)
    print("Stage 6: adding default values where needed.")
    dv_ast = default_value.DefaultValue(context).main(tc_ast)
    print("Stage 7: translating standard aliases.")
    sa_ast = std_alias.StdAlias(context).main(dv_ast)
    print("Stage 8: translating Adrian structs to C structs.")
    oop_ast = oop.OOP(context).main(sa_ast)
    print("Compiled!")
    return oop_ast


def compile_file(
        file_name, exit_on_error=True, mangle_names=False, encoding="utf-8"):
    return "\n".join([
        compile(
            line, exit_on_error=exit_on_error,
            mangle_names=mangle_names, file_hash=_get_file_hash(file_name))
        for line in _read_file(file_name, encoding)
    ])
