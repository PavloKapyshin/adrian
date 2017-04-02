import hashlib

from . import lex_parse
from . import name_checking
from . import type_checking
from . import name_mangling
from . import ast as margo_ast


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
    lp_ast = lex_parse.main(text, exit_on_error=context.exit_on_error)
    nc_ast = name_checking.main(lp_ast, context=context)
    result = type_checking.main(nc_ast, context=context)
    if mangle_names:
        result = name_mangling.main(result, file_hash=file_hash)
    return result


def compile_file(file_name, exit_on_error=True, mangle_names=False, encoding="utf-8"):
    return "\n".join([
        compile(
            line, exit_on_error=exit_on_error,
            mangle_names=mangle_names, file_hash=_get_file_hash(file_name))
        for line in _read_line(file_name, encoding)
    ])
