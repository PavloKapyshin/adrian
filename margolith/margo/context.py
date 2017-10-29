import enum
import threading
import contextlib

from . import astlib, errors
from .patterns import A


context = threading.local()


@contextlib.contextmanager
def new_context(*, env, exit_on_error, file_hash, tmp_count):
    context.env = env
    context.exit_on_error = exit_on_error
    context.file_hash = file_hash
    context.tmp_count = tmp_count
    yield


class NodeType(enum.Enum):
    var = 1
    let = 2
    fun = 3
    struct = 4


def split_body(body):
    fields, methods = [], []
    if body in A(astlib.Body):
        body = body.as_list()
    for stmt in body:
        if stmt in A(astlib.FieldDecl):
            fields.append(stmt)
        else:
            methods.append(stmt)
    return fields, methods


def add_scope():
    context.env.add_scope()


def del_scope():
    context.env.del_scope()


def get_node_type(name):
    entity = get(name)
    return entity["node_type"]


def add_to_env(statement):
    if statement in A(astlib.VarDecl, astlib.LetDecl):
        if statement in A(astlib.VarDecl):
            node_type = NodeType.var
        else:
            node_type = NodeType.let
        context.env.add(str(statement.name), {
            "type": statement.type_,
            "node_type": node_type
        })

    if statement in A(astlib.FuncDecl):
        context.env.add(str(statement.name), {
            "type": statement.rettype,
            "node_type": NodeType.fun
        })
        if statement.args in A(astlib.Args, astlib.Empty):
            args = statement.args.as_list()
            for name, type_ in args:
                context.env.add(str(name), {
                    "type": type_,
                    "node_type": NodeType.var
                })
        else:
            for arg in statement.args:
                context.env.add(str(arg.name), {
                    "type": arg.type_,
                    "node_type": NodeType.var
                })

    if statement in A(astlib.StructFuncDecl):
        entity = get(statement.struct)
        methods = entity["methods"]
        methods[str(statement.func)] = {
            "node_type": NodeType.fun,
            "type": statement.rettype,
            "args": statement.args,
        }
        entity["methods"] = methods
        if statement.args in A(astlib.Args, astlib.Empty):
            args = statement.args.as_list()
            for name, type_ in args:
                context.env.add(str(name), {
                    "type": type_,
                    "node_type": NodeType.var
                })
        else:
            for arg in statement.args:
                context.env.add(str(arg.name), {
                    "type": arg.type_,
                    "node_type": NodeType.var
                })
        context.env.add(str(statement.struct), entity)

    if statement in A(astlib.StructDecl):
        context.env.add("self", {
            "type": statement.name,
            "node_type": NodeType.var
        })

        field_decls, method_decls = split_body(statement.body)
        fields = {}
        for field_decl in field_decls:
            fields[str(field_decl.name)] = {
                "type": field_decl.type_,
                "node_type": NodeType.var
            }

        methods = {}
        for method_decl in method_decls:
            methods[str(method_decl.name)] = {
                "type": method_decl.rettype,
                "args": method_decl.args,
                "node_type": NodeType.fun
            }

        context.env.add(str(statement.name), {
            "type": statement.name,
            "fields": fields,
            "methods": methods,
            "var_types": statement.var_types,
            "node_type": NodeType.struct
        })


def raw_get(name):
    if name in A(astlib.Name):
        return context.env.get(str(name))
    return context.env.get(name)


def get(name):
    result = raw_get(name)
    if result:
        return result
    errors.non_existing_name(context.exit_on_error, name=name)


def get_in_current_scope(name):
    if name in A(astlib.Name):
        entity = context.env.get_with_scope(str(name))
        if entity[1] == context.env.scope:
            return entity[0]
        return None
