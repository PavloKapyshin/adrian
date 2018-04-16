import importlib
import pathlib

from . import astlib, layers, errors, defs, utils
from .context import context
from .utils import A


def get_file_hash(file_path):
    return utils.get_hash(read_file(str(file_path)))


def read_file(file_path):
    with open(file_path, "r") as file:
        contents = file.read()
    return contents


def find_file(file_name):
    file_name_with_extension = ".".join([
        str(file_name), defs.ADRIAN_FILE_EXTENSION])
    for dir_name in context.module_paths:
        dir_path = pathlib.Path(dir_name)
        for entity in dir_path.iterdir():
            if (entity.is_file() and
                    entity.name == file_name_with_extension):
                return entity
    errors.cannot_find_file(file_name_with_extension)


def load_module(module_name):
    hash_ = context.loaded_modules.get(module_name, None)
    if hash_ is None:
        file_path = find_file(module_name)
        prev_main_file_hash = context.main_file_hash
        pia = importlib.import_module("pia")
        nodes = pia.compile_from_file(str(file_path), stop_after=Loader)
        context.main_file_hash = prev_main_file_hash
        context.loaded_lines.extend(nodes)
        hash_ = get_file_hash(file_path)
        context.loaded_modules[module_name] = hash_
    return hash_


def translate_to_py_object(node):
    if node in (defs.PRINT, defs.LEN):
        return astlib.PyFunc(node)
    return astlib.PyType(node)


def n(name, hash_=None):
    hash_ = hash_ or context.main_file_hash
    return "_".join([hash_[:defs.MANGLING_PREFIX_LEN], str(name)])


def a(args, hash_=None):
    hash_ = hash_ or context.main_file_hash
    return [e(arg, hash_=hash_) for arg in args]


def t(type_, hash_=None):
    hash_ = hash_ or context.main_file_hash
    if type_ in A(astlib.ModuleMember):
        if type_.module == defs.PY_MODULE:
            return translate_to_py_object(type_.member)
        else:
            hash_ = load_module(type_.module)
            return t(type_.member, hash_=hash_)
    elif type_ in A(astlib.Name):
        if type_ not in (defs.VOID, defs.BOOL):
            return n(type_, hash_=hash_), []
    elif type_ in A(astlib.GenericType):
        return astlib.GenericType(
            t(type_.base, hash_=hash_),
            [t(param, hash_=hash_) for param in type_.parameters])
    return type_


def e(expr, hash_=None):
    hash_ = hash_ or context.main_file_hash
    if expr in A(astlib.ModuleMember):
        if expr.module == defs.PY_MODULE:
            return translate_to_py_object(expr.member)
        else:
            hash_ = load_module(expr.module)
            return e(expr.member, hash_=hash_)
    elif expr in A(astlib.Name):
        if expr not in (defs.TRUE, defs.FALSE, defs.REF, defs.SELF):
            return n(expr, hash_=hash_)
    elif expr in A(astlib.FuncCall):
        return astlib.FuncCall(e(expr.name, hash_=hash_), a(expr.args, hash_))
    elif expr in A(astlib.MethodCall):
        return astlib.MethodCall(
            e(expr.base, hash_=hash_), expr.name, a(expr.args))
    elif expr in A(astlib.Expr):
        return astlib.Expr(
            e(expr.left, hash_=hash_), expr.op, e(expr.right, hash_=hash_))
    elif expr in A(astlib.StructField):
        return astlib.StructField(e(expr.struct, hash_=hash_), expr.field)
    elif expr in A(astlib.Not):
        return astlib.Not(e(expr.expr, hash_=hash_))
    elif expr in A(astlib.Literal):
        if expr.type_ == astlib.LiteralT.vector:
            return astlib.Literal(
                expr.type_, a(expr.literal, hash_=hash_))
    return expr


def decl_args(args, hash_=None):
    hash_ = hash_ or context.main_file_hash
    return [
        (n(name, hash_=hash_), t(type_, hash_=hash_)) for name, type_ in args]


class Loader(layers.Layer):

    def __init__(self):
        self.b = layers.b(Loader)

    def if_stmt(self, stmt):
        yield astlib.If(e(stmt.expr), self.b(stmt.body))

    def elif_stmt(self, stmt):
        yield astlib.Elif(e(stmt.expr), self.b(stmt.body))

    def else_stmt(self, stmt):
        yield astlib.Else(self.b(stmt.body))

    @layers.register(astlib.Cond)
    def cond(self, stmt):
        if_ = self.if_stmt(stmt.if_)
        elifs_ = []
        for elif_ in stmt.elifs_:
            elifs_.append(self.elif_stmt(elif_))
        else_ = stmt.else_
        if else_ is not None:
            else_ = self.else_stmt(else_)
        yield astlib.Cond(if_, elifs_, else_)

    @layers.register(astlib.Return)
    def return_(self, stmt):
        yield astlib.Return(e(stmt.expr))

    @layers.register(astlib.While)
    def while_(self, stmt):
        yield astlib.While(e(stmt.expr), self.b(stmt.body))

    @layers.register(astlib.AdtMember)
    def adt_member(self, stmt):
        # TODO
        yield stmt

    def decl(self, stmt):
        yield type(stmt)(n(stmt.name), t(stmt.type_), e(stmt.expr))

    @layers.register(astlib.FuncCall)
    def func_call(self, stmt):
        yield e(stmt)

    @layers.register(astlib.MethodCall)
    def method_call(self, stmt):
        yield e(stmt)

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        yield astlib.Assignment(e(stmt.left), stmt.op, e(stmt.right))

    @layers.register(astlib.LetDecl)
    def let_decl(self, stmt):
        yield from self.decl(stmt)

    @layers.register(astlib.VarDecl)
    def var_decl(self, stmt):
        yield from self.decl(stmt)

    @layers.register(astlib.FieldDecl)
    def field_decl(self, stmt):
        yield astlib.FieldDecl(stmt.name, t(stmt.type_))

    def callable_decl(self, name, stmt):
        yield type(stmt)(
            name, decl_args(stmt.args), t(stmt.rettype), self.b(stmt.body))

    @layers.register(astlib.FuncDecl)
    def func_decl(self, stmt):
        yield from self.callable_decl(n(stmt.name), stmt)

    @layers.register(astlib.StructFuncDecl)
    def struct_func_decl(self, stmt):
        yield from self.callable_decl(stmt.name, stmt)

    @layers.register(astlib.FuncPrototype)
    def func_prototype(self, stmt):
        yield astlib.FuncPrototype(
            stmt.name, decl_args(stmt.args), t(stmt.rettype))

    def data_decl(self, stmt):
        protocols = [n(protocol) for protocol in stmt.protocols]
        yield type(stmt)(
            n(stmt.name), stmt.parameters, protocols, self.b(stmt.body))

    @layers.register(astlib.StructDecl)
    def struct_decl(self, stmt):
        yield from self.data_decl(stmt)

    @layers.register(astlib.AdtDecl)
    def adt_decl(self, stmt):
        yield from self.data_decl(stmt)

    @layers.register(astlib.ProtocolDecl)
    def protocol_decl(self, stmt):
        yield from self.data_decl(stmt)

    @layers.register(astlib.AST)
    def main(self, nodes, registry):
        translated_nodes = []
        for node in nodes:
            translated_nodes.extend(
                layers.transform_node(node, registry=registry))
        return context.loaded_lines + translated_nodes
