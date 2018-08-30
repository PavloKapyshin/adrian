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
        pipa = importlib.import_module("pipa")
        nodes = pipa.compile_from_file(str(file_path), stop_after=Loader)
        context.main_file_hash = prev_main_file_hash
        context.loaded.extend(nodes)
        hash_ = get_file_hash(file_path)
        context.loaded_modules[module_name] = hash_
    return hash_


def n(name, hash_=None):
    hash_ = hash_ or context.main_file_hash
    return astlib.Name(
        "_".join([hash_[:defs.MANGLING_LENGTH], str(name)]))


def t(type_, hash_=None):
    hash_ = hash_ or context.main_file_hash
    if type_ in A(astlib.ModuleMember):
        if type_.module == defs.MODULE_PY:
            return make_py_object(type_.member)
        else:
            hash_ = load_module(type_.module)
            return t(type_.member, hash_=hash_)
    elif type_ in A(astlib.Name):
        if type_ == defs.TYPE_VOID:
            return type_
        return n(type_, hash_)
    elif type_ in A(astlib.GenericType):
        return astlib.GenericType(
            t(type_.base, hash_=hash_),
            [t(param, hash_=hash_) for param in type_.parameters])
    elif type_ in A(astlib.Empty):
        return type_
    elif type_ in A(astlib.TypeCombination):
        if type_.right in A(list):
            right = [t(elem, hash_=hash_) for elem in type_.right]
        else:
            right = t(type_.right, hash_=hash_)
        return astlib.TypeCombination(
            t(type_.left, hash_=hash_), type_.op, right)
    else:
        errors.later()


def e(expr, hash_=None):
    hash_ = hash_ or context.main_file_hash
    if expr in A(astlib.Name):
        if expr in (defs.SELF, defs.CONSTANT_TRUE, defs.CONSTANT_FALSE):
            return expr
        return n(expr, hash_=hash_)
    elif expr in A(astlib.ModuleMember):
        if expr.module == defs.MODULE_PY:
            return make_py_object(expr.member)
        else:
            hash_ = load_module(expr.module)
            return e(expr.member, hash_=hash_)
    elif expr in A(astlib.StructPath):
        expr = expr.path
        return astlib.StructPath([e(expr[0], hash_=hash_)] + [
            only_args(elem, hash_=hash_) for elem in expr[1:]])
    elif expr in A(astlib.FuncCall):
        name = e(expr.name, hash_=hash_)
        args = a(expr.args, hash_=hash_)
        if name in A(astlib.PyFunc):
            return astlib.PyFuncCall(name.name, args)
        elif name in A(astlib.PyType):
            return astlib.PyTypeCall(name.name, args)
        if expr.name in A(astlib.ModuleMember):
            h = name[:defs.MANGLING_LENGTH]
            new_args = []
            for arg in args:
                if arg in A(astlib.KeywordArg):
                    new_args.append(
                        astlib.KeywordArg(
                            astlib.Name(
                                "_".join([str(h), str(arg.name)[defs.MANGLING_LENGTH+1:]])),
                            arg.expr))
                else:
                    new_args.append(arg)
            args = new_args
        return astlib.FuncCall(name, args)
    elif expr in A(astlib.Literal):
        if expr.type_ == astlib.LiteralT.vector:
            return astlib.Literal(
                expr.type_, a(expr.literal, hash_=hash_))
        elif expr.type_ == astlib.LiteralT.set_:
            return astlib.Literal(
                expr.type_, set(a(expr.literal, hash_=hash_)))
        elif expr.type_ == astlib.LiteralT.dict_:
            return astlib.Literal(
                expr.type_, {
                    e(key, hash_=hash_): e(val, hash_=hash_)
                    for key, val in expr.literal.items()})
        return expr
    elif expr in A(astlib.Expr):
        return astlib.Expr(
            e(expr.left, hash_=hash_), expr.op,
            e(expr.right, hash_=hash_))
    elif expr in A(astlib.Not):
        return astlib.Not(e(expr.expr))
    elif expr in A(astlib.Slice):
        return astlib.Slice(
            e(expr.base, hash_=hash_), e(expr.start, hash_=hash_),
            e(expr.end, hash_=hash_))
    elif expr in A(astlib.Subscript):
        return astlib.Subscript(
            e(expr.base, hash_=hash_), e(expr.index, hash_=hash_))
    elif expr in A(astlib.KeywordArg):
        return astlib.KeywordArg(
            e(expr.name, hash_=hash_), e(expr.expr, hash_=hash_))
    elif expr in A(astlib.PyTypeCall):
        return astlib.PyTypeCall(expr.name, [e(arg) for arg in expr.args])
    elif expr in A(astlib.Empty):
        return expr
    else:
        errors.later()


def a(args, hash_=None):
    hash_ = hash_ or context.main_file_hash
    return [e(arg, hash_=hash_) for arg in args]


def only_args(node, hash_=None):
    hash_ = hash_ or context.main_file_hash
    if node in A(astlib.FuncCall):
        return astlib.FuncCall(node.name, a(node.args))
    return node


def make_py_object(node):
    if node in defs.PY_FUNCS:
        return astlib.PyFunc(node)
    elif node == defs.CONSTANT_ARGV:
        return astlib.PyConstant(node)
    return astlib.PyType(node)


class Loader(layers.Layer):

    def __init__(self):
        self.b = layers.b(Loader)

    def declaration(self, decl):
        return type(decl)(n(decl.name), t(decl.type_), e(decl.expr))

    def struct_like_decl(self, decl):
        return type(decl)(
            n(decl.name), decl.parameters,
            [t(impled) for impled in decl.implemented_protocols],
            self.b(decl.body))

    @layers.register(astlib.LetDecl)
    def let_declaration(self, decl):
        yield self.declaration(decl)

    @layers.register(astlib.VarDecl)
    def var_declaration(self, decl):
        yield self.declaration(decl)

    @layers.register(astlib.StructDecl)
    def struct_declaration(self, decl):
        yield self.struct_like_decl(decl)

    @layers.register(astlib.ExtensionDecl)
    def extension_declaration(self, decl):
        yield self.struct_like_decl(decl)

    @layers.register(astlib.ProtocolDecl)
    def protocol_declaration(self, decl):
        yield self.struct_like_decl(decl)

    @layers.register(astlib.FuncProtoDecl)
    def func_proto_declaration(self, decl):
        args = [(n(name), t(type_)) for name, type_ in decl.args]
        yield astlib.FuncProtoDecl(n(decl.name), args, t(decl.rettype))

    @layers.register(astlib.FuncDecl)
    def func_declaration(self, decl):
        args = [(n(name), t(type_)) for name, type_ in decl.args]
        yield astlib.FuncDecl(
            n(decl.name), args, t(decl.rettype), self.b(decl.body))

    @layers.register(astlib.MethodDecl)
    def method_declaration(self, decl):
        args = [(n(name), t(type_)) for name, type_ in decl.args]
        yield astlib.MethodDecl(
            decl.name, args, t(decl.rettype), self.b(decl.body))

    @layers.register(astlib.FieldDecl)
    def field_declaration(self, decl):
        yield astlib.FieldDecl(decl.name, t(decl.type_))

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        yield astlib.Assignment(
            e(stmt.left), stmt.op, e(stmt.right))

    @layers.register(astlib.Cond)
    def cond(self, stmt):
        yield astlib.Cond(
            self.if_stmt(stmt.if_stmt),
            [self.elif_stmt(elif_) for elif_ in stmt.elifs],
            (None if stmt.else_stmt is None else self.else_stmt(stmt.else_stmt)))

    @layers.register(astlib.While)
    def while_stmt(self, stmt):
        yield astlib.While(e(stmt.expr), self.b(stmt.body))

    @layers.register(astlib.For)
    def for_stmt(self, stmt):
        yield astlib.For(
            [n(name) for name in stmt.names], e(stmt.container),
            self.b(stmt.body))

    @layers.register(astlib.Return)
    def return_stmt(self, stmt):
        yield astlib.Return(e(stmt.expr))

    @layers.register(astlib.FuncCall)
    def func_call(self, call):
        yield e(call)

    @layers.register(astlib.StructPath)
    def struct_path(self, struct_path):
        yield e(struct_path)

    @layers.register(astlib.AST)
    def main(self, nodes, registry):
        translated_nodes = []
        for node in nodes:
            translated_nodes.extend(
                layers.transform_node(node, registry=registry))
        return context.loaded + translated_nodes

    def if_stmt(self, stmt):
        return astlib.If(e(stmt.expr), self.b(stmt.body))

    def elif_stmt(self, stmt):
        return astlib.Elif(e(stmt.expr), self.b(stmt.body))

    def else_stmt(self, stmt):
        return astlib.Else(self.b(stmt.body))
