import hashlib
import os

from .context import context
from . import astlib, defs, foreign_parser, layers, parser, errors, env_api
from .utils import A


def make_py_obj(member):
    if member in (defs.PRINT, defs.LEN):
        return astlib.PyFunc(member)
    return astlib.PyType(member)


def find_file(file_name):
    file_name = ".".join([str(file_name), "adr"])
    module_paths = context.module_paths
    for dir_ in module_paths:
        for entity in os.listdir(dir_):
            full_path = os.path.join(dir_, entity)
            if (os.path.isfile(full_path) and
                    entity == file_name):
                return full_path
    errors.cannot_find_file(file_name)


def get_file_hash(contents):
    hash_ = hashlib.new("md5")
    hash_.update(bytes(contents, "utf-8"))
    return hash_.hexdigest()


def read_file(file_name):
    with open(file_name, "r") as file:
        contents = file.read()
    return contents


class Linker(layers.Layer):

    def __init__(self, main_file_hash, inlined_modules=None):
        self.main_file_hash = main_file_hash
        self.inlined_modules = inlined_modules or {}
        self.update_b()

    def inner_n(self, name, hash_):
        if name.is_mangled or name == "self":
            return name
        return astlib.Name(
            "".join([
                defs.U_STRING, hash_[:defs.MANGLING_PREFIX_LEN], str(name)]),
            is_user_name=True,
            is_mangled=True
        )

    def n(self, name):
        return self.inner_n(name, self.main_file_hash)

    def inner_t(self, type_, hash_):
        if type_ in A(astlib.Name):
            if type_ not in ("Void", defs.BOOL):
                return self.inner_n(type_, hash_)
        if type_ in A(astlib.GenericType):
            return astlib.GenericType(
                self.inner_t(type_.base, hash_),
                [self.inner_t(t, hash_) for t in type_.params])
        return type_

    def inner_a(self, args, hash_):
        return [self.inner_e(a, hash_) for a in args]

    def inner_e(self, expr, hash_):
        if expr in A(astlib.Name):
            if expr not in (defs.TRUE, defs.FALSE, defs.REF):
                return self.inner_n(expr, hash_)
        if expr in A(astlib.DataMember):
            parent = self.inner_e(expr.parent, hash_)
            member = self.inner_e(expr.member, hash_)
            return astlib.DataMember(
                expr.datatype, parent, member
            )
        if expr in A(astlib.Not):
            expr_ = self.inner_e(expr.expr, hash_)
            return astlib.Not(expr_)
        if expr in A(astlib.Callable):
            if expr.callabletype == astlib.CallableT.fun:
                name = self.inner_e(expr.name, hash_)
                args = self.inner_a(expr.args, hash_)
                return astlib.Callable(
                    expr.callabletype, expr.parent, name, args
                )
            if expr.callabletype == astlib.CallableT.struct_func:
                parent = self.inner_e(expr.parent, hash_)
                name = self.inner_e(expr.name, hash_)
                args = self.inner_a(expr.args, hash_)
                return astlib.Callable(
                    expr.callabletype, parent, name, args
                )
        if expr in A(astlib.Expr):
            left = self.inner_e(expr.left, hash_)
            right = self.inner_e(expr.right, hash_)
            return astlib.Expr(left, expr.op, right)
        return expr

    def t_params(self, params):
        stmts, ps = [], []
        for param in params:
            res = self.t(param)
            ps.append(res[0])
            stmts.extend(res[1])
        return ps, stmts

    def up(self, d_):
        for key, value in d_.items():
            if key not in self.inlined_modules:
                self.inlined_modules[key] = value

    def t(self, type_):
        if type_ in A(astlib.DataMember):
            if type_.parent != defs.PY_MODULE:
                if str(type_.parent) not in self.inlined_modules:
                    file_contents = read_file(find_file(type_.parent))
                    file_hash = get_file_hash(file_contents)
                    self.inlined_modules[str(type_.parent)] = file_hash
                    parser_ = parser.Parser()
                    current_ast = foreign_parser.main(
                        parser_.parse(file_contents))
                    linker = Linker(file_hash, self.inlined_modules)
                    stmts = list(layers.transform_ast(current_ast,
                        registry=linker.get_registry()))
                    new_type = self.inner_t(type_.member, file_hash)
                    self.up(linker.inlined_modules)
                    return new_type, stmts
                return self.inner_t(type_.member, self.inlined_modules[str(type_.parent)]), []
            return astlib.PyType(type_.member), []
        if type_ in A(astlib.Name):
            if type_ not in ("Void", defs.BOOL):
                return self.n(type_), []
        if type_ in A(astlib.GenericType):
            base, stmts1 = self.t(type_.base)
            params, stmts2 = self.t_params(type_.params)
            return astlib.GenericType(base, params), stmts1 + stmts2
        return type_, []

    def a(self, args):
        args_, stmts = [], []
        for arg in args:
            res = self.e(arg)
            args_.append(res[0])
            stmts.extend(res[1])
        return args_, stmts

    def e(self, expr):
        if expr in A(astlib.Name):
            if expr not in (defs.TRUE, defs.FALSE, defs.REF):
                return self.n(expr), []
        if expr in A(astlib.Callable):
            if expr.callabletype == astlib.CallableT.fun:
                name, stmts1 = self.e(expr.name)
                args, stmts2 = self.a(expr.args)
                return astlib.Callable(
                    expr.callabletype, expr.parent, name,
                    args
                ), stmts1 + stmts2
            if expr.callabletype == astlib.CallableT.struct_func:
                parent, stmts1 = self.e(expr.parent)
                name, stmts2 = self.e(expr.name)
                args, stmts3 = self.a(expr.args)
                return astlib.Callable(
                    expr.callabletype, parent, name, args
                ), stmts1 + stmts2 + stmts3
        if expr in A(astlib.Expr):
            left, stmts1 = self.e(expr.left)
            right, stmts2 = self.e(expr.right)
            return astlib.Expr(left, expr.op, right), stmts1 + stmts2
        if expr in A(astlib.DataMember):
            if expr.datatype == astlib.DataT.module:
                if expr.parent != defs.PY_MODULE:
                    if str(expr.parent) not in self.inlined_modules:
                        file_contents = read_file(find_file(expr.parent))
                        file_hash = get_file_hash(file_contents)
                        self.inlined_modules[str(expr.parent)] = file_hash
                        parser_ = parser.Parser()
                        current_ast = foreign_parser.main(
                            parser_.parse(file_contents))
                        linker = Linker(file_hash, self.inlined_modules)
                        new_expr = self.inner_e(expr.member, file_hash)
                        stmts = list(layers.transform_ast(current_ast,
                            registry=linker.get_registry()))
                        self.up(linker.inlined_modules)
                        return new_expr, stmts
                    return self.inner_e(expr.member,
                        self.inlined_modules[str(expr.parent)]), []
                return make_py_obj(expr.member), []
            elif expr.datatype == astlib.DataT.struct:
                parent, stmts1 = self.e(expr.parent)
                member, stmts2 = self.e(expr.member)
                return astlib.DataMember(
                    expr.datatype, parent, member
                ), stmts1 + stmts2
        if expr in A(astlib.Not):
            expr, stmts = self.e(expr.expr)
            return astlib.Not(expr), stmts
        if expr in A(astlib.Literal):
            if expr.type_ == astlib.LiteralT.vector:
                args, stmts = self.a(expr.literal)
                return astlib.Literal(expr.type_, args), stmts
        return expr, []

    def update_b(self):
        self.b = layers.b(
            Linker, main_file_hash=self.main_file_hash,
            inlined_modules=self.inlined_modules)

    def decl_args(self, args):
        new_args, stmts = [], []
        for arg_name, arg_type in args:
            name = self.n(arg_name)
            type_, stmts1 = self.t(arg_type)
            new_args.append((name, type_))
            stmts.extend(stmts1)
        return new_args, stmts

    @layers.register(astlib.While)
    def while_(self, stmt):
        expr, stmts = self.e(stmt.expr)
        self.update_b()
        body = self.b(stmt.body)
        context.inlining.extend(stmts)
        yield astlib.While(expr, body)

    def _if_stmt(self, stmt):
        expr, stmts = self.e(stmt.expr)
        self.update_b()
        body = self.b(stmt.body)
        context.inlining.extend(stmts)
        return astlib.If(expr, body)

    def _elif_stmt(self, stmt):
        expr, stmts = self.e(stmt.expr)
        self.update_b()
        body = self.b(stmt.body)
        context.inlining.extend(stmts)
        return astlib.Elif(expr, body)

    def _else(self, stmt):
        self.update_b()
        body = self.b(stmt.body)
        return astlib.Else(body)

    @layers.register(astlib.Cond)
    def cond(self, stmt):
        if_ = self._if_stmt(stmt.if_)
        elifs_ = []
        for elif_ in stmt.elifs_:
            elifs_.append(self._elif_stmt(elif_))
        if stmt.else_ in A(list):
            else_ = stmt.else_
        else:
            else_ = self._else(stmt.else_)
        yield astlib.Cond(if_, elifs_, else_)

    @layers.register(astlib.Callable)
    def callable_(self, stmt):
        expr, stmts = self.e(stmt)
        context.inlining.extend(stmts)
        yield expr

    @layers.register(astlib.Return)
    def return_stmt(self, stmt):
        expr, stmts = self.e(stmt.expr)
        context.inlining.extend(stmts)
        yield astlib.Return(expr)

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        left, stmts1 = self.e(stmt.left)
        right, stmts2 = self.e(stmt.right)
        context.inlining.extend(stmts1 + stmts2)
        yield astlib.Assignment(left, stmt.op, right)

    @layers.register(astlib.Decl)
    def decl(self, stmt):
        type_, stmts1 = self.t(stmt.type_)
        expr, stmts2 = self.e(stmt.expr)
        context.inlining.extend(stmts1 + stmts2)
        yield astlib.Decl(stmt.decltype, self.n(stmt.name), type_, expr)

    @layers.register(astlib.CallableDecl)
    def call_decl(self, stmt):
        name = self.n(stmt.name)
        args, stmts1 = self.decl_args(stmt.args)
        type_, stmts2 = self.t(stmt.rettype)
        parent, stmts3 = self.e(stmt.parent)
        self.update_b()
        body = self.b(stmt.body)
        context.inlining.extend(stmts1 + stmts2 + stmts3)
        yield astlib.CallableDecl(stmt.decltype, parent, name, args, type_, body)

    def adt_body(self, body):
        result, stmts = [], []
        for stmt in body:
            res = self.t(stmt)
            result.append(res[0])
            stmts.extend(res[1])
        return result, stmts

    @layers.register(astlib.DataMember)
    def data_member(self, stmt):
        expr, stmts = self.e(stmt)
        context.inlining.extend(stmts)
        yield expr

    @layers.register(astlib.StructDecl)
    def struct_decl(self, stmt):
        name = self.n(stmt.name)
        params = [self.n(param) for param in stmt.params]
        protocols = [self.n(proto) for proto in stmt.protocols]
        self.update_b()
        body = self.b(stmt.body)
        yield astlib.StructDecl(name, params, protocols, body)

    @layers.register(astlib.DataDecl)
    def data_decl(self, stmt):
        if stmt.decltype == astlib.DeclT.adt:
            name = self.n(stmt.name)
            params = [self.n(param) for param in stmt.params]
            body, stmts = self.adt_body(stmt.body)
            context.inlining.extend(stmts)
            yield astlib.DataDecl(
                stmt.decltype, name, params, body)
        else:
            name = self.n(stmt.name)
            params = [self.n(param) for param in stmt.params]
            self.update_b()
            body = self.b(stmt.body)
            yield astlib.DataDecl(
                stmt.decltype, name, params, body)

    @layers.register(astlib.AST)
    def main(self, stmts, registry):
        result = list(layers.transform_ast(stmts, registry=registry))
        yield from context.inlining
        yield from result


def unwrap_module_usage(input_code):
    hash_ = hashlib.new("md5")
    hash_.update(bytes(input_code, "utf-8"))
    main_file_hash = hash_.hexdigest()
    parser_ = parser.Parser()
    current_ast = foreign_parser.main(parser_.parse(input_code))
    linker = Linker(main_file_hash)
    return list(
        layers.expand_ast(current_ast, registry=linker.get_registry()))
