from . import astlib, layers, env, inference, utils, defs
from .context import context
from .utils import A


def _for_env(type_):
    if type_ in A(astlib.ParamedType):
        return type_.type_
    return type_


def deinit(name, type_):
    return astlib.Callable(
        astlib.CallableT.struct_func, type_,
        defs.DEINIT_METHOD, [name])


class ARC(layers.Layer):

    def __init__(self, flist=None):
        self.flist = flist or env.Env()
        self.b = layers._b(ARC, flist=self.flist)

    # Registration.
    def register_var_or_let(self, decl):
        context.env[decl.name] = {
            "node_type": utils.declt_to_nodet(decl.decltype),
            "type_": decl.type_,
            "initialized": self.provide_initialized(
                decl.name, decl.type_, decl.expr)
        }

    def register_args(self, args):
        for name, type_ in args:
            context.env[name] = {
                "node_type": astlib.NodeT.let,
                "type_": type_,
                "initialized": self.provide_initialized_from_type(name, type_)
            }

    # Misc.
    def update_b(self):
        self.b = layers._b(ARC, flist=self.flist)

    def is_initialized(self, name):
        if name in A(astlib.Name):
            info = context.env[name]["initialized"]
            return name in info
        elif name in A(astlib.DataMember):
            if name.parent in A(astlib.Name):
                info = context.env[name.parent]["initialized"]
                if name.parent in info:
                    return name.member in info[name.parent]
                return False
            else:
                parent = name.parent
                to_unwrap = []
                while parent in A(astlib.DataMember):
                    to_unwrap.append(parent.member)
                    parent = parent.parent
                to_unwrap.append(parent)
                to_unwrap = reversed(to_unwrap)
                info = context.env[parent]["initialized"]
                for memb in to_unwrap:
                    if memb not in info:
                        return False
                    info = info[memb]
                return True

    def provide_initialized_from_type(self, name, type_):
        if type_ in A(astlib.Name, astlib.DataMember):
            if (type_ in context.env and
                    context.env[type_]["node_type"] in (
                        astlib.NodeT.struct, astlib.NodeT.adt,
                        astlib.NodeT.protocol)):
                fields = context.env[type_]["fields"]
                result = {}
                for field, info in fields.items():
                    res = self.provide_initialized_from_type(
                        field, info["type_"])
                    result[field] = res[field]
                return {name: result}
            else:
                return {name: {}}

    def provide_initialized(self, name, type_, expr):
        if type_ in A(astlib.Void):
            return {}
        if (expr in A(astlib.Callable) and
                expr.callabletype == astlib.CallableT.cfunc):
            return {name: {}}
        def loop(n, t):
            if (_for_env(t) not in context.env or
                    not utils.is_type(_for_env(t))):
                return {}
            t_info = context.env[_for_env(t)]
            fields = t_info["fields"]
            name_inited = {}
            for field_name, field_info in fields.items():
                res = loop(field_name, field_info["type_"])
                if field_name in res:
                    name_inited[field_name] = res[field_name]
                else:
                    name_inited[field_name] = {}
            return {n: name_inited}
        return loop(name, type_)

    def write_initiazed(self, name):
        if name in A(astlib.Name):
            if name not in context.env[name]["initialized"]:
                context.env[name]["initialized"][name] = {}
        elif name in A(astlib.DataMember):
            parent = name.parent
            to_unwrap = []
            while parent in A(astlib.DataMember):
                to_unwrap.append(parent.member)
                parent = parent.parent
            to_unwrap.append(parent)
            to_unwrap = reversed(to_unwrap)
            info = context.env[parent]["initialized"]
            for memb in to_unwrap:
                if memb not in info:
                    info[memb] = {}
                info = info[memb]

    def remove_return_expr_from_flist(self, expr):
        if expr in A(astlib.Name):
            self.delfromflist(expr)
        if expr in A(astlib.DataMember):
            self.remove_return_expr_from_flist(expr.parent)

    def free(self, expr):
        if expr in A(astlib.Name):
            return deinit(expr, inference.infer_type(expr))
        elif expr in A(astlib.DataMember):
            return deinit(expr, inference.infer_type(expr))
        return self.free(astlib.Name(expr))

    def addtoflist(self, stmt):
        if stmt.expr not in A(astlib.Ref):
            self.flist[stmt.name] = {
                "type_": stmt.type_
            }

    def delfromflist(self, name):
        if name in self.flist:
            del self.flist[name]

    def free_scope(self):
        for key, info in sorted(self.flist.cspace()):
            yield self.free(key)

    # Subcore funcs.
    def fun_decl(self, stmt):
        utils.register_func(stmt.name, stmt.rettype, stmt.args)
        +context.env
        +self.flist
        self.register_args(stmt.args)
        self.update_b()
        body = self.b(stmt.body)
        if stmt.rettype in A(astlib.Void):
            body += list(self.free_scope())
        yield astlib.CallableDecl(
            stmt.decltype, stmt.parent, stmt.name,
            stmt.args, stmt.rettype, body)
        -context.env
        -self.flist

    def struct_func_decl(self, stmt):
        utils.register_func_as_child(
            stmt.parent, stmt.name, stmt.rettype, stmt.args)
        +context.env
        +self.flist
        self.register_args(stmt.args)
        self.update_b()
        body = self.b(stmt.body)
        if stmt.rettype in A(astlib.Void):
            body += list(self.free_scope())
        yield astlib.CallableDecl(
            stmt.decltype, stmt.parent, stmt.name,
            stmt.args, stmt.rettype, body)
        -context.env
        -self.flist

    # Core funcs.
    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        if self.is_initialized(stmt.left):
            yield self.free(stmt.left)
        yield stmt
        self.write_initiazed(stmt.left)

    @layers.register(astlib.Return)
    def return_stmt(self, stmt):
        self.remove_return_expr_from_flist(stmt.expr)
        yield from self.free_scope()
        yield stmt

    @layers.register(astlib.Callable)
    def callable_stmt(self, stmt):
        yield stmt

    @layers.register(astlib.Decl)
    def decl(self, stmt):
        if stmt.decltype == astlib.DeclT.field:
            utils.register_field(stmt.name, stmt.type_)
            yield stmt
        else:
            self.register_var_or_let(stmt)
            self.addtoflist(stmt)
            yield stmt

    @layers.register(astlib.CallableDecl)
    def callable_decl(self, stmt):
        if stmt.decltype == astlib.DeclT.fun:
            yield from self.fun_decl(stmt)
        if stmt.decltype == astlib.DeclT.struct_func:
            yield from self.struct_func_decl(stmt)

    @layers.register(astlib.DataDecl)
    def data_decl(self, stmt):
        utils.register_data_decl(stmt.name, stmt.decltype, stmt.params)
        +context.env
        +self.flist
        context.parent = stmt.name
        utils.register_params(stmt.params)
        self.update_b()
        yield astlib.DataDecl(
            stmt.decltype, stmt.name, stmt.params, self.b(stmt.body))
        -context.env
        -self.flist

    @layers.register(astlib.AST)
    def main(self, ast_, registry):
        yield from layers.transform_ast(ast_, registry=registry)
        yield from self.free_scope()
