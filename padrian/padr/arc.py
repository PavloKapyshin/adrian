from . import astlib, layers, env, inference, utils, defs, errors
from .context import context
from .utils import A


def deinit(name, type_):
    return astlib.Callable(
        astlib.CallableT.struct_func, type_,
        defs.DEINIT_METHOD, [name])


class ARC(layers.Layer):

    def __init__(self, flist=None):
        self.flist = flist or env.Env()
        self.b = layers._b(ARC, flist=self.flist)

    def register_(self, decl):
        context.env[decl.name] = {
            "node_type": utils.declt_to_nodet(decl.decltype),
            "type_": decl.type_,
            "initialized": self.provide_initialized(
                decl.name, decl.type_, decl.expr),
            "expr": decl.expr,
        }

    def register_args(self, args):
        for name, type_ in args:
            context.env[name] = {
                "node_type": astlib.NodeT.arg,
                "type_": type_,
                "initialized": self.provide_initialized_from_type(name, type_),
            }

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
            if (t not in context.env or
                    not context.env.is_type(context.env.get_node_type(t))):
                return {}
            t_info = context.env[t]
            if t_info["node_type"] == astlib.NodeT.commont:
                return {n: {}}
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

    def get_parent(self, expr):
        if expr in A(astlib.DataMember):
            return self.get_parent(expr.parent)
        return expr

    def free(self, expr):
        if expr in A(astlib.Name):
            info = context.env.get_variable_info(expr)
            if ("expr" not in info or
                    info["expr"] in A(astlib.Empty)):
                return None
            generic_type = inference.infer_generic_type(expr)
            if context.env.is_adt(context.env.get_node_type(generic_type)):
                type_ = inference.infer_type(
                    context.env.get_variable_info(expr)["expr"])
                expr = self.get_adt_field_by_type(expr, type_)
            else:
                type_ = inference.infer_type(expr)
                expr = expr
            return deinit(expr, type_)
        elif expr in A(astlib.DataMember):
            parent = self.get_parent(expr)
            parent_info = context.env.get_variable_info(parent)
            if "expr" not in parent_info:
                return deinit(expr, inference.infer_type(expr))
            if parent_info["expr"] in A(astlib.Empty):
                return None
            generic_type = parent_info["type_"]
            if context.env.is_adt(context.env.get_node_type(generic_type)):
                type_ = inference.infer_type(parent_info["expr"])
                expr = self.get_adt_field_by_type(parent, type_)
            else:
                type_ = inference.infer_type(expr)
                expr = expr
            return deinit(expr, type_)
        return self.free(astlib.Name(expr))

    def types_are_equal(self, type1, type2):
        if type1 in A(astlib.ParamedType) and type2 in A(astlib.ParamedType):
            return type1.base == type2.base
        elif type1 in A(astlib.Name) and type2 in A(astlib.Name):
            return type1 == type2
        return False

    def get_adt_field_by_type(self, parent, type_):
        adt_type = context.env.get_variable_info(parent)["type_"]
        adt_type_info = context.env.get_type_info(adt_type)
        for field_name, field_info in adt_type_info["fields"].items():
            if self.types_are_equal(field_info["type_"], type_):
                return astlib.DataMember(
                    astlib.DataT.adt, parent, astlib.Name(field_name))
        errors.no_adt_field(adt_type, type_)

    def addtoflist(self, stmt):
        if stmt.expr in A(astlib.Ref):
            return
        elif stmt.expr in A(astlib.Callable):
            if stmt.expr.callabletype == astlib.CallableT.struct_func:
                type_info = context.env[stmt.expr.parent]
                if type_info["node_type"] == astlib.NodeT.commont:
                    return
                methods = type_info["methods"]
                if "is_arg_return" in methods[stmt.expr.name]:
                    if methods[stmt.expr.name]["is_arg_return"]:
                        return
            elif stmt.expr.callabletype == astlib.CallableT.fun:
                if "is_arg_return" in context.env[stmt.expr.name]:
                    if context.env[stmt.expr.name]["is_arg_return"]:
                        return
        self.flist[stmt.name] = {
            "type_": stmt.type_
        }

    def delfromflist(self, name):
        if name in self.flist:
            del self.flist[name]

    def free_scope(self):
        for key, info in sorted(self.flist.cspace()):
            result = self.free(key)
            if result is not None:
                yield result

    # Subcore funcs.
    def fun_decl(self, stmt):
        utils.register(stmt)
        +context.env
        +self.flist
        self.register_args(stmt.args)
        self.update_b()
        context.func = stmt.name
        body = self.b(stmt.body)
        if stmt.rettype in A(astlib.Void):
            body += list(self.free_scope())
        yield astlib.CallableDecl(
            stmt.decltype, stmt.parent, stmt.name,
            stmt.args, stmt.rettype, body)
        -context.env
        -self.flist

    def struct_func_decl(self, stmt):
        utils.register(stmt)
        +context.env
        +self.flist
        self.register_args(stmt.args)
        self.update_b()
        context.func = (stmt.name, stmt.parent)
        body = self.b(stmt.body)
        if stmt.rettype in A(astlib.Void):
            body += list(self.free_scope())
        yield astlib.CallableDecl(
            stmt.decltype, stmt.parent, stmt.name,
            stmt.args, stmt.rettype, body)
        -context.env
        -self.flist

    def is_arg(self, expr):
        if expr in A(astlib.Name):
            return context.env[expr]["node_type"] == astlib.NodeT.arg
        elif expr in A(astlib.DataMember):
            return self.is_arg(expr.parent)

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        if self.is_initialized(stmt.left):
            result = self.free(stmt.left)
            if result is not None:
                yield result
        yield stmt
        utils.register(stmt)
        self.write_initiazed(stmt.left)

    @layers.register(astlib.Return)
    def return_stmt(self, stmt):
        self.remove_return_expr_from_flist(stmt.expr)
        if self.is_arg(stmt.expr):
            if isinstance(context.func, tuple):
                func, struct = context.func
                context.env[struct]["methods"][func]["is_arg_return"] = True
            else:
                context.env[context.func]["is_arg_return"] = True
        yield from self.free_scope()
        yield stmt

    @layers.register(astlib.Callable)
    def callable_stmt(self, stmt):
        yield stmt

    @layers.register(astlib.Decl)
    def decl(self, stmt):
        if stmt.decltype == astlib.DeclT.field:
            utils.register(stmt)
            yield stmt
        else:
            self.register_(stmt)
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
        utils.register(stmt)
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
