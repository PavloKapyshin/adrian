from . import astlib, layers, env, inference, utils, defs, errors, env_api
from .context import context
from .utils import A


def deinit(name, type_):
    if type_ not in A(astlib.LiteralType):
        return astlib.Callable(
            astlib.CallableT.struct_func, type_,
            defs.DEINIT_METHOD, [name])
    return None


class ARC(layers.Layer):

    def __init__(self, flist=None):
        self.flist = flist or env.Env()
        self.b = layers.b(ARC, flist=self.flist)

    def register_(self, decl):
        context.env[decl.name] = {
            "node_type": (
                astlib.NodeT.var if decl.decltype == astlib.DeclT.var
                else astlib.NodeT.let),
            "type_": decl.type_,
            "initialized": self.provide_initialized(
                decl.name, decl.type_, decl.expr),
            "mapping": env_api.create_type_mapping(decl.type_),
            "expr": decl.expr,
        }

    def register_args(self, args):
        for name, type_ in args:
            context.env[name] = {
                "node_type": astlib.NodeT.arg,
                "type_": type_,
                "initialized": self.provide_initialized_from_type(name, type_),
                "mapping": env_api.create_type_mapping(type_),
            }

    def update_b(self):
        self.b = layers.b(ARC, flist=self.flist)

    def is_initialized(self, name):
        if name in A(astlib.Name):
            info = context.env[name]["initialized"]
            return name in info
        elif name in A(astlib.DataMember):
            if name.parent in A(astlib.Name):
                info = context.env[name.parent]["initialized"]
                if info is None:
                    return True
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
            if type_ in context.env and utils.is_real_type(type_):
                fields = context.env[type_]["fields"]
                result = {}
                for field, info in fields.items():
                    res = self.provide_initialized_from_type(
                        field, info["type_"])
                    if res is None:
                        result[field] = {}
                    else:
                        result[field] = res[field]
                return {name: result}
            else:
                return {name: {}}

    def provide_initialized(self, name, type_, expr):
        if type_ in A(astlib.Void):
            return {}
        if type_ in context.env and utils.is_adt(type_):
            return {name: {}}
        if (expr in A(astlib.Callable) and
                expr.callabletype == astlib.CallableT.cfunc):
            return {name: {}}
        def loop(n, t):
            if t not in context.env or not utils.is_type(t):
                return {}
            t_info = context.env[t]
            if t_info["node_type"] == astlib.NodeT.parameter:
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
            if info is None:
                return
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
            info = context.env[expr]
            if not info or ("expr" not in info or
                    info["expr"] in A(astlib.Empty)):
                return None
            generic_type = inference.infer_general_type(expr)
            type_ = inference.infer_general_type(expr)
            expr = expr
            return deinit(expr, type_)
        elif expr in A(astlib.DataMember):
            parent = utils.scroll_to_parent(expr)
            parent_info = env_api.variable_info(parent)
            if "expr" not in parent_info:
                return deinit(expr, inference.infer_type(expr))
            if parent_info["expr"] in A(astlib.Empty):
                return None
            generic_type = parent_info["type_"]
            type_ = inference.infer_type(expr)
            expr = expr
            return deinit(expr, type_)
        return self.free(astlib.Name(expr))

    def types_are_equal(self, type1, type2):
        if type1 in A(astlib.GenericType) and type2 in A(astlib.GenericType):
            return type1.base == type2.base
        elif type1 in A(astlib.Name) and type2 in A(astlib.Name):
            return type1 == type2
        return False

    def get_adt_field_by_type(self, parent, type_):
        adt_type = env_api.variable_info(parent)["type_"]
        adt_type_info = env_api.type_info(adt_type)
        for field_name, field_info in adt_type_info["fields"].items():
            if self.types_are_equal(field_info["type_"], type_):
                return astlib.DataMember(
                    astlib.DataT.adt, parent, astlib.Name(field_name))
        errors.no_such_field(adt_type, type_)

    def addtoflist(self, stmt):
        if stmt.expr in A(astlib.Ref):
            return
        elif stmt.expr in A(astlib.Callable):
            if stmt.expr.callabletype == astlib.CallableT.struct_func:
                type_info = context.env[stmt.expr.parent]
                if type_info["node_type"] == astlib.NodeT.parameter:
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

    def return_free_scope(self):
        for key, info in sorted(self.flist.cspace_return()):
            result = self.free(key)
            if result is not None:
                yield result

    # Subcore funcs.
    def fun_decl(self, stmt):
        env_api.register(stmt)
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
        env_api.register(stmt)
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

    def no_return_in_body(self, body):
        for stmt in body:
            if stmt in A(astlib.Return):
                return False
            elif stmt in A(astlib.Cond):
                if_stmt = stmt.if_
                elifs = stmt.elifs_
                else_ = stmt.else_

                return_in_if = not self.no_return_in_body(if_stmt.body)
                return_in_elifs = []
                for elif_ in elifs:
                    return_in_elifs.append(not self.no_return_in_body(elif_.body))
                return_in_elifs = any(return_in_elifs)
                return_in_else = True
                if else_ is not None:
                    return_in_else = not self.no_return_in_body(else_.body)
                if return_in_if or return_in_elifs or return_in_else:
                    return False
            elif stmt in A(astlib.While):
                result = not self.no_return_in_body(stmt.body)
                if result:
                    return False
        return True

    @layers.register(astlib.While)
    def while_stmt(self, stmt):
        context.env.add_virtual_scope()
        self.flist.add_virtual_scope()
        self.update_b()
        body = self.b(stmt.body)
        if self.no_return_in_body(body):
            body += list(self.free_scope())
        yield astlib.While(stmt.expr, body)
        context.env.remove_virtual_scope()
        self.flist.remove_virtual_scope()

    def _if(self, stmt):
        context.env.add_virtual_scope()
        self.flist.add_virtual_scope()
        self.update_b()
        body = self.b(stmt.body)
        if self.no_return_in_body(body):
            body += list(self.free_scope())
        result = astlib.If(stmt.expr, body)
        context.env.remove_virtual_scope()
        self.flist.remove_virtual_scope()
        return result

    def _elif(self, stmt):
        context.env.add_virtual_scope()
        self.flist.add_virtual_scope()
        self.update_b()
        body = self.b(stmt.body)
        if self.no_return_in_body(body):
            body += list(self.free_scope())
        result = astlib.Elif(stmt.expr, body)
        context.env.remove_virtual_scope()
        self.flist.remove_virtual_scope()
        return result

    def _else(self, stmt):
        context.env.add_virtual_scope()
        self.flist.add_virtual_scope()
        self.update_b()
        body = self.b(stmt.body)
        if self.no_return_in_body(body):
            body += list(self.free_scope())
        result = astlib.Else(body)
        context.env.remove_virtual_scope()
        self.flist.remove_virtual_scope()
        return result

    @layers.register(astlib.Cond)
    def translate_cond(self, stmt: astlib.Cond):
        if_ = self._if(stmt.if_)
        elifs = []
        for elif_ in stmt.elifs_:
            elifs.append(self._elif(elif_))
        if stmt.else_ is None:
            else_ = None
        else:
            else_ = self._else(stmt.else_)
        yield astlib.Cond(if_, elifs, else_)

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        if self.is_initialized(stmt.left):
            result = self.free(stmt.left)
            if result is not None:
                yield result
        yield stmt
        self.write_initiazed(stmt.left)
        env_api.register(stmt)

    @layers.register(astlib.Return)
    def return_stmt(self, stmt):
        self.remove_return_expr_from_flist(stmt.expr)
        if self.is_arg(stmt.expr):
            if isinstance(context.func, tuple):
                func, struct = context.func
                context.env[struct]["methods"][func]["is_arg_return"] = True
            else:
                context.env[context.func]["is_arg_return"] = True
        yield from self.return_free_scope()
        yield stmt

    @layers.register(astlib.Callable)
    def callable_stmt(self, stmt):
        yield stmt

    @layers.register(astlib.Decl)
    def decl(self, stmt):
        if stmt.decltype == astlib.DeclT.field:
            env_api.register(stmt)
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
        env_api.register(stmt)
        +context.env
        +self.flist
        context.parent = stmt.name
        env_api.register_params(stmt.params)
        self.update_b()
        yield astlib.DataDecl(
            stmt.decltype, stmt.name, stmt.params, self.b(stmt.body))
        -context.env
        -self.flist

    @layers.register(astlib.AST)
    def main(self, ast_, registry):
        yield from layers.transform_ast(ast_, registry=registry)
        yield from self.free_scope()