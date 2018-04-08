from copy import deepcopy

from . import astlib, defs, env, errors, inference, layers, env_api, utils
from .context import context
from .utils import A


class _Mapping:

    def __init__(self):
        self._mapping = {}

    def _validate_key(self, key):
        if key in A(astlib.GenericType):
            return self._validate_key(key.base)
        return str(key)

    def __contains__(self, key):
        key = self._validate_key(key)
        return key in self._mapping

    def __getitem__(self, key):
        initial_key = key
        key = self._validate_key(key)
        found = self._mapping.get(key)
        if not found:
            errors.unknown_name(initial_key)
        return found

    def get(self, key):
        key = self._validate_key(key)
        found = self._mapping.get(key)
        return found

    def __setitem__(self, key, value):
        key = self._validate_key(key)
        self._mapping[key] = value

    def __bool__(self):
        return bool(self._mapping)

    def __str__(self):
        return str(self._mapping)


class Mapping:

    def __init__(self):
        self.type_mapping = _Mapping()
        self.args_mapping = _Mapping()

    def apply_for_type(self, type_):
        if type_ in A(astlib.Name):
            found = self.type_mapping.get(type_)
            return (found if found else type_)
        elif type_ in A(astlib.GenericType):
            return astlib.GenericType(
                type_.base, list(map(self.apply_for_type, type_.params)))
        return type_

    def apply_for_node(self, node):
        if node in A(astlib.Name):
            found = self.args_mapping.get(node)
            if found:
                return found
            return node
        elif node in A(astlib.DataMember):
            return astlib.DataMember(
                node.datatype, self.apply_for_node(node.parent), node.member)
        elif node in A(astlib.Callable):
            return astlib.Callable(
                node.callabletype, self.apply_for_type(node.parent),
                node.name, self.apply(node.args))
        return node

    def apply(self, for_):
        return list(map(self.apply_for_node, for_))

    def fill_type_mapping(self, type_):
        if type_ in A(
                astlib.Name, astlib.DataMember,
                astlib.Empty, astlib.LiteralType, astlib.Void):
            return
        struct_info = env_api.type_info(type_)
        for decl_param, param in zip(struct_info["params"], type_.params):
            self.type_mapping[decl_param] = param

    def fill_args_mapping(self, decl_args, args):
        for (arg_name, _), arg_value in zip(decl_args, args):
            self.args_mapping[arg_name] = arg_value


class _CoreInlining(layers.Layer):

    def __init__(self, mapping=None, env_=None):
        self.mapping = mapping or Mapping()
        self.env = env_ or env.Env()
        self.update_b()

    def update_b(self):
        self.b = layers.b(_CoreInlining, mapping=self.mapping, env_=self.env)

    def _e_callable(self, stmt):
        if stmt.callabletype in (
                astlib.CallableT.cfunc, astlib.CallableT.struct_func):
            return astlib.Callable(
                stmt.callabletype, self.t(stmt.parent),
                stmt.name, self.a(stmt.args))
        return stmt

    def n(self, name):
        if name in self.env:
            return astlib.Name(
                "".join([
                    defs.I_STRING, str(context.i_count), "_", str(name)]),
                is_user_name=False)
        return name

    def t(self, type_):
        if type_ in A(astlib.Name):
            found = self.mapping.type_mapping.get(type_)
            return found if found else type_
        elif type_ in A(astlib.GenericType):
            return astlib.GenericType(
                type_.base, list(map(self.t, type_.params)))
        return type_

    def e(self, expr):
        if expr in A(astlib.Name):
            found = self.mapping.args_mapping.get(expr)
            return self.n(found) if found else self.n(expr)
        elif expr in A(astlib.Callable):
            return self._e_callable(expr)
        elif expr in A(astlib.StructScalar):
            return astlib.StructScalar(self.t(expr.type_))
        elif expr in A(astlib.DataMember):
            parent = self.e(expr.parent)
            print("HERERERER    ", parent, "\nmember=", expr.member)
            # if expr.parent in A(astlib.DataMember):
            if expr.parent in A(astlib.DataMember) or parent in A(astlib.DataMember):
                parent = astlib.Cast(
                    parent, self.t(inference.infer_type(parent)))
            return astlib.DataMember(expr.datatype, parent, expr.member)
        elif expr in A(astlib.CExpr):
            return astlib.CExpr(self.e(expr.left), expr.op, self.e(expr.right))
        return expr

    def a(self, args):
        return list(map(self.e, args))

    def _if_stmt(self, stmt):
        context.env.add_scope()
        self.update_b()
        result = astlib.If(self.e(stmt.expr), self.b(stmt.body))
        context.env.remove_scope()
        return result

    def _elif_stmt(self, stmt):
        context.env.add_scope()
        self.update_b()
        result = astlib.Elif(self.e(stmt.expr), self.b(stmt.body))
        context.env.remove_scope()
        return result

    def _else(self, stmt):
        context.env.add_scope()
        self.update_b()
        result = astlib.Else(self.b(stmt.body))
        context.env.remove_scope()
        return result

    @layers.register(astlib.Cond)
    def translate_cond(self, stmt: astlib.Cond):
        if_stmt = self._if_stmt(stmt.if_)
        elifs = []
        for elif_ in stmt.elifs_:
            elifs.append(self._elif_stmt(elif_))
        if stmt.else_ is None:
            else_ = None
        else:
            else_ = self._else(stmt.else_)
        yield astlib.Cond(if_stmt, elifs, else_)

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        right = self.e(stmt.right)
        self.env[stmt.left] = 1
        yield astlib.Assignment(
            self.e(stmt.left), stmt.op, right)

    @layers.register(astlib.Return)
    def return_stmt(self, stmt):
        yield astlib.Return(self.e(stmt.expr))

    @layers.register(astlib.Callable)
    def callable_stmt(self, stmt):
        result = self._e_callable(stmt)
        yield result

    @layers.register(astlib.Decl)
    def decl(self, stmt):
        self.env[stmt.name] = 1
        type_ = self.t(stmt.type_)
        name = self.n(stmt.name)
        expr = self.e(stmt.expr)
        env_api.register(stmt, type_=type_, name=name, expr=expr)
        yield astlib.Decl(stmt.decltype, name, type_, expr)

    def main(self, body, mapping):
        yield from list(layers.transform_ast(
            body, registry=_CoreInlining(mapping=mapping).get_registry()))


class Inlining(layers.Layer):

    def __init__(self, inliner=None, mapping=None):
        self.inliner = inliner or _CoreInlining()
        self.mapping = mapping or Mapping()
        self.b = layers.b(
            Inlining, inliner=self.inliner, mapping=self.mapping)

    def register_func_as_child(self, stmt):
        """Custom registration of struct_funcs."""
        context.env[stmt.parent]["methods"][stmt.name] = {
            "type_": stmt.rettype,
            "args": stmt.args,
            "body": stmt.body
        }

    def update_b(self):
        self.b = layers.b(
            Inlining, inliner=self.inliner, mapping=self.mapping)

    def fix_cast(self, args):
        def _fix(arg):
            if arg in A(astlib.DataMember):
                if arg.parent in A(astlib.DataMember):
                    return astlib.DataMember(
                        arg.datatype,
                        astlib.Cast(
                            arg.parent, inference.infer_type(arg.parent)),
                        arg.member)
            elif arg in A(astlib.Callable):
                return astlib.Callable(
                    arg.callabletype, arg.parent, arg.name,
                    [fix_cast(a) for a in arg.args])
            return arg
        return [_fix(arg) for arg in args]

    def inline(self, method_info, call_args):
        print()
        print()
        print()
        print("-----------")
        print()
        args = method_info["args"]
        print("ALREADY in", self.mapping.args_mapping)
        if self.mapping.args_mapping:
            call_args = self.mapping.apply(call_args)
            call_args = self.fix_cast(call_args)
            self.mapping.args_mapping = _Mapping()
        # Empty mapping to avoid wrong arguments.
        # self.mapping.args_mapping = _Mapping()
        self.mapping.fill_args_mapping(args, call_args)
        inlined_body = list(
            self.inliner.main(method_info["body"], self.mapping))
        print()
        print("DECL_ARGS {}\nCALL_ARGS {}".format(args, call_args))
        print()
        print("before inlining (declaration)  ", method_info["body"])
        print()
        print("inlined (between)              ", inlined_body)
        print("-----------")
        inlined_body = self.b(inlined_body)
        expr = None
        if inlined_body and inlined_body[-1] in A(astlib.Return):
            # Yeah TODO: add ability to use multiply returns (ifs)
            expr = inlined_body[-1].expr
            inlined_body = inlined_body[:-1]
        self.mapping.args_mapping = _Mapping()
        return expr, inlined_body

    def optional_inlining(self, structure_name, method_name, call_args):
        structure_info = env_api.type_info(structure_name)
        if structure_info["params"]:
            method_info = env_api.method_info(structure_name, method_name)
            result = self.inline(method_info, call_args)
            context.i_count += 1
            return True, result[0], result[1]
        return False, None, []

    def _e_callable(self, expr):
        if expr.callabletype == astlib.CallableT.struct:
            is_inlined, expr_, stmts = self.optional_inlining(
                expr.name, defs.INIT_METHOD, expr.args)
            if is_inlined:
                return expr_, stmts
        elif expr.callabletype == astlib.CallableT.struct_func:
            is_inlined, expr_, stmts = self.optional_inlining(
                expr.parent, expr.name, expr.args)
            if is_inlined:
                return expr_, stmts
        return expr, []

    def e(self, expr):
        if expr in A(astlib.Callable):
            return self._e_callable(expr)
        return expr, []

    def get_the_most_high_level_type(self, expr):
        if expr in A(astlib.Name):
            return env_api.variable_info(expr)["type_"]
        elif expr in A(astlib.DataMember):
            return self.get_the_most_high_level_type(expr.parent)
        elif expr in A(astlib.Ref):
            return self.get_the_most_high_level_type(expr.expr)
        elif expr in A(astlib.Callable):
            if expr.callabletype == astlib.CallableT.struct_func:
                if not expr.args:
                    errors.fatal_error("self is undefined")
                # try:
                return inference.infer_type(expr.args[0])
                # except:
                #     print("STATE", expr)
            return inference.infer_type(expr)

    @layers.register(astlib.While)
    def while_stmt(self, stmt):
        context.env.add_scope()
        expr, decls = self.e(stmt.expr)
        yield from decls
        yield astlib.While(expr, self.b(stmt.body))
        context.env.remove_scope()

    def _if_stmt(self, stmt):
        context.env.add_scope()
        expr, decls = self.e(stmt.expr)
        result = (astlib.If(expr, self.b(stmt.body)), decls)
        context.env.remove_scope()
        return result

    def _elif_stmt(self, stmt):
        context.env.add_scope()
        expr, decls = self.e(stmt.expr)
        result = (astlib.Elif(expr, self.b(stmt.body)), decls)
        context.env.remove_scope()
        return result

    def _else(self, stmt):
        context.env.add_scope()
        result = astlib.Else(self.b(stmt.body))
        context.env.remove_scope()
        return result

    @layers.register(astlib.Cond)
    def translate_cond(self, stmt: astlib.Cond):
        if_stmt, main_decls = self._if_stmt(stmt.if_)
        elifs = []
        for elif_ in stmt.elifs_:
            res = self._elif_stmt(elif_)
            elifs.append(res[0])
            main_decls.extend(res[1])
        if stmt.else_ is None:
            else_ = None
        else:
            else_ = self._else(stmt.else_)
        yield from main_decls
        yield astlib.Cond(if_stmt, elifs, else_)

    @layers.register(astlib.Callable)
    def callable_stmt(self, stmt):
        if stmt.callabletype == astlib.CallableT.struct_func:
            self.mapping.fill_type_mapping(
                self.get_the_most_high_level_type(stmt))
            new_stmt, stmts = self._e_callable(stmt)
            yield from stmts
            if new_stmt:
                yield new_stmt
        else:
            yield stmt

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        self.mapping.fill_type_mapping(
            self.get_the_most_high_level_type(stmt.left))
        expr, stmts = self.e(stmt.right)
        env_api.register(stmt, right=expr)
        yield from stmts
        yield astlib.Assignment(stmt.left, stmt.op, expr)

    @layers.register(astlib.Return)
    def return_stmt(self, stmt):
        self.mapping.fill_type_mapping(
            self.get_the_most_high_level_type(stmt.expr))
        expr, stmts = self.e(stmt.expr)
        yield from stmts
        yield astlib.Return(expr)

    @layers.register(astlib.Decl)
    def decl(self, stmt):
        if stmt.decltype == astlib.DeclT.field:
            env_api.register(stmt)
            yield stmt
        else:
            self.mapping.fill_type_mapping(stmt.type_)
            expr, stmts = self.e(stmt.expr)
            env_api.register(stmt, expr=expr)
            yield from stmts
            yield astlib.Decl(stmt.decltype, stmt.name, stmt.type_, expr)

    @layers.register(astlib.CallableDecl)
    def callable_decl(self, stmt):
        if stmt.decltype == astlib.DeclT.struct_func:
            self.register_func_as_child(stmt)
        else:
            env_api.register(stmt)
        +context.env
        env_api.register_args(stmt.args)
        body = self.b(stmt.body)
        -context.env
        yield astlib.CallableDecl(
            stmt.decltype, stmt.parent, stmt.name, stmt.args,
            stmt.rettype, body
        )

    @layers.register(astlib.DataDecl)
    def data_decl(self, stmt):
        env_api.register(stmt)
        +context.env
        context.parent = stmt.name
        env_api.register_params(stmt.params)
        yield astlib.DataDecl(
            stmt.decltype, stmt.name, stmt.params, self.b(stmt.body))
        -context.env
