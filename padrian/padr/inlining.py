from copy import deepcopy

from . import astlib, layers, utils, env, defs, errors, inference
from .context import context
from .utils import A


class _Mapping:

    def __init__(self):
        self._mapping = {}

    def _validate_key(self, key):
        if key in A(astlib.ParamedType):
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
            errors.key_error(initial_key, request=key, container=_Mapping)
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
        elif type_ in A(astlib.ParamedType):
            return astlib.ParamedType(
                type_.base, list(map(self.apply_for_type, type_.params)))
        return type_

    def apply_for_node(self, node):
        if node in A(astlib.Name):
            tfound = self.type_mapping.get(node)
            if tfound:
                return tfound
            afound = self.args_mapping.get(node)
            if afound:
                return afound
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
        if type_ in A(astlib.Name, astlib.DataMember):
            return
        struct_info = utils.get_type_info(type_)
        for decl_param, param in zip(struct_info["params"], type_.params):
            self.type_mapping[decl_param] = param

    def fill_args_mapping(self, decl_args, args):
        for (arg_name, _), arg_value in zip(decl_args, args):
            self.args_mapping[arg_name] = arg_value


class _CoreInlining(layers.Layer):

    def __init__(self, mapping=None):
        self.mapping = mapping or Mapping()
        self.env = env.Env()

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
            return (found if found else type_)
        elif type_ in A(astlib.ParamedType):
            return astlib.ParamedType(
                type_.base, list(map(self.t, type_.params)))
        return type_

    def e(self, expr):
        if expr in A(astlib.Name):
            found = self.mapping.args_mapping.get(expr)
            return (self.n(found) if found else self.n(expr))
        elif expr in A(astlib.Callable):
            return self._e_callable(expr)
        elif expr in A(astlib.StructScalar):
            return astlib.StructScalar(self.t(expr.type_))
        elif expr in A(astlib.DataMember):
            parent = self.e(expr.parent)
            if expr.parent in A(astlib.DataMember):
                parent = astlib.Cast(
                    parent, self.t(inference.infer_type(parent)))
            return astlib.DataMember(expr.datatype, parent, expr.member)
        return expr

    def a(self, args):
        return list(map(self.e, args))

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        right = self.e(stmt.right)
        yield astlib.Assignment(
            self.e(stmt.left), stmt.op, right)

    @layers.register(astlib.Return)
    def return_stmt(self, stmt):
        yield astlib.Return(self.e(stmt.expr))

    @layers.register(astlib.Callable)
    def callable_stmt(self, stmt):
        yield self._e_callable(stmt)

    @layers.register(astlib.Decl)
    def decl(self, stmt):
        self.env[stmt.name] = 1
        expr = self.e(stmt.expr)
        yield astlib.Decl(
            stmt.decltype, self.n(stmt.name), self.t(stmt.type_), expr)

    def main(self, body, mapping):
        yield from list(layers.transform_ast(
            body, registry=_CoreInlining(mapping=mapping).get_registry()))


class Inlining(layers.Layer):

    def __init__(self, inliner=None, mapping=None):
        self.inliner = inliner or _CoreInlining()
        self.mapping = mapping or Mapping()
        self.b = layers._b(
            Inlining, inliner=self.inliner, mapping=self.mapping)

    def register_func_as_child(self, stmt):
        """Custom registration of struct_funcs."""
        # TODO: refactor with utils.registration.
        context.env.update(stmt.parent, {
            "methods": utils.add_dicts(context.env[stmt.parent], {
                stmt.name : {
                    "type_": stmt.rettype,
                    "args": stmt.args,
                    "body": stmt.body
                }
            })
        })

    def update_b(self):
        self.b = layers._b(
            Inlining, inliner=self.inliner, mapping=self.mapping)

    def inline(self, parent, method_name, args):
        method_info, struct_info = utils.get_method_and_parent_infos(
            parent, method_name)
        method_decl_args = method_info["args"]
        self.mapping.fill_args_mapping(method_decl_args, args)
        +context.env
        utils.register_args(method_decl_args)
        self.update_b()
        # ???????
        old_mapping = deepcopy(self.mapping)
        body = self.b(method_info["body"])
        self.mapping = old_mapping
        self.mapping.args_mapping = _Mapping()
        self.mapping.fill_args_mapping(method_decl_args, args)
        # end ???
        -context.env
        inlined_body = list(self.inliner.main(body, self.mapping))
        expr = None
        if inlined_body and inlined_body[-1] in A(astlib.Return):
            expr = inlined_body[-1].expr
            inlined_body = inlined_body[:-1]
        return expr, inlined_body

    def optional_inlining(self, parent, method_name, args):
        if (parent not in context.env or
                not utils.is_real_type(utils.get_node_type(parent))):
            parent = self.mapping.apply_for_type(parent)
        struct_info = utils.raw_get_type_info(parent)
        if self.mapping.args_mapping:
            args = self.mapping.apply(args)
        if struct_info and struct_info["params"]:
            result = self.inline(parent, method_name, args)
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
            return utils.get_variable_info(expr)["type_"]
        elif expr in A(astlib.DataMember):
            return self.get_the_most_high_level_type(expr.parent)
        elif expr in A(astlib.Ref):
            return self.get_the_most_high_level_type(expr.expr)
        elif expr in A(astlib.Callable):
            if expr.callabletype == astlib.CallableT.struct_func:
                if not expr.args:
                    errors.fatal_error("self is undefined")
                return inference.infer_type(expr.args[0])
            return inference.infer_type(expr)
        errors.not_implemented(
            "stmt {} is unknown".format(expr),
            func=self.get_the_most_high_level_type)

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
            utils.register_field(stmt.name, stmt.type_)
            yield stmt
        else:
            self.mapping.fill_type_mapping(stmt.type_)
            expr, stmts = self.e(stmt.expr)
            utils.register_var_or_let(
                stmt.name, stmt.decltype, stmt.type_, expr)
            yield from stmts
            yield astlib.Decl(stmt.decltype, stmt.name, stmt.type_, expr)

    @layers.register(astlib.CallableDecl)
    def callable_decl(self, stmt):
        if stmt.decltype == astlib.DeclT.struct_func:
            self.register_func_as_child(stmt)
        else:
            utils.register_func(stmt.name, stmt.rettype, stmt.args)
        +context.env
        utils.register_args(stmt.args)
        -context.env
        yield stmt

    @layers.register(astlib.DataDecl)
    def data_decl(self, stmt):
        utils.register_data_decl(
            stmt.name, stmt.decltype, stmt.params)
        +context.env
        context.parent = stmt.name
        utils.register_params(stmt.params)
        yield astlib.DataDecl(
            stmt.decltype, stmt.name, stmt.params, self.b(stmt.body))
        -context.env
