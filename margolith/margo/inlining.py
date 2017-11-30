import sys
import itertools

from . import layers, astlib, errors, defs, inference
from .context import context, get, add_to_env
from .patterns import A
from .env import Env


class TypeCaster(layers.Layer):
    def __init__(self, type_dict):
        self.type_dict = type_dict

    def cast(self, body):
        reg = TypeCaster(self.type_dict).get_registry()
        body = list(itertools.chain.from_iterable(
            map(lambda stmt: list(
                    layers.transform_node(stmt, registry=reg)),
                body)))
        return body

    def has_type_void_ptr(self, type_):
        if type_ in A(astlib.Name):
            return str(type_) in self.type_dict
        return False

    def t(self, type_):
        if type_ in A(astlib.Name):
            return self.type_dict.get(str(type_), type_)

        if type_ in A(astlib.ParameterizedType):
            type_type = self.t(type_.type_)
            parameters = []
            for t in type_.parameters:
                parameters.append(self.t(t))
            return astlib.ParameterizedType(type_type, parameters)

        return type_


    def e(self, expr):
        if expr in A(astlib.Deref):
            type_ = inference.infer(expr.expr)
            if self.has_type_void_ptr(type_):
                return astlib.Deref(
                    astlib.CCast(
                        expr=expr.expr, to=self.t(type_)))
        return expr

    @layers.register(astlib.VarDecl)
    @layers.register(astlib.LetDecl)
    def decl(self, decl):
        add_to_env(decl)
        expr = self.e(decl.expr)
        if decl in A(astlib.VarDecl):
            yield astlib.VarDecl(decl.name, decl.type_, expr)
        else:
            yield astlib.LetDecl(decl.name, decl.type_, expr)

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        yield astlib.Assignment(
            self.e(stmt.variable), stmt.op, self.e(stmt.expr))


class Fixer(layers.Layer):
    def __init__(self, type_dict):
        self.type_dict = type_dict

    def fix(self, body):
        reg = Fixer(self.type_dict).get_registry()
        body = list(itertools.chain.from_iterable(
            map(lambda stmt: list(
                    layers.transform_node(stmt, registry=reg)),
                body)))
        return TypeCaster(self.type_dict).cast(body)

    @layers.register(astlib.VarDecl)
    def var_decl(self, declaration):
        add_to_env(declaration)
        new_expr, assignments = self.e(declaration.expr, declaration.name)
        yield astlib.VarDecl(
            declaration.name, declaration.type_, new_expr)
        yield from assignments

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        new_expr, assignments = self.e(stmt.expr, stmt.variable)
        yield astlib.Assignment(
            stmt.variable, stmt.op, new_expr)
        yield from assignments

    @layers.register(astlib.StructFuncCall)
    def struct_func_call(self, call):
        if call.struct in A(astlib.CType):
            if call.func_name == defs.DEINIT_METHOD_NAME:
                yield astlib.CFuncCall("free", args=[call.args[0]])
            else:
                errors.not_implemented(context.exit_on_error, ":D")
        else:
            yield call

    def e(self, expr, name):
        if expr in A(astlib.StructFuncCall):
            if expr.struct in A(astlib.CType):
                return self.fix_struct_func_call(name, expr.struct, expr)
        return expr, []

    def get_val(self, value):
        if value in A(astlib.Name, astlib.StructMember):
            return astlib.Deref(value)
        if value in A(astlib.Expr):
            return astlib.Expr(
                value.op, self.get_val(value.left_expr),
                self.get_val(value.right_expr))
        return value

    def get_assignment(self, name, val):
        return astlib.Assignment(
            astlib.Deref(name), "=", val)

    def heapify(self, name, type_, expr):
        if type_ in A(astlib.CType):
            allocation = astlib.CFuncCall(
                "malloc", [astlib.CFuncCall(
                    "sizeof", [astlib.StructScalar(type_)])])
            assignment = self.get_assignment(name, self.get_val(expr))
            return allocation, [assignment]
        return astlib.StructFuncCall(
            type_, defs.COPY_METHOD_NAME, args=[expr]), []

    def fix_struct_func_call(self, name, type_, call):
        if call.func_name == defs.INIT_METHOD_NAME:
            return self.heapify(name, type_, call.args[0])
        if call.func_name == defs.COPY_METHOD_NAME:
            return self.heapify(name, type_, call.args[0])
        return call, []


class Applier(layers.Layer):

    def __init__(self, type_dict, arg_dict, name_dict=None):
        self.type_dict = type_dict
        self.arg_dict = arg_dict
        self.name_dict = name_dict or {}

    def apply(self, func, type_dict, arg_dict):
        reg = Applier(type_dict, arg_dict, name_dict=self.name_dict).get_registry()
        body = list(map(
            lambda stmt: list(layers.transform_node(stmt, registry=reg))[0],
            func))
        # We hope that only the last statement can be `return`.
        new_expr = None
        if body[-1] in A(astlib.Return):
            new_expr = body[-1].expr
            body = body[:-1]
        return Fixer(self.type_dict).fix(body), new_expr

    def t(self, type_):
        if type_ in A(astlib.Name):
            return self.type_dict.get(str(type_), type_)

        if type_ in A(astlib.ParameterizedType):
            type_type = self.t(type_.type_)
            parameters = []
            for t in type_.parameters:
                parameters.append(self.t(t))
            return astlib.ParameterizedType(type_type, parameters)

        return type_

    def call_args(self, args):
        return [self.e(arg) for arg in args]

    def new_tmp(self):
        tmp_name = astlib.Name(
            "".join([defs.T_STRING, str(context.tmp_count)]),
            is_tmp=True)
        context.tmp_count += 1
        return tmp_name

    def n(self, name):
        res = self.new_tmp()
        n = name
        if n in A(astlib.Name):
            n = str(n)
        self.name_dict[n] = res
        return res

    def e(self, expr):
        if expr in A(astlib.CFuncCall):
            return expr

        if expr in A(astlib.StructFuncCall):
            return astlib.StructFuncCall(
                self.t(expr.struct), expr.func_name,
                self.call_args(expr.args))

        if expr in A(astlib.Name):
            name = self.arg_dict.get(str(expr), expr)
            return self.name_dict.get(str(name), name)

        if expr in A(astlib.Deref):
            return astlib.Deref(self.e(expr.expr))

        if expr in A(astlib.Ref):
            return astlib.Ref(self.e(expr.expr))

        if expr in A(astlib.StructMember):
            name = self.arg_dict.get(str(expr.struct), expr.struct)
            struct = self.name_dict.get(str(name), name)
            member = expr.member
            if member in A(astlib.StructMember):
                member = self.e(member)
            return astlib.StructMember(struct, member)

        return expr

    @layers.register(astlib.VarDecl)
    def var_decl(self, declaration):
        type_ = self.t(declaration.type_)
        expr = self.e(declaration.expr)
        name = self.n(declaration.name)
        result = astlib.VarDecl(name, type_, expr)
        add_to_env(result)
        yield result

    @layers.register(astlib.AssignmentAndAlloc)
    def ass_and_alloc(self, statement):
        name = self.e(statement.name)
        type_ = self.t(statement.type_)
        expr = self.e(statement.expr)
        yield astlib.AssignmentAndAlloc(name, type_, expr)

    @layers.register(astlib.Assignment)
    def assignment(self, statement):
        variable = self.e(statement.variable)
        expr = self.e(statement.expr)
        yield astlib.Assignment(variable, statement.op, expr)

    @layers.register(astlib.Return)
    def return_(self, statement):
        yield astlib.Return(self.e(statement.expr))

    @layers.register(astlib.CFuncCall)
    def cfunc_call(self, call):
        yield astlib.CFuncCall(call.name, [self.e(arg) for arg in call.args])

    @layers.register(astlib.StructFuncCall)
    def struct_func_call(self, call):
        yield astlib.StructFuncCall(
            self.t(call.struct), call.func_name, self.call_args(call.args))


class Inlining(layers.Layer):

    def __init__(self, inlined_structs=None, type_dicts=None):
        self.inlined_structs = inlined_structs or Env()
        self.type_dicts = type_dicts or {}

    def get_declaration(self, expr):
        if expr in A(astlib.StructCall):
            entity = get(expr.name)
            return entity["methods"]["__init__"]["body"]

        if expr in A(astlib.StructFuncCall):
            entity = get(expr.struct)
            return entity["methods"][expr.func_name]["body"]
        errors.not_implemented(context.exit_on_error, "...")

    def check(self, declaration):
        if declaration in A(astlib.StructDecl):
            return declaration.var_types != [], declaration.var_types
        if declaration in A(astlib.StructFuncDecl):
            return self.inlined_structs.exists(
                str(declaration.struct)), []
        return False, []

    def replace_field_types(self, declaration):
        body = []
        for field_decl in declaration.body:
            if str(field_decl.type_) in declaration.var_types:
                type_ = astlib.CObject()
            else:
                type_ = field_decl.type_
            body.append(astlib.FieldDecl(field_decl.name, type_))
        return astlib.StructDecl(
            declaration.name, declaration.var_types, body)

    def need_to_inline(self, expr):
        if expr in A(astlib.StructCall):
            return self.inlined_structs.exists(str(expr.name))
        if expr in A(astlib.StructFuncCall):
            type_ = expr.struct
            if type_ in A(astlib.ParameterizedType):
                type_ = type_.type_
            return self.inlined_structs.exists(str(type_))
        return False

    def raw_inline(self, type_dict, arg_dict, body):
        return Applier(type_dict, arg_dict).apply(body, type_dict, arg_dict)

    def make_type_dict(self, value_types, decl_types):
        type_dict = {}
        for parameter_name, parameter_value in zip(decl_types, value_types):
            type_dict[str(parameter_name)] = parameter_value
        return type_dict

    def make_arg_dict(self, call_args, decl_args):
        arg_dict = {}
        for arg_name, arg_value in zip(decl_args, call_args):
            arg_dict[str(arg_name)] = arg_value
        return arg_dict

    def inline(self, type_, expr, name=None):
        if type_ in A(astlib.ParameterizedType):
            entry = get(type_.type_)
            if expr in A(astlib.StructCall):
                method = entry["methods"]["__init__"]
            elif expr in A(astlib.StructFuncCall):
                method = entry["methods"][expr.func_name]
            type_dict = self.make_type_dict(type_.parameters, entry["var_types"])
            arg_dict = self.make_arg_dict(expr.args, [arg.name for arg in method["args"]])
            if name:
                self.type_dicts[str(name)] = {
                    "main_type": type_.type_,
                    "type_dict": type_dict
                }
            return self.raw_inline(type_dict, arg_dict, self.get_declaration(expr))
        else:
            errors.not_implemented(
                context.exit_on_error,
                "no need to inline, but it is queried")

    @layers.register(astlib.StructFuncCall)
    def struct_func_call(self, call):
        if self.need_to_inline(call):
            type_ = inference.infer(call.args[0])
            inlined_body, expr = self.inline(type_, call)
            yield from inlined_body
        else:
            yield call

    @layers.register(astlib.VarDecl)
    @layers.register(astlib.LetDecl)
    def var_decl(self, declaration):
        add_to_env(declaration)
        if self.need_to_inline(declaration.expr):
            inlined_body, expr = self.inline(declaration.type_, declaration.expr, name=declaration.name)
            yield from inlined_body
            if declaration in A(astlib.VarDecl):
                yield astlib.VarDecl(declaration.name, declaration.type_, expr)
            else:
                yield astlib.LetDecl(declaration.name, declaration.type_, expr)
        else:
            yield declaration

    def get_type_dict(self, name):
        if name in A(astlib.Name):
            return self.type_dicts[str(name)]
        if name in A(astlib.StructMember):
            return self.get_type_dict(name.struct)
        if name in A(astlib.Deref):
            return self.get_type_dict(name.expr)

    def cast(self, assignment):
        variable = assignment.variable.expr
        # variable in A(astlib.Deref) == True
        return astlib.Deref(astlib.CCast(variable, to=inference.infer(assignment.expr)))

    def really_need_to_cast(self, name):
        if name in A(astlib.Name):
            return str(name) in self.type_dicts
        if name in A(astlib.StructMember):
            return self.really_need_to_cast(name.struct)
        return False

    def need_to_cast(self, name):
        if name in A(astlib.Deref):
            return self.really_need_to_cast(name.expr)
        return False

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        if self.need_to_cast(stmt.variable):
            yield astlib.Assignment(
                self.cast(stmt), stmt.op, stmt.expr)
        elif self.need_to_inline(stmt.expr):
            res = self.get_type_dict(stmt.variable)
            type_dict = res["type_dict"]
            entry = get(res["main_type"])
            if stmt.expr in A(astlib.StructCall):
                method = entry["methods"]["__init__"]
            elif stmt.expr in A(astlib.StructFuncCall):
                method = entry["methods"][stmt.expr.func_name]
            arg_dict = self.make_arg_dict(
                stmt.expr.args, [arg.name for arg in method["args"]])
            inlined_body, expr = self.raw_inline(
                type_dict, arg_dict, self.get_declaration(stmt.expr))
            yield from inlined_body
            yield astlib.Assignment(
                stmt.variable, stmt.op, expr)
        else:
            yield stmt

    @layers.register(astlib.StructDecl)
    def struct_decl(self, declaration):
        add_to_env(declaration)
        need_to_inline, var_types = self.check(declaration)
        if need_to_inline:
            self.inlined_structs.add(str(declaration.name), {
                "var_types": var_types,
                "funcs": {}
            })
            yield self.replace_field_types(declaration)
        else:
            yield declaration

    @layers.register(astlib.StructFuncDecl)
    def struct_func_decl(self, declaration):
        add_to_env(declaration)
        need_to_inline, _ = self.check(declaration)
        if need_to_inline:
            entry = self.inlined_structs.get(str(declaration.struct))
            if entry:
                funcs = entry["funcs"]
                funcs[str(declaration.func)] = declaration
                self.inlined_structs.add(str(declaration.struct), {
                    "var_types": entry["var_types"],
                    "funcs": funcs
                })
            else:
                errors.not_implemented(
                    context.exit_on_error, "unknown error")
            yield from []
        else:
            yield declaration
