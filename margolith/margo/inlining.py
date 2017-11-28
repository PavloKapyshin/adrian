import sys
import itertools

from . import layers, astlib, errors, defs, heapify
from .context import context, get, add_to_env
from .patterns import A
from .env import Env


class Fixer(layers.Layer):
    def fix(self, body):
        reg = Fixer().get_registry()
        body = list(itertools.chain.from_iterable(
            map(lambda stmt: list(
                    layers.transform_node(stmt, registry=reg)),
                body)))
        return body

    @layers.register(astlib.VarDecl)
    def var_decl(self, declaration):
        new_expr, assignments = self.e(declaration.expr, declaration.name)
        add_to_env(declaration)
        yield astlib.VarDecl(
            declaration.name, declaration.type_, new_expr)
        yield from assignments

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        new_expr, assignments = self.e(stmt.expr, stmt.variable)
        yield astlib.Assignment(
            stmt.variable, stmt.op, new_expr)
        yield from assignments

    def e(self, expr, name):
        if expr in A(astlib.StructFuncCall):
            if expr.struct in A(astlib.CType):
                return self.fix_struct_func_call(expr, name)
        return expr, []

    def fix_struct_func_call(self, call, name):
        if call.func_name == defs.INIT_METHOD_NAME:
            return heapify.heapify(call.args[0], name)
        if call.func_name == defs.COPY_METHOD_NAME:
            return heapify.heapify(call.args[0], name)
        return call, []

#     def fix_struct_func_call(self, call, name):
#         if call.func_name == defs.INIT_METHOD_NAME:
#             #return call.args[0]
#             return heapify.heapify(call.args[0], name)
#         if call.func_name == defs.COPY_METHOD_NAME:
#             #return call.args[0]
#             return heapify.heapify(call.args[0], name)
#         if call.func_name == defs.DEINIT_METHOD_NAME:
#             return astlib.CFuncCall("free", args=[call.args[0]]), []
#         return call, []

#     # @layers.register(astlib.AssignmentAndAlloc)
#     # def ass_and_alloc(self, statement):
#     #     yield astlib.AssignmentAndAlloc(
#     #         statement.name, statement.type_, self.e(statement.expr))


class Applier(layers.Layer):

    def __init__(self, type_dict, arg_dict):
        self.type_dict = type_dict
        self.arg_dict = arg_dict

    def apply(self, func, type_dict, arg_dict):
        reg = Applier(type_dict, arg_dict).get_registry()
        body = list(map(
            lambda stmt: list(layers.transform_node(stmt, registry=reg))[0],
            func))
        # We hope that only the last statement can be `return`.
        new_expr = None
        if body[-1] in A(astlib.Return):
            new_expr = body[-1].expr
            body = body[:-1]
        #return Fixer().fix(body, new_expr)
        return Fixer().fix(body), new_expr
        #return body, new_expr

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
        if expr in A(astlib.CFuncCall):
            return expr

        if expr in A(astlib.StructFuncCall):
            struct = self.t(expr.struct)
            args = []
            for arg in expr.args:
                args.append(self.e(arg))
            return astlib.StructFuncCall(struct, expr.func_name, args)

        if expr in A(astlib.Name):
            return self.arg_dict.get(str(expr), expr)

        if expr in A(astlib.StructMember):
            struct = self.arg_dict.get(str(expr.struct), expr.struct)
            member = expr.member
            if member in A(astlib.StructMember):
                member = self.e(member)
            return astlib.StructMember(struct, member)

        return expr

    @layers.register(astlib.VarDecl)
    def var_decl(self, declaration):
        type_ = self.t(declaration.type_)
        expr = self.e(declaration.expr)
        add_to_env(declaration)
        yield astlib.VarDecl(declaration.name, type_, expr)

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


class Inlining(layers.Layer):

    def __init__(self, inlined_structs=None):
        self.inlined_structs = inlined_structs or Env()

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

    def inline(self, type_, expr):
        if expr in A(astlib.StructCall):
            if type_ in A(astlib.ParameterizedType):
                type_dict = {}
                entry = get(type_.type_)
                for parameter_name, parameter_value in zip(entry["var_types"], type_.parameters):
                    type_dict[str(parameter_name)] = parameter_value

                arg_dict = {}
                init_method = entry["methods"]["__init__"]
                for arg_name, arg_value in zip([arg.name for arg in init_method["args"]], expr.args):
                    arg_dict[str(arg_name)] = arg_value

                return Applier(type_dict, arg_dict).apply(self.get_declaration(expr), type_dict, arg_dict)
            else:
                errors.not_implemented(
                    context.exit_on_error,
                    "no need to inline, but it is queried")

        if expr in A(astlib.StructFuncCall):
            if type_ in A(astlib.ParameterizedType):
                type_dict = {}
                entry = get(type_.type_)
                for parameter_name, parameter_value in zip(entry["var_types"], type_.parameters):
                    type_dict[str(parameter_name)] = parameter_value

                arg_dict = {}
                method = entry["methods"][expr.func_name]
                for arg_name, arg_value in zip([arg.name for arg in method["args"]], expr.args):
                    arg_dict[str(arg_name)] = arg_value

                return Applier(type_dict, arg_dict).apply(self.get_declaration(expr), type_dict, arg_dict)
            else:
                errors.not_implemented(
                    context.exit_on_error,
                    "no need to inline, but it is queried")

        return [], expr

    @layers.register(astlib.VarDecl)
    def var_decl(self, declaration):
        add_to_env(declaration)
        if self.need_to_inline(declaration.expr):
            inlined_body, expr = self.inline(declaration.type_, declaration.expr)
            yield from inlined_body
            yield astlib.VarDecl(declaration.name, declaration.type_, expr)
        else:
            yield declaration

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
