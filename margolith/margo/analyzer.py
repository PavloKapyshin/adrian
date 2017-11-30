"""
Translates some FuncCalls to StructCall and StructFuncCall objects.
Translates linked lists to python's lists.
Infer types.
"""

from . import layers, astlib, errors, defs, inference
from .context import (
    context, add_to_env, add_scope, del_scope,
    get_node_type, NodeType)
from .patterns import A


def unsupported_module():
    errors.not_implemented(
        context.exit_on_error, "only c module is supported")


def is_type(name):
    return get_node_type(name) == NodeType.struct


def e_func_call(func_call):
    if func_call.name in A(astlib.ModuleMember):
        if func_call.name.module != defs.CMODULE_NAME:
            unsupported_module()
        args = func_call.args
        yield getattr(
            astlib, "C" + str(func_call.name.member))(args.value.literal)
    elif is_type(func_call.name):
        yield astlib.StructCall(
            func_call.name, call_args(func_call.args))
    else:
        yield astlib.FuncCall(
            func_call.name, call_args(func_call.args))


def e_struct_member(struct_member):
    if struct_member in A(astlib.StructMember):
        if struct_member.member in A(astlib.FuncCall):
            func_call = struct_member.member
            analyzed_member = e(struct_member.struct)
            return astlib.StructFuncCall(
                struct=inference.infer(analyzed_member),
                func_name=func_call.name,
                args=[analyzed_member] + call_args(func_call.args))

        if struct_member.member in A(astlib.Name):
            return astlib.StructMember(
                struct=e(struct_member.struct),
                member=struct_member.member)

    errors.not_implemented(
        context.exit_on_error,
        "something went wrong in analyzer.e_struct_member")


def call_args(args):
    return list(map(e, args.as_list()))


def decl_args(args):
    return [
        astlib.Arg(name, t(type_))
        for name, type_ in args.as_list()]


def var_types(types):
    return types.as_list()


def type_parameters(types):
    return [t(type_) for type_ in types.as_list()]


def t(type_):
    if type_ in A(astlib.ModuleMember):
        if type_.module == defs.CMODULE_NAME:
            return astlib.CType(str(type_.member))
        unsupported_module()

    if type_ in A(astlib.Name):
        if type_ == "Void":
            return astlib.CType("Void")

    if type_ in A(astlib.ParameterizedType):
        return astlib.ParameterizedType(
            type_.type_, type_parameters(type_.parameters))

    return type_


def e(expr):
    if expr in A(astlib.FuncCall):
        return list(e_func_call(expr))[0]

    if expr in A(astlib.StructMember):
        return e_struct_member(expr)

    if expr in A(astlib.Expr):
        return astlib.Expr(
            expr.op,
            e(expr.left_expr),
            e(expr.right_expr))

    return expr


class Analyzer(layers.Layer):

    def body(self, body):
        reg = Analyzer().get_registry()
        return list(map(
            lambda stmt: list(
                layers.transform_node(stmt, registry=reg))[0],
            body.as_list()))

    @layers.register(astlib.VarDecl)
    @layers.register(astlib.LetDecl)
    def decl(self, declaration):
        expr = e(declaration.expr)
        if declaration.type_ in A(astlib.Empty):
            declaration.type_ = inference.infer(expr)
        # Add to env after translation of the expression because
        # self-linking is an error.
        add_to_env(declaration)
        yield type(declaration)(
            declaration.name, t(declaration.type_), expr)

    @layers.register(astlib.Assignment)
    def assignment(self, assignment):
        yield astlib.Assignment(
            e(assignment.variable), assignment.op,
            e(assignment.expr))

    @layers.register(astlib.Return)
    def return_(self, return_):
        yield astlib.Return(e(return_.expr))

    @layers.register(astlib.FuncDecl)
    def func_decl(self, declaration):
        add_to_env(declaration)
        # Add to env before translation of the body because
        # self-linking is NOT an error.
        add_scope()
        body = self.body(declaration.body)
        yield astlib.FuncDecl(
            declaration.name, decl_args(declaration.args),
            t(declaration.rettype), body)
        del_scope()

    @layers.register(astlib.MethodDecl)
    def method_decl(self, declaration):
        add_scope()
        body = self.body(declaration.body)
        yield astlib.MethodDecl(
            declaration.name, decl_args(declaration.args),
            t(declaration.rettype), body)
        del_scope()

    @layers.register(astlib.FieldDecl)
    def field_decl(self, declaration):
        yield astlib.FieldDecl(
            declaration.name, t(declaration.type_))

    @layers.register(astlib.StructMember)
    def struct_member(self, struct_member):
        yield e_struct_member(struct_member)

    @layers.register(astlib.FuncCall)
    def func_call(self, func_call):
        yield from e_func_call(func_call)

    @layers.register(astlib.StructDecl)
    def struct_decl(self, declaration):
        if declaration.body in A(astlib.Empty):
            errors.not_implemented(
                context.exit_on_error, "empty structs are not supported")
        add_to_env(declaration)
        add_scope()
        body = self.body(declaration.body)
        yield astlib.StructDecl(
            declaration.name, var_types(declaration.var_types), body)
        del_scope()
