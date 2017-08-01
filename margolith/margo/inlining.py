"""Inlining layer."""

import sys

from . import layers, astlib, errors, inlinelib
from .context import context


def prepare_type_for_indexing(type_):
    if isinstance(type_, astlib.ParamedType):
        return type_.base
    return type_


def get_declaration_of_callable(callable):
    if isinstance(callable, astlib.Instance):
        struct_info = context.ts.get(str(callable.name))
        return struct_info["methods"]["__init__"]
    elif isinstance(callable, astlib.MethodCall):
        base_info = context.ns.get(str(callable.base))
        struct_info = context.ts.get(
            str(prepare_type_for_indexing(base_info["type_"])))
        return struct_info["methods"][str(callable.method)]
    elif isinstance(callable, astlib.FuncCall):
        return context.fs.get(str(callable.name))
    errors.not_implemented(":( (inlining)")


def get_type_of_base(base):
    return context.ns.get(str(base))


def get_self_type(self_entry):
    type_info = get_type_info(self_entry["type_"])
    param_types = type_info["param_types"]
    if param_types:
        return astlib.ParamedType(self_entry["type_"].base, param_types)
    return self_entry["type_"]


def get_type_info(type_):
    search_request = type_
    if isinstance(type_, astlib.ParamedType):
        search_request = type_.base
    return context.ts.get(str(search_request))


class Inlining(layers.Layer):

    def __init__(self):
        self.inliner = inlinelib.Inliner()
        self.type_mapping = {}
        self.expr_mapping = {}

    def empty_maps(self):
        self.type_mapping = {}
        self.expr_mapping = {}

    def prepare_expr_mapping(self, callable, declaration):
        if isinstance(callable, astlib.MethodCall):
            self.expr_mapping["self"] = callable.base
        index = 0
        for arg in declaration.args:
            self.expr_mapping[str(arg.name)] = callable.args[index]
            index += 1

    def type_(self, type_):
        if isinstance(type_, astlib.ParamedType):
            struct_info = get_type_info(type_)
            orig_param_types = struct_info["param_types"]
            index = 0
            for param_type in orig_param_types:
                self.type_mapping[str(param_type)] = type_.params[index]
                index += 1
            return type_
        return type_
        # errors.not_implemented("type_ (inlining)")

    def expr(self, expr):
        """Returns (new expression, inlined block)."""
        empty = expr, []
        if isinstance(expr, (astlib.CINT_TYPES, astlib.Name)):
            return empty
        elif isinstance(expr, (
                astlib.MethodCall, astlib.FuncCall, astlib.Instance)):
            declaration = get_declaration_of_callable(expr)
            self.prepare_expr_mapping(expr, declaration)
            self_type = None
            if isinstance(expr, astlib.MethodCall):
                self_type = get_self_type(get_type_of_base(expr.base))
            if self.inliner.need_to_inline(declaration, self_type=self_type):
                return self.inliner.inline(
                    expr, declaration=declaration,
                    type_mapping=self.type_mapping,
                    expr_mapping=self.expr_mapping)
            return empty
        return empty
        # errors.not_implemented("expr (inlining)")

    @layers.register(astlib.Decl)
    def decl(self, decl):
        type_ = self.type_(decl.type_)
        context.ns.add(str(decl.name), {
            "type_": type_
        })
        expr, inlined = self.expr(decl.expr)
        yield from inlined
        yield astlib.Decl(decl.name, type_, expr)
        self.empty_maps()

    @layers.register(astlib.Struct)
    def struct(self, struct):
        methods = {}
        for method in (
                stmt for stmt in struct.body
                if isinstance(stmt, astlib.Method)):
            methods[str(method.name)] = method

        context.ts.add(str(struct.name), {
            "methods": methods,
            "param_types": struct.param_types
        })

        if struct.param_types:
            yield astlib.Struct(
                struct.name, struct.param_types,
                [stmt for stmt in struct.body
                if isinstance(stmt, astlib.Field)])
        else:
            yield struct

    @layers.register(astlib.MethodCall)
    def method_call(self, call):
        expr, inlined = self.expr(call)
        yield from inlined
        if expr:
            yield expr
        self.empty_maps()

    @layers.register(astlib.Func)
    def func(self, func):
        context.fs.add(str(func.name), func)
        yield func

    # @layers.register(astlib.FuncCall)
    # def func_call(self, call):
    #     errors.not_implemented("func_call (inlining)")
