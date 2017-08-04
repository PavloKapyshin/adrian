"""InlineLib"""

import sys

from . import astlib, mappinglib, structs, defs
from .context import context


def get_type_of_assignment_expr(expr):
    if isinstance(expr, astlib.Name):
        return context.ns.get(str(expr))["type_"]
    elif isinstance(expr, astlib.StructElem):
        name_type = context.ns.get(str(expr.name))["type_"]
        type_fields = get_type_info(name_type)["fields"]
        return type_fields[str(expr.elem)].type_


def get_type_info(type_):
    search_request = type_
    if isinstance(type_, astlib.ParamedType):
        search_request = type_.base
    return context.ts.get(str(search_request))


class Inliner:

    def __init__(self):
        self.mapping = None

    def rettype_depends_on_param(self, rettype):
        if isinstance(rettype, astlib.ParamedType):
            for param in rettype.params:
                if self.rettype_depends_on_param(param):
                    return True
        elif isinstance(rettype, astlib.Name):
            if defs.VAR_NAME_REGEX.fullmatch(str(rettype)):
                return True
        return False

    def body_depends_on_param(self, declaration, self_type=None):
        ns = structs.Namespace()

        for arg in declaration.args:
            ns.add(str(arg.name), arg.type_)
        if self_type:
            ns.add("self", self_type)

        def _call_args_depend_on_param(args):
            for arg in args:
                if isinstance(arg, astlib.Name):
                    if self.rettype_depends_on_param(ns.get(str(arg))):
                        return True
            return False

        for stmt in declaration.body:
            if isinstance(stmt, astlib.CFuncCall):
                if _call_args_depend_on_param(stmt.args):
                    return True
        return False

    def need_to_inline(self, declaration, self_type=None):
        return (self.rettype_depends_on_param(declaration.rettype) or \
                self.body_depends_on_param(declaration, self_type=self_type))

    def inline(self, call, *, declaration, type_mapping, expr_mapping):
        self.mapping = mappinglib.Mapping(
            expr_mapping=expr_mapping, type_mapping=type_mapping)
        if isinstance(call, astlib.MethodCall):
            return self.inline_method(call, declaration)
        elif isinstance(call, astlib.Instance):
            return self.inline_instance(call, declaration)
        return self.inline_func(call, declaration)

    def check_for_cast(self, type_):
        if isinstance(type_, astlib.Name):
            return defs.VAR_NAME_REGEX.fullmatch(str(type_))

    def maybe_cast(self, expr, type_):
        if self.check_for_cast(type_):
            if isinstance(expr, astlib.CINT_TYPES):
                return astlib.CCast(expr, to=astlib.CPtr(astlib.CVoid()))
        return expr

    def inline_instance(self, instance, declaration):
        body = self.mapping.apply(declaration.body)
        return_expr, inlined = None, []
        for stmt in body:
            if isinstance(stmt, astlib.Decl):
                context.ns.add(str(stmt.name), {
                    "type_": stmt.type_
                })
                inlined.append(
                    astlib.Decl(stmt.name, stmt.type_, stmt.expr))
            elif isinstance(stmt, astlib.Assignment):
                expr = self.maybe_cast(
                    stmt.expr, get_type_of_assignment_expr(stmt.var))
                inlined.append(
                    astlib.Assignment(stmt.var, stmt.op, expr))
            elif isinstance(stmt, astlib.Return):
                return_expr = stmt.expr
        return return_expr, inlined

    def inline_method(self, method, declaration):
        body = self.mapping.apply(declaration.body)
        return_expr, inlined = None, []
        for stmt in body:
            if isinstance(stmt, astlib.CFuncCall):
                inlined.append(
                    astlib.CFuncCall(stmt.name, stmt.args))
            elif isinstance(stmt, astlib.MethodCall):
                inlined.append(
                    astlib.MethodCall(stmt.base, stmt.method, stmt.args))
            elif isinstance(stmt, astlib.Return):
                return_expr = stmt.expr
        return return_expr, inlined

    def inline_func(self, func, declaration):
        pass
