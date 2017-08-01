"""Inlining layer."""

import sys

from . import layers, astlib, errors, defs, structs
from .context import context


class Mapping:

    def __init__(self):
        self.expr = {}
        self.type_ = {}

    def fill_expr(self, entry, call):
        index = 0
        if isinstance(call, astlib.MethodCall):
            self.expr["self"] = call.base
        for arg in entry["args"]:
            self.expr[str(arg.name)] = call.args[index]
            index += 1

    def fill_type(self, type_):
        if isinstance(type_, astlib.ParamedType):
            index = 0
            param_types = context.ts.get(str(type_.base))["param_types"]
            for param_type in param_types:
                self.type_[str(param_type)] = type_.params[index]
                index += 1


class Inlining(layers.Layer):

    def __init__(self):
        self.struct_to_methods = structs.Namespace()

    def args_depends_on_param_types(self, args):
        return False

    def body_depends_on_param_types(self, body, mapping):
        for stmt in body:
            if isinstance(stmt, astlib.CFuncCall):
                for arg in stmt.args:
                    if isinstance(arg, astlib.Name):
                        if str(arg) in mapping.expr:
                            return True
        return False

    def _elem_info(self, struct_elem):
        name_type = context.ns.get(str(struct_elem.name))
        if isinstance(name_type, astlib.ParamedType):
            name_type = name_type.base
        return context.ts.get(name_type)["fields"][str(struct_elem.elem)]

    def type_depends_on_param_types(self, type_):
        if isinstance(type_, astlib.Name):
            if defs.VAR_NAME_REGEX.fullmatch(str(type_)):
                return True
        elif isinstance(type_, astlib.ParamedType):
            for param in type_.params:
                if self.type_depends_on_param_types(param):
                    return True
        return False

    def inline_call_args(self, args, mapping):
        exprs, inlined = [], []
        for arg in args:
            subres = arg
            if isinstance(subres, astlib.Name):
                if str(subres) in mapping.expr:
                    subres = mapping.expr[str(subres)]
            expr, subinlined = self.inline_expr(subres, mapping)
            exprs.append(expr)
            inlined.extend(subinlined)
        return exprs, inlined

    def _inline_method_call_struct_elem(self, call, mapping):
        name = call.base.name
        if str(name) in mapping.expr:
            name = mapping.expr[str(name)]
        elem_type = self._elem_info(astlib.StructElem(name, call.base.elem))
        if (isinstance(elem_type, astlib.CType) and \
                str(call.method) in (
                    defs.COPY_METHOD_NAME, defs.DEINIT_METHOD_NAME)):
            return astlib.StructElem(name, call.base.elem), []
        if isinstance(elem_type, astlib.ParamedType):
            elem_type = elem_type.base
        if str(elem_type) in mapping.type_:
            elem_type = mapping.type_[str(elem_type)]
        print("TYPEMAPPING", mapping.type_, file=sys.stderr)
        print("ELEM_TYPE", elem_type)
        methods = self.struct_to_methods.get(str(elem_type))
        method = methods[str(call.method)]
        mapping_ = Mapping()
        mapping_.fill_expr(method, call)
        if self.need_to_inline(method, mapping_):
            return self.inline(method, mapping_)
        return astlib.MethodCall(
            name, call.method, call.args), []

    def inline_method_call(self, call, mapping):
        base = call.base
        if isinstance(base, astlib.StructElem):
            return self._inline_method_call_struct_elem(call, mapping)
        if str(base) in mapping.expr:
            base = mapping.expr[str(base)]
        if (isinstance(base, astlib.CINT_TYPES) and \
                str(call.method) in (
                    defs.COPY_METHOD_NAME, defs.DEINIT_METHOD_NAME)):
            return base, []
        type_ = context.ns.get(str(base))
        if isinstance(type_, astlib.ParamedType):
            type_ = type_.base
        methods = self.struct_to_methods.get(str(type_))
        method = methods[str(call.method)]
        mapping_ = Mapping()
        mapping_.fill_expr(method, call)
        if self.need_to_inline(method, mapping_):
            return self.inline(method, mapping_)
        return astlib.MethodCall(
            base, call.method, call.args), []

    def inline_type(self, type_):
        if isinstance(type_, astlib.ParamedType):
            return type_.base
        elif isinstance(type_, (astlib.Name, astlib.CType)):
            return type_
        errors.not_implemented("fuck...")

    def inline_expr(self, expr, mapping):
        if isinstance(expr, astlib.CFuncCall):
            exprs, inlined = self.inline_call_args(expr.args, mapping)
            return astlib.CFuncCall(
                expr.name, exprs), inlined
        elif isinstance(expr, astlib.MethodCall):
            # TODO: make it in function.
            return self.inline_method_call(expr, mapping)
        elif isinstance(expr, astlib.StructScalar):
            return expr, []
        elif isinstance(expr, astlib.Name):
            if str(expr) in mapping.expr:
                return mapping.expr[str(expr)], []
            return expr, []
        errors.not_implemented("inline expr ({} has t {})".format(
            expr, type(expr)))

    def inline_assignment(self, assment, mapping):
        cast = None
        if isinstance(assment.var, astlib.StructElem):
            elem_type = self._elem_info(assment.var)
            if (isinstance(elem_type, astlib.Name) and \
                    defs.VAR_NAME_REGEX.fullmatch(str(elem_type))):
                cast = astlib.CPtr(astlib.CVoid())
        expr, inlined = self.inline_expr(assment.expr, mapping)
        if cast:
            expr = astlib.CCast(expr, to=cast)
        return inlined + [astlib.Assignment(assment.var, assment.op, expr)]

    def inline(self, entry, mapping):
        inlined_result = []
        return_expr = None
        for stmt in entry["body"]:
            if isinstance(stmt, astlib.Decl):
                context.ns.add(str(stmt.name), stmt.type_)
                expr, inlined = self.inline_expr(stmt.expr, mapping)
                inlined_result.extend(inlined)
                inlined_result.append(
                    astlib.Decl(
                        stmt.name, stmt.type_, expr))
            elif isinstance(stmt, astlib.Assignment):
                inlined_result.extend(
                    self.inline_assignment(stmt, mapping))
            elif isinstance(stmt, astlib.CFuncCall):
                exprs, inlined = self.inline_call_args(stmt.args, mapping)
                inlined_result.extend(inlined)
                inlined_result.append(astlib.CFuncCall(
                    stmt.name, exprs))
            elif isinstance(stmt, astlib.MethodCall):
                _, inlined = self.inline_expr(stmt, mapping)
                inlined_result.extend(inlined)
            elif isinstance(stmt, astlib.Return):
                expr, inlined = self.inline_expr(stmt.expr, mapping)
                inlined_result.extend(inlined)
                return_expr = expr
            else:
                errors.not_implemented("cant inline {}".format(type(stmt)))
        return return_expr, inlined_result

    def expr(self, expr, type_=None):
        if isinstance(expr, astlib.Instance):
            methods = self.struct_to_methods.get(str(expr.name))
            init_method_entry = methods[defs.INIT_METHOD_NAME]
            mapping = Mapping()
            mapping.fill_expr(init_method_entry, expr)
            if type_:
                mapping.fill_type(type_)
            if self.need_to_inline(init_method_entry, mapping):
                return self.inline(init_method_entry, mapping)
            return expr, []
        elif isinstance(expr, astlib.MethodCall):
            type_ = context.ns.get(str(expr.base))
            if isinstance(type_, astlib.ParamedType):
                type_ = type_.base
            methods = self.struct_to_methods.get(str(type_))
            method = methods[str(expr.method)]
            mapping = Mapping()
            mapping.fill_expr(method, expr)
            if type_:
                mapping.fill_type(type_)
            if self.need_to_inline(method, mapping):
                return self.inline(method, mapping)
            return expr, []
        errors.not_implemented("{} has type {}".format(expr, type(expr)))

    @layers.register(astlib.Decl)
    def decl(self, decl):
        context.ns.add(str(decl.name), decl.type_)
        expr, inlined = self.expr(decl.expr, type_=decl.type_)
        yield from inlined
        yield astlib.Decl(decl.name, decl.type_, expr)

    @layers.register(astlib.MethodCall)
    def method_call(self, call):
        expr, inlined = self.expr(call)
        yield from inlined
        if expr:
            yield expr

    def need_to_inline(self, method, mapping):
        return (self.type_depends_on_param_types(method["rettype"]) or \
                self.args_depends_on_param_types(method["args"]) or \
                self.body_depends_on_param_types(method["body"], mapping))

    def split_body(self, body):
        fields, methods = [], []
        for stmt in body:
            if isinstance(stmt, astlib.Field):
                fields.append(stmt)
            else:
                methods.append(stmt)
        return fields, methods

    def replace_with_ptr_void(self, fields):
        result = []
        for field in fields:
            result_type = field.type_
            # TODO: when name inside of paramedType:
            # sct LOL(someType) {
            #   value: Maybe(someType)
            # }
            if isinstance(result_type, astlib.Name):
                if defs.VAR_NAME_REGEX.fullmatch(str(result_type)):
                    result_type = astlib.CPtr(astlib.CVoid())
            result.append(astlib.Field(field.name, result_type))
        return result

    @layers.register(astlib.Struct)
    def struct(self, struct):
        fields, methods = self.split_body(struct.body)

        methods_entries = {}
        for method in methods:
            methods_entries[str(method.name)] = {
                "rettype": method.rettype,
                "args": method.args,
                "body": method.body
            }
        self.struct_to_methods.add(str(struct.name), methods_entries)

        fields_entries = {}
        for field in fields:
            fields_entries[str(field.name)] = field.type_
        context.ts.add(str(struct.name), {
            "fields": fields_entries,
            "param_types": struct.param_types
        })

        if not struct.param_types:
            yield struct
        else:
            yield astlib.Struct(
                struct.name, struct.param_types,
                self.replace_with_ptr_void(fields))