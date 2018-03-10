from . import layers, astlib, errors, defs, inference
from .context import (context, get, add_to_env, add_scope, del_scope)
from .patterns import A
from .utils import (find_file, is_adt, is_struct, is_user_type)


def unsupported_module():
    errors.not_implemented("only c module is supported")

def find_c_module(module_name):
    return find_file(
        ".".join([module_name, "c"]), context.module_paths)

def find_c_header(module_name):
    return find_file(
        ".".join([module_name, "h"]), context.module_paths)

def get_adt_subtypes(adt_type):
    entry = get(adt_type)
    return entry["fields"]

def check_adt_type(adt_type, expr):
    possible_types = get_adt_subtypes(adt_type)
    expr_type = inference.infer(expr)
    if not expr_type in possible_types:
        errors.types_are_not_equal(adt_type, expr_type)

def provide_type(type_, expr):
    if type_:
        if is_adt(type_):
            check_adt_type(type_, expr)
        return type_
    return inference.infer(expr)

def add_found_module(name):
    module_found = find_c_module(name)
    if module_found not in context.clibs_inc:
        context.clibs_inc.append(module_found)

def add_found_header(name):
    if not hexists(name + ".h"):
        context.clibs_cinc.append({
            "src": name + ".h",
            "type": "adr"
        })

def hexists(name):
    for entry in context.clibs_cinc:
        if entry["src"] == name:
            return True
    return False

def add_c_include(name):
    if not hexists(name):
        context.clibs_cinc.append({
            "src": name,
            "type": "c"
        })


def adt_f_by_t(name, adt, type_):
    adt_decl_entry = get(adt)
    return astlib.AdtMember(
        name, adt_decl_entry["fields"][type_])


class Analyzer(layers.Layer):

    def check_body(self, body, string):
        if body == []:
            errors.not_implemented(
                "empty {} are not supported".format(string))

    def call_args(self, args):
        return list(map(self.e, args))

    def decl_args(self, args):
        return [astlib.Arg(arg.name, self.t(arg.type_)) for arg in args]

    def e_func_call(self, stmt):
        if stmt.name in A(astlib.ModuleMember):
            if stmt.name.module != defs.CMODULE_NAME:
                unsupported_module()
            add_found_module(defs.ACT_CMODULE_NAME)
            add_c_include("stdint.h")
            add_found_header(defs.ACT_CMODULE_NAME)
            yield astlib.StructCall(
                stmt.name, [astlib.IntLiteral(stmt.args[0].literal)])
        elif str(stmt.name) == defs.REF:
            yield astlib.Ref(self.call_args(stmt.args)[0])
        elif is_user_type(stmt.name):
            yield astlib.StructCall(
                stmt.name, self.call_args(stmt.args))
        else:
            yield astlib.FuncCall(
                stmt.name, self.call_args(stmt.args))

    def e_struct_member(self, stmt):
        if stmt.member in A(astlib.FuncCall):
            func_call = stmt.member
            analyzed_struct = self.e(stmt.struct)
            return astlib.StructFuncCall(
                struct=inference.infer(analyzed_struct),
                func_name=func_call.name,
                args=[analyzed_struct] + self.call_args(func_call.args))
        if stmt.member in A(astlib.Name):
            return astlib.StructMember(
                struct=self.e(stmt.struct),
                member=stmt.member)

    def e(self, expr):
        if expr in A(astlib.FuncCall):
            return list(self.e_func_call(expr))[0]
        if expr in A(astlib.StructMember):
            return self.e_struct_member(expr)
        if expr in A(astlib.Expr):
            translated_left = self.e(expr.left_expr)
            return astlib.StructFuncCall(
                inference.infer(translated_left),
                defs.OPERATOR_TO_METHOD_NAME[expr.op],
                args=[translated_left, self.e(expr.right_expr)])
        return expr

    def t(self, type_):
        if type_ in A(astlib.ModuleMember):
            if type_.module != defs.CMODULE_NAME:
                unsupported_module()
            add_found_module(defs.ACT_CMODULE_NAME)
            add_c_include("stdint.h")
            add_found_header(defs.ACT_CMODULE_NAME)
            return type_
        if type_ in A(astlib.Name):
            if type_ == "Void":
                return astlib.CVoid()
        if type_ in A(astlib.ParameterizedType):
            errors.not_implemented("try another day")
        return type_

    def adt_body(self, body):
        return [
            astlib.FieldDecl("".join(["type_", str(i)]), self.t(stmt))
            for i, stmt in enumerate(body)]

    def body(self, body):
        reg = Analyzer().get_registry()
        return list(map(
            lambda stmt: list(
                layers.transform_node(stmt, registry=reg))[0], body))

    def split_adt_usage(self, name, type_, expr):
        yield astlib.VarDecl(name, type_, astlib.LeaveEmpty())
        yield astlib.Assignment(
            adt_f_by_t(name, type_, inference.infer(expr)), "=", expr)

    def _decl(self, stmt):
        expr = self.e(stmt.expr)
        type_ = self.t(provide_type(stmt.type_, expr))
        if is_adt(type_):
            result = self.split_adt_usage(stmt.name, type_, expr)
        else:
            result = [type(stmt)(stmt.name, type_, expr)]
        add_to_env(result)
        yield from result

    @layers.register(astlib.VarDecl)
    def decl(self, stmt):
        yield from self._decl(stmt)

    @layers.register(astlib.LetDecl)
    def ldecl(self, stmt):
        yield from self._decl(stmt)

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        yield astlib.Assignment(
            self.e(stmt.variable), stmt.op, self.e(stmt.expr))

    @layers.register(astlib.While)
    def while_(self, stmt):
        add_scope()
        yield astlib.While(
            self.e(stmt.expr), self.body(stmt.body))
        del_scope()

    @layers.register(astlib.Return)
    def return_(self, stmt):
        yield astlib.Return(self.e(stmt.expr))

    @layers.register(astlib.FuncDecl)
    def func_decl(self, stmt):
        self.check_body(stmt.body, "functions")
        add_to_env(stmt)
        add_scope()
        body = self.body(stmt.body)
        yield astlib.FuncDecl(
            stmt.name, self.decl_args(stmt.args),
            self.t(stmt.rettype), body)
        del_scope()

    @layers.register(astlib.StructFuncDecl)
    def method_decl(self, stmt):
        self.check_body(stmt.body, "methods")
        add_to_env(stmt)
        add_scope()
        body = self.body(stmt.body)
        yield astlib.StructFuncDecl(
            stmt.struct, stmt.func, self.decl_args(stmt.args),
            self.t(stmt.rettype), body)
        del_scope()

    @layers.register(astlib.FieldDecl)
    def field_decl(self, stmt):
        yield astlib.FieldDecl(
            stmt.name, self.t(stmt.type_))

    @layers.register(astlib.FuncCall)
    def func_call(self, stmt):
        yield from self.e_func_call(stmt)

    @layers.register(astlib.StructMember)
    def struct_member(self, stmt):
        yield self.e_struct_member(stmt)

    @layers.register(astlib.StructDecl)
    def struct_decl(self, stmt):
        self.check_body(stmt.body, "structures")
        add_to_env(stmt)
        add_scope()
        body = self.body(stmt.body)
        yield astlib.StructDecl(
            stmt.name, stmt.var_types, body)
        del_scope()

    @layers.register(astlib.ADTDecl)
    def adt_decl(self, stmt):
        self.check_body(stmt.body, "adts")
        add_scope()
        body = self.adt_body(stmt.body)
        del_scope()
        new_stmt = astlib.ADTDecl(stmt.name, body)
        add_to_env(new_stmt)
        yield new_stmt
