"""
Analyzes names and translates them into more specific.
Translates some FuncCalls to Instance objects.
"""

from . import layers, astlib, errors, cdefs, defs
from .context import context


class Analyzer(layers.Layer):

    def type_(self, type_):
        if isinstance(type_, astlib.ModuleMember):
            if type_.module == cdefs.CMODULE_NAME:
                return astlib.CType(str(type_.member))
        elif isinstance(type_, astlib.Name):
            if type_ == "None":
                return astlib.CType("Void")
            return type_
        elif isinstance(type_, astlib.Ref):
            return astlib.Ref(self.type_(type_.literal))
        elif isinstance(type_, astlib.Empty):
            return astlib.Empty()
        errors.not_implemented("type is not supported")

    def expr(self, expr):
        if isinstance(expr, astlib.Name):
            return expr
        elif isinstance(expr, astlib.Expr):
            return astlib.Expr(
                expr.op, lexpr=self.expr(expr.lexpr),
                rexpr=self.expr(expr.rexpr))
        elif isinstance(expr, astlib.MethodCall):
            return list(self.method_call(expr))[0]
        elif isinstance(expr, astlib.FuncCall):
            if (isinstance(expr.name, astlib.ModuleMember) and \
                    expr.name.module == cdefs.CMODULE_NAME):
                module = expr.name
                arg_length = (len(expr.args)
                    if isinstance(expr.args, astlib.CallArgs)
                    else 0)
                if arg_length != 1:
                    errors.wrong_number_of_args(
                        context.exit_on_error, expected=1, got=arg_length)
                arg = expr.args.arg
                return getattr(astlib, "C" + str(module.member))(arg.literal)
            return list(self.func_call(expr))[0]
        elif isinstance(expr, astlib.StructElem):
            return astlib.StructElem(
                self.expr(expr.name), self.expr(expr.elem))
        elif isinstance(expr, astlib.Ref):
            return astlib.Ref(self.expr(expr.literal))
        elif isinstance(expr, astlib.Unref):
            return astlib.Unref(self.expr(expr.literal))
        errors.not_implemented("expr is not supported")

    def body(self, body, registry):
        if isinstance(body, astlib.Empty):
            return astlib.Empty()
        return astlib.Body(
            list(layers.transform_node(body.stmt, registry=registry))[0],
            self.body(body.rest, registry))

    def call_args(self, args):
        if isinstance(args, astlib.Empty):
            return astlib.Empty()
        return astlib.CallArgs(
            self.expr(args.arg),
            self.call_args(args.rest))

    def args(self, args):
        if isinstance(args, astlib.Empty):
            return astlib.Empty()
        return astlib.Args(
            args.name,
            self.type_(args.type_),
            self.args(args.rest))

    @layers.register(astlib.Decl)
    def decl(self, decl):
        yield astlib.Decl(
            decl.name, self.type_(decl.type_), self.expr(decl.expr))

    @layers.register(astlib.Assignment)
    def assignment(self, assment):
        yield astlib.Assignment(
            self.expr(assment.var), assment.op,
            self.expr(assment.expr))

    @layers.register(astlib.FuncCall)
    def func_call(self, call):
        node_cls = astlib.FuncCall
        if defs.TYPE_NAME_REGEX.fullmatch(str(call.name)):
            node_cls = astlib.Instance
        yield node_cls(call.name, self.call_args(call.args))

    @layers.register(astlib.MethodCall)
    def method_call(self, call):
        yield astlib.MethodCall(
            call.base, call.method, self.call_args(call.args))

    @layers.register(astlib.Return)
    def return_(self, return_):
        yield astlib.Return(self.expr(return_.expr))

    @layers.register(astlib.Field)
    def field(self, field):
        yield astlib.Field(
            field.name, self.type_(field.type_))

    @layers.register(astlib.Struct)
    def struct(self, struct):
        registry = Analyzer().get_registry()
        yield astlib.Struct(
            struct.name, self.body(struct.body, registry))

    @layers.register(astlib.Func)
    def func(self, func):
        registry = Analyzer().get_registry()
        yield astlib.Func(
            func.name, self.args(func.args), self.type_(func.rettype),
            self.body(func.body, registry))

    @layers.register(astlib.Method)
    def method(self, method):
        registry = Analyzer().get_registry()
        yield astlib.Method(
            method.name, self.args(method.args), self.type_(method.rettype),
            self.body(method.body, registry))
