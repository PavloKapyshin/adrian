"""
Analyzes names and translates them into more specific.
Translates some FuncCalls to Instance objects.
"""

from . import layers, astlib, errors, cdefs, defs
from .context import context


class Analyzer(layers.Layer):

    def type_(self, type_):
        if isinstance(type_, astlib.ModuleMember):
            if type_.module_name == cdefs.CMODULE_NAME:
                return astlib.CType(str(type_.member))
        elif isinstance(type_, astlib.Name):
            if type_ == "None":
                return astlib.CType("Void")
            return astlib.TypeName(str(type_))
        elif isinstance(type_, astlib.Empty):
            return astlib.Empty()
        errors.not_implemented("type is not supported")

    def expr(self, expr):
        if isinstance(expr, astlib.Name):
            name_type = astlib.VariableName
            return name_type(str(expr))
        elif isinstance(expr, astlib.SExpr):
            return astlib.SExpr(
                op=expr.op, expr1=self.expr(expr.expr1),
                expr2=self.expr(expr.expr2))
        elif isinstance(expr, astlib.MethodCall):
            return list(self.method_call(expr))[0]
        elif isinstance(expr, astlib.FuncCall):
            if (isinstance(expr.name, astlib.ModuleMember) and \
                    expr.name.module_name == cdefs.CMODULE_NAME):
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
            astlib.VariableName(str(args.name)),
            self.type_(args.type_),
            self.args(args.rest))

    @layers.register(astlib.Decl)
    def decl(self, decl):
        yield astlib.Decl(
            astlib.VariableName(str(decl.name)),
            type_=self.type_(decl.type_), expr=self.expr(decl.expr))

    @layers.register(astlib.Assignment)
    def assignment(self, assignment):
        yield astlib.Assignment(
            self.expr(assignment.name), assignment.op,
            self.expr(assignment.expr))

    @layers.register(astlib.FuncCall)
    def func_call(self, call):
        name = str(call.name)
        if defs.TYPE_NAME_REGEX.fullmatch(str(call.name)):
            name = "".join([
                str(call.name), defs.INIT_METHOD_NAME])
        yield astlib.FuncCall(
            astlib.FunctionName(name),
            self.call_args(call.args))

    @layers.register(astlib.MethodCall)
    def method_call(self, call):
        yield astlib.MethodCall(
            astlib.VariableName(str(call.struct)),
            astlib.MethodName(str(call.method)),
            self.call_args(call.args))

    @layers.register(astlib.Return)
    def return_(self, return_):
        yield astlib.Return(self.expr(return_.expr))

    @layers.register(astlib.Field)
    def field(self, field):
        yield astlib.Field(
            astlib.VariableName(str(field.name)),
            type_=self.type_(field.type_))

    @layers.register(astlib.Struct)
    def struct(self, struct):
        registry = Analyzer().get_registry()
        yield astlib.Struct(
            astlib.TypeName(str(struct.name)),
            self.body(struct.body, registry))

    @layers.register(astlib.Func)
    def func(self, func):
        registry = Analyzer().get_registry()
        yield astlib.Func(
            astlib.FunctionName(str(func.name)),
            args=self.args(func.args),
            type_=self.type_(func.type_),
            body=self.body(func.body, registry))

    @layers.register(astlib.Method)
    def method(self, method):
        registry = Analyzer().get_registry()
        yield astlib.Method(
            astlib.MethodName(str(method.name)),
            args=self.args(method.args),
            type_=self.type_(method.type_),
            body=self.body(method.body, registry))
