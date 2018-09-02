from . import astlib, layers, defs, errors
from .context import context
from .utils import A


def is_py_obj(obj):
    if obj in A(astlib.ModuleMember):
        return obj.module == defs.MODULE_PY
    elif obj in A(astlib.Expr):
        return is_py_obj(obj.left)
    return obj in A(astlib.PyObject)


def _literal_to_struct_call(adr_type, py_type_name, args):
    return astlib.ModuleMember(
        astlib.Name(defs.MODULE_PRELUDE),
        astlib.FuncCall(
            adr_type, [astlib.PyTypeCall(py_type_name, args)]))


def desugar_literal(literal):
    if literal.type_ == astlib.LiteralT.number:
        return _literal_to_struct_call(
            astlib.Name(defs.TYPE_NUMBER), astlib.Name(defs.TYPE_INT),
            [literal])
    elif literal.type_ == astlib.LiteralT.string:
        return _literal_to_struct_call(
            astlib.Name(defs.TYPE_STRING), astlib.Name(defs.TYPE_STR),
            [literal])
    elif literal.type_ == astlib.LiteralT.vector:
        return _literal_to_struct_call(
            astlib.Name(defs.TYPE_VECTOR), astlib.Name(defs.TYPE_LIST),
            [astlib.Literal(
                literal.type_, [e(lit) for lit in literal.literal])])
    elif literal.type_ == astlib.LiteralT.dict_:
        return _literal_to_struct_call(
            astlib.Name(defs.TYPE_DICT), astlib.Name(defs.TYPE_DICT),
            [astlib.Literal(
                literal.type_,
                {e(key): e(val) for key, val in literal.literal.items()})])
    elif literal.type_ == astlib.LiteralT.set_:
        return _literal_to_struct_call(
            astlib.Name(defs.TYPE_SET), astlib.Name(defs.TYPE_SET),
            [astlib.Literal(
                literal.type_,
                {e(elem) for elem in literal.literal})])
    return literal


def _e(expr):
    if expr in A(astlib.FuncCall):
        if (expr.name in A(astlib.ModuleMember) and
                expr.name.module in (defs.MODULE_PRELUDE, defs.MODULE_PY)):
            return astlib.FuncCall(expr.name, [_e(arg) for arg in expr.args])
        else:
            return astlib.FuncCall(expr.name, [e(arg) for arg in expr.args])
    elif expr in A(astlib.Name):
        if expr in defs.PRELUDE_OBJS:
            return astlib.ModuleMember(defs.MODULE_PRELUDE, expr)
    elif expr in A(astlib.StructPath):
        return astlib.StructPath([_e(elem) for elem in expr.path])
    elif expr in A(astlib.KeywordArg):
        return astlib.KeywordArg(expr.name, _e(expr.expr))
    return expr


def e(expr):
    if expr in A(astlib.Literal):
        return desugar_literal(expr)
    elif expr in A(astlib.FuncCall):
        if expr.name in A(astlib.Name) and expr.name in defs.PRELUDE_OBJS:
            return astlib.FuncCall(
                astlib.ModuleMember(defs.MODULE_PRELUDE, expr.name),
                list(map(e, expr.args)))
    elif expr in A(astlib.Slice):
        return astlib.Slice(e(expr.base), e(expr.start), e(expr.end))
    elif expr in A(astlib.KeywordArg):
        return astlib.KeywordArg(expr.name, e(expr.expr))
    elif expr in A(astlib.Subscript):
        return astlib.Subscript(e(expr.base), e(expr.index))
    elif expr in A(astlib.Expr):
        return astlib.Expr(e(expr.left), expr.op, e(expr.right))
    elif expr in A(astlib.Not):
        return astlib.Not(e(expr.expr))
    elif expr in A(astlib.StructPath):
        return astlib.StructPath([e(elem) for elem in expr.path])
    return _e(expr)


class Desugar(layers.Layer):

    def __init__(self):
        self.b = layers.b(Desugar)

    def _make_tmp(self, expr, is_constant=True):
        tmp_name = astlib.Name(
            defs.TMP_FMT_STRING.format(context.tmp_counter))
        context.tmp_counter += 1
        if is_constant:
            decl = astlib.LetDecl(tmp_name, astlib.Empty(), expr)
        else:
            decl = astlib.VarDecl(tmp_name, astlib.Empty(), expr)
        return tmp_name, decl

    def declaration(self, decl):
        if decl.name in A(astlib.Unpacking):
            tmp_name, tmp_decl = self._make_tmp(e(decl.expr))
            yield tmp_decl
            component_n = 1
            for name in decl.name.names:
                yield type(decl)(
                    name, astlib.Empty(),
                    astlib.StructPath([
                        tmp_name,
                        defs.SPEC_METHOD_COMPONENT.format(component_n)]))
                component_n += 1
        else:
            yield type(decl)(decl.name, decl.type_, e(decl.expr))

    def struct_like_decl(self, decl):
        def _lookup_for_prelude_proto(protocol_name):
            if protocol_name in A(astlib.GenericType):
                return astlib.GenericType(
                    _lookup_for_prelude_proto(protocol_name.base),
                    [_lookup_for_prelude_proto(p) for p in protocol_name.parameters])
            if protocol_name in defs.PRELUDE_OBJS:
                return astlib.ModuleMember(defs.MODULE_PRELUDE, protocol_name)
            return protocol_name
        return type(decl)(
            _lookup_for_prelude_proto(decl.name), decl.parameters,
            [_lookup_for_prelude_proto(p) for p in decl.implemented_protocols],
            self.b(decl.body))

    @layers.register(astlib.LetDecl)
    def let_declaration(self, decl):
        yield from self.declaration(decl)

    @layers.register(astlib.VarDecl)
    def var_declaration(self, decl):
        yield from self.declaration(decl)

    @layers.register(astlib.StructDecl)
    def struct_declaration(self, decl):
        yield self.struct_like_decl(decl)

    @layers.register(astlib.ExtensionDecl)
    def extension_declaration(self, decl):
        yield self.struct_like_decl(decl)

    @layers.register(astlib.ProtocolDecl)
    def protocol_declaration(self, decl):
        yield self.struct_like_decl(decl)

    @layers.register(astlib.FuncProtoDecl)
    def func_proto_declaration(self, decl):
        yield astlib.FuncProtoDecl(decl.name, decl.args, decl.rettype)

    @layers.register(astlib.FuncDecl)
    def func_declaration(self, decl):
        yield astlib.FuncDecl(
            decl.name, decl.args, decl.rettype, self.b(decl.body))

    @layers.register(astlib.MethodDecl)
    def method_declaration(self, decl):
        yield astlib.MethodDecl(
            decl.name, decl.args, decl.rettype, self.b(decl.body))

    @layers.register(astlib.FieldDecl)
    def field_declaration(self, decl):
        yield astlib.FieldDecl(decl.name, decl.type_)

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        yield astlib.Assignment(
            e(stmt.left), stmt.op, e(stmt.right))

    @layers.register(astlib.Cond)
    def cond(self, stmt):
        decls, if_stmt = self.if_stmt(stmt.if_stmt)
        yield from decls
        yield astlib.Cond(
            if_stmt,
            [self.elif_stmt(elif_) for elif_ in stmt.elifs],
            (None if stmt.else_stmt is None
                else self.else_stmt(stmt.else_stmt)))

    @layers.register(astlib.While)
    def while_stmt(self, stmt):
        if stmt.expr in A(astlib.LetDecl):
            let_expr = e(stmt.expr.expr)
            tmp_name = astlib.Name(
                defs.TMP_FMT_STRING.format(context.tmp_counter))
            context.tmp_counter += 1
            yield astlib.VarDecl(tmp_name, astlib.Empty(), let_expr)
            yield astlib.While(
                e(astlib.Expr(tmp_name, "is", astlib.Name(defs.TYPE_SOME))),
                [astlib.LetDecl(
                    stmt.expr.name, astlib.Empty(),
                    astlib.StructPath([
                        tmp_name, astlib.Name(
                            defs.FIELD_OF_SOME)]))] + self.b(stmt.body) + [
                astlib.Assignment(tmp_name, "=", let_expr)])
        else:
            yield astlib.While(e(stmt.expr), self.b(stmt.body))

    @layers.register(astlib.For)
    def for_stmt(self, stmt):
        container = e(stmt.container)
        if is_py_obj(container):
            yield astlib.For(stmt.names, container, self.b(stmt.body))
        else:
            def next_call(parent):
                return astlib.StructPath([
                    parent, astlib.FuncCall(defs.SPEC_METHOD_NEXT, [])
                ])

            def component_call(parent, n):
                call_ = astlib.FuncCall(defs.SPEC_METHOD_COMPONENT.format(n), [])
                if parent in A(astlib.StructPath):
                    return astlib.StructPath(parent.path + [call_])
                return astlib.StructPath([parent, call_])

            def init_names(names, parent):
                inits = []
                n = 1
                for name in names:
                    if name in A(astlib.Unpacking):
                        tmp_name, tmp_decl = self._make_tmp(
                            parent, is_constant=False)
                        inits.append(tmp_decl)
                        inits.extend(init_names(
                            name.names, component_call(tmp_name, n)))
                    else:
                        inits.append(astlib.VarDecl(
                            name, astlib.Empty(), component_call(parent, n)))
                    n += 1
                return inits

            tmp_name, tmp_decl = self._make_tmp(
                astlib.StructPath([
                    container, astlib.FuncCall(defs.SPEC_METHOD_ITER, [])]))
            yield tmp_decl
            elem_name, elem_decl = self._make_tmp(
                next_call(tmp_name), is_constant=False)
            yield elem_decl
            if len(stmt.names) == 1 and stmt.names[0] not in A(astlib.Unpacking):
                preassignments = [
                    astlib.VarDecl(
                        stmt.names[0], astlib.Empty(),
                        astlib.StructPath([elem_name, "data"]))]
            else:
                assert(len(stmt.names) == 1 and stmt.names[0] in A(astlib.Unpacking))
                preassignments = init_names(
                    stmt.names[0].names, astlib.StructPath([elem_name, "data"]))
            body = preassignments + self.b(stmt.body) + [
                    astlib.Assignment(elem_name, defs.EQ, next_call(tmp_name))
                ]
            yield astlib.While(
                e(astlib.Expr(elem_name, defs.IS, astlib.Name(defs.TYPE_SOME))),
                body)

    def tail_call(self, expr):
        if expr in A(astlib.Expr):
            left_tmp_name, left_tmp_decls = self.tail_call(expr.left)
            if expr.op not in (defs.IS,):
                right_tmp_name, right_tmp_decls = self.tail_call(expr.right)
            else:
                right_tmp_name, right_tmp_decls = expr.right, []
            return (
                astlib.Expr(left_tmp_name, expr.op, right_tmp_name),
                left_tmp_decls + right_tmp_decls)
        elif expr in A(astlib.Name):
            return expr, []
        elif expr in A(astlib.FuncCall):
            args, arg_decls = [], []
            for arg in expr.args:
                tmp_name, tmp_decls = self.tail_call(arg)
                args.append(tmp_name)
                arg_decls.extend(tmp_decls)
            tmp_name, tmp_decl = self._make_tmp(
                astlib.FuncCall(expr.name, args))
            return tmp_name, arg_decls + [tmp_decl]
        return expr, []

    @layers.register(astlib.Return)
    def return_stmt(self, stmt):
        expr, decls = self.tail_call(e(stmt.expr))
        yield from decls
        yield astlib.Return(expr)

    @layers.register(astlib.FuncCall)
    def func_call(self, call):
        yield e(call)

    @layers.register(astlib.StructPath)
    def struct_path(self, struct_path):
        yield e(struct_path)

    def if_stmt(self, stmt):
        if stmt.expr in A(astlib.LetDecl):
            tmp_name = astlib.Name(
                defs.TMP_FMT_STRING.format(context.tmp_counter))
            context.tmp_counter += 1
            return (
                [astlib.LetDecl(tmp_name, astlib.Empty(), e(stmt.expr.expr))],
                astlib.If(
                    e(astlib.Expr(tmp_name, "is", astlib.Name(defs.TYPE_SOME))),
                    [astlib.LetDecl(
                        stmt.expr.name, astlib.Empty(),
                        astlib.StructPath([
                            tmp_name,
                            astlib.Name(
                                defs.FIELD_OF_SOME)]))] + self.b(stmt.body)))
        return [], astlib.If(e(stmt.expr), self.b(stmt.body))

    def elif_stmt(self, stmt):
        return astlib.Elif(e(stmt.expr), self.b(stmt.body))

    def else_stmt(self, stmt):
        return astlib.Else(self.b(stmt.body))
