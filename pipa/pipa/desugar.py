from . import astlib, layers, defs, errors
from .context import context
from .utils import A


def is_py_obj(obj):
    if obj in A(astlib.ModuleMember):
        return obj.module == defs.MODULE_PY
    elif obj in A(astlib.Expr):
        return is_py_obj(obj.left)
    return obj in A(astlib.PyObject)


def replace_name_references_for_for_stmt_translation(body, name):
    def _replace(stmt):
        if stmt in A(astlib.LetDecl, astlib.VarDecl):
            return type(stmt)(stmt.name, stmt.type_, _replace(stmt.expr))
        elif stmt in A(astlib.Assignment):
            return astlib.Assignment(
                _replace(stmt.left), stmt.op, _replace(stmt.right))
        elif stmt in A(astlib.Cond):
            return astlib.Cond(
                _replace(stmt.if_stmt),
                [_replace(elif_stmt) for elif_stmt in stmt.elifs],
                _replace(stmt.else_stmt))
        elif stmt in A(astlib.If):
            return astlib.If(
                _replace(stmt.expr), [_replace(s) for s in stmt.body])
        elif stmt in A(astlib.Elif):
            return astlib.Elif(
                _replace(stmt.expr), [_replace(s) for s in stmt.body])
        elif stmt in A(astlib.Else):
            return astlib.Else([_replace(s) for s in stmt.body])
        elif stmt in A(astlib.While):
            return astlib.While(
                _replace(stmt.expr), [_replace(s) for s in stmt.body])
        elif stmt in A(astlib.For):
            return astlib.For(
                stmt.names, _replace(stmt.container),
                [_replace(s) for s in stmt.body])
        elif stmt in A(astlib.Return):
            return astlib.Return(_replace(stmt.expr))
        elif stmt in A(astlib.FuncCall):
            return astlib.FuncCall(
                stmt.name, [_replace(a) for a in stmt.args])
        elif stmt in A(astlib.StructPath):
            def _replace_path_member(path_member):
                if path_member in A(astlib.Name):
                    return path_member
                return astlib.FuncCall(
                    path_member.name, [_replace(a) for a in path_member.args])
            return astlib.StructPath(
                [_replace(stmt.path[0])] + [
                    _replace_path_member(p) for p in stmt.path[1:]])
        elif stmt in A(astlib.Name):
            return (astlib.StructPath([name, astlib.Name("data")])
                    if stmt == name else stmt)
        elif stmt in A(astlib.Expr):
            return astlib.Expr(
                _replace(stmt.left), stmt.op, _replace(stmt.right))
        elif stmt in A(astlib.Slice):
            return astlib.Slice(
                _replace(stmt.base), _replace(stmt.start), _replace(stmt.end))
        elif stmt in A(astlib.Subscript):
            return astlib.Subscript(_replace(stmt.base), _replace(stmt.index))
        elif stmt in A(astlib.Not):
            return astlib.Not(_replace(stmt.expr))
        elif stmt is None or stmt in A(
                astlib.BreakEvent, astlib.Empty, astlib.PyObject):
            return stmt
        elif stmt in A(astlib.ModuleMember):
            return astlib.ModuleMember(stmt.module, _replace(stmt.member))
        else:
            errors.later()
    return [_replace(stmt) for stmt in body]


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

    def declaration(self, decl):
        return type(decl)(decl.name, decl.type_, e(decl.expr))

    def struct_like_decl(self, decl):
        def _lookup_for_prelude_proto(protocol_name):
            if protocol_name in defs.PRELUDE_OBJS:
                return astlib.ModuleMember(defs.MODULE_PRELUDE, protocol_name)
            return protocol_name
        return type(decl)(
            decl.name, decl.parameters,
            [_lookup_for_prelude_proto(p) for p in decl.implemented_protocols],
            self.b(decl.body))

    @layers.register(astlib.LetDecl)
    def let_declaration(self, decl):
        yield self.declaration(decl)

    @layers.register(astlib.VarDecl)
    def var_declaration(self, decl):
        yield self.declaration(decl)

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
        yield astlib.Cond(
            self.if_stmt(stmt.if_stmt),
            [self.elif_stmt(elif_) for elif_ in stmt.elifs],
            (None if stmt.else_stmt is None
                else self.else_stmt(stmt.else_stmt)))

    @layers.register(astlib.While)
    def while_stmt(self, stmt):
        yield astlib.While(e(stmt.expr), self.b(stmt.body))

    @layers.register(astlib.For)
    def for_stmt(self, stmt):
        container = e(stmt.container)
        if is_py_obj(container):
            yield astlib.For(stmt.names, container, self.b(stmt.body))
        else:
            if len(stmt.names) != 1:
                errors.later()
            container_var_name = astlib.Name(
                defs.TMP_FMT_STRING.format(context.tmp_counter))
            context.tmp_counter += 1
            yield astlib.LetDecl(
                container_var_name, astlib.Empty(), container)
            yield astlib.VarDecl(
                stmt.names[0], astlib.Empty(),
                astlib.StructPath(
                    [container_var_name, astlib.FuncCall(defs.SPEC_METHOD_NEXT, [])]))
            yield astlib.While(
                e(astlib.Expr(
                    stmt.names[0], defs.IS, astlib.Name(defs.TYPE_SOME))),
                replace_name_references_for_for_stmt_translation(
                    self.b(stmt.body), name=stmt.names[0]) + [
                    astlib.Assignment(stmt.names[0], defs.EQ,
                        astlib.StructPath([
                            container_var_name,
                            astlib.FuncCall(defs.SPEC_METHOD_NEXT, [])]))])

    @layers.register(astlib.Return)
    def return_stmt(self, stmt):
        yield astlib.Return(e(stmt.expr))

    @layers.register(astlib.FuncCall)
    def func_call(self, call):
        yield e(call)

    @layers.register(astlib.StructPath)
    def struct_path(self, struct_path):
        yield e(struct_path)

    def if_stmt(self, stmt):
        return astlib.If(e(stmt.expr), self.b(stmt.body))

    def elif_stmt(self, stmt):
        return astlib.Elif(e(stmt.expr), self.b(stmt.body))

    def else_stmt(self, stmt):
        return astlib.Else(self.b(stmt.body))
