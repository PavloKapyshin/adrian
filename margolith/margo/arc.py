import enum
import itertools

from . import astlib, layers, errors, inference, defs
from .context import context, add_to_env, add_scope, del_scope, get
from .patterns import A
from .env import Env


REGION_ID_START = 1
context.region_id = REGION_ID_START
context.memory_regions = Env()


class RegionStatus(enum.Enum):
    allocated = 1
    freed = 2


class Region:
    """Memory region."""

    def __init__(self, content_type, status=RegionStatus.allocated):
        self.status = status
        self.content_type = content_type
        self.id = context.region_id
        context.region_id += 1


def is_malloc(expr):
    return expr in A(astlib.CFuncCall) and expr.name == "malloc"


def deinit(expr):
    return astlib.StructFuncCall(
        inference.infer(expr), defs.DEINIT_METHOD_NAME, [expr])


class ARC(layers.Layer):

    def var_to_region(self, name):
        for id in range(REGION_ID_START, context.region_id):
            current_region = context.memory_regions[id]
            if name in current_region["pointers"]:
                return current_region["region"]
        errors.not_implemented("variable doesn't point to any region")

    def region_to_vars(self, region):
        return context.memory_regions[region.id]["pointers"]

    def add_var(self, name, region):
        if region.id in context.memory_regions:
            old_info = context.memory_regions[region.id]
            pointers = old_info["pointers"]
            if name in pointers:
                errors.not_implemented("this region is already added")
            pointers.append(name)
            context.memory_regions[region.id] = {
                "region": region,
                "pointers": pointers
            }
        else:
            context.memory_regions[region.id] = {
                "region": region,
                "pointers": [name]
            }

    def any_var_pointing_to_region(self, region):
        pointers = sorted(self.region_to_vars(region))
        if pointers == []:
            errors.not_implemented("region is anonymous")
        return pointers[0]

    def free_region(self, region):
        stmt = deinit(self.any_var_pointing_to_region(region))
        region.status = RegionStatus.freed
        # maybe rewritting of status will help.
        return stmt

    def raw_free_region(self, region):
        return deinit(self.any_var_pointing_to_region(region))

    def raw_free_var(self, var):
        return deinit(var)

    def free_scope(self):
        for region_id, info in sorted(
                context.memory_regions.current_space.items()):
            region = info["region"]
            if region.status != RegionStatus.freed:
                yield self.free_region(region)

    def region_from_expr(self, expr):
        if is_malloc(expr):
            return Region(inference.infer(expr))
        if expr in A(astlib.Ref):
            if expr.expr in A(astlib.Name):
                return self.var_to_region(expr.expr)
        if expr in A(astlib.StructCall, astlib.StructFuncCall, astlib.FuncCall):
            rettype = inference.infer(expr)
            if not rettype in A(astlib.CVoid):
                return Region(rettype)
        errors.not_implemented("can't infer region")

    def body(self, body):
        reg = ARC().get_registry()
        return list(itertools.chain.from_iterable(
            map(lambda stmt: list(
                    layers.transform_node(stmt, registry=reg)),
                body)))

    @layers.register(astlib.LetDecl)
    def let_decl(self, stmt):
        add_to_env(stmt)
        self.add_var(stmt.name, self.region_from_expr(stmt.expr))
        yield stmt

    @layers.register(astlib.VarDecl)
    def decl(self, stmt):
        add_to_env(stmt)
        self.add_var(stmt.name, self.region_from_expr(stmt.expr))
        yield stmt

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        to_free = None
        if stmt.variable in A(astlib.StructMember):
            struct = stmt.variable
            while struct in A(astlib.StructMember):
                struct = struct.struct
            expr = get(struct)["expr"]
            if not is_malloc(expr):
                to_free = stmt.variable
        else:
            to_free = stmt.variable
        if stmt.expr in A(astlib.Name):
            self.add_var(stmt.variable, self.region_from_expr(stmt.expr))
        if to_free:
            yield self.raw_free_var(to_free)
        yield stmt

    @layers.register(astlib.Return)
    def return_(self, stmt):
        for free in self.free_scope():
            if stmt.expr in A(astlib.Name, astlib.StructMember):
                if not str(free.args[0]) == str(stmt.expr):
                    yield free
        yield stmt

    @layers.register(astlib.FuncDecl)
    def func_decl(self, stmt):
        add_to_env(stmt)
        add_scope()
        context.memory_regions.add_scope()
        body = self.body(stmt.body)
        if not body[-1] in A(astlib.Return):
            body = body + list(self.free_scope())
        yield astlib.FuncDecl(
            stmt.name, stmt.args, stmt.rettype, body)
        context.memory_regions.del_scope()
        del_scope()

    @layers.register(astlib.StructDecl)
    def struct_decl(self, stmt):
        add_to_env(stmt)
        add_scope()
        yield stmt
        del_scope()

    @layers.register(astlib.StructFuncDecl)
    def struct_func_decl(self, stmt):
        add_to_env(stmt)
        add_scope()
        context.memory_regions.add_scope()
        body = self.body(stmt.body)
        if not body[-1] in A(astlib.Return):
            body = body[:-1] + list(self.free_scope()) + body[-1:]
        yield astlib.StructFuncDecl(
            stmt.struct, stmt.func, stmt.args, stmt.rettype, body)
        context.memory_regions.del_scope()
        del_scope()

    @layers.register(astlib.AST)
    def main(self, ast_, registry):
        yield from layers.transform_ast(ast_, registry=registry)
        yield from self.free_scope()