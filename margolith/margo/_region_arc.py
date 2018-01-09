import enum
import itertools

from . import astlib, layers, errors, inference
from .context import context, add_to_env, add_scope, del_scope
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


def free(name):
    return astlib.CFuncCall("free", [name])


class ARC(layers.Layer):

    def variable_to_region(self, name):
        for id in range(REGION_ID_START, context.region_id):
            current_region = context.memory_regions[id]
            if name in current_region["pointers"]:
                return current_region["region"]
        errors.not_implemented("variable doesn't point to any region")

    def region_to_variables(self, region):
        return context.memory_regions[region.id]["pointers"]

    def add_variable(self, name, region):
        if region.id in context.memory_regions:
            old_info = context.memory_regions[region.id]
            pointers = old_info["pointers"]
            if name in pointers:
                errors.not_implemented("this regions is already added")
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

    def any_variable_pointing_to_region(self, region):
        pointers = sorted(self.region_to_variables(region))
        if pointers == []:
            errors.not_implemented("region is anonymous")
        return pointers[0]

    def free_region(self, region):
        type_ = region.content_type
        if type_ in A(astlib.CType):
            stmt = free(self.any_variable_pointing_to_region(region))
            region.status = RegionStatus.freed
            return stmt
        errors.not_implemented("try another day")

    def free_scope(self):
        for region_id, info in sorted(context.memory_regions.current_space.items()):
            region = info["region"]
            if region.status != RegionStatus.freed:
                yield self.free_region(region)

    def region_from_expr(self, expr):
        if is_malloc(expr):
            return Region(inference.infer(expr))
        if expr in A(astlib.Ref):
            if expr.expr in A(astlib.Name):
                return self.variable_to_region(expr.expr)
        errors.not_implemented("can't infer region.")

    def body(self, body):
        reg = ARC().get_registry()
        return list(itertools.chain.from_iterable(
            map(lambda stmt: list(
                    layers.transform_node(stmt, registry=reg)),
                body)))

    @layers.register(astlib.VarDecl)
    def var_decl(self, stmt):
        add_to_env(stmt)
        self.add_variable(stmt.name, self.region_from_expr(stmt.expr))
        yield stmt

    @layers.register(astlib.AST)
    def main(self, ast_, registry):
        yield from layers.transform_ast(ast_, registry=registry)
        yield from self.free_scope()