"""Helper for creating new layers."""

import inspect
import itertools

from . import astlib


NODE_CLASSES = (astlib.BaseNode, astlib.Name)


def take_first(iter_):
    for item in iter_:
        return item


def create_with(instance, **keywords):
    return type(instance)(**keywords)


def get_child_nodes(node):
    sig = inspect.signature(type(node))
    return {name: getattr(node, name) for name in sig.parameters}


def loop_transform_loop(node, node_func, *, registry):
    child_nodes = get_child_nodes(node)
    kargs = {}
    for param_name, param_value in child_nodes.items():
        if isinstance(param_value, NODE_CLASSES):
            kargs[param_name] = take_first(
                transform_node(param_value, registry=registry))
        else:
            kargs[param_name] = param_value
    if node_func:
        yield from node_func(create_with(node, **kargs))
    else:
        yield create_with(node, **kargs)


def _old_transform_node(node, *, registry):
    use_all = registry.get(ALL)
    if use_all:
        yield from loop_transform_loop(node, use_all, registry=registry)
    node_func = registry.get(type(node))
    if node_func:
        yield from loop_transform_loop(node, node_func, registry=registry)
    elif not use_all:
        yield from loop_transform_loop(node, None, registry=registry)


def transform_node(node, *, registry):
    use_all = registry.get(ALL)
    if use_all:
        yield from loop_transform_loop(node, use_all, registry=registry)
    else:
        node_func = registry.get(type(node))
        yield from loop_transform_loop(node, node_func, registry=registry)


def transform_ast(ast_, *, registry):
    return list(
        itertools.chain.from_iterable(
            transform_node(node, registry=registry) for node in ast_))
