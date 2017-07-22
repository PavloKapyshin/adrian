"""Helper for creating new layers."""

import types
#import inspect
#import itertools
import functools

from vendor.paka import funcreg

from . import astlib, context


ALL = object()


NODE_CLASSES = (astlib.BaseNode, astlib.Name)


_PREREGISTERED_NODE_CLASSES_ATTR_NAME = "_layer_prereg_nodes_clss"
_INTERNAL_REGISTRY_ATTR_NAME = "_registry"


def register(*node_classes):
    def _wrapper(func):
        setattr(func, _PREREGISTERED_NODE_CLASSES_ATTR_NAME, node_classes)
        return func
    return _wrapper


class LayerMeta(type):

    def __new__(cls, name, bases, attrs):
        # Layer does not have bases, so we filter it out this way.
        if bases:
            registry = funcreg.TypeRegistry()
            for value in attrs.values():
                if isinstance(value, types.FunctionType):
                    for node_class in getattr(
                            value, _PREREGISTERED_NODE_CLASSES_ATTR_NAME, ()):
                        registry.register(value, node_class)
            attrs[_INTERNAL_REGISTRY_ATTR_NAME] = registry
        return type.__new__(cls, name, bases, attrs)


class Layer(metaclass=LayerMeta):

    def __init__(self):
        pass

    def get_registry(self):
        reg = funcreg.TypeRegistry()
        for node_class, func in getattr(
                self, _INTERNAL_REGISTRY_ATTR_NAME).items():
            reg.register(functools.partial(func, self), node_class)
        return reg


# def create_with(instance, **keywords):
#     return type(instance)(**dict(get_child_nodes(instance), **keywords))


# def get_child_nodes(node):
#     sig = inspect.signature(type(node))
#     return {name: getattr(node, name) for name in sig.parameters}


# def loop_transform_loop(node, node_func, *, registry):
#     child_nodes = get_child_nodes(node)
#     tail = []
#     kargs = {}
#     for param_name, param_value in child_nodes.items():
#         if isinstance(param_value, NODE_CLASSES):
#             value = list(transform_node(param_value, registry=registry))
#             tail.extend(value[1:])
#             kargs[param_name] = value[0]
#         else:
#             kargs[param_name] = param_value
#     if node_func:
#         yield from node_func(create_with(node, **kargs))
#     else:
#         yield create_with(node, **kargs)
#     yield from map(lambda n: astlib.Pair(line=node.line, stmt=n), tail)

# def transform_node(node, *, registry):
#     use_all = registry.get(ALL)
#     context.set_position(0)
#     if use_all:
#         yield from loop_transform_loop(node, use_all, registry=registry)
#     else:
#         node_func = registry.get(type(node))
#         yield from loop_transform_loop(node, node_func, registry=registry)

# def transform_ast(ast_, *, registry):
#     return list(
#         itertools.chain.from_iterable(
#             transform_node(node, registry=registry) for node in ast_))


def transform_ast(ast_, *, registry):
    for node in ast_:
        node_func = registry.get(type(node))
        yield from node_func(node)
