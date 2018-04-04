"""Helper for creating new layers."""

import itertools
import typing

import types
import functools

from paka import funcreg

from . import astlib


NODE_CLASSES = (astlib.BaseNode, astlib.Name)


_PREREGISTERED_NODE_CLASSES_ATTR_NAME = "_layer_prereg_nodes_clss"
_INTERNAL_REGISTRY_ATTR_NAME = "_registry"


def register(*node_classes):
    def _wrapper(func):
        setattr(
            func, _PREREGISTERED_NODE_CLASSES_ATTR_NAME,
            node_classes)
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
                            value,
                            _PREREGISTERED_NODE_CLASSES_ATTR_NAME, ()):
                        registry.register(value, node_class)
            attrs[_INTERNAL_REGISTRY_ATTR_NAME] = registry
        return type.__new__(cls, name, bases, attrs)


class Layer(metaclass=LayerMeta):

    def get_registry(self):
        reg = funcreg.TypeRegistry()
        for node_class, func in getattr(
                self, _INTERNAL_REGISTRY_ATTR_NAME).items():
            reg.register(functools.partial(func, self), node_class)
        return reg


def transform_node(node, *, registry):
    node_func = registry.get(type(node))
    if node_func:
        yield from node_func(node)
    else:
        yield node


def transform_ast(ast_, *, registry):
    for node in ast_:
        yield from transform_node(node, registry=registry)


def expand_ast(ast_, *, registry):
    yield from registry.get(astlib.AST)(ast_, registry)


def b(layer: typing.Type[Layer], **kwords):
    def helper(body: typing.Sequence[astlib.Node]):
        reg = layer(**kwords).get_registry()
        return list(itertools.chain.from_iterable(
            map(lambda stmt: list(
                transform_node(stmt, registry=reg)
            ), body)
        ))
    return helper
