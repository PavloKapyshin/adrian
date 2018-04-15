import pathlib

from . import astlib, layers, errors, defs
from .context import context
from .utils import A


def get_file_hash(file_path):
    return utils.get_hash(read_file(str(file_path)))


def read_file(file_path):
    with open(file_path, "r") as file:
        contents = file.read()
    return contents


def find_file(file_name):
    file_name_with_extension = ".".join([
        str(file_name), defs.ADRIAN_FILE_EXTENSION])
    for dir_name in context.module_paths:
        dir_path = pathlib.Path(dir_name)
        for entity in dir_path.iterdir():
            if entity.is_file() and entity.name == file_name:
                return entity
    errors.cannot_find_file(file_name)


class Loader(layers.Layer):

    def __init__(self):
        self.b = layers.b(Loader)

    def decl(self, stmt):
        pass

    @layers.register(astlib.FuncCall)
    def func_call(self, stmt):
        yield e(stmt)

    @layers.register(astlib.MethodCall)
    def method_call(self, stmt):
        yield e(stmt)

    @layers.register(astlib.LetDecl)
    def let_decl(self, stmt):
        yield from self.decl(stmt)

    @layers.register(astlib.VarDecl)
    def var_decl(self, stmt):
        yield from self.decl(stmt)

    def data_decl(self, stmt):
        pass

    @layers.register(astlib.StructDecl)
    def struct_decl(self, stmt):
        yield from self.data_decl(stmt)

    @layers.register(astlib.AdtDecl)
    def adt_decl(self, stmt):
        yield from self.data_decl(stmt)

    @layers.register(astlib.ProtocolDecl)
    def protocol_decl(self, stmt):
        yield from self.data_decl(stmt)

    @layers.register(astlib.AST)
    def main(nodes, *, registry):
        translated_nodes = []
        for node in nodes:
            translated_nodes.extend(
                layers.transform_node(node, registry=registry))
        return context.loaded_lines + translated_nodes
