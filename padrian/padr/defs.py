import re
from functools import partial

from . import env, astlib


RESERVED_WORDS = {
    keyword: keyword.upper()
    for keyword in (
        "var", "let", "struct",
        "return",
        "fun", #"adt", "protocol"
    )
}


NAME_REGEX = re.compile(r"[a-z][a-zA-Z0-9]*")
FUNC_REGEX = NAME_REGEX
TYPE_REGEX = re.compile(r"[A-Z][a-zA-Z0-9]*")
MODULE_REGEX = re.compile(r"[a-z][a-z_0-9]*")
METHOD_REGEX = re.compile(
    r"((__)[a-z][a-zA-Z0-9]*(__))|([a-z][a-zA-Z0-9]*)")
COMMON_REGEX = "".join([
    "(",
    ")|(".join([regex.pattern
        for regex in (NAME_REGEX, FUNC_REGEX,
        TYPE_REGEX, MODULE_REGEX, METHOD_REGEX)]),
    ")"])


INIT_METHOD = "__init__"
DEINIT_METHOD = "__deinit__"
COPY_METHOD = "__copy__"
CMODULE = "c"
T_STRING = "t"

_c_module_member = partial(
    astlib.DataMember, astlib.DataT.module, astlib.Name(CMODULE))
ENV = env.Env()
ENV[_c_module_member(astlib.Name("IntFast8"))] = {
    "node_type": astlib.NodeT.struct
}
ENV[_c_module_member(astlib.Name("IntFast16"))] = {
    "node_type": astlib.NodeT.struct
}
ENV[_c_module_member(astlib.Name("IntFast32"))] = {
    "node_type": astlib.NodeT.struct
}
ENV[_c_module_member(astlib.Name("IntFast64"))] = {
    "node_type": astlib.NodeT.struct
}
ENV[_c_module_member(astlib.Name("UIntFast8"))] = {
    "node_type": astlib.NodeT.struct
}
ENV[_c_module_member(astlib.Name("UIntFast16"))] = {
    "node_type": astlib.NodeT.struct
}
ENV[_c_module_member(astlib.Name("UIntFast32"))] = {
    "node_type": astlib.NodeT.struct
}
ENV[_c_module_member(astlib.Name("UIntFast64"))] = {
    "node_type": astlib.NodeT.struct
}