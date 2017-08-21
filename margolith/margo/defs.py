import re

from adrian.cgen import libc


RESERVED_WORDS = {
    keyword: keyword.upper()
    for keyword in (
        "var", "let", "fun", "ref",
        "protocol", "return", "is",
        "struct"
    )
}

VAR_NAME_REGEX = re.compile(r"[a-z_][a-zA-Z0-9]*")
FUNC_NAME_REGEX = re.compile(r"[a-z_][a-zA-Z0-9]*")
TYPE_NAME_REGEX = re.compile(r"[A-Z_][a-zA-Z0-9]*")
MODULE_NAME_REGEX = re.compile(r"[a-z_][a-z_0-9]*")
METHOD_NAME_REGEX = re.compile(r"[_][_][a-z][a-zA-Z0-9]*[_][_]")

INIT_METHOD_NAME = "__init__"
COPY_METHOD_NAME = "__copy__"
DEINIT_METHOD_NAME = "__deinit__"

ADR_PREFIX = "adr"
USER_PREFIX = "u"



# c module definitions.

CMODULE_NAME = "c"
FREE_FUNC_NAME = "free"
MALLOC_FUNC_NAME = "malloc"
SIZEOF_FUNC_NAME = "sizeof"

CFUNCS = (
    FREE_FUNC_NAME,
    MALLOC_FUNC_NAME
)

MALLOC_FUNC_DESCR = libc.malloc
FREE_FUNC_DESCR = libc.free
