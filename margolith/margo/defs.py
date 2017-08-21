import re


RESERVED_WORDS = {
    "var": "VAR",
    "let": "LET",
    "protocol": "PROTOCOL",
    "is": "IS",
    "fun": "FUN",
    "return": "RETURN",
    "struct": "STRUCT",
    "ref": "REF"
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