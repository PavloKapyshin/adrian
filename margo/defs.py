import re


RESERVED_WORDS = (
    "var",
    "fun",
    "ret",
    "sct",
)

VAR_NAME_REGEX = re.compile(r"[a-z_][a-zA-Z0-9]*")
FUNC_NAME_REGEX = re.compile(r"[a-z_][a-zA-Z0-9]*")
TYPE_NAME_REGEX = re.compile(r"[A-Z_][a-zA-Z0-9]*")
MODULE_NAME_REGEX = re.compile(r"[a-z_][a-z_0-9]*")
METHOD_NAME_REGEX = re.compile(r"[_][_][a-z][a-zA-Z0-9]*[_][_]")

INIT_METHOD_NAME = "__init__"
DEINIT_METHOD_NAME = "__deinit__"
COPY_METHOD_NAME = "__copy__"

TMP_PREFIX = "adr_tmp_"
VAR_PREFIX = "adr_var_"
FUNC_PREFIX = "adr_func_"
STRUCT_PREFIX = "adr_struct_"
FIELD_PREFIX = "adr_field_"
METHOD_PREFIX = "adr_method_"