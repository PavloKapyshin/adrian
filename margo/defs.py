import re


RESERVED_WORDS = (
    "var",
    "fun",
)

TYPE_NAME_REGEX = re.compile(r"[A-Z_][a-zA-Z0-9]*")
VAR_NAME_REGEX = re.compile(r"[a-z_][a-zA-Z0-9]*")
MODULE_NAME_REGEX = re.compile(r"[a-z_][a-z_0-9]*")