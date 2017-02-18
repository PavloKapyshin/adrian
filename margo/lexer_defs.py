"""Contains all of the regular expression rules and tables used during lexing.
For use see parser.py.
"""

from . import ast
from . import errors


# Reserved words.
reserved = {
    "var": "VAR",

    # Standard types.
    "Integer": "TYPE_INTEGER",
    "String": "TYPE_STRING",
}


# List of tokens.
tokens = [
    "INTEGER",
    "STRING",
    "VARIABLE_NAME",
    "TYPE_NAME",

    "LP",       # (
    "RP",       # )

    "EQ",       # ==
    "LE",       # <=
    "GE",       # >=
    "NE",       # !=

    "GT",       # >
    "LT",       # <
    "EQUAL",    # =
    "PLUS",     # +
    "MINUS",    # -
    "TIMES",    # *
    "DIVIDE",   # /
    "COLON",    # :
] + list(reserved.values())  # Reserved words are also tokens.


# Regular expressions for simple tokens.
# Longest must be first.
t_EQ = r"=="
t_LE = r"<="
t_GE = r">="
t_NE = r"!="

t_LP = r"\("
t_RP = r"\)"

t_LT = r"<"
t_GT = r">"
t_EQUAL = r"="
t_COLON = r":"
t_PLUS = r"\+"
t_MINUS = r"-"
t_TIMES = r"\*"
t_DIVIDE = r"/"


# Regular expressions with some action code.
def t_INTEGER(token):
    r"""[-]?\d+"""
    token.value = ast.Integer(value=token.value)
    return token


def t_STRING(token):
    r"""["][^"]*?["]"""
    token.value = ast.String(value=token.value[1:-1])
    return token


def t_VARIABLE_NAME(token):
    r"""[a-z_]+[a-zA-Z0-9]*"""
    token.type = reserved.get(token.value, "VARIABLE_NAME")  # Check for reserved words.
    return token


def t_TYPE_NAME(token):
    r"""[A-Z_]+[a-zA-Z0-9]*"""
    token.type = reserved.get(token.value, "TYPE_NAME")  # Check for reserved words.
    return token


def t_newline(token):
    r"""\n+"""
    token.lexer.lineno += len(token.value)  # Number of newlines.

def t_comment(token):
    r"""--.*"""
    pass


# Ignoring.
t_ignore = " \t"


def t_error(token):
    """Error handling rule."""
    errors.illegal_char(line=token.lexer.lineno, char=token.value[0])
