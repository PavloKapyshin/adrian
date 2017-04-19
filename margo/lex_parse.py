"""Lex and parse input and return low-level AST."""

import sys

from vendor.ply import lex
from vendor.ply import yacc

from . import ast
from . import defs
from . import errors


# Lexer defs.
# Reserved words.
reserved = {
    "var": "VAR",
    "struct": "STRUCT",

    # Standard types.
    "Integer": "TYPE_INTEGER",
    "String": "TYPE_STRING",
}


# List of tokens.
tokens = [
    "INTEGER",
    "STRING",
    "NAME",

    "LP",       # (
    "RP",       # )
    "LBRACE",   # {
    "RBRACE",   # }

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
    "HASHTAG",  # #
] + list(reserved.values())  # Reserved words are also tokens.


# Regular expressions for simple tokens.
# Longest must be first.
t_EQ = r"=="
t_LE = r"<="
t_GE = r">="
t_NE = r"!="

t_LP = r"\("
t_RP = r"\)"
t_LBRACE = r"\{"
t_RBRACE = r"\}"

t_LT = r"<"
t_GT = r">"
t_EQUAL = r"="
t_COLON = r":"
t_HASHTAG = r"\#"

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


def t_NAME(token):
    r"""[a-zA-Z_][a-z_A-Z0-9]*"""
    token.type = reserved.get(token.value, "NAME")  # Check for reserved words.
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
    errors.illegal_char(line=token.lexer.lineno, exit_on_error=EXIT_ON_ERROR, char=token.value[0])


# Parser defs.
# Precedence of operators. Last operators have higher precedence.
precedence = (
    ("right", "LE", "GE", "LT", "GT"),
    ("right", "EQ", "NE"),
    ("right", "PLUS", "MINUS"),
    ("right", "TIMES", "DIVIDE"),
)


def add_to_list(dest, src):
    """Append an item to list (if item is a list, flatten)."""
    if isinstance(src, list):
        dest.extend(src)
    else:
        dest.append(src)


def list_parsed_content(content):
    """Move parsed content (without punctuation) into a list."""
    content[0] = []
    for tok in content[1:]:
        if tok not in (",", "."):
            add_to_list(content[0], tok)
    return content[0]


def list_expr(expression):
    """Move parsed expression into a list in polish notation."""
    if len(expression) == 3 + 1:
        return [expression[2], expression[1], expression[3]]
    return [expression[1]]


# AST.
def p_ast_1(content):
    """ast : ast pair"""
    content[0] = list_parsed_content(content)


def p_ast_2(content):
    """ast : pair"""
    content[0] = [content[1]]  # AST must be list.


# Pair.
def p_pair(content):
    """pair : stmt"""
    content[0] = ast.Pair(line=content.lineno(0), stmt=content[1])


# Statement.
def p_stmt(content):
    """
    stmt : assignment_stmt
         | struct_stmt
    """
    # Atom_expr can be a func call.
    # atom_expr -> atom trailers
    # func_call -> NAME (args)
    content[0] = content[1]


# Struct statement.
def p_struct_stmt(content):
    """struct_stmt : STRUCT NAME LBRACE struct_body RBRACE"""
    content[0] = ast.StructDecl(name=content[2], body=content[4])


# Struct body.
def p_struct_body_1(content):
    """struct_body : struct_body assignment_stmt"""
    content[0] = content[1] + [content[2]]


def p_struct_body_2(content):
    """struct_body : empty"""
    content[0] = []


# Assignment statement.
def p_assignment_stmt_1(content):
    """assignment_stmt : VAR NAME COLON type assignop bool_expr"""
    content[0] = ast.Assignment(
        name=content[2], type_=content[4], value=content[6])


def p_assignment_stmt_2(content):
    """assignment_stmt : VAR NAME COLON type"""
    content[0] = ast.Assignment(
        name=content[2], type_=content[4], value=None)


def p_assignment_stmt_3(content):
    """assignment_stmt : NAME assignop bool_expr"""
    content[0] = ast.Assignment(
        name=content[1], type_=None, value=content[3])


def p_assignment_stmt_4(content):
    """assignment_stmt : VAR NAME assignop bool_expr"""
    content[0] = ast.Assignment(
        name=content[2], type_=None, value=content[4])

# Type.
def p_type(content):
    """
    type : TYPE_INTEGER
         | TYPE_STRING
         | name_from_module
    """
    content[0] = content[1]


def p_name_from_module_1(content):
    """name_from_module : NAME"""
    content[0] = ast.Name(content[1])


def p_name_from_module_2(content):
    """name_from_module : NAME HASHTAG NAME"""
    content[0] = ast.ModuleMember(
        module_name=content[1], member=ast.Name(content[3]))


# Assignment operator.
def p_assignop(content):
    """assignop : EQUAL"""
    content[0] = content[1]


# Bool operator.
def p_boolop(content):
    """
    boolop : EQ
           | NE
           | LE
           | GE
           | LT
           | GT
    """
    content[0] = content[1]


# Bool expression.
def p_bool_expr_1(content):
    """bool_expr : bool_expr boolop expr"""
    content[0] = list_expr(content)


def p_bool_expr_2(content):
    """bool_expr : expr"""
    content[0] = content[1]


# Expression.
def p_expr_1(content):
    """
    expr : expr PLUS expr
         | expr MINUS expr
         | expr TIMES expr
         | expr DIVIDE expr
    """
    content[0] = list_expr(content)


def p_expr_2(content):
    """expr : factor"""
    content[0] = content[1]


# Factor.
def p_factor(content):
    """factor : atom_expr"""
    content[0] = content[1]


def p_atom_expr(content):
    """atom_expr : atom"""
    content[0] = content[1]


# Atom.
def p_atom_1(content):
    """
    atom : INTEGER
         | STRING
    """
    content[0] = content[1]


def p_atom_2(content):
    """atom : LP bool_expr RP"""
    content[0] = content[2]


def p_atom_3(content):
    """atom : name_from_module"""
    content[0] = content[1]


# Empty rule.
def p_empty(content):
    """empty :"""


def p_error(content):
    """Error handling function."""
    errors.syntax_error(line=content.lineno, exit_on_error=EXIT_ON_ERROR)


def main(code, *, exit_on_error=True):
    """Build lexer and build parser."""
    global EXIT_ON_ERROR
    EXIT_ON_ERROR = exit_on_error
    lexer = lex.lex(module=sys.modules[__name__])
    lexer.input(code)
    parser = yacc.yacc(module=sys.modules[__name__], debug=False)
    # Parse data got from lexer.
    ast_ = parser.parse(input=code, lexer=lexer, tracking=True)
    return ast_
