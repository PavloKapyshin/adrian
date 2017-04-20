"""Lex and parse input and return low-level AST."""

import sys
import collections

from vendor.ply import lex
from vendor.ply import yacc

from . import ast
from . import defs
from . import errors


# Lexer defs.
# Reserved words.
reserved = collections.OrderedDict({
    "var": "VAR",
    "fun": "FUN",
    "struct": "STRUCT",
    "return": "RETURN",
})


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
    "SEMI",     # ;
    "COMMA",    # ,
    "PERIOD",   # .
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
t_SEMI = r";"
t_COMMA = r","
t_PERIOD = r"\."
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
    stmt : decl_stmt
         | assignment_stmt
         | struct_decl_stmt
         | func_decl_stmt
    """
    # Atom_expr can be a func call.
    # atom_expr -> atom trailers
    # func_call -> NAME (args)
    content[0] = content[1]


# Return statement.
def p_return_stmt(content):
    """return_stmt : RETURN bool_expr"""
    content[0] = ast.ReturnStmt(value=content[2])


# Function declaration statement.
def p_func_decl_stmt_1(content):
    """
    func_decl_stmt : FUN NAME LP args RP COLON type LBRACE func_body RBRACE
    """
    content[0] = ast.FuncDecl(
        name=content[2], args=content[4], type_=content[7], body=content[9])


def p_func_decl_stmt_2(content):
    """func_decl_stmt : FUN NAME LP args RP LBRACE func_body RBRACE"""
    content[0] = ast.FuncDecl(
        name=content[2], args=content[4], type_=None, body=content[7])


# Function body.
def p_func_body_1(content):
    """func_body : func_body func_body_stmt"""
    content[0] = content[1] + [content[2]]


def p_func_body_2(content):
    """func_body : empty"""
    content[0] = []


# Function body statement.
def p_func_body_stmt(content):
    """func_body_stmt : decl_stmt
                      | assignment_stmt
                      | return_stmt
    """
    content[0] = ast.Pair(line=content.lineno(0), stmt=content[1])


# Function arguments.
def p_args_1(content):
    """args : args SEMI args"""
    content[0] = content[1] + content[3]


def p_arg_2(content):
    """args : arg_names COLON type"""
    content[0] = []
    for name in content[1]:
        content[0].append(ast.Arg(name=name, type_=content[3]))


def p_args_3(content):
    """args : empty"""
    content[0] = []


# Argument names.
def p_arg_names_1(content):
    """arg_names : NAME COMMA arg_names"""
    content[0] = [content[1]] + content[3]

def p_arg_names_2(content):
    """arg_names : NAME"""
    content[0] = [content[1]]


# Struct declaration statement.
def p_struct_decl_stmt(content):
    """struct_decl_stmt : STRUCT NAME LBRACE struct_body RBRACE"""
    content[0] = ast.StructDecl(name=content[2], body=content[4])


# Struct body.
def p_struct_body_1(content):
    """struct_body : struct_body struct_body_stmt"""
    content[0] = content[1] + [content[2]]


def p_struct_body_2(content):
    """struct_body : empty"""
    content[0] = []


# Struct body statement.
def p_struct_body_stmt(content):
    """struct_body_stmt : decl_stmt
                        | func_decl_stmt
    """
    content[0] = ast.Pair(line=content.lineno(0), stmt=content[1])


# Variable declaration statement.
def p_decl_stmt_1(content):
    """decl_stmt : VAR NAME COLON type EQUAL bool_expr"""
    content[0] = ast.Decl(
        name=ast.Name(content[2]), type_=content[4], value=content[6])


def p_decl_stmt_2(content):
    """decl_stmt : VAR NAME COLON type"""
    content[0] = ast.Decl(
        name=ast.Name(content[2]), type_=content[4], value=None)


def p_decl_stmt_3(content):
    """decl_stmt : VAR NAME EQUAL bool_expr"""
    content[0] = ast.Decl(
        name=ast.Name(content[2]), type_=None, value=content[4])


# Variable assignment statement.
def p_assignment_stmt(content):
    """assignment_stmt : name_from_struct assignop bool_expr"""
    content[0] = ast.Assignment(name=content[1], op=content[2], value=content[3])


# Type.
def p_type(content):
    """type : name_from_module"""
    content[0] = content[1]


# Name from struct.
def p_name_from_struct_1(content):
    """name_from_struct : NAME"""
    content[0] = ast.Name(content[1])


def p_name_from_struct_2(content):
    """name_from_struct : name_from_struct PERIOD NAME"""
    content[0] = ast.StructElem(struct_name=content[1], elem=content[3])


# Name from module.
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
