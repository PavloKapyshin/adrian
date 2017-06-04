import re
import sys

from vendor.ply import lex, yacc

from . import ast
from . import defs
from . import errors


_RESERVED_WORDS = dict((word, word.upper()) for word in defs.RESERVED_WORDS)


_TOKENS = {
    "==": "EQEQ",
    "<=": "LTEQ",
    ">=": "GTEQ",
    "!=": "NEQ",

    "=": "EQ",
    "<": "LT",
    ">": "GT",

    "+": "PLUS",
    "-": "MINUS",
    "*": "TIMES",
    "/": "DIVIDE",

    "(": "LPAREN",
    ")": "RPAREN",
    "{": "LBRACE",
    "}": "RBRACE",

    ":": "COLON",
    ";": "SEMI",
    ",": "COMMA",
    ".": "PERIOD",
    "#": "HASH"
}


tokens = (
    "INTEGER",
    "STRING",
    "NAME",
) + tuple(_TOKENS.values()) + tuple(_RESERVED_WORDS.values())


def _escape_tok_regex(regex, escape=set("#.{}()*+")):
    """Escape chars in regex string if they are in escape set."""
    for char in regex:
        if char in escape:
            yield re.escape(char)
        else:
            yield char


# Longest regexs must be first.
for tok_regex, const_name in sorted(
        _TOKENS.items(), key=lambda t: len(t[0])):
    # E.g. t_EQEQ = r"=="
    globals()["t_" + const_name] = "".join(_escape_tok_regex(tok_regex))


def t_INTEGER(token):
    r"""[-]?\d+"""
    token.value = ast.Integer(token.value)
    return token


def t_STRING(token):
    r"""["][^"]*?["]"""
    token.value = ast.String(token.value[1:-1])
    return token


def t_NAME(token):
    r"""[_]?[_]?[a-zA-Z][a-zA-Z0-9_]*[_]?[_]?"""
    # Check for reserved words.
    token.type = _RESERVED_WORDS.get(token.value, "NAME")
    return token


def t_newline(token):
    r"""\n+"""
    token.lexer.lineno += len(token.value)


def t_comment(token):
    r"""--.*"""
    pass


t_ignore = " \t"


def t_error(token):
    """Error handling rule."""
    errors.illegal_char(
        token.lexer.lineno, EXIT_ON_ERROR, char=token.value[0])


# Parser defs.
precedence = (
    ("right", "LTEQ", "GTEQ", "LT", "GT"),
    ("right", "EQEQ", "NEQ"),
    ("right", "PLUS", "MINUS"),
    ("right", "TIMES", "DIVIDE"),
)


def sexpr(expr):
    if len(expr) == 3 + 1:
        return [expr[2], expr[1], expr[3]]
    return [expr[1]]


def p_ast_1(content):
    """ast : pair ast"""
    content[0] = [content[1]] + content[2]


def p_ast_2(content):
    """ast : pair"""
    content[0] = [content[1]]


def p_pair(content):
    """pair : stmt"""
    content[0] = ast.Pair(content.lineno(0), content[1])


def p_stmt(content):
    """
    stmt : decl
         | assignment
         | struct_decl
         | func_decl
    """
    content[0] = content[1]


def p_return(content):
    """return : RET bool_expr"""
    content[0] = ast.Return(content[2])


def p_func_decl_1(content):
    """
    func_decl : FUN NAME LPAREN args RPAREN COLON type LBRACE func_body RBRACE
    """
    content[0] = ast.Func(
        name=ast.Name(content[2]), args=content[4],
        type_=content[7], body=content[9])


def p_func_decl_2(content):
    """func_decl : FUN NAME LPAREN args RPAREN LBRACE func_body RBRACE"""
    content[0] = ast.Func(
        name=ast.Name(content[2]), args=content[4],
        type_=None, body=content[7])


def p_func_body_1(content):
    """func_body : func_body_stmt func_body"""
    content[0] = [content[1]] + content[2]


def p_func_body_2(content):
    """func_body : empty"""
    content[0] = []


def p_func_body_stmt(content):
    """
    func_body_stmt : decl
                   | assignment
                   | return
    """
    content[0] = ast.Pair(content.lineno(0), content[1])

def p_args_1(content):
    """args : arg_names COLON type SEMI args"""
    content[0] = []
    for name in content[1]:
        content[0].append(ast.Arg(name, content[3]))
    content[0] += content[5]

def p_args_2(content):
    """args : empty"""
    content[0] = []


def p_arg_names_1(content):
    """arg_names : NAME COMMA arg_names"""
    content[0] = [ast.Name(content[1])] + content[3]


def p_arg_names_2(content):
    """arg_names : NAME"""
    content[0] = [ast.Name(content[1])]


def p_struct_decl(content):
    """struct_decl : DATA NAME LBRACE struct_body RBRACE"""
    content[0] = ast.Data(ast.Name(content[2]), content[4])


def p_struct_body_1(content):
    """struct_body : struct_body struct_body_stmt"""
    content[0] = content[1] + [content[2]]


def p_struct_body_2(content):
    """struct_body : empty"""
    content[0] = []


def p_struct_body_stmt(content):
    """
    struct_body_stmt : decl
                     | func_decl
    """
    content[0] = ast.Pair(content.lineno(0), content[1])


def p_decl_1(content):
    """decl : VAR NAME COLON type EQ bool_expr"""
    content[0] = ast.Decl(
        ast.Name(content[2]), type_=content[4], expr=content[6])


def p_decl_2(content):
    """decl : VAR NAME COLON type"""
    content[0] = ast.Decl(
        ast.Name(content[2]), type_=content[4], expr=None)


def p_decl_3(content):
    """decl : VAR NAME EQ bool_expr"""
    content[0] = ast.Decl(
        ast.Name(content[2]), type_=None, expr=content[4])


def p_assignment(content):
    """assignment : name_from_struct assignop bool_expr"""
    content[0] = ast.Assignment(content[1], content[2], content[3])


def p_method_call(content):
    """method_call : type LPAREN call_args RPAREN"""
    content[0] = ast.MethodCall(method=content[1], args=content[3])


def p_call_args_1(content):
    """call_args : bool_expr COMMA call_args"""
    content[0] = [content[1]] + content[3]


def p_call_args_2(content):
    """call_args : bool_expr"""
    content[0] = [content[1]]


def p_call_args_3(content):
    """call_args : empty"""
    content[0] = []


def p_type(content):
    """type : name_from_struct"""
    content[0] = content[1]


def p_name_from_struct_1(content):
    """name_from_struct : name_from_module PERIOD name_from_struct"""
    content[0] = ast.StructElem(content[1], content[3])


def p_name_from_struct_2(content):
    """name_from_struct : name_from_module"""
    content[0] = content[1]


def p_name_from_module_1(content):
    """name_from_module : NAME HASH NAME"""
    content[0] = ast.ModuleMember(ast.Name(content[1]), ast.Name(content[3]))


def p_name_from_module_2(content):
    """name_from_module : NAME"""
    content[0] = ast.Name(content[1])


def p_assignop(content):
    """assignop : EQ"""
    content[0] = content[1]


def p_boolop(content):
    """
    boolop : EQEQ
           | NEQ
           | LTEQ
           | GTEQ
           | LT
           | GT
    """
    content[0] = content[1]


def p_bool_expr_1(content):
    """bool_expr : expr boolop bool_expr"""
    content[0] = sexpr(content)


def p_bool_expr_2(content):
    """bool_expr : expr"""
    content[0] = content[1]


def p_expr_1(content):
    """
    expr : expr PLUS expr
         | expr MINUS expr
         | expr TIMES expr
         | expr DIVIDE expr
    """
    content[0] = sexpr(content)


def p_expr_2(content):
    """expr : factor"""
    content[0] = content[1]


def p_factor(content):
    """factor : atom_expr"""
    content[0] = content[1]


def p_atom_expr(content):
    """atom_expr : atom"""
    content[0] = content[1]


def p_atom_1(content):
    """
    atom : INTEGER
         | STRING
         | name_from_struct
         | method_call
    """
    content[0] = content[1]


def p_atom_2(content):
    """atom : LPAREN bool_expr RPAREN"""
    content[0] = content[2]


def p_empty(content):
    """empty :"""
    pass

def p_error(content):
    """Error handling function."""
    errors.syntax_error(content.lineno, EXIT_ON_ERROR)


def main(code, *, exit_on_error=True):
    """Build lexer and build parser."""
    global EXIT_ON_ERROR
    EXIT_ON_ERROR = exit_on_error
    lexer = lex.lex(module=sys.modules[__name__])
    lexer.input(code)
    parser = yacc.yacc(module=sys.modules[__name__], debug=False)
    # Parse data got from lexer.
    return parser.parse(input=code, lexer=lexer, tracking=True)
