import re
import sys

from vendor.ply import lex, yacc

from . import parser_astlib, defs, errors


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
    #"{": "LBRACE",
    #"}": "RBRACE",

    ":": "COLON",
    #";": "SEMI",
    ",": "COMMA",
    #".": "PERIOD",
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
    token.value = [parser_astlib.INTEGER, token.value]
    return token


def t_STRING(token):
    r"""["][^"]*?["]"""
    token.value = [parser_astlib.STRING, token.value[1:-1]]
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
        token.lexer.lineno, context.exit_on_error, char=token.value[0])


# Parser defs.
precedence = (
    ("right", "LTEQ", "GTEQ", "LT", "GT"),
    ("right", "EQEQ", "NEQ"),
    ("right", "PLUS", "MINUS"),
    ("right", "TIMES", "DIVIDE"),
)


def sexpr(expr):
    if len(expr) == 3 + 1:
        return [parser_astlib.SEXPR, expr[2], expr[1], expr[3]]
    return [expr[1]]


def p_ast_1(content):
    """ast : pair ast"""
    content[0] = [content[1]] + content[2]


def p_ast_2(content):
    """ast : pair"""
    content[0] = [content[1]]


def p_pair(content):
    """pair : stmt"""
    content[0] = [parser_astlib.PAIR, content.lineno(0), content[1]]


def p_stmt(content):
    """
    stmt : decl
         | func_call
    """
    content[0] = content[1]


def p_func_call_1(content):
    """func_call : name_from_module LPAREN call_args RPAREN"""
    content[0] = [parser_astlib.FUNC_CALL, content[1], content[3]]


# def p_return(content):
#     """return : RET bool_expr"""
#     content[0] = ast.Return(content[2])


# def p_func_decl_1(content):
#     """
#     func_decl : FUN NAME LPAREN args RPAREN COLON type LBRACE func_body RBRACE
#     """
#     content[0] = ast.FuncDecl(
#         name=content[2], args=content[4],
#         type_=content[7], body=content[9])


# def p_func_decl_2(content):
#     """func_decl : FUN NAME LPAREN args RPAREN LBRACE func_body RBRACE"""
#     content[0] = ast.FuncDecl(
#         name=content[2], args=content[4],
#         type_=None, body=content[7])


# def p_func_body_1(content):
#     """func_body : func_body_stmt func_body"""
#     content[0] = [content[1]] + content[2]


# def p_func_body_2(content):
#     """func_body : empty"""
#     content[0] = []


# def p_func_body_stmt(content):
#     """
#     func_body_stmt : decl
#                    | assignment
#                    | return
#                    | func_call
#     """
#     content[0] = ast.Pair(content.lineno(0), column=1, stmt=content[1])

# def p_args_1(content):
#     """args : arg_names COLON type SEMI args"""
#     return [ast.Arg(name, content[3]) for name in content[1]] + content[5]

# def p_args_2(content):
#     """args : empty"""
#     content[0] = []


# def p_arg_names_1(content):
#     """arg_names : NAME COMMA arg_names"""
#     content[0] = [content[1]] + content[3]


# def p_arg_names_2(content):
#     """arg_names : NAME"""
#     content[0] = [content[1]]


# def p_struct_decl(content):
#     """struct_decl : SCT NAME LBRACE struct_body RBRACE"""
#     content[0] = ast.StructDecl(content[2], content[4])


# def p_struct_body_1(content):
#     """struct_body : struct_body struct_body_stmt"""
#     content[0] = content[1] + [content[2]]


# def p_struct_body_2(content):
#     """struct_body : empty"""
#     content[0] = []


# def p_struct_body_stmt(content):
#     """
#     struct_body_stmt : decl
#                      | func_decl
#     """
#     content[0] = ast.Pair(content.lineno(0), column=1, stmt=content[1])


def p_decl_1(content):
    """decl : VAR NAME COLON type EQ bool_expr"""
    content[0] = [parser_astlib.DECL, content[2], content[4], content[6]]


def p_decl_2(content):
    """decl : VAR NAME COLON type"""
    content[0] = [parser_astlib.DECL, content[2], content[4], [parser_astlib.EMPTY]]


def p_decl_3(content):
    """decl : VAR NAME EQ bool_expr"""
    content[0] = [parser_astlib.DECL, content[2], [parser_astlib.EMPTY], content[4]]


# def p_assignment(content):
#     """assignment : name_from_struct assignop bool_expr"""
#     content[0] = ast.Assignment(content[1], content[2], content[3])


# def p_method_call(content):
#     """method_call : type LPAREN call_args RPAREN"""
#     content[0] = [parser_astlib.METHOD_CALL, content[1][0], content[1][1], content[3]]


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
    """type : name_from_module"""
    content[0] = content[1]


# def p_name_from_struct_1(content):
#     """name_from_struct : name_from_module PERIOD name_from_struct"""
#     content[0] = [parser_astlib.STRUCT_ELEM, content[1], content[3]]


# def p_name_from_struct_2(content):
#     """name_from_struct : name_from_module"""
#     content[0] = content[1]


def p_name_from_module_1(content):
    """name_from_module : NAME HASH NAME"""
    content[0] = [parser_astlib.MODULE_MEMBER, content[1], [parser_astlib.NAME, content[3]]]


def p_name_from_module_2(content):
    """name_from_module : NAME"""
    content[0] = [parser_astlib.NAME, content[1]]


# def p_assignop(content):
#     """assignop : EQ"""
#     content[0] = content[1]


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
         | name_from_module
         | func_call
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
    errors.syntax_error(content.lineno, context.exit_on_error)


def main(code):
    """Build lexer and build parser."""
    lexer = lex.lex(module=sys.modules[__name__])
    # Lex code.
    lexer.input(code)
    parser = yacc.yacc(module=sys.modules[__name__], debug=False)
    # Parse data got from lexer.
    return parser.parse(input=code, lexer=lexer, tracking=True)
