import re
import sys

from ply import lex, yacc

from . import parser_astlib, astlib, defs, errors


_RESERVED_WORDS = defs.RESERVED_WORDS

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
    ",": "COMMA",
    ".": "DOT",
    "#": "HASH"
}

tokens = (
    "INTEGER",
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
    globals()["t_" + const_name] = "".join(
        _escape_tok_regex(tok_regex))


def t_INTEGER(token):
    r"""[-]?\d*[\.]?\d+"""
    token.value = [
        parser_astlib.LITERAL, astlib.LiteralT.integer, token.value]
    return token


def t_NAME(token):
    # Check for reserved words.
    token.type = _RESERVED_WORDS.get(token.value, "NAME")
    return token
t_NAME.__doc__ = defs.COMMON_REGEX


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
        token.lexer.lineno, char=token.value[0])


precedence = (
    ("left", "LTEQ", "GTEQ", "LT", "GT"),
    ("left", "EQEQ", "NEQ"),
    ("left", "PLUS", "MINUS"),
    ("left", "TIMES", "DIVIDE"),
    ("left", "DOT"),
)


def sexpr(expr):
    if len(expr) == 3 + 1:
        return [parser_astlib.SEXPR, expr[1], expr[2], expr[3]]
    return [expr[1]]


def p_ast_1(content):
    """ast : ast stmt"""
    content[0] = content[1] + [content[2]]


def p_ast_2(content):
    """ast : stmt"""
    content[0] = [content[1]]


def p_stmt(content):
    """
    stmt : let_decl
         | var_decl
         | fun_decl
         | struct_decl
         | assignment
         | factor
         | adt_decl
         | protocol_decl
    """
    content[0] = content[1]


def p_let_decl_1(content):
    """let_decl : LET NAME COLON type EQ bool_expr"""
    content[0] = [
        parser_astlib.DECL, astlib.DeclT.let,
        [parser_astlib.NAME, content[2]],
        content[4], content[6]]


def p_let_decl_2(content):
    """let_decl : LET NAME EQ bool_expr"""
    content[0] = [
        parser_astlib.DECL, astlib.DeclT.let,
        [parser_astlib.NAME, content[2]],
        [parser_astlib.EMPTY], content[4]]


def p_var_decl_1(content):
    """var_decl : VAR NAME COLON type EQ bool_expr"""
    content[0] = [
        parser_astlib.DECL, astlib.DeclT.var,
        [parser_astlib.NAME, content[2]],
        content[4], content[6]]


def p_var_decl_2(content):
    """var_decl : VAR NAME COLON type"""
    content[0] = [
        parser_astlib.DECL, astlib.DeclT.var,
        [parser_astlib.NAME, content[2]],
        content[4], [parser_astlib.EMPTY]]


def p_var_decl_3(content):
    """var_decl : VAR NAME EQ bool_expr"""
    content[0] = [
        parser_astlib.DECL, astlib.DeclT.var,
        [parser_astlib.NAME, content[2]],
        [parser_astlib.EMPTY], content[4]]


def p_fun_decl(content):
    """
    fun_decl : FUN NAME LPAREN decl_args RPAREN COLON type LBRACE fun_body RBRACE
    """
    content[0] = [
        parser_astlib.CALLABLE_DECL, astlib.DeclT.fun,
        [parser_astlib.EMPTY], [parser_astlib.NAME, content[2]],
        content[4], content[7], content[9]]


def p_struct_decl_1(content):
    """
    struct_decl : STRUCT NAME LBRACE struct_body RBRACE
    """
    content[0] = [
        parser_astlib.DATA_DECL, astlib.DeclT.struct,
        [parser_astlib.NAME, content[2]], [parser_astlib.EMPTY], content[4]]


def p_struct_decl_2(content):
    """
    struct_decl : STRUCT NAME LPAREN params RPAREN LBRACE struct_body RBRACE
    """
    content[0] = [
        parser_astlib.DATA_DECL, astlib.DeclT.struct,
        [parser_astlib.NAME, content[2]], content[4], content[7]]


def p_adt_decl_1(content):
    """
    adt_decl : ADT NAME LBRACE adt_body RBRACE
    """
    content[0] = [
        parser_astlib.DATA_DECL, astlib.DeclT.adt,
        [parser_astlib.NAME, content[2]], [parser_astlib.EMPTY], content[4]]


def p_adt_decl_2(content):
    """
    adt_decl : ADT NAME LPAREN params RPAREN LBRACE adt_body RBRACE
    """
    content[0] = [
        parser_astlib.DATA_DECL, astlib.DeclT.adt,
        [parser_astlib.NAME, content[2]], content[4], content[7]]


def p_protocol_decl(content):
    """
    protocol_decl : PROTOCOL NAME LBRACE protocol_body RBRACE
    """
    content[0] = [
        parser_astlib.DATA_DECL, astlib.DeclT.protocol,
        [parser_astlib.NAME, content[2]], [parser_astlib.EMPTY], content[4]]


def p_params_1(content):
    """params : NAME COMMA params"""
    content[0] = [
        parser_astlib.LLNODE,
        astlib.LLT.params,
        [parser_astlib.NAME, content[1]], content[3]]

def p_params_2(content):
    """params : NAME"""
    content[0] = [
        parser_astlib.LLNODE,
        astlib.LLT.params,
        [parser_astlib.NAME, content[1]], [parser_astlib.EMPTY]]

def p_params_3(content):
    """params : empty"""
    content[0] = [parser_astlib.EMPTY]


def p_field_decl(content):
    """field_decl : NAME COLON type"""
    content[0] = [
        parser_astlib.DECL, astlib.DeclT.field,
        [parser_astlib.NAME, content[1]],
        content[3], [parser_astlib.EMPTY]]


def p_return_stmt(content):
    """return_stmt : RETURN bool_expr"""
    content[0] = [parser_astlib.RETURN, content[2]]


def p_assignment(content):
    """
    assignment : factor EQ bool_expr
    """
    content[0] = [
        parser_astlib.ASSIGNMENT, content[1], content[2], content[3]]


def p_fun_body_1(content):
    """fun_body : fun_body_stmt fun_body"""
    content[0] = [
        parser_astlib.LLNODE, astlib.LLT.body,
        content[1], content[2]]


def p_fun_body_2(content):
    """fun_body : empty"""
    content[0] = [parser_astlib.EMPTY]


def p_fun_body_stmt(content):
    """
    fun_body_stmt : var_decl
                  | let_decl
                  | assignment
                  | return_stmt
                  | factor
    """
    content[0] = content[1]


def p_struct_body_1(content):
    """struct_body : struct_body_stmt struct_body"""
    content[0] = [
        parser_astlib.LLNODE, astlib.LLT.body,
        content[1], content[2]]


def p_struct_body_2(content):
    """struct_body : empty"""
    content[0] = [parser_astlib.EMPTY]


def p_adt_body_1(content):
    """adt_body : adt_body_stmt COMMA adt_body"""
    content[0] = [
        parser_astlib.LLNODE, astlib.LLT.body,
        content[1], content[3]]


def p_adt_body_2(content):
    """adt_body : adt_body_stmt"""
    content[0] = [
        parser_astlib.LLNODE, astlib.LLT.body,
        content[1], [parser_astlib.EMPTY]]


def p_adt_body_3(content):
    """adt_body : empty"""
    content[0] = [parser_astlib.EMPTY]


def p_protocol_body_1(content):
    """protocol_body : protocol_body_stmt protocol_body"""
    content[0] = [
        parser_astlib.LLNODE, astlib.LLT.body,
        content[1], content[2]]


def p_protocol_body_2(content):
    """protocol_body : empty"""
    content[0] = [parser_astlib.EMPTY]


def p_adt_body_stmt(content):
    """adt_body_stmt : type"""
    content[0] = content[1]


def p_protocol_body_stmt_1(content):
    """protocol_body_stmt : field_decl"""
    content[0] = content[1]


def p_protocol_body_stmt_2(content):
    """protocol_body_stmt : fun_decl"""
    content[0] = [
        parser_astlib.CALLABLE_DECL, astlib.DeclT.method,
        [parser_astlib.EMPTY], content[1][1],
        content[1][2], content[1][3], content[1][4]]


def p_protocol_body_stmt_3(content):
    """protocol_body_stmt : FUN NAME LPAREN decl_args RPAREN COLON type"""
    content[0] = [
        parser_astlib.CALLABLE_DECL, astlib.DeclT.method,
        [parser_astlib.EMPTY], [parser_astlib.NAME, content[2]],
        content[4], content[7], [parser_astlib.EMPTY]]


def p_struct_body_stmt_1(content):
    """struct_body_stmt : field_decl"""
    content[0] = content[1]


def p_struct_body_stmt_2(content):
    """struct_body_stmt : fun_decl"""
    content[0] = [
        parser_astlib.CALLABLE_DECL, astlib.DeclT.method,
        [parser_astlib.EMPTY], content[1][1],
        content[1][2], content[1][3], content[1][4]]


def p_decl_args_1(content):
    """decl_args : NAME COLON type COMMA decl_args"""
    content[0] = [
        parser_astlib.ARGS, [parser_astlib.NAME, content[1]],
        content[3], content[5]]


def p_decl_args_2(content):
    """decl_args : NAME COLON type"""
    content[0] = [
        parser_astlib.ARGS, [parser_astlib.NAME, content[1]],
        content[3], [parser_astlib.EMPTY]]


def p_decl_args_3(content):
    """decl_args : empty"""
    content[0] = [parser_astlib.EMPTY]


def p_type_1(content):
    """type : module_member"""
    content[0] = content[1]


def p_type_2(content):
    """type : module_member LPAREN types RPAREN"""
    content[0] = [parser_astlib.PARAMED_TYPE, content[1], content[3]]


def p_types_1(content):
    """types : type"""
    content[0] = [parser_astlib.LLNODE, content[1], [parser_astlib.EMPTY]]


def p_types_2(content):
    """types : type COMMA types"""
    content[0] = [parser_astlib.LLNODE, content[1], content[3]]


def p_types_3(content):
    """types : empty"""
    content[0] = [parser_astlib.EMPTY]


def p_arg_list_1(content):
    """arg_list : bool_expr COMMA arg_list"""
    content[0] = [
        parser_astlib.LLNODE, astlib.LLT.call_args,
        content[1], content[3]]


def p_arg_list_2(content):
    """arg_list : bool_expr"""
    content[0] = [
        parser_astlib.LLNODE, astlib.LLT.call_args,
        content[1], [parser_astlib.EMPTY]]


def p_arg_list_3(content):
    """arg_list : empty"""
    content[0] = [parser_astlib.EMPTY]


def p_module_member_1(content):
    """module_member : NAME HASH NAME"""
    content[0] = [
        parser_astlib.DATA_MEMBER, astlib.DataT.module,
        [parser_astlib.NAME, content[1]],
        [parser_astlib.NAME, content[3]]]


def p_module_member_2(content):
    """module_member : NAME"""
    content[0] = [parser_astlib.NAME, content[1]]


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


def p_factor_1(content):
    """factor : atom LPAREN arg_list RPAREN"""
    content[0] = [
        parser_astlib.CALLABLE, astlib.CallableT.fun,
        [parser_astlib.EMPTY],
        content[1], content[3]]


def p_factor_2(content):
    """factor : factor DOT factor"""
    content[0] = [
        parser_astlib.DATA_MEMBER, astlib.DataT.struct,
        content[1], content[3]]


def p_factor_3(content):
    """factor : atom"""
    content[0] = content[1]


def p_atom_1(content):
    """
    atom : INTEGER
         | module_member
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
    line = 0
    if content is not None:
        line = content.lineno
    errors.syntax_error(line)


class Parser:

    def parse(self, code):
        """Build lexer and parser."""
        lexer = lex.lex(module=sys.modules[__name__])
        lexer.input(code)
        parser = yacc.yacc(module=sys.modules[__name__], debug=False)
        return parser.parse(input=code, lexer=lexer, tracking=True)
