import sys

import re

from ply import lex, yacc
from . import astlib, defs, errors
from .utils import A


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
    "[": "LBRACKET",
    "]": "RBRACKET",

    ":": "COLON",
    ",": "COMMA",
    ".": "DOT",
    "#": "HASH"
}

tokens = (
     "INTEGER",
     "STRING",
     "NAME",
 ) + tuple(_TOKENS.values()) + tuple(_RESERVED_WORDS.values())


def _escape_tok_regex(regex, escape=set("#.{}()[]*+")):
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
    token.value = astlib.Literal(astlib.LiteralT.number, token.value)
    return token


def cut_quotes(string):
    return string[1:-1]


def t_STRING(token):
    r'''\"([^\\\n]|(\\.))*?\"'''
    token.value = astlib.Literal(astlib.LiteralT.string, cut_quotes(token.value))
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
    errors.illegal_char(char=token.value[0])


precedence = (
    ("left", "LTEQ", "GTEQ", "LT", "GT"),
    ("left", "EQEQ", "NEQ"),
    ("left", "PLUS", "MINUS"),
    ("left", "TIMES", "DIVIDE"),
    ("right", "DOT"),
)


def sexpr(expr):
    if len(expr) == 3 + 1:
        return astlib.Expr(expr[1], expr[2], expr[3])
    return expr[1]


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
         | extension_decl
         | assignment
         | factor
         | adt_decl
         | protocol_decl
         | cond
         | while_stmt
    """
    content[0] = content[1]


def p_while_stmt(content):
    """while_stmt : WHILE bool_expr LBRACE if_body RBRACE"""
    content[0] = astlib.While(content[2], content[4])


def p_cond_1(content):
    """cond : if_stmt else_if_stmts else_stmt"""
    content[0] = astlib.Cond(content[1], content[2], content[3])


def p_cond_2(content):
    """cond : if_stmt else_if_stmts"""
    content[0] = astlib.Cond(content[1], content[2], None)


def p_cond_3(content):
    """cond : if_stmt else_stmt"""
    content[0] = astlib.Cond(content[1], [], content[2])


def p_cond_4(content):
    """cond : if_stmt"""
    content[0] = astlib.Cond(content[1], [], None)


def p_if_stmt(content):
    """if_stmt : IF bool_expr LBRACE if_body RBRACE"""
    content[0] = astlib.If(content[2], content[4])


def p_else_if_stmts_1(content):
    """else_if_stmts : else_if_stmt"""
    content[0] = [content[1]]


def p_else_if_stmts_2(content):
    """else_if_stmts : else_if_stmt else_if_stmts"""
    content[0] = [content[1]] + content[2]


def p_else_if_stmts_3(content):
    """else_if_stmts : empty"""
    content[0] = []


def p_else_if_stmt(content):
    """else_if_stmt : ELIF bool_expr LBRACE if_body RBRACE"""
    content[0] = astlib.Elif(content[2], content[4])


def p_else_stmt(content):
    """else_stmt : ELSE LBRACE if_body RBRACE"""
    content[0] = astlib.Else(content[3])


def p_if_body_1(content):
    """if_body : if_body_stmt if_body"""
    content[0] = [content[1]] + content[2]


def p_if_body_2(content):
    """if_body : empty"""
    content[0] = []


def p_if_body_stmt(content):
    """
    if_body_stmt : let_decl
                 | var_decl
                 | assignment
                 | factor
                 | cond
                 | while_stmt
                 | return_stmt
    """
    content[0] = content[1]


def p_let_decl_1(content):
    """let_decl : LET NAME COLON type EQ bool_expr"""
    content[0] = astlib.LetDecl(astlib.Name(content[2]), content[4], content[6])


def p_let_decl_2(content):
    """let_decl : LET NAME EQ bool_expr"""
    content[0] = astlib.LetDecl(
        astlib.Name(content[2]), astlib.Empty(), content[4])


def p_var_decl_1(content):
    """var_decl : VAR NAME COLON type EQ bool_expr"""
    content[0] = astlib.VarDecl(
        astlib.Name(content[2]), content[4], content[6])


def p_var_decl_2(content):
    """var_decl : VAR NAME COLON type"""
    content[0] = astlib.VarDecl(
        astlib.Name(content[2]), content[4], astlib.Empty())


def p_var_decl_3(content):
    """var_decl : VAR NAME EQ bool_expr"""
    content[0] = astlib.VarDecl(
        astlib.Name(content[2]), astlib.Empty(), content[4])


def p_fun_decl(content):
    """
    fun_decl : FUN NAME LPAREN decl_args RPAREN COLON type LBRACE fun_body RBRACE
    """
    content[0] = astlib.FuncDecl(
        astlib.Name(content[2]), content[4], content[7], content[9])


def p_protocol_impls_1(content):
    """protocol_impls : IS arg_list"""
    content[0] = content[2]


def p_protocol_impls_2(content):
    """protocol_impls : IS LPAREN arg_list RPAREN"""
    content[0] = content[3]


def p_protocol_impls_3(content):
    """protocol_impls : empty"""
    content[0] = []


def p_struct_decl(content):
    """struct_decl : STRUCT NAME parameters protocol_impls LBRACE struct_body RBRACE"""
    content[0] = astlib.StructDecl(
        astlib.Name(content[2]), content[3], content[4], content[6])


def p_extension_decl(content):
    """extension_decl : EXTENSION module_member parameters protocol_impls LBRACE extension_body RBRACE"""
    content[0] = astlib.ExtensionDecl(
        content[2], content[3], content[4], content[6])


def p_adt_decl(content):
    """adt_decl : ADT NAME parameters protocol_impls LBRACE adt_body RBRACE"""
    content[0] = astlib.AdtDecl(
        astlib.Name(content[2]), content[3], content[4], content[6])


def p_protocol_decl(content):
    """protocol_decl : PROTOCOL NAME parameters protocol_impls LBRACE protocol_body RBRACE"""
    content[0] = astlib.ProtocolDecl(
        astlib.Name(content[2]), content[3], content[4], content[6])


def p_parameters_1(content):
    """parameters : LPAREN params RPAREN"""
    content[0] = content[2]


def p_parameters_2(content):
    """parameters : empty"""
    content[0] = []


def p_params_1(content):
    """params : NAME COMMA params"""
    content[0] = [content[1]] + content[3]


def p_params_2(content):
    """params : NAME"""
    content[0] = [content[1]]


def p_params_3(content):
    """params : empty"""
    content[0] = []


def p_field_decl(content):
    """field_decl : NAME COLON type"""
    content[0] = astlib.FieldDecl(astlib.Name(content[1]), content[3])


def p_return_stmt(content):
    """return_stmt : RETURN bool_expr"""
    content[0] = astlib.Return(content[2])


def p_assignment(content):
    """assignment : factor EQ bool_expr"""
    content[0] = astlib.Assignment(content[1], content[2], content[3])


def p_fun_body_1(content):
    """fun_body : fun_body_stmt fun_body"""
    content[0] = [content[1]] + content[2]


def p_fun_body_2(content):
    """fun_body : empty"""
    content[0] = []


def p_fun_body_stmt(content):
    """
    fun_body_stmt : var_decl
                  | let_decl
                  | assignment
                  | return_stmt
                  | factor
                  | cond
                  | while_stmt
    """
    content[0] = content[1]


def p_extension_body_1(content):
    """extension_body : fun_decl extension_body"""
    content[0] = [astlib.StructFuncDecl(
        content[1].name, content[1].args, content[1].rettype,
        content[1].body)] + content[2]


def p_extension_body_2(content):
    """extension_body : empty"""
    content[0] = []


def p_struct_body_1(content):
    """struct_body : struct_body_stmt struct_body"""
    content[0] = [content[1]] + content[2]


def p_struct_body_2(content):
    """struct_body : empty"""
    content[0] = []


def p_adt_body_1(content):
    """adt_body : adt_body_stmt COMMA adt_body"""
    content[0] = [content[1]] + content[3]


def p_adt_body_2(content):
    """adt_body : adt_body_stmt"""
    content[0] = [content[1]]


def p_adt_body_3(content):
    """adt_body : empty"""
    content[0] = []


def p_protocol_body_1(content):
    """protocol_body : protocol_body_stmt protocol_body"""
    content[0] = [content[1]] + content[2]


def p_protocol_body_2(content):
    """protocol_body : empty"""
    content[0] = []


def p_adt_body_stmt(content):
    """adt_body_stmt : type"""
    content[0] = content[1]


def p_protocol_body_stmt_1(content):
    """protocol_body_stmt : field_decl"""
    content[0] = content[1]


def p_protocol_body_stmt_2(content):
    """protocol_body_stmt : fun_prototype"""
    content[0] = content[1]


def p_fun_prototype(content):
    """fun_prototype : FUN NAME LPAREN decl_args RPAREN COLON type"""
    content[0] = astlib.FuncPrototype(
        astlib.Name(content[2]), content[4], content[7])


def p_struct_body_stmt_1(content):
    """struct_body_stmt : field_decl"""
    content[0] = content[1]


def p_struct_body_stmt_2(content):
    """struct_body_stmt : fun_decl"""
    content[0] = astlib.StructFuncDecl(
        content[1].name, content[1].args, content[1].rettype, content[1].body)


def p_decl_args_1(content):
    """decl_args : NAME COLON type COMMA decl_args"""
    content[0] = [(astlib.Name(content[1]), content[3])] + content[5]


def p_decl_args_2(content):
    """decl_args : NAME COLON type"""
    content[0] = [(astlib.Name(content[1]), content[3])]


def p_decl_args_3(content):
    """decl_args : empty"""
    content[0] = []


def p_type_1(content):
    """type : module_member"""
    content[0] = content[1]


def p_type_2(content):
    """type : module_member LPAREN types RPAREN"""
    content[0] = astlib.GenericType(content[1], content[3])


def p_types_1(content):
    """types : type"""
    content[0] = [content[1]]


def p_types_2(content):
    """types : type COMMA types"""
    content[0] = [content[1]] + content[3]


def p_types_3(content):
    """types : empty"""
    content[0] = []


def p_arg_list_1(content):
    """arg_list : bool_expr COMMA arg_list"""
    content[0] = [content[1]] + content[3]


def p_arg_list_2(content):
    """arg_list : bool_expr"""
    content[0] = [content[1]]


def p_arg_list_3(content):
    """arg_list : empty"""
    content[0] = []


def p_module_member_1(content):
    """module_member : NAME HASH NAME"""
    content[0] = astlib.ModuleMember(
        astlib.Name(content[1]), astlib.Name(content[3]))


def p_module_member_2(content):
    """module_member : NAME"""
    content[0] = astlib.Name(content[1])


def p_boolop(content):
    """
    boolop : EQEQ
           | NEQ
           | LTEQ
           | GTEQ
           | LT
           | GT
           | AND
           | OR
           | IS
    """
    content[0] = content[1]


def p_bool_expr_1(content):
    """bool_expr : expr boolop bool_expr"""
    content[0] = sexpr(content)


def p_bool_expr_2(content):
    """bool_expr : NOT bool_expr"""
    content[0] = astlib.Not(content[2])


def p_bool_expr_3(content):
    """
    bool_expr : expr
    """
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
    content[0] = astlib.FuncCall(content[1], content[3])


def p_factor_2(content):
    """factor : factor DOT factor"""
    content[0] = astlib.StructField(content[1], content[3])


def p_factor_3(content):
    """factor : atom LBRACKET subscription RBRACKET"""
    content[0] = astlib.Subscript(content[1], content[3])


def p_factor_4(content):
    """factor : atom"""
    content[0] = content[1]


def p_subscription(content):
    """subscription : bool_expr"""
    content[0] = content[1]


def p_vector(content):
    """vector : LBRACKET arg_list RBRACKET"""
    content[0] = astlib.Literal(astlib.LiteralT.vector, content[2])


def p_dict(content):
    """dict : LBRACE inner_dict RBRACE"""
    content[0] = astlib.Literal(astlib.LiteralT.dict_, content[2])


def p_set(content):
    """set : LBRACE arg_list RBRACE"""
    content[0] = astlib.Literal(astlib.LiteralT.set_, set(content[2]))


def p_inner_dict_1(content):
    """inner_dict : inner_dict_elem COMMA inner_dict"""
    content[0] = {**content[1], **content[3]}


def p_inner_dict_2(content):
    """inner_dict : inner_dict_elem"""
    content[0] = content[1]


def p_inner_dict_3(content):
    """inner_dict : empty"""
    content[0] = dict()


def p_inner_dict_elem(content):
    """inner_dict_elem : bool_expr EQ bool_expr"""
    content[0] = {content[1]: content[3]}


def p_atom_1(content):
    """
    atom : INTEGER
         | STRING
         | vector
         | dict
         | set
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
