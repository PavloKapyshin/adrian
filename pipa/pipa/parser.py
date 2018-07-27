import sys
from itertools import chain

import re

from ply import lex, yacc
from . import astlib, defs, errors
from .utils import A


_RESERVED_WORDS = defs.RESERVED_WORDS

_TOKENS = {
    "+=": "PLUSEQ",
    "-=": "MINUSEQ",
    "*=": "TIMESEQ",
    "/=": "DIVIDEEQ",
    # "==": "EQEQ",
    # "<=": "LTEQ",
    # ">=": "GTEQ",
    # "!=": "NEQ",

    "=": "EQ",
    # "<": "LT",
    # ">": "GT",

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


def without_quotes(string):
    return string[1:-1]


def t_STRING(token):
    r'''\"([^\\\n]|(\\.))*?\"'''
    token.value = astlib.Literal(
        astlib.LiteralT.string, without_quotes(token.value))
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
    ("left", "PLUS", "MINUS"),
    ("left", "TIMES", "DIVIDE"),
)


def p_ast_1(content):
    """ast : stmt ast"""
    content[0] = [content[1]] + content[2]


def p_ast_2(content):
    """ast : empty"""
    content[0] = []


def p_stmt(content):
    """
    stmt : let_decl
         | var_decl
         | func_decl
         | struct_decl
         | field_decl
         | assignment
         | factor
         | for_stmt
         | return_stmt
    """
    content[0] = content[1]


def p_let_decl_1(content):
    """let_decl : LET NAME COLON type EQ expr"""
    content[0] = astlib.LetDecl(
        astlib.Name(content[2]), content[4], content[6])


def p_let_decl_2(content):
    """let_decl : LET NAME EQ expr"""
    content[0] = astlib.LetDecl(
        astlib.Name(content[2]), astlib.Empty(), content[4])


def p_var_decl_1(content):
    """var_decl : VAR NAME COLON type EQ expr"""
    content[0] = astlib.VarDecl(
        astlib.Name(content[2]), content[4], content[6])


def p_var_decl_2(content):
    """var_decl : VAR NAME EQ expr"""
    content[0] = astlib.VarDecl(
        astlib.Name(content[2]), astlib.Empty(), content[4])


def p_var_decl_3(content):
    """var_decl : VAR NAME COLON type"""
    content[0] = astlib.VarDecl(
        astlib.Name(content[2]), content[4], astlib.Empty())


def p_func_decl(content):
    """func_decl : FUN NAME LPAREN func_parameters RPAREN COLON type LBRACE ast RBRACE"""
    content[0] = astlib.FuncDecl(
        content[2], content[4], content[7], content[9])


def p_struct_decl(content):
    """struct_decl : STRUCT NAME struct_parameters LBRACE ast RBRACE"""
    body = []
    for stmt in content[5]:
        if stmt in A(astlib.FuncDecl):
            body.append(
                astlib.MethodDecl(
                    stmt.name, stmt.args, stmt.rettype, stmt.body))
        else:
            body.append(stmt)
    content[0] = astlib.StructDecl(content[2], content[3], body)


def p_field_decl(content):
    """field_decl : NAME COLON type"""
    content[0] = astlib.FieldDecl(astlib.Name(content[1]), content[3])


def p_assignment(content):
    """assignment : expr assignment_operator expr"""
    content[0] = astlib.Assignment(content[1], content[2], content[3])


def p_for_stmt(content):
    """for_stmt : FOR arg_list IN expr LBRACE ast RBRACE"""
    content[0] = astlib.For(content[2], content[4], content[6])


def p_return_stmt(content):
    """return_stmt : RETURN expr"""
    content[0] = astlib.Return(content[2])


def p_assignment_operator(content):
    """
    assignment_operator : EQ
                        | PLUSEQ
                        | MINUSEQ
                        | TIMESEQ
                        | DIVIDEEQ
    """
    content[0] = content[1]


def p_struct_parameters_1(content):
    """struct_parameters : LPAREN arg_list RPAREN"""
    content[0] = content[2]

def p_struct_parameters_2(content):
    """struct_parameters : empty"""
    content[0] = []


def p_func_parameters_1(content):
    """func_parameters : NAME COLON type COMMA func_parameters"""
    content[0] = [(astlib.Name(content[1]), content[3])] + content[5]

def p_func_parameters_2(content):
    """func_parameters : NAME COLON type"""
    content[0] = [(astlib.Name(content[1]), content[3])]

def p_func_parameters_3(content):
    """func_parameters : empty"""
    content[0] = []


def p_type_1(content):
    """type : module_member"""
    content[0] = content[1]

def p_type_2(content):
    """type : NAME"""
    content[0] = astlib.Name(content[1])


def p_arg_list_1(content):
    """arg_list : expr COMMA arg_list"""
    content[0] = [content[1]] + content[3]


def p_arg_list_2(content):
    """arg_list : expr"""
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


def p_operator(content):
    """
    operator : PLUS
             | MINUS
             | TIMES
             | DIVIDE
    """
    content[0] = content[1]


def p_expr_1(content):
    """expr : factor operator expr"""
    content[0] = astlib.Expr(content[1], content[2], content[3])


def p_expr_2(content):
    """expr : factor"""
    content[0] = content[1]


def p_factor_1(content):
    """factor : atom LPAREN arg_list RPAREN"""
    content[0] = astlib.FuncCall(content[1], content[3])


def p_factor_3(content):
    """factor : factor DOT factor"""
    def merge(acc, elem):
        if elem in A(list):
            acc.extend(elem)
        elif elem in A(astlib.StructPath):
            acc.extend(elem.path)
        else:
            acc.append(elem)
        return acc

    content[0] = []
    content[0] = merge(content[0], content[1])
    content[0] = merge(content[0], content[3])
    content[0] = astlib.StructPath(content[0])


def p_factor_2(content):
    """factor : atom"""
    content[0] = content[1]


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
    """atom : LPAREN expr RPAREN"""
    content[0] = content[2]


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
    """inner_dict_elem : expr EQ expr"""
    content[0] = {content[1]: content[3]}


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
