import functools

from pyparsing import (
    Literal, Optional, Regex, OneOrMore, ZeroOrMore, Combine,
    Forward, Suppress, Group, ParseResults, Empty,
    operatorPrecedence, opAssoc, oneOf, dblQuotedString, lineno)

from . import ast


def sexpr(tokens):
    if isinstance(tokens, ParseResults):
        tokens = tokens.asList()
        return [tokens[1], sexpr(tokens[0]), sexpr(tokens[2])]
    elif isinstance(tokens, list) and len(tokens) == 1:
        return tokens[0]
    return tokens


def expr_wrapper(expr):
    result = expr.asList()
    if len(result) == 1:
        return result[0]
    return result


def parse_call(tokens):
    args = []
    if tokens[1] != "":
        append = args.append
        for arg in tokens[1:]:
            if isinstance(arg, list):
                append(arg[0])
            else:
                append(arg)
    if isinstance(tokens[0], ast.StructElem):
        return ast.MethodCall(
            struct=tokens[0].name, method=tokens[0].elem,
            args=args)
    return ast.FuncCall(
        name=tokens[0], args=args)


whites = OneOrMore(Literal(" "))

var = Literal("var")

underscore = Literal("_")
period = Literal(".")
comma = Literal(",")
hash_ = Literal("#")

plus = Literal("+")
minus = Literal("-")
star = Literal("*")
slash = Literal("/")

equal = Literal("=")
colon = Literal(":")
lparen = Literal("(")
rparen = Literal(")")

lowercase_letter = Regex(r"[a-z]")
uppercase_letter = Regex(r"[A-Z]")
letter = lowercase_letter | uppercase_letter
number = Regex(r"\d")

name = Combine(
    Optional(underscore) +
    Optional(underscore) +
    letter +
    ZeroOrMore(letter | number | underscore) +
    Optional(underscore) +
    Optional(underscore)).setParseAction(
        lambda tokens: ast.Name("".join(tokens)))

name_from_module_ = (
        name + Suppress(hash_) + name).setParseAction(
            lambda tokens: ast.ModuleMember(
                name=tokens[0],
                member=tokens[1]))
name_from_module = (name_from_module_ | name)

name_from_struct = Forward()
name_from_struct_recursive = (
    name_from_module + Suppress(period) +
    name_from_struct).setParseAction(
        lambda tokens: ast.StructElem(
            name=tokens[0],
            elem=tokens[1]))
name_from_struct << (name_from_struct_recursive | name_from_module)

integer = Combine(
    Optional(minus) +
    OneOrMore(number)).setParseAction(
        lambda tokens: ast.Integer("".join(tokens)))

string = dblQuotedString.setParseAction(
    lambda tokens: ast.String(tokens[0][1:-1]))

type_ = name_from_struct

operator = (plus | minus | star | slash)

expr = Forward()

call_args = Forward()
call_args_one = Group(expr).setParseAction(
    lambda tokens: expr_wrapper(tokens[0]))
call_args_list = (
    Group(expr) + Suppress(comma) + call_args).setParseAction(
        lambda tokens: tokens.asList())
call_args << (call_args_list | call_args_one)

call = (
    name_from_struct + Suppress(lparen) +
    (call_args | Combine(Empty())) + Suppress(rparen)).setParseAction(parse_call)

expr_atom = (call | integer | string | name_from_struct)
expr_list = (
    expr_atom + operator + Group(expr)).setParseAction(sexpr)
expr << (expr_list | expr_atom)


decl1 = (
    Suppress(var) + name + Suppress(colon) +
    type_ + Suppress(equal) + Group(expr)).setParseAction(
        lambda tokens: ast.Decl(
            name=tokens[0], type_=tokens[1],
            expr=expr_wrapper(tokens[2])))

decl2 = (
    Suppress(var) + name + Suppress(colon) + type_).setParseAction(
        lambda tokens: ast.Decl(
            name=tokens[0], type_=tokens[1],
            expr=None))

decl3 = (
    Suppress(var) + name + Suppress(equal) +
    Group(expr)).setParseAction(
        lambda tokens: ast.Decl(
            name=tokens[0], type_=None,
            expr=expr_wrapper(tokens[1])))

decl = (decl1 | decl2 | decl3)

stmt = (decl | call).setParseAction(
    lambda st, locn, tokens: ast.Pair(
        line=lineno(locn, st),
        stmt=tokens[0]))

ast_ = OneOrMore(stmt)

def main(inp):
    return ast_.parseString(inp).asList()
