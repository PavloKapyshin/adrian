from pygments.lexer import RegexLexer
from pygments.token import *


__all__ = ["AdrianLexer"]


class AdrianLexer(RegexLexer):
    name = "Adrian"
    aliases = ["adrian"]
    filenames = ["*.adr"]

    tokens = {
        "root": [
            (r"(print|iff|elif|else|ret)", Keyword.Reserved),
            (r"(var|cst|fun)", Keyword.Declaration),
            (r"(Integer|String|None)", Keyword.Type),
            (r"[a-zA-Z0-9_]+", Name),
            (r"[-]?\d+", Number.Integer),
            (r"--.+", Comment),
            (r"(\+|\*|/|%|-|\+=|-=|\*=|/=)", Operator),
            (r'"', Literal.String, "string"),
            (r"[\n\t ]", Text),
            (r"[\[\]\{\}\.\(\):=,<>\#]", Punctuation),
        ],
        "string": [
            (r'[^"\\]+', Literal.String),
            (r"\\.", Literal.String.Escape),
            ('"', Literal.String, "#pop"),
        ]
    }
