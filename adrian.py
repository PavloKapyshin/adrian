from pygments.lexer import RegexLexer
from pygments.token import *


__all__ = ["AdrianLexer"]


class AdrianLexer(RegexLexer):
    name = "Adrian"
    aliases = ["adrian"]
    filenames = ["*.adn"]

    tokens = {
        "root": [
            (r"(func|print|if|elif|else|return)", Keyword.Reserved),
            (r"(var|cst)", Keyword.Declaration),
            (r"(Integer|String|Void)", Keyword.Type),
            (r"[a-zA-Z0-9_]+", Name),
            (r"[-]?\d+", Number.Integer),
            (r"--.+", Comment),
            (r"(\+|\*|/|%|-|\+=|-=|\*=|/=)", Operator),
            (r'"', Literal.String, "string"),
            (r"[\n\t ]", Text),
            (r"[\[\]\{\}\.\(\):=,<>]", Punctuation),
        ],
        "string": [
            (r'[^"\\]+', Literal.String),
            (r"\\.", Literal.String.Escape),
            ('"', Literal.String, "#pop"),
        ]
    }
