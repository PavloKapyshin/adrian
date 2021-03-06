from pygments.lexer import RegexLexer
from pygments.token import *


__all__ = ["AdrianLexer"]


class AdrianLexer(RegexLexer):
    name = "Adrian"
    aliases = ["adrian"]
    filenames = ["*.adr"]

    tokens = {
        "root": [
            (r"(if|else|elif|return|for|while|in|is|not|and|or|break)", Keyword.Reserved),
            (r"(var|let|fun|struct|adt|protocol|extension)", Keyword.Declaration),
            (r"[a-zA-Z0-9_]+", Name),
            (r"[-]?\d*[\.]?\d+", Number.Integer),
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
