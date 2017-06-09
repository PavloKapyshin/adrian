DECL = "decl"
PAIR = "pair"
NAME = "name"
SEXPR = "sexpr"

INTEGER = "integer"
STRING = "string"

MODULE_MEMBER = "module-member"
STRUCT_ELEM = "struct-elem"
METHOD_CALL = "method-call"
FUNC_CALL = "func-call"

EMPTY = ""


def type_(node):
    return node[0]


def isinstance_(node, node_type):
    return type_(node) == node_type
