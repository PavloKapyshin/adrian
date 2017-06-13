DECL = "Decl"
PAIR = "Pair"
NAME = "Name"
SEXPR = "SExpr"

INTEGER = "Integer"
STRING = "String"

MODULE_MEMBER = "ModuleMember"
STRUCT_ELEM = "StructElem"
METHOD_CALL = "MethodCall"
FUNC_CALL = "FuncCall"

EMPTY = "Empty"


def type_(node):
    return node[0]


def isinstance_(node, node_type):
    return type_(node) == node_type
