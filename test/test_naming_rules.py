import unittest

from margo import (
    lex_parse, foreign_parser, naming_rules, ast, cdefs, structs)


def compile(inp):
    pseudo_result = naming_rules.NamingRules(structs.Context()).main(
        foreign_parser.ForeignParser().main(
            lex_parse.main(inp,
                exit_on_error=True),
            exit_on_error=True),
        exit_on_error=True)
    # We need only stmt info.
    return [str(pair.stmt) for pair in pseudo_result]


class CommonTestCase(unittest.TestCase):

    def check(self, inp, expected):
        self.assertEqual([
            str(stmt) for stmt in expected], compile(inp))


class DeclOnlyTypeTest(CommonTestCase):

    def test_integer(self):
        expected = (ast.Decl(
            name="myVariable", type_=ast.TypeName("Integer"),
            expr=ast.Empty()), )
        self.check("var myVariable: Integer", expected)

    def test_string(self):
        expected = (ast.Decl(
            name="a", type_=ast.TypeName("String"),
            expr=ast.Empty()), )
        self.check("var a: String", expected)

    def test_c_intfast8(self):
        expected = (ast.Decl(
            name="a1", type_=ast.ModuleMember(
                name=ast.ModuleName(cdefs.CMODULE_NAME),
                member=ast.TypeName("IntFast8")),
            expr=ast.Empty()), )
        self.check("var a1: c#IntFast8", expected)

    def test_c_intfast32(self):
        expected = (ast.Decl(
            name="a1", type_=ast.ModuleMember(
                name=ast.ModuleName(cdefs.CMODULE_NAME),
                member=ast.TypeName("IntFast32")),
            expr=ast.Empty()), )
        self.check("var a1: c#IntFast32", expected)

    def test_c_uintfast8(self):
        expected = (ast.Decl(
            name="a1", type_=ast.ModuleMember(
                name=ast.ModuleName(cdefs.CMODULE_NAME),
                member=ast.TypeName("UIntFast8")),
            expr=ast.Empty()), )
        self.check("var a1: c#UIntFast8", expected)

    def test_c_uintfast32(self):
        expected = (ast.Decl(
            name="a1", type_=ast.ModuleMember(
                name=ast.ModuleName(cdefs.CMODULE_NAME),
                    member=ast.TypeName("UIntFast32")),
            expr=ast.Empty()), )
        self.check("var a1: c#UIntFast32", expected)

    def test_user_type_one_letter(self):
        expected = (ast.Decl(
            name="a", type_=ast.TypeName("I"),
            expr=ast.Empty()), )
        self.check("var a: I", expected)

    def test_user_type_underscore_one_letter(self):
        expected = (ast.Decl(
            name="a", type_=ast.TypeName("_I"),
            expr=ast.Empty()), )
        self.check("var a: _I", expected)


class DeclOnlyExprTest(CommonTestCase):

    def test_integer(self):
        expected = (ast.Decl(
            name="a", type_=ast.Empty(), expr=ast.Integer(
                literal="1")), )
        self.check("var a = 1", expected)

    def test_integer_list1(self):
        expected = (ast.Decl(
            name="a", type_=ast.Empty(), expr=ast.SExpr(
                "+",
                ast.Integer(literal="1"),
                ast.Integer(literal="1"))), )
        self.check("var a = 1 + 1", expected)

    def test_integer_list2(self):
        expected = (ast.Decl(
            name="a", type_=ast.Empty(), expr=ast.SExpr(
                "+",
                ast.Integer(literal="2"),
                ast.SExpr(
                    "*",
                    ast.Integer(literal="9"),
                    ast.Integer(literal="1")))), )
        self.check("var a = 2 + 9 * 1", expected)

    def test_string(self):
        expected = (ast.Decl(
            name="a", type_=ast.Empty(), expr=ast.String(
                literal="Hello, world!")), )
        self.check('var a = "Hello, world!"', expected)

    def test_c_intfast8(self):
        expected = (ast.Decl(
            name="a", type_=ast.Empty(), expr=ast.FuncCall(
                name=ast.ModuleMember(
                    name=ast.ModuleName(cdefs.CMODULE_NAME),
                    member=ast.TypeName("IntFast8")),
                args=[ast.Integer("0")])), )
        self.check("var a = c#IntFast8(0)", expected)

    def test_c_uintfast8(self):
        expected = (ast.Decl(
            name="a", type_=ast.Empty(), expr=ast.FuncCall(
                name=ast.ModuleMember(
                    name=ast.ModuleName(cdefs.CMODULE_NAME),
                    member=ast.TypeName("UIntFast8")),
                args=[ast.Integer("0")])), )
        self.check("var a = c#UIntFast8(0)", expected)

    def test_c_intfast32(self):
        expected = (ast.Decl(
            name="a", type_=ast.Empty(), expr=ast.FuncCall(
                name=ast.ModuleMember(
                    name=ast.ModuleName(cdefs.CMODULE_NAME),
                    member=ast.TypeName("IntFast32")),
                args=[ast.Integer("0")])), )
        self.check("var a = c#IntFast32(0)", expected)

    def test_c_uintfast32(self):
        expected = (ast.Decl(
            name="a", type_=ast.Empty(), expr=ast.FuncCall(
                name=ast.ModuleMember(
                    name=ast.ModuleName(cdefs.CMODULE_NAME),
                    member=ast.TypeName("UIntFast32")),
                args=[ast.Integer("0")])), )
        self.check("var a = c#UIntFast32(0)", expected)


if __name__ == "__main__":
    print("Naming rules")
    unittest.main()
