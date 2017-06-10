import unittest

from margo import (
    lex_parse, foreign_parser, naming_rules, analyzer, ast, structs, cdefs,
    type_inference)


def compile(inp):
    pseudo_result = type_inference.TypeInference(structs.Context()).main(
        analyzer.Analyzer(structs.Context()).main(
            naming_rules.NamingRules(structs.Context()).main(
                foreign_parser.ForeignParser().main(
                    lex_parse.main(inp, exit_on_error=True),
                    exit_on_error=True),
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


class DeclOnlyExprTest(CommonTestCase):

    def test_integer(self):
        expected = (ast.Decl(
            name="a", type_=ast.TypeName("Integer"), expr=ast.Integer(
                literal="1")), )
        self.check("var a = 1", expected)

    def test_integer_list1(self):
        expected = (ast.Decl(
            name="a", type_=ast.TypeName("Integer"), expr=ast.SExpr(
                "+",
                ast.Integer(literal="1"),
                ast.Integer(literal="1")
            )), )
        self.check("var a = 1 + 1", expected)

    def test_integer_list2(self):
        expected = (ast.Decl(
            name="a", type_=ast.TypeName("Integer"), expr=ast.SExpr(
                "*",
                ast.SExpr(
                    "+",
                    ast.Integer(literal="2"),
                    ast.Integer(literal="9")
                ),
                ast.Integer(literal="1")
            )), )
        self.check("var a = (2 + 9) * 1", expected)

    def test_string(self):
        expected = (ast.Decl(
            name="a", type_=ast.TypeName("String"), expr=ast.String(
                literal="Hello, world!")), )
        self.check('var a = "Hello, world!"', expected)

    def test_c_intfast8(self):
        expected = (ast.Decl(
            name="a", type_=ast.ModuleMember(
                name=ast.ModuleName(cdefs.CMODULE_NAME),
                member=ast.TypeName("IntFast8")
            ), expr=ast.CIntFast8(literal="0")), )
        self.check("var a = c#IntFast8(0)", expected)

    def test_c_uintfast8(self):
        expected = (ast.Decl(
            name="a", type_=ast.ModuleMember(
                name=ast.ModuleName(cdefs.CMODULE_NAME),
                member=ast.TypeName("UIntFast8")
            ), expr=ast.CUIntFast8(
                literal="0")), )
        self.check("var a = c#UIntFast8(0)", expected)

    def test_c_intfast32(self):
        expected = (ast.Decl(
            name="a", type_=ast.ModuleMember(
                name=ast.ModuleName(cdefs.CMODULE_NAME),
                member=ast.TypeName("IntFast32")
            ), expr=ast.CIntFast32(
                literal="0")), )
        self.check("var a = c#IntFast32(0)", expected)

    def test_c_uintfast32(self):
        expected = (ast.Decl(
            name="a", type_=ast.ModuleMember(
                name=ast.ModuleName(cdefs.CMODULE_NAME),
                member=ast.TypeName("UIntFast32")
            ), expr=ast.CUIntFast32(
                literal="0")), )
        self.check("var a = c#UIntFast32(0)", expected)


if __name__ == "__main__":
    print("Type inference")
    unittest.main()
