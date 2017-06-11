import unittest

from margo import lex_parse, foreign_parser, ast, cdefs


def compile(inp):
    pseudo_result = foreign_parser.ForeignParser().main(
        lex_parse.main(inp, exit_on_error=True),
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
            name="myVariable", type_=ast.Name("Integer"),
            expr=ast.Empty()), )
        self.check("var myVariable: Integer", expected)

    def test_string(self):
        expected = (ast.Decl(
            name="a", type_=ast.Name("String"),
            expr=ast.Empty()), )
        self.check("var a: String", expected)

    def test_c_intfast8(self):
        expected = (ast.Decl(
            name="a1", type_=ast.ModuleMember(
                name=cdefs.CMODULE_NAME,
                member=ast.Name("IntFast8")),
            expr=ast.Empty()), )
        self.check("var a1: c#IntFast8", expected)

    def test_c_intfast32(self):
        expected = (ast.Decl(
            name="a1", type_=ast.ModuleMember(
                name=cdefs.CMODULE_NAME,
                member=ast.Name("IntFast32")),
            expr=ast.Empty()), )
        self.check("var a1: c#IntFast32", expected)

    def test_c_uintfast8(self):
        expected = (ast.Decl(
            name="a1", type_=ast.ModuleMember(
                name=cdefs.CMODULE_NAME,
                member=ast.Name("UIntFast8")),
            expr=ast.Empty()), )
        self.check("var a1: c#UIntFast8", expected)

    def test_c_uintfast32(self):
        expected = (ast.Decl(
            name="a1", type_=ast.ModuleMember(
                name=cdefs.CMODULE_NAME,
                    member=ast.Name("UIntFast32")),
            expr=ast.Empty()), )
        self.check("var a1: c#UIntFast32", expected)


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
                    name=cdefs.CMODULE_NAME,
                    member=ast.Name("IntFast8")),
                args=[ast.Integer("0")])), )
        self.check("var a = c#IntFast8(0)", expected)

    def test_c_uintfast8(self):
        expected = (ast.Decl(
            name="a", type_=ast.Empty(), expr=ast.FuncCall(
                name=ast.ModuleMember(
                    name=cdefs.CMODULE_NAME,
                    member=ast.Name("UIntFast8")),
                args=[ast.Integer("0")])), )
        self.check("var a = c#UIntFast8(0)", expected)

    def test_c_intfast32(self):
        expected = (ast.Decl(
            name="a", type_=ast.Empty(), expr=ast.FuncCall(
                name=ast.ModuleMember(
                    name=cdefs.CMODULE_NAME,
                    member=ast.Name("IntFast32")),
                args=[ast.Integer("0")])), )
        self.check("var a = c#IntFast32(0)", expected)

    def test_c_uintfast32(self):
        expected = (ast.Decl(
            name="a", type_=ast.Empty(), expr=ast.FuncCall(
                name=ast.ModuleMember(
                    name=cdefs.CMODULE_NAME,
                    member=ast.Name("UIntFast32")),
                args=[ast.Integer("0")])), )
        self.check("var a = c#UIntFast32(0)", expected)

    def test_c_malloc(self):
        expected = (ast.Decl(
            name="myMemory", type_=ast.Empty(),
            expr=ast.FuncCall(
                name=ast.ModuleMember(
                    name=cdefs.CMODULE_NAME,
                    member=ast.Name("malloc")),
                args=[ast.Integer("10")])), )
        # TODO: c#malloc's first argument must have c type.
        self.check("var myMemory = c#malloc(10)", expected)


class DeclTest(CommonTestCase):

    def test_integer(self):
        expected = (ast.Decl(
            name="myVariable", type_=ast.Name("Integer"),
            expr=ast.Integer("1")), )
        self.check("var myVariable: Integer = 1", expected)

    def test_c_intfast8(self):
        expected = (ast.Decl(
            name="myVariable", type_=ast.ModuleMember(
                name=cdefs.CMODULE_NAME,
                member=ast.Name("IntFast8")),
            expr=ast.FuncCall(
                name=ast.ModuleMember(
                    name=cdefs.CMODULE_NAME,
                    member=ast.Name("IntFast8")),
                args=[ast.Integer("10")])), )
        self.check("var myVariable: c#IntFast8 = c#IntFast8(10)", expected)


class FuncCall(CommonTestCase):

    def test_c_tpr(self):
        expected = (ast.FuncCall(
            name=ast.ModuleMember(
                name=cdefs.CMODULE_NAME,
                member=ast.Name("tpr")),
            args=[]), )
        self.check("c#tpr()", expected)


if __name__ == "__main__":
    print("PLY Parser")
    unittest.main()
