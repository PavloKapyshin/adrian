import unittest

from margo import parser, ast, cdefs


def compile(inp):
    pseudo_result = parser.main(inp)
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
            expr=None), )
        self.check("var myVariable: Integer", expected)

    def test_string(self):
        expected = (ast.Decl(
            name="a", type_=ast.Name("String"),
            expr=None), )
        self.check("var a: String", expected)

    def test_c_intfast8(self):
        expected = (ast.Decl(
            name="a1", type_=ast.ModuleMember(
                name=cdefs.CMODULE_NAME,
                member=ast.Name("IntFast8")),
            expr=None), )
        self.check("var a1: c#IntFast8", expected)

    def test_c_intfast32(self):
        expected = (ast.Decl(
            name="a1", type_=ast.ModuleMember(
                name=cdefs.CMODULE_NAME,
                member=ast.Name("IntFast32")),
            expr=None), )
        self.check("var a1: c#IntFast32", expected)

    def test_c_uintfast8(self):
        expected = (ast.Decl(
            name="a1", type_=ast.ModuleMember(
                name=cdefs.CMODULE_NAME,
                member=ast.Name("UIntFast8")),
            expr=None), )
        self.check("var a1: c#UIntFast8", expected)

    def test_c_uintfast32(self):
        expected = (ast.Decl(
            name="a1", type_=ast.ModuleMember(
                name=cdefs.CMODULE_NAME,
                    member=ast.Name("UIntFast32")),
            expr=None), )
        self.check("var a1: c#UIntFast32", expected)


class DeclOnlyExprTest(CommonTestCase):

    def test_integer(self):
        expected = (ast.Decl(
            name="a", type_=None, expr=ast.Integer(
                literal="1")), )
        self.check("var a = 1", expected)

    def test_integer_list1(self):
        expected = (ast.Decl(
            name="a", type_=None, expr=[
                "+",
                ast.Integer(literal="1"),
                ast.Integer(literal="1")
            ]), )
        self.check("var a = 1 + 1", expected)

    def test_integer_list2(self):
        expected = (ast.Decl(
            name="a", type_=None, expr=[
                "+",
                ast.Integer(literal="2"),
                [
                    "*",
                    ast.Integer(literal="9"),
                    ast.Integer(literal="1")
                ]
            ]), )
        self.check("var a = 2 + 9 * 1", expected)

    def test_string(self):
        expected = (ast.Decl(
            name="a", type_=None, expr=ast.String(
                literal="Hello, world!")), )
        self.check('var a = "Hello, world!"', expected)

    def test_c_intfast8(self):
        expected = (ast.Decl(
            name="a", type_=None, expr=ast.FuncCall(
                name=ast.ModuleMember(
                    name=cdefs.CMODULE_NAME,
                    member=ast.Name("IntFast8")),
                args=[ast.Integer("0")])), )
        self.check("var a = c#IntFast8(0)", expected)

    def test_c_uintfast8(self):
        expected = (ast.Decl(
            name="a", type_=None, expr=ast.FuncCall(
                name=ast.ModuleMember(
                    name=cdefs.CMODULE_NAME,
                    member=ast.Name("UIntFast8")),
                args=[ast.Integer("0")])), )
        self.check("var a = c#UIntFast8(0)", expected)

    def test_c_intfast32(self):
        expected = (ast.Decl(
            name="a", type_=None, expr=ast.FuncCall(
                name=ast.ModuleMember(
                    name=cdefs.CMODULE_NAME,
                    member=ast.Name("IntFast32")),
                args=[ast.Integer("0")])), )
        self.check("var a = c#IntFast32(0)", expected)

    def test_c_uintfast32(self):
        expected = (ast.Decl(
            name="a", type_=None, expr=ast.FuncCall(
                name=ast.ModuleMember(
                    name=cdefs.CMODULE_NAME,
                    member=ast.Name("UIntFast32")),
                args=[ast.Integer("0")])), )
        self.check("var a = c#UIntFast32(0)", expected)


if __name__ == "__main__":
    print("PyParsing Parser")
    unittest.main()
