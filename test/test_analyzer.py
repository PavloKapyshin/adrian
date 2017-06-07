import unittest

from margo import lex_parse, analyzer, ast, cdefs


def compile(inp):
    pseudo_result = analyzer.Analyzer(
        ast.Context(exit_on_error=True)).main(
            lex_parse.main(inp, exit_on_error=True))
    # We don't need line info.
    return [str(pair.stmt) for pair in pseudo_result]


class DeclOnlyType(unittest.TestCase):

    def test_integer(self):
        inp = "var myVariable: Integer"
        out = [str(stmt) for stmt in (
            ast.Decl(
                name="myVariable", type_=ast.Name("Integer"),
                expr=None), )]
        self.assertEqual(out, compile(inp))

    def test_string(self):
        inp = "var a: String"
        out = [str(stmt) for stmt in (
            ast.Decl(
                name="a", type_=ast.Name("String"),
                expr=None), )]
        self.assertEqual(out, compile(inp))

    def test_c_intfast8(self):
        inp = "var a1: c#IntFast8"
        out = [str(stmt) for stmt in (
            ast.Decl(
                name="a1", type_=ast.ModuleMember(
                    name=cdefs.CMODULE_NAME,
                    member=ast.Name("IntFast8")),
                expr=None), )]
        self.assertEqual(out, compile(inp))

    def test_c_intfast32(self):
        inp = "var a1: c#IntFast32"
        out = [str(stmt) for stmt in (
            ast.Decl(
                name="a1", type_=ast.ModuleMember(
                    name=cdefs.CMODULE_NAME,
                    member=ast.Name("IntFast32")),
                expr=None), )]
        self.assertEqual(out, compile(inp))

    def test_c_uintfast8(self):
        inp = "var a1: c#UIntFast8"
        out = [str(stmt) for stmt in (
            ast.Decl(
                name="a1", type_=ast.ModuleMember(
                    name=cdefs.CMODULE_NAME,
                    member=ast.Name("UIntFast8")),
                expr=None), )]
        self.assertEqual(out, compile(inp))

    def test_c_uintfast32(self):
        inp = "var a1: c#UIntFast32"
        out = [str(stmt) for stmt in (
            ast.Decl(
                name="a1", type_=ast.ModuleMember(
                    name=cdefs.CMODULE_NAME,
                    member=ast.Name("UIntFast32")),
                expr=None), )]
        self.assertEqual(out, compile(inp))


class DeclOnlyExpr(unittest.TestCase):

    def test_integer(self):
        inp = "var a = 1"
        out = [str(stmt) for stmt in (
            ast.Decl(
                name="a", type_=None, expr=ast.Integer(
                    literal="1")), )]
        self.assertEqual(out, compile(inp))

    def test_integer_list1(self):
        inp = "var a = 1 + 1"
        out = [str(stmt) for stmt in (
            ast.Decl(
                name="a", type_=None, expr=[
                    "+",
                    ast.Integer(literal="1"),
                    ast.Integer(literal="1")
                ]), )]
        self.assertEqual(out, compile(inp))

    def test_integer_list2(self):
        inp = "var a = (2 + 9) * 1"
        out = [str(stmt) for stmt in (
            ast.Decl(
                name="a", type_=None, expr=[
                    "*",
                    [
                        "+",
                        ast.Integer(literal="2"),
                        ast.Integer(literal="9")
                    ],
                    ast.Integer(literal="1")
                ]), )]
        self.assertEqual(out, compile(inp))

    def test_string(self):
        inp = 'var a = "Hello, world!"'
        out = [str(stmt) for stmt in (
            ast.Decl(
                name="a", type_=None, expr=ast.String(
                    literal="Hello, world!")), )]
        self.assertEqual(out, compile(inp))

    def test_c_intfast8(self):
        inp = 'var a = c#IntFast8(0)'
        out = [str(stmt) for stmt in (
            ast.Decl(
                name="a", type_=None, expr=ast.CIntFast8(
                    literal="0")), )]
        self.assertEqual(out, compile(inp))

    def test_c_uintfast8(self):
        inp = 'var a = c#UIntFast8(0)'
        out = [str(stmt) for stmt in (
            ast.Decl(
                name="a", type_=None, expr=ast.CUIntFast8(
                    literal="0")), )]
        self.assertEqual(out, compile(inp))

    def test_c_intfast32(self):
        inp = 'var a = c#IntFast32(0)'
        out = [str(stmt) for stmt in (
            ast.Decl(
                name="a", type_=None, expr=ast.CIntFast32(
                    literal="0")), )]
        self.assertEqual(out, compile(inp))

    def test_c_uintfast32(self):
        inp = 'var a = c#UIntFast32(0)'
        out = [str(stmt) for stmt in (
            ast.Decl(
                name="a", type_=None, expr=ast.CUIntFast32(
                    literal="0")), )]
        self.assertEqual(out, compile(inp))


if __name__ == "__main__":
    unittest.main()
