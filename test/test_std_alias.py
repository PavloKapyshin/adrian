import unittest

from margo import (
    lex_parse, foreign_parser, naming_rules, analyzer, ast, structs, cdefs,
    defs, type_inference, default_value, std_alias)


def compile(inp):
    pseudo_result = std_alias.StdAlias(structs.Context()).main(
        default_value.DefaultValue(structs.Context()).main(
            type_inference.TypeInference(structs.Context()).main(
                analyzer.Analyzer(structs.Context()).main(
                    naming_rules.NamingRules(structs.Context()).main(
                        foreign_parser.ForeignParser().main(
                            lex_parse.main(inp, exit_on_error=True),
                            exit_on_error=True),
                        exit_on_error=True),
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


class DeclTest(CommonTestCase):

    def test_integer(self):
        expected = (ast.Decl(
            name="myVariable", type_=ast.ModuleMember(
                name=ast.ModuleName(defs.STD_TYPES_MODULE_NAME),
                member=ast.TypeName("Integer")),
            expr=ast.FuncCall(
                name=ast.ModuleMember(
                    name=ast.ModuleName(defs.STD_TYPES_MODULE_NAME),
                    member=ast.TypeName("Integer")),
                args=[ast.CString("1")])), )
        self.check("var myVariable: Integer = 1", expected)

    def test_string(self):
        expected = (ast.Decl(
            name="a", type_=ast.ModuleMember(
                name=ast.ModuleName(defs.STD_TYPES_MODULE_NAME),
                member=ast.TypeName("String")),
            expr=ast.FuncCall(
                name=ast.ModuleMember(
                    name=ast.ModuleName(defs.STD_TYPES_MODULE_NAME),
                    member=ast.TypeName("String")),
                args=[ast.CString("Hi!")])), )
        self.check('var a: String = "Hi!"', expected)

    def test_c_intfast8(self):
        expected = (ast.Decl(
            name="a1", type_=ast.ModuleMember(
                name=ast.ModuleName(cdefs.CMODULE_NAME),
                member=ast.TypeName("IntFast8")),
            expr=ast.CIntFast8("0")), )
        self.check("var a1: c#IntFast8 = c#IntFast8(0)", expected)

    def test_c_intfast32(self):
        expected = (ast.Decl(
            name="a1", type_=ast.ModuleMember(
                name=ast.ModuleName(cdefs.CMODULE_NAME),
                member=ast.TypeName("IntFast32")),
            expr=ast.CIntFast32("0")), )
        self.check("var a1: c#IntFast32 = c#IntFast32(0)", expected)

    def test_c_uintfast8(self):
        expected = (ast.Decl(
            name="a1", type_=ast.ModuleMember(
                name=ast.ModuleName(cdefs.CMODULE_NAME),
                member=ast.TypeName("UIntFast8")),
            expr=ast.CUIntFast8("0")), )
        self.check("var a1: c#UIntFast8 = c#UIntFast8(0)", expected)

    def test_c_uintfast32(self):
        expected = (ast.Decl(
            name="a1", type_=ast.ModuleMember(
                name=ast.ModuleName(cdefs.CMODULE_NAME),
                member=ast.TypeName("UIntFast32")),
            expr=ast.CUIntFast32("0")), )
        self.check("var a1: c#UIntFast32 = c#UIntFast32(0)", expected)


if __name__ == "__main__":
    print("STD Alias")
    unittest.main()
