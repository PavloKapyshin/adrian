import cmd
import sys

from margo import lex_parse
from margo import name_checking
from margo import type_checking


class REPL(cmd.Cmd):
    intro = "This is Adrian testing REPL.\n"
    prompt = ">>> "
    keywords = {}

    def do_keywords(self, keywords):
        if keywords:
            for kw in keywords.split(" "):
                key, value = kw.split("=")
                self.keywords[key] = eval(value)
        else:
            print(self.keywords)

    def do_eval(self, inp):
        ast = lex_parse.main(inp, **self.keywords)
        nc_ast = name_checking.main(ast, **self.keywords)
        tc_ast = type_checking.main(nc_ast, **self.keywords)
        print(tc_ast)

    def do_exit(self, arg):
        sys.exit(0)

    def default(self, inp):
        self.do_eval(inp)

    do_quit = do_exit


if __name__ == "__main__":
    REPL().cmdloop()
