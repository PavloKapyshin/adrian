import cmd
import sys

from margo import ast
from margo import errors
from margo import lex_parse
from margo import name_checking
from margo import type_checking
from margo import name_mangling


class REPL(cmd.Cmd):
    intro = "This is Adrian testing REPL.\n"
    prompt = ">>> "
    _settings = {"exit_on_error": True, "mangle_names": False}
    context = ast.Context(_settings["exit_on_error"])

    def do_settings(self, settings):
        if settings:
            for kw in settings.split(" "):
                key, value = kw.split("=")
                self._settings[key] = eval(value)
        else:
            print(self._settings)

    def do_eval(self, inp):
        try:
            # Keep settings up to date.
            self.context.exit_on_error = self._settings["exit_on_error"]

            settings = self._settings
            mangle_names = settings["mangle_names"]
            del settings["mangle_names"]

            # Compiling.
            ast = lex_parse.main(inp, **settings)
            nc_ast = name_checking.main(ast, context=self.context)
            tc_ast = type_checking.main(nc_ast, context=self.context)
            if mangle_names:
                nm_ast = name_mangling.main(tc_ast, file_hash="FILEHASH")
                print(nm_ast)
            else:
                print(tc_ast)
            self._settings["mangle_names"] = mangle_names
        except errors.CompilationError as e:
            pass

    def do_exit(self, arg):
        sys.exit(0)

    def default(self, inp):
        self.do_eval(inp)

    do_quit = do_exit
    do_set = do_settings


if __name__ == "__main__":
    REPL().cmdloop()
