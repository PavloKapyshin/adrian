import ast
import cmd
import sys
import pprint

import margo


class REPL(cmd.Cmd):
    _AVAILABLE_SETTINGS = ("exit_on_error", "mangle_names")
    intro = "Welcome to Adrian testing REPL.\n"
    prompt = ">>> "
    settings = {"exit_on_error": False, "mangle_names": False}
    contexts = (lambda settings: {
        layer_name: margo.ast.Context(
            exit_on_error=settings["exit_on_error"])
        for layer_name in (
            "analyzer",
            "naming_rules",
            "type_inference",
            "default_value",
            "std_alias",
            "oop",
            "simple_expr",
            "name_existence",
            "type_checking",
            "arc",
            "cgen"
        )
    })(settings)
    # For lexing and parsing layer.
    contexts["exit_on_error"] = settings["exit_on_error"]

    def do_settings(self, settings):
        if settings:
            for kw in settings.split(" "):
                key, value = kw.split("=")
                self.settings[key] = ast.literal_eval(value)
                if key == "exit_on_error":
                    exit_on_error = self.settings["exit_on_error"]
                    self.contexts["exit_on_error"] = exit_on_error
                    # Updating all contexts.
                    for layer_name, context in self.contexts.items():
                        if layer_name != "exit_on_error":
                            context.exit_on_error = exit_on_error
        else:
            print(self.settings)

    def complete_settings(self, text, line, begindx, endidx):
        return [i for i in self._AVAILABLE_SETTINGS if i.startswith(text)]

    def do_eval(self, inp):
        try:
            # Compiling.
            pprint.pprint(margo.compile_repl(
                inp, self.contexts,
                mangle_names=self.settings["mangle_names"]))
        except margo.errors.CompilationError as e:
            print(e.message, file=sys.stderr)

    def do_exit(self, arg):
        sys.exit(0)

    def default(self, inp):
        if inp == "EOF":
            return True
        self.do_eval(inp)

    do_quit = do_exit
    do_q = do_exit
    do_e = do_exit
    do_set = do_settings
    complete_set = complete_settings


if __name__ == "__main__":
    REPL().cmdloop()
