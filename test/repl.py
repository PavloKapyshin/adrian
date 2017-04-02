import ast
import cmd
import sys

import margo


class REPL(cmd.Cmd):
    _AVAILABLE_SETTINGS = ("exit_on_error", "mangle_names")
    intro = "This is Adrian testing REPL.\n"
    prompt = ">>> "
    settings = {"exit_on_error": True, "mangle_names": False}
    context = margo.ast.Context(exit_on_error=settings["exit_on_error"])

    def do_settings(self, settings):
        if settings:
            for kw in settings.split(" "):
                key, value = kw.split("=")
                self.settings[key] = ast.literal_eval(value)
        else:
            print(self.settings)

    def complete_settings(self, text, line, begindx, endidx):
        return [i for i in self._AVAILABLE_SETTINGS if i.startswith(text)]

    def do_eval(self, inp):
        try:
            # Keep context up to date.
            self.context.exit_on_error = self.settings["exit_on_error"]
            # Compile.
            margo.compile(
                inp, self.context,
                mangle_names=self.settings["mangle_names"])
        except margo.errors.CompilationError as e:
            print(e.message, file=sys.stderr)

    def do_exit(self, arg):
        sys.exit(0)

    def default(self, inp):
        self.do_eval(inp)

    do_quit = do_exit
    do_set = do_settings
    complete_set = complete_settings


if __name__ == "__main__":
    REPL().cmdloop()
