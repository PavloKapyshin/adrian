import traceback

import ast
import cmd
import sys

import margo


class REPL(cmd.Cmd):
    intro = "Adrian testing REPL.\n"
    prompt = ">>> "
    input_ = []
    contexts = {
        layer: {
            "env": margo.env.Env(),
            "exit_on_error": True,
            "file_hash": margo.REPL_FILE_HASH,
            "tmp_count": 0}
        for layer, _ in margo.LAYERS
    }

    def do_eval(self, inp):
        try:
            print(
                margo.compile_repl(
                    "\n".join(self.input_), contexts=self.contexts))
        except margo.errors.CompileTimeError as e:
            print(e.message, file=sys.stderr)
        except Exception as e:
            traceback.print_exc(chain=False)

    def command(self, command):
        if command == "genc":
            self.do_eval(self.input_)

    def do_exit(self, arg):
        sys.exit(0)

    def default(self, inp):
        if inp == "EOF":
            return True
        if inp.startswith(":"):
            self.command(inp.split(":")[1])
        else:
            self.input_.append(inp)

    do_quit = do_exit
    do_q = do_exit
    do_e = do_exit

if __name__ == "__main__":
    REPL().cmdloop()
