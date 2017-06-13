import ast
import cmd
import sys
import pprint

import margo


class REPL(cmd.Cmd):
    intro = "Adrian testing REPL.\n"
    prompt = ">>> "
    ns = margo.structs.Namespace()
    ts = margo.structs.Namespace()
    fs = margo.structs.Namespace()

    def do_eval(self, inp):
        try:
            pprint.pprint(
                margo.compile_repl(
                    inp, ns=self.ns, ts=self.ts,
                    fs=self.fs, exit_on_error=True))
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

if __name__ == "__main__":
    REPL().cmdloop()
