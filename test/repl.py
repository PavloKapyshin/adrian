import ast
import cmd
import sys
import pprint

import margo


class REPL(cmd.Cmd):
    intro = "Adrian testing REPL.\n"
    prompt = ">>> "
    contexts = {
        layer: {
            "ns": margo.structs.Namespace(),
            "ts": margo.structs.Namespace(),
            "fs": margo.structs.Namespace(),
            "exit_on_error": True,
            "file_hash": margo.REPL_FILE_HASH}
        for layer, _ in margo.LAYERS
    }

    def do_eval(self, inp):
        try:
            pprint.pprint(
                margo.compile_repl(
                    inp, contexts=self.contexts))
        except margo.errors.CompileTimeError as e:
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
