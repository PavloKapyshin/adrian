import traceback

import cmd
import sys

import padr


class REPL(cmd.Cmd):
    intro = "Test it.\n"
    prompt = ">>> "
    input_ = []

    def do_eval(self):
        try:
            print(padr.compile_repl("\n".join(self.input_)))
        except padr.errors.CompileTimeError as e:
            print(e.message, file=sys.stderr)
        except Exception as e:
            traceback.print_exc(chain=False)

    def do_debug(self):
        try:
            reg = padr.debug_formatter.DebugFormatter().get_registry()
            compiled = padr.compile_repl("\n".join(self.input_))
            result = list(padr.layers.transform_ast(
                compiled, registry=reg))
            print("\n".join(result))
        except padr.errors.CompileTimeError as e:
            print(e.message, file=sys.stderr)
        except Exception as e:
            traceback.print_exc(chain=False)

    def command(self, command):
        if command == "genc":
            self.do_eval()
        if command == "clear":
            self.input_ = []
        if command == "undo":
            self.input_ = self.input_[:-1]
        if command == "debug":
            self.do_debug()

    def do_exit(self, arg):
        sys.exit(0)

    def default(self, inp):
        if inp == "EOF":
            return True
        if inp.startswith(":"):
            self.command(inp.split(":")[1])
        else:
            self.input_.append(inp)

    def emptyline(self):
        pass

    do_quit = do_exit
    do_q = do_exit
    do_e = do_exit

if __name__ == "__main__":
    REPL().cmdloop()
