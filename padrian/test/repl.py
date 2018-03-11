import traceback

import cmd
import sys

import padr


class REPL(cmd.Cmd):
    intro = "Test it.\n"
    prompt = ">>> "
    input_ = []

    def do_eval(self, inp):
        try:
            print(padr.compile_("\n".join(self.input_)))
        except padr.errors.CompileTimeError as e:
            print(e.message, file=sys.stderr)
        except Exception as e:
            traceback.print_exc(chain=False)

    def command(self, command):
        if command == "genc":
            self.do_eval(self.input_)
        if command == "clear":
            self.input_ = []
        if command == "undo":
            self.input_ = self.input_[:-1]

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
