import threading
import contextlib
from collections import OrderedDict


context = threading.local()


@contextlib.contextmanager
def new_context(*, env, exit_on_error, module_paths, clibs_includes, i_count):
    context.env = env
    context.exit_on_error = exit_on_error
    context.module_paths = module_paths
    context.clibs_includes = clibs_includes or OrderedDict()
    context.i_count = i_count
    context.func = None
    context.parent = None
    yield


def modified_context_args():
    return {
        key: getattr(context, key)
        for key in (
            "env", "exit_on_error", "module_paths",
            "clibs_includes", "i_count"
        )
    }


class A:

    def __init__(self, *types):
        self.types = types

    def __contains__(self, other):
        return isinstance(other, self.types)
