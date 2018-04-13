import threading
import contextlib
from collections import OrderedDict


context = threading.local()


@contextlib.contextmanager
def new_context(*, env, exit_on_error, module_paths):
    context.env = env
    context.exit_on_error = exit_on_error
    context.module_paths = module_paths
    context.func = None
    context.parent = None
    context.inlining = []
    yield


def modified_context_args():
    return {
        key: getattr(context, key)
        for key in ("env", "exit_on_error", "module_paths")
    }
