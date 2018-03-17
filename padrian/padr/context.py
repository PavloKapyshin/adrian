import threading
import contextlib

from . import astlib

context = threading.local()

@contextlib.contextmanager
def new_context(*, env, exit_on_error, module_paths):
    context.env = env
    context.exit_on_error = exit_on_error
    context.module_paths = module_paths
    context.parent = None
    yield
