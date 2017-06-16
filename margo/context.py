import threading
import contextlib


context = threading.local()


def set_position(position):
    context.position = position


@contextlib.contextmanager
def new_context(*, ns, ts, fs, exit_on_error):
    context.ns = ns
    context.ts = ts
    context.fs = fs
    context.exit_on_error = exit_on_error
    yield
