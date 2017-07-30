import threading
import contextlib


context = threading.local()


@contextlib.contextmanager
def new_context(*, ns, ts, fs, exit_on_error, file_hash, tmp_count):
    context.ns = ns
    context.ts = ts
    context.fs = fs
    context.exit_on_error = exit_on_error
    context.file_hash = file_hash
    context.tmp_count = tmp_count
    yield
