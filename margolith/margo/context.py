import threading
import contextlib


context = threading.local()


@contextlib.contextmanager
def new_context(*, env, exit_on_error, file_hash, tmp_count):
    context.env = env
    context.exit_on_error = exit_on_error
    context.file_hash = file_hash
    context.tmp_count = tmp_count
    yield
