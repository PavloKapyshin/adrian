import threading
import contextlib

from . import astlib
from .patterns import A


context = threading.local()


@contextlib.contextmanager
def new_context(*, env, exit_on_error, file_hash, tmp_count):
    context.env = env
    context.exit_on_error = exit_on_error
    context.file_hash = file_hash
    context.tmp_count = tmp_count
    yield


def get(name):
    if name in A(astlib.Name):
        return context.env.get(str(name))