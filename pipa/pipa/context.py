import threading
import contextlib


context = threading.local()


@contextlib.contextmanager
def new_context(*, env, exit_on_error):
    context.env = env
    context.exit_on_error = exit_on_error
    yield


def modified_context_args():
    return {
        key: getattr(context, key)
        for key in ("env", "exit_on_error")
    }
