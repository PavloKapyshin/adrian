import threading
import contextlib


context = threading.local()


@contextlib.contextmanager
def new_context(*, env, exit_on_error, main_file_hash, module_paths, loaded_modules=None):
    context.env = env
    context.exit_on_error = exit_on_error
    context.main_file_hash = main_file_hash
    context.module_paths = module_paths
    context.loaded = []
    context.loaded_modules = loaded_modules or {}
    context.parent_struct = None
    context.tmp_counter = 0
    yield


def modified_context_args():
    return {
        key: getattr(context, key)
        for key in (
            "env", "exit_on_error", "main_file_hash", "module_paths",
            "loaded_modules")
    }
