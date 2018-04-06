import threading
import contextlib
from collections import OrderedDict


context = threading.local()


@contextlib.contextmanager
def new_context(
        *, env, exit_on_error, module_paths, clibs_includes,
        i_count, structs_to_type_tag):
    context.env = env
    context.structs_to_type_tag = structs_to_type_tag
    context.exit_on_error = exit_on_error
    context.module_paths = module_paths
    context.clibs_includes = clibs_includes or OrderedDict()
    context.i_count = i_count
    context.func = None
    context.parent = None
    context.inlining = []
    yield


def modified_context_args():
    return {
        key: getattr(context, key)
        for key in (
            "env", "exit_on_error", "module_paths",
            "clibs_includes", "i_count", "structs_to_type_tag"
        )
    }
