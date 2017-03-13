from . import defs


def name_exists(name, *, context):
    """Check for existence of a name."""
    return context.namespace.exists(name)


def type_exists(type_, *, context):
    """Check for existence of a type."""
    return type_ in defs.STANDARD_TYPE_NAMES or context.typespace.exists(type_)


def func_exists(func, *, context):
    """Check for existence of a function."""
    return func in defs.STANDARD_FUNC_NAMES or context.funcspace.exists(func)

