def mangle_name(name, *, file_hash):
    return "_".join(["adrian", file_hash, name])
