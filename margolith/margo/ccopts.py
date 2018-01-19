from .context import context


def make(cc, lib_path, out_file):
    return "{cc} -o {out} {lib_paths} {files} {src_file}".format_map({
        "cc": cc, "out": "OUT_FILE", "lib_paths": " ".join(["-I" + lib for lib in lib_path]),
        "files": " ".join(context.clibs_inc), "src_file": out_file.name})
