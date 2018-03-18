from .context import context


def make(cc, lib_path, out_file):
    files = []
    for k, v in context.clibs_includes.items():
        if "source" in v:
            files.append(v["source"])
    return "{cc} -o {out} {lib_paths} {files} {src_file}".format_map({
        "cc": cc, "out": "OUT_FILE",
        "lib_paths": " ".join(["-I" + lib for lib in lib_path]),
        "files": " ".join(files),
        "src_file": out_file.name})
