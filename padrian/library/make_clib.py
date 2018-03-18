
SOURCE_FILE_NAME = "adrian_c_source"
HEADER_FILE_NAME = "adrian_c_header"
EXTENSION = "pattern"
LIB_TAG_DEFINED = "ADRIAN_C_DEFINED"
SOURCE_PREPEND = "\n".join([
    "#include {}".format(lname)
    for lname in ("<stdint.h>", "<stdlib.h>", "<adrian_c.h>")
])
HEADER_PREPEND = "\n".join([
    "#ifndef {}".format(LIB_TAG_DEFINED),
    "#define {}".format(LIB_TAG_DEFINED),
])
HEADER_APPEND = "#endif\n"
TYPE_LIST = (
    ("IntFast8", "int_fast8_t"),
    ("IntFast16", "int_fast16_t"),
    ("IntFast32", "int_fast32_t"),
    ("IntFast64", "int_fast64_t"),
    ("UIntFast8", "uint_fast8_t"),
    ("UIntFast16", "uint_fast16_t"),
    ("UIntFast32", "uint_fast32_t"),
    ("UIntFast64", "uint_fast64_t"),
)

SOURCE_RESULT_FILE = "adrian_c.c"
HEADER_RESULT_FILE = "adrian_c.h"


def read_file(file_name):
    with open(file_name, "r") as file:
        contents = file.read()
    return contents


def write_file(file_name, contents):
    with open(file_name, "w") as file:
        file.write(contents)


def main():
    pattern = read_file(".".join([SOURCE_FILE_NAME, EXTENSION]))
    source_code_body = "\n".join([
        pattern.format_map({
            "type": type, "ctype": ctype
        })
        for type, ctype in TYPE_LIST
    ])
    pattern = read_file(".".join([HEADER_FILE_NAME, EXTENSION]))
    header_code_body = "\n".join([
        pattern.format_map({
            "type": type, "ctype": ctype
        })
        for type, ctype in TYPE_LIST
    ])

    source_code = "\n".join([
        SOURCE_PREPEND,
        source_code_body
    ])
    header_code = "\n".join([
        HEADER_PREPEND,
        header_code_body,
        HEADER_APPEND
    ])

    write_file(SOURCE_RESULT_FILE, source_code)
    write_file(HEADER_RESULT_FILE, header_code)


if __name__ == "__main__":
    main()
