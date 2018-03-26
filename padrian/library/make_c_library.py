HEADER_PATTERN_FILE_NAME = "adrian_c_header"
SOURCE_PATTERN_FILE_NAME = "adrian_c_source"
LIBRARY_FILE_NAME = "adrian_c"

PATTERN_FILE_EXTENSION = "pattern"
HEADER_FILE_EXTENSION = "h"
SOURCE_FILE_EXTENSION = "c"

HEADER_FORMAT_STRING = ("#ifndef {library_tag}\n"
                        "#define {library_tag}\n\n"
                        "{body}\n"
                        "#endif")
SOURCE_FORMAT_STRING = ("{includes}\n\n"
                        "{body}")

LIBRARY_TAG = "ADRIAN_C_DEFINED"
SOURCE_INCLUDES = "\n".join([
    f"#include <{module_name}.h>"
    for module_name in ("stdint", "stdlib", LIBRARY_FILE_NAME)
])

C_TYPE_LIST = (
    ("IntFast8", "int_fast8_t"),
    ("IntFast16", "int_fast16_t"),
    ("IntFast32", "int_fast32_t"),
    ("IntFast64", "int_fast64_t"),
    ("UIntFast8", "uint_fast8_t"),
    ("UIntFast16", "uint_fast16_t"),
    ("UIntFast32", "uint_fast32_t"),
    ("UIntFast64", "uint_fast64_t"),
)


def read_file(file_name):
    with open(file_name, "r") as file:
        contents = file.read()
    return contents


def write_file(file_name, contents):
    with open(file_name, "w") as file:
        file.write(contents)


def join_file_name(name, extension):
    return ".".join([name, extension])


def make_body_from_pattern(pattern):
    return "\n".join([
        pattern.format_map({
            "type": type, "ctype": ctype
        })
        for type, ctype in C_TYPE_LIST
    ])


def merge_header_file(header_pattern):
    return HEADER_FORMAT_STRING.format_map({
        "library_tag": LIBRARY_TAG,
        "body": make_body_from_pattern(header_pattern)
    })


def merge_source_file(source_pattern):
    return SOURCE_FORMAT_STRING.format_map({
        "includes": SOURCE_INCLUDES,
        "body": "\n".join([
            source_pattern.format_map({
                "type": type, "ctype": ctype, "type_tag_number": i,
            })
            for i, (type, ctype) in enumerate(C_TYPE_LIST)
        ])
    })


def main():
    source_pattern = read_file(
        join_file_name(SOURCE_PATTERN_FILE_NAME, PATTERN_FILE_EXTENSION))
    header_pattern = read_file(
        join_file_name(HEADER_PATTERN_FILE_NAME, PATTERN_FILE_EXTENSION))
    source_file_contents = merge_source_file(source_pattern)
    header_file_contents = merge_header_file(header_pattern)
    write_file(
        join_file_name(LIBRARY_FILE_NAME, SOURCE_FILE_EXTENSION),
        source_file_contents)
    write_file(
        join_file_name(LIBRARY_FILE_NAME, HEADER_FILE_EXTENSION),
        header_file_contents)


if __name__ == "__main__":
    main()
