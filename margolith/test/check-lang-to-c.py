#!/usr/bin/env python3

"""Script that helps test C code generation.

It takes path to compiler, and paths to two dirs: 1) code in some
language that compiler is able to translate into C; 2) code in C to
check against translation results. Every file in (1) has counterpart
in (2), these are matched by numeric prefix before dash. For example,
2-cintfast_types.adr is matched with the 2-cintfast_types.c.

Every file from (2) is compiled (by default with clang) and checked
for memory leaks (with valgrind).

"""

import os
import sys
import shlex
import random
import tempfile
import unittest
import argparse
import subprocess


NATURAL_ORDER = "natural"
RANDOM_ORDER = "random"

VALGRIND_ERROR_EXIT_CODE = 5
DEFAULT_VALGRIND_CMD = (  # without leak-check the error-exitcode doesn't work
    "valgrind --leak-check=full --error-exitcode={:d} {{bin}}".format(
        VALGRIND_ERROR_EXIT_CODE))

TEST_DATA_MEMBER_NAME = "_check_lang_to_c_test_data"

GLOBAL_COMMON_KWARGS = {
    "shell": True,
    "check": False,
    "stdout": subprocess.DEVNULL,
    "stderr": subprocess.DEVNULL}


def make_c_code_valgrind_test(data):
    """Make test that compiles & valgrinds expected C code."""

    def _test(self):
        with tempfile.TemporaryDirectory() as tmp_dir:
            with tempfile.NamedTemporaryFile(dir=tmp_dir) as tmp_binary_file:
                tmp_binary = tmp_binary_file.name

                c_path = data["specific"]["c_path"]
                common_kwargs = {"cwd": tmp_dir}

                binary_cmd = data["tmpls"]["binary"].format(
                    dest=shlex.quote(tmp_binary), src=shlex.quote(c_path))
                proc = subprocess.run(
                    binary_cmd, **GLOBAL_COMMON_KWARGS, **common_kwargs)
                self.assertEqual(
                    proc.returncode, 0,
                    msg="compilation of {} failed with {:d}".format(
                        c_path, proc.returncode))

                valgrind_cmd = data["tmpls"]["valgrind"].format(
                    bin=shlex.quote(tmp_binary))
                proc = subprocess.run(
                    valgrind_cmd, **GLOBAL_COMMON_KWARGS, **common_kwargs)
                self.longMessage = False  # to avoid "5 == 5" noize in output
                self.assertNotEqual(
                    proc.returncode, VALGRIND_ERROR_EXIT_CODE,
                    msg="valgrind of {} failed".format(c_path))

    return _test


def make_expected_c_code_test(data):
    """Make test that translates into C code and compares it to expected."""

    def _test(self):
        with tempfile.NamedTemporaryFile() as tmp_source_file:
            tmp_source = tmp_source_file.name

            c_path = data["specific"]["c_path"]
            lang_path = data["specific"]["lang_path"]

            source_cmd = data["tmpls"]["source"].format(
                compiler=shlex.quote(data["compiler"]),
                dest=shlex.quote(tmp_source),
                src=shlex.quote(lang_path))
            proc = subprocess.run(source_cmd, **GLOBAL_COMMON_KWARGS)
            self.assertEqual(proc.returncode, 0)

            with open(tmp_source, "r", encoding="utf-8") as file:
                c_code = file.read()
            expected_c_code = data["specific"]["expected_c_code"]
            self.assertEqual(
                c_code, expected_c_code,
                msg="compilation of {} into {} failed".format(
                    lang_path, c_path))

    return _test


class CaseMeta(type):
    """Metaclass that builds test case with tests made by TEST_FACTORIES."""

    TEST_FACTORIES = (
        (make_c_code_valgrind_test, "test_no_valgrind_warnings_for_c_code"),
        (make_expected_c_code_test, "test_compiler_outputs_expected_c_code"))

    def __new__(cls, name, bases, attrs):
        data = attrs.pop(TEST_DATA_MEMBER_NAME)
        attrs.update({
            attr_name: factory(data)
            for factory, attr_name in cls.TEST_FACTORIES})
        attrs["maxDiff"] = None
        return super().__new__(cls, name, bases, attrs)


def make_parser():
    """Build command-line parser."""
    parser = argparse.ArgumentParser(
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
        description=__doc__)
    parser.add_argument("compiler")
    parser.add_argument("lang_source_dir")
    parser.add_argument("c_source_dir")
    parser.add_argument(
        "--source", default="{compiler} -o {dest} {src}",
        help="how to generate C source (from lang source)")
    parser.add_argument(
        "--binary", default="clang -o {dest} {src}",
        help="how to generate binary (from C source)")
    parser.add_argument(
        "--valgrind", default=DEFAULT_VALGRIND_CMD,
        help="how to run valgrind on binary")
    parser.add_argument(
        "--order", default=NATURAL_ORDER, choices=(
            NATURAL_ORDER, RANDOM_ORDER),
        help="order in which to run test cases")
    return parser


def get_test_data(lang_source_dir, c_source_dir):
    """Collect test data from lang and C source dirs."""
    def _get_number(filename):
        return int(filename.split("-", 1)[0])

    test_data = {}

    for name in os.listdir(lang_source_dir):
        number = _get_number(name)
        assert number not in test_data
        test_data[number] = {
            "lang_path": os.path.join(lang_source_dir, name)}

    for name in os.listdir(c_source_dir):
        number = _get_number(name)
        path = os.path.join(c_source_dir, name)

        data = test_data[number]
        data["c_path"] = path
        with open(path, "r", encoding="utf-8") as file:
            data["expected_c_code"] = file.read()

    return test_data


def get_cases():
    """Generate test cases according to command-line arguments."""
    parser = make_parser()
    args = parser.parse_args()

    test_data = get_test_data(
        lang_source_dir=os.path.abspath(args.lang_source_dir),
        c_source_dir=os.path.abspath(args.c_source_dir))

    # Order test case numbers.
    numbers = list(test_data)
    if args.order == RANDOM_ORDER:
        random.shuffle(numbers)
    else:
        assert args.order == NATURAL_ORDER
        numbers = sorted(numbers)

    # Build test cases.
    tmpls = {
        "source": args.source, "binary": args.binary,
        "valgrind": args.valgrind}
    compiler = os.path.abspath(args.compiler)
    for number in numbers:
        attrs = {
            TEST_DATA_MEMBER_NAME: {
                "specific": test_data[number], "tmpls": tmpls,
                "compiler": compiler}}
        yield CaseMeta(
            "Test{:d}".format(number), (unittest.TestCase, ), attrs)


def load_tests(loader, standard_tests, pattern):
    """Build test suite for unittest."""
    suite = unittest.TestSuite()
    for case in get_cases():
        suite.addTests(loader.loadTestsFromTestCase(case))
    return suite


if __name__ == "__main__":
    # Here unittest gets just the minimum of arguments it needs.
    # This way custom arguments for main are parsed correctly,
    # and unittest.main does not complain about them.
    unittest.main(argv=sys.argv[:1], catchbreak=True)
