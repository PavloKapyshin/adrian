#!/usr/bin/env python3

import os
import sys
import pathlib
import unittest

import padr


CURRENT_DIR = "test"
INPUT_TEST_DIR = "adrian_tests"
OUTPUT_TEST_DIR = "c_tests"


def cfile(file_name):
    return os.path.splitext(file_name)[0] + ".c"


def read_file(file_name):
    with open(file_name, "r", encoding="utf-8") as f:
        contents = f.read()
    return contents


class TestAll(unittest.TestCase):
    maxDiff = None
    input_dir = pathlib.Path(
        os.path.join(CURRENT_DIR, INPUT_TEST_DIR))

    def test_main(self):
        for test in self.input_dir.iterdir():
            path_to_adrian_input = pathlib.Path(test)
            path_to_c_output = pathlib.Path(
                os.path.join(CURRENT_DIR, OUTPUT_TEST_DIR, cfile(test.name)))
            if not path_to_c_output.exists():
                print("Doesn't exist: {}".format(str(path_to_c_output)))
            else:
                got = padr.compile_from_file(str(path_to_adrian_input))
                expected = read_file(str(path_to_c_output))
                with self.subTest(test=str(test)):
                    self.assertEqual(got, expected)


if __name__ == "__main__":
    unittest.main()
