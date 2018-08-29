#!/usr/bin/env python3

import pathlib
import unittest
import subprocess


CURRENT_DIR = "tests"
INPUT_DIR = "adrian"
OUTPUT_DIR = "expected"


def read_file(file_name):
    with open(file_name, "r") as f:
        contents = f.read()
    return contents


class TestAll(unittest.TestCase):
    maxDiff = None
    input_dir = pathlib.Path(CURRENT_DIR).joinpath(INPUT_DIR)

    def test_main(self):
        for test in self.input_dir.iterdir():
            path_to_adrian_input = pathlib.Path(test)
            path_to_expected = pathlib.Path(
                CURRENT_DIR).joinpath(OUTPUT_DIR).joinpath(
                test.name.split(".")[0])
            proc = subprocess.run(
                ["./interpreter", path_to_adrian_input], stdout=subprocess.PIPE)
            output = proc.stdout.decode("utf-8")
            expected = read_file(str(path_to_expected))
            with self.subTest(test=str(test)):
                self.assertEqual(output, expected)


if __name__ == "__main__":
    unittest.main()
