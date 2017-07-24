import argparse

import margo


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("in_file")
    args = parser.parse_args()
    print(
        margo.compile_from_file(args.in_file, ""),
        end="")

if __name__ == "__main__":
    main()