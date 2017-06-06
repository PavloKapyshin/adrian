import unittest
import collections


def arc(namespace_projection, _identifier=None):
    if _identifier:
        print(_identifier, "@" * 40)

    regions = {}

    for key, values in sorted(namespace_projection.items()):
        reg = regions.get(key)
        if not reg:
            regions[key] = {key}
        if len(values) == 1:  # x = y
            alias = values[0]
            regions[key].add(alias)
            for k in sorted(regions[key]):
                regions[k] = regions[key]
        # Any value from values will eventually come as key,
        # so there is no need to handle values with len > 1 or < 1.

    unique_regions = {}
    for region in sorted(regions.values()):
        key = "@region@".join(region)
        unique_regions[key] = region

    def _get_var(region):
        for var in sorted(region):
            return var
    yield from map(_get_var, sorted(unique_regions.values()))


class ARCTest(unittest.TestCase):

    def check(self, namespace_projection, expected, _identifier):
        self.assertEqual(set(arc(namespace_projection)), expected)

    def test_one(self):
        # a = 1
        namespace_projection = {"a": ["tmp1"], "tmp1": []}
        self.check(namespace_projection, {"a"}, "one")

    def test_two(self):
        # a = 2 + 2
        namespace_projection = {"a": ["tmp3"], "tmp3": ["tmp1", "tmp2"], "tmp1": [], "tmp2": []}
        self.check(namespace_projection, {"a", "tmp1", "tmp2"}, "two")

    def test_three(self):
        # a = 1
        # s = a
        namespace_projection = {"a": ["tmp1"], "tmp1": [], "s": ["tmp2"], "tmp2": ["a"]}
        self.check(namespace_projection, {"a"}, "three")

    def test_four(self):
        # a = 1
        # s = a + 2
        namespace_projection = {
            "a": ["tmp1"], "tmp1": [], "tmp2": ["a"],
            "tmp3": [], "tmp4": ["tmp2", "tmp3"], "s": ["tmp4"]
        }
        self.check(namespace_projection, {"a", "s", "tmp3"}, "four")


if __name__ == "__main__":
    unittest.main()
