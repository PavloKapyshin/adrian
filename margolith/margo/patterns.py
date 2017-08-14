class A:

    def __init__(self, *types):
        self.types = types

    def __contains__(self, other):
        return isinstance(other, self.types)
