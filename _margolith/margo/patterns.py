class A:

    def __init__(self, *types):
        self.types = ()
        for type_ in types:
            if isinstance(type_, (list, tuple)):
                self.types = self.types + tuple(type_)
            else:
                self.types = self.types + (type_, )

    def __contains__(self, other):
        return isinstance(other, self.types)
