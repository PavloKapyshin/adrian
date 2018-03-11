class Env:

    def __init__(self):
        self.scope = 0
        self.space = {self.scope: {}}

    def __neg__(self):
        del self.space[self.scope]
        self.scope -= 1

    def __pos__(self):
        self.scope += 1
        self.space[self.scope] = {}

    def __setitem__(self, key, value):
        self.space[self.scope][key] = value

    def __contains__(self, key):
        return self[key]

    def __getitem__(self, key):
        scope = self.scope
        while scope >= 0:
            if key in self.space[scope]:
                return self.space[scope][name]
            scope -= 1
        return None
