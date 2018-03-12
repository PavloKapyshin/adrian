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
        key = str(key)
        self.space[self.scope][key] = value

    def __contains__(self, key):
        return self[key] is not None

    def __getitem__(self, key):
        key = str(key)
        scope = self.scope
        while scope >= 0:
            if key in self.space[scope]:
                return self.space[scope][key]
            scope -= 1
        return None
