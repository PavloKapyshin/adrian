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

    def _get_with_scope(self, key):
        key = str(key)
        scope = self.scope
        while scope >= 0:
            if key in self.space[scope]:
                return self.space[scope][key], scope
            scope -= 1
        return None, 0

    def __getitem__(self, key):
        return self._get_with_scope(key)[0]

    def update(self, key, value):
        key = str(key)
        entry, scope = self._get_with_scope(key)
        for k, v in value.items():
            entry[k] = v
        self.space[scope][key] = entry

    def __delitem__(self, key):
        del self.space[self.scope][key]

    def cspace(self):
        return self.space[self.scope].items()
