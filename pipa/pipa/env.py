class Env:

    def __init__(self):
        self.scope = 0
        self.space = {self.scope: {}}

    def add_scope(self):
        self.scope += 1
        self.space[self.scope] = {}

    def remove_scope(self):
        del self.space[self.scope]
        self.scope -= 1

    def __setitem__(self, key, value):
        self.space[self.scope][key] = value

    def __contains__(self, key):
        return self[key] is not None

    def __getitem__(self, key):
        return self.get_with_scope(key)[0]

    def __delitem__(self, key):
        scope = self.scope
        while scope >= 0:
            if key in self.space[scope]:
                del self.space[scope][key]
                return
            scope -= 1

    def get_with_scope(self, key):
        scope = self.scope
        while scope >= 0:
            found = self.space[scope].get(key)
            if found:
                return found, scope
            scope -= 1
        return None, -1
