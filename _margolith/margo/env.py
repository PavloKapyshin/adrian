class Env:

    def __init__(self):
        self._scope = 0
        self._space = {self._scope: {}}

    def __setitem__(self, key, val):
        self.add(key, val)

    def __getitem__(self, key):
        return self.get(key)

    def __contains__(self, key):
        return self.exists(key)

    def add(self, name, entry):
        self._space[self._scope][name] = entry

    def del_(self, name):
        del self._space[self._scope][name]

    def add_scope(self):
        self._scope += 1
        self._space[self._scope] = {}

    def del_scope(self):
        del self._space[self._scope]
        self._scope -= 1

    def exists(self, name):
        scope = self._scope
        while scope >= 0:
            if name in self._space[scope]:
                return True
            scope -= 1
        return False

    def exists_in_scope(self, name, scope):
        return name in self._space[scope]

    def get_with_scope(self, name):
        scope = self._scope
        while scope >= 0:
            if self.exists_in_scope(name, scope):
                return self._space[scope][name], scope
            scope -= 1
        return None, 0

    def get(self, name):
        return self.get_with_scope(name)[0]

    def space(self):
        return self._space

    @property
    def current_space(self):
        return self._space[self._scope]

    @property
    def scope(self):
        return self._scope
