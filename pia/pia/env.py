from . import astlib, errors
from .utils import A


class Env:

    def _validate_key(self, key):
        if key in A(astlib.GenericType):
            return self._validate_key(key.base)
        return str(key)

    def __init__(self):
        self.scope = 0
        self.virtual_scope = 0
        self.space = {self.scope: {}}

    def add_virtual_scope(self):
        self.virtual_scope += 1
        self.space[self.virtual_scope] = {}

    def remove_virtual_scope(self):
        del self.space[self.virtual_scope]
        self.virtual_scope -= 1

    def add_scope(self):
        self.scope += 1
        self.virtual_scope += 1
        self.space[self.scope] = {}

    def remove_scope(self):
        del self.space[self.scope]
        self.scope -= 1
        self.virtual_scope -= 1

    def __neg__(self):
        del self.space[self.scope]
        self.scope -= 1
        self.virtual_scope -= 1

    def __pos__(self):
        self.scope += 1
        self.virtual_scope += 1
        self.space[self.scope] = {}

    def __setitem__(self, key, value):
        scope = (self.scope if self.scope >= self.virtual_scope
            else self.virtual_scope)
        key = self._validate_key(key)
        self.space[scope][key] = value

    def __contains__(self, key):
        return self[key] is not None

    def _get_with_scope(self, key):
        key = self._validate_key(key)
        scope = (
            self.scope if self.scope >= self.virtual_scope
            else self.virtual_scope)
        while scope >= 0:
            found = self.space[scope].get(key)
            if found:
                return found, scope
            scope -= 1
        return None, -1

    def __getitem__(self, key):
        return self._get_with_scope(key)[0]

    def __delitem__(self, key):
        scope = (
            self.scope if self.scope >= self.virtual_scope
            else self.virtual_scope)
        key = self._validate_key(key)
        while scope >= 0:
            if key in self.space[scope]:
                del self.space[scope][key]
                return
            scope -= 1

    def cspace(self):
        scope = (
            self.scope if self.scope >= self.virtual_scope
            else self.virtual_scope)
        return self.space[scope].items()

    def cspace_return(self):
        if self.virtual_scope == self.scope:
            return self.space[self.scope].items()
        result = {}
        for i in reversed(range(self.scope, self.virtual_scope + 1)):
            result = {**result, **self.space[i]}
        return result.items()
