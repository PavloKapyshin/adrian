from . import astlib
from .utils import A

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
        key = self.validate_key(key)
        self.space[self.scope][key] = value

    def __contains__(self, key):
        return self[key] is not None

    def _get_with_scope(self, key):
        key = self.validate_key(key)
        scope = self.scope
        while scope >= 0:
            found = self.space[scope].get(key)
            if found:
                return found, scope
            scope -= 1
        return None, 0

    def __getitem__(self, key):
        return self._get_with_scope(key)[0]

    def update(self, key, value):
        key = self.validate_key(key)
        entry, scope = self._get_with_scope(key)
        for k, v in value.items():
            entry[k] = v
        self.space[scope][key] = entry

    def __delitem__(self, key):
        del self.space[self.scope][key]

    def cspace(self):
        return self.space[self.scope].items()

    def validate_key(self, key):
        if key in A(astlib.ParamedType):
            return self.validate_key(key.type_)
        return str(key)
