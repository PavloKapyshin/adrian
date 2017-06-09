import copy


class Includes:

    def __init__(self):
        self._includes = {}

    def __contains__(self, name):
        return name in self._includes

    def __getitem__(self, key):
        return self._includes[key]

    def __setitem__(self, key, value):
        self._includes[key] = value

    def get_cgen_ast(self):
        ast_ = []
        for key, value in self._includes.items():
            for key_, value_ in value.items():
                if key_ != "__INCLUDE__":
                    ast_.append(value_)
        return ast_


class Namespace:

    def __init__(self):
        self._scope = 0
        self._space = {self._scope: {}}

    def add(self, name, value):
        self._space[self._scope][name] = value

    def del_(self, name):
        del self._space[self._scope][name]

    def add_scope(self):
        self._scope += 1
        self._space[self._scope] = {}

    def del_scope(self):
        self._scope -= 1
        del self._space[self._scope]

    def exists(self, name):
        scope = self._scope
        while scope >= 0:
            if name in self._space[scope]:
                return True
            scope -= 1
        return False

    def exists_in_scope(self, name, scope):
        return name in self._space[scope]

    def exists_in_current_scope(self, name):
        return self.exists_in_scope(name, self._scope)

    def get_with_scope(self, name):
        scope = self._scope
        while scope >= 0:
            if self.exists_in_scope(name, scope):
                return self._space[scope][name], scope
            scope -= 1
        return None, 0

    def get(self, name):
        return get_with_scope(name)[0]

    def get_from_current_scope(self, name):
        if self.exists_in_current_scope(name):
            return self._space[self._scope][name]
        return None

    def update_in_current_scope(self, name, values):
        for key, val in values.items():
            self._space[self._scope][name][key] = val

    def update(self, name, values):
        _, scope = self.get_with_scope(name)
        for key, val in values.items():
            self._space[scope][name][key] = val

    @property
    def space(self):
        return self._space

    @property
    def scope(self):
        return self._scope


class Context:

    def __init__(self, exit_on_error):
        self.namespace = Namespace()
        self.typespace = Namespace()
        self.funcspace = Namespace()
        # TODO
        # self.namespace = Namespace()
        # self.typespace = Namespace()
        # self.funcspace = Namespace()
        # self.includes = Includes()
        # self.exit_on_error = exit_on_error
        # self.module_paths = module_paths
        # self.var_types = {}

    def copy(self):
        return copy.deepcopy(self)
