from . import astlib, errors
from .utils import A

class Env:

    def _validate_key(self, key):
        if key in A(astlib.ParamedType):
            return self._validate_key(key.type_)
        return str(key)

    def _check_found_info(self, info, request, nodetype_validator):
        if not info:
            errors.unknown_name(request)
        node_type = info["node_type"]
        if not nodetype_validator(node_type):
            errors.wrong_node_type(request, node_type)

    def _get_info_maker(self, node_type_checker):
        def helper(request):
            info = self[request]
            self._check_found_info(info, request, node_type_checker)
            return info
        return helper


    def _is_of_nodetype_maker(self, *node_types):
        def helper(node_type):
            return node_type in node_types
        return helper

    def __init__(self):
        self.scope = 0
        self.space = {self.scope: {}}
        self.is_var = self._is_of_nodetype_maker(astlib.NodeT.var)
        self.is_let = self._is_of_nodetype_maker(astlib.NodeT.let)
        self.is_fun = self._is_of_nodetype_maker(astlib.NodeT.fun)
        self.is_protocol = self._is_of_nodetype_maker(astlib.NodeT.protocol)
        self.is_adt = self._is_of_nodetype_maker(astlib.NodeT.adt)
        self.is_struct = self._is_of_nodetype_maker(astlib.NodeT.struct)
        self.is_variable = self._is_of_nodetype_maker(
            astlib.NodeT.let, astlib.NodeT.var, astlib.NodeT.arg)
        self.is_type = self._is_of_nodetype_maker(
            astlib.NodeT.struct, astlib.NodeT.commont,
            astlib.NodeT.adt, astlib.NodeT.protocol)
        self.is_real_type = self._is_of_nodetype_maker(
            astlib.NodeT.struct, astlib.NodeT.adt,
            astlib.NodeT.protocol)
        self.is_function = self._is_of_nodetype_maker(astlib.NodeT.fun)
        self.get_type_info = self._get_info_maker(self.is_type)
        self.get_adt_info = self._get_info_maker(self.is_adt)
        self.get_variable_info = self._get_info_maker(self.is_variable)
        self.get_function_info = self._get_info_maker(self.is_function)

    def raw_get_type_info(self, request):
        return self[request]

    def get_parent_info(self, expr):
        if expr in A(astlib.Name):
            return self.get_variable_info(expr)
        return self.get_parent_info(expr.parent)

    def get_node_type(self, request):
        info = self[request]
        if not info:
            errors.unknown_name(request)
        return info["node_type"]

    def get_method_and_parent_infos(self, parent, method_name):
        parent_info = self.get_type_info(parent)
        methods = parent_info["methods"]
        method_info = methods.get(method_name)
        if not method_info:
            errors.no_such_method(parent, method_name)
        return method_info, parent_info

    def get_method_info(self, parent, method_name):
        return self.get_method_and_parent_infos(parent, method_name)[0]

    def get_field_info(self, parent, field_name):
        if parent in A(astlib.Name):
            parent_info = self.get_parent_info(parent)
        else:
            parent_info = self.get_field_info(parent.parent, parent.member)
        parent_type = parent_info["type_"]
        parent_type_info = self.get_type_info(parent_type)
        fields = parent_type_info["fields"]
        field_info = fields.get(field_name)
        if not field_info:
            errors.no_such_field(parent, parent_type, field_name)
        return field_info

    def __neg__(self):
        del self.space[self.scope]
        self.scope -= 1

    def __pos__(self):
        self.scope += 1
        self.space[self.scope] = {}

    def __setitem__(self, key, value):
        key = self._validate_key(key)
        self.space[self.scope][key] = value

    def __contains__(self, key):
        return self[key] is not None

    def _get_with_scope(self, key):
        key = self._validate_key(key)
        scope = self.scope
        while scope >= 0:
            found = self.space[scope].get(key)
            if found:
                return found, scope
            scope -= 1
        return None, -1

    def __getitem__(self, key):
        return self._get_with_scope(key)[0]

    def update(self, key, value):
        key = self._validate_key(key)
        entry, scope = self._get_with_scope(key)
        for k, v in value.items():
            entry[k] = v
        self.space[scope][key] = entry

    def __delitem__(self, key):
        del self.space[self.scope][key]

    def cspace(self):
        return self.space[self.scope].items()
