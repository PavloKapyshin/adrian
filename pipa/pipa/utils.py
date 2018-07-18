import hashlib

class A:

    def __init__(self, *types):
        self.types = types

    def __contains__(self, other):
        return isinstance(other, self.types)


def get_hash(data):
    hash_ = hashlib.new("md5")
    hash_.update(bytes(data, "utf-8"))
    return hash_.hexdigest()
