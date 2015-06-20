from rpython.rlib import jit
from slipy.exceptions import SlipException
from slipy.values import w_undefined


class Cell(object):
    def __init__(self, value):
        self._value = value

    def get_value(self):
        return self._value

    def set_value(self, value):
        self._value = value


class Env(object):
    _immutable_fields_ = ["previous", "bindings[*]", "structure[*]", "scope"]

    @jit.unroll_safe
    def __init__(self, size, previous=None):
        self.previous = previous
        self.bindings = [None] * size
        for i in range(0, size):
            self.bindings[i] = Cell(w_undefined)
        if previous:
            self.structure = previous.structure + [self]
        else:
            self.structure = [self]
        self.scope = len(self.structure) - 1

    # @jit.elidable
    def get_var(self, scope, offset):
        env = self.structure[scope]
        cell = env.bindings[offset]
        return cell

    def set_var(self, scope, offset, val):
        cell = self.structure[scope].bindings[offset]
        cell.set_value(val)
        return val

    def __str__(self):
        return "#<env>"


class _GlobalEnv(object):
    def __init__(self):
        self.global_env = None
_global_env = _GlobalEnv()


def get_global_env():
    return _global_env.global_env


def set_global_env(env):
    _global_env.global_env = env