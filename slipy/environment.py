from rpython.rlib import jit
from slipy.exceptions import SlipException


class Cell(object):
    def __init__(self, value):
        self._value = value

    def get_value(self):
        return self._value

    def set_value(self, value):
        self._value = value


class Env(object):
    # TODO: initialise bindings so it can be immutable too
    _immutable_fields_ = ["_previous"]

    def __init__(self, previous=None):
        self._previous = previous
        self._bindings = {}

    @jit.elidable
    def get_var(self, id):
        if id in self._bindings:
            return self._bindings[id]
        elif self._previous:
            return self._previous.get_var(id)
        else:
            # TODO: Better error msg
            raise SlipException("var `%s' not found" % id.to_string())

    def add_var(self, sym, val):
        # if sym in self._bindings:
        #     raise Exception("Can not overwrite binding")
        # c = Cell(val)
        # self._bindings[sym] = c
        if sym in self._bindings:
            c = self._bindings[sym]
            c.set_value(val)
        else:
            c = Cell(val)
            self._bindings[sym] = c

    def __str__(self):
        keys = " ".join(map(str, self._bindings.keys()))
        return "#<Env {%s} prev=%s>" % (keys, self._previous)