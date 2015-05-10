from slipy.environment import Env
from slipy.exceptions import *
from slipy.continuation import *

# TODO: __str__ for all classes
# TODO: getters and setters

_symbol_pool = {}


class W_SlipObject(object):
    def equal(self, obj):
        return self is obj

    def __str__(self):
        return "<W_SlipObject>"


class W_Pair(W_SlipObject):
    # TODO: set! operations

    def __init__(self, car, cdr):
        self._car = car
        self._cdr = cdr

    def car(self):
        return self._car

    def cdr(self):
        return self._cdr


class W_Vector(W_SlipObject):
    pass


class W_Null(W_SlipObject):
    pass


class W_Number(W_SlipObject):
    def __init__(self, value):
        self._val = value

    def value(self):
        return self._val

    def __str__(self):
        return str(self._val)

class W_Boolean(W_SlipObject):
    def __init__(self, value):
        self._value = value

    @staticmethod
    def from_value(value):
        return w_true if value else w_false

    def __str__(self):
        return "#t" if self._value else "#f"


class W_Symbol(W_SlipObject):
    def __init__(self, str):
        self._str = str

    @staticmethod
    def from_string(str):
        if str in _symbol_pool:
            return _symbol_pool[str]
        else:
            sym = W_Symbol(str)
            _symbol_pool[str] = sym
            return sym

    def __str__(self):
        return self._str


class W_String(W_SlipObject):
    def __init__(self, str):
        self._str = str


class W_Callable(W_SlipObject):
    def call(self, args, env, cont):
        raise Exception("abstract base class")


class W_NativeFunction(W_Callable):
    def __init__(self, func):
        self._func = func

    def call(self, args, env, cont):
        from slipy.interpreter import return_value_direct
        val = self._func(args)
        return return_value_direct(val, env, cont)


class W_Closure(W_Callable):
    def __init__(self, args, env, body):
        self._args = args
        self._env = env
        self._body = body

    def call(self, args, env, cont):
        # TODO: stuff like len calls could be optimized maybe?
        if len(args) != len(self._args):
            raise SlipException("Incorrect length of argument list")
        new_env = Env(previous=env)
        for sym, val in zip(self._args, args):
            new_env.add_var(sym, val)
        return self._body, new_env, RestoreEnvContinuation(cont, env)


class W_Undefined(W_SlipObject):
    def __str__(self):
        return "#<undefined>"


# Default values

w_empty = W_Null()
w_true = W_Boolean(True)
w_false = W_Boolean(False)
w_undefined = W_Undefined()


def is_true(val):
    return not(isinstance(val, W_Boolean) and val is w_false)