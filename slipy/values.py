from slipy.environment import Env
from slipy.exceptions import *
from slipy.continuation import *
from slipy.util import zip

# TODO: __str__ for all classes
# TODO: getters and setters?
# TODO: memoized constructors for nums etc?

_symbol_pool = {}


class W_SlipObject(object):
    def equal(self, obj):
        return self is obj

    def __str__(self):
        return "<W_SlipObject>"

    def to_string(self):
        return self.__str__()

    def to_display(self):
        return self.__str__()


class W_Pair(W_SlipObject):
    # TODO: set! operations
    # TODO: Fix for to_display

    def __init__(self, car, cdr):
        self._car = car
        self._cdr = cdr

    def car(self):
        return self._car

    def cdr(self):
        return self._cdr

    def _to_lstring(self):
        car = self._car.to_string()
        cdr = self._cdr
        if isinstance(cdr, W_Pair):
            return "%s %s" % (car, cdr._to_lstring())
        elif cdr is w_empty:
            return car
        else:
            return "%s . %s" % (car, cdr.to_string())

    def __str__(self):
        # TODO: fix if quote is first symbol
        return "(%s)" % self._to_lstring()


class W_Vector(W_SlipObject):
    _imutable_fields_ = ["length"]

    def __init__(self, values, length):
        self._values = values
        self.length = length

    @staticmethod
    def make(length, val):
        vals = [val] * length
        return W_Vector(vals, length)

    def ref(self, idx):
        if idx >= self.length:
            raise SlipException("index out of bounds")
        return self._values[idx]

    def set(self, idx, val):
        if idx >= self.length:
            raise SlipException("index out of bounds")
        self._values[idx] = val

    # TODO: Fix for to_display
    def __str__(self):
        vals = [None] * self.length
        for i, val in enumerate(self._values):
            vals[i] = val.to_string()
        vals = " ".join(vals)
        return "(vector %s)" % vals


class W_Null(W_SlipObject):
    def __str__(self):
        return "()"


# TODO: Specialized classes for different types of numbers (trick)
class W_Number(W_SlipObject):
    is_int = is_float = False

    def add(self, other):
        raise Exception("abstract method")

    def sub(self, other):
        raise Exception("abstract method")

    def mul(self, other):
        raise Exception("abstract method")

    def div(self, other):
        raise Exception("abstract method")

    def is_eq(self, other):
        raise Exception("abstract method")


class W_Integer(W_Number):
    _imutable_fields_ = ["_val"]

    is_int = True

    def __init__(self, value):
        self._val = int(value)

    def value(self):
        return self._val

    def __str__(self):
        return str(self._val)

    def add(self, other):
        if isinstance(other, W_Float):
            return W_Float(float(self._val) + other.value())
        else:
            return W_Integer(self._val + other.value())

    def sub(self, other):
        if isinstance(other, W_Float):
            return W_Float(float(self._val) - other.value())
        else:
            return W_Integer(self._val - other.value())

    def mul(self, other):
        if isinstance(other, W_Float):
            return W_Float(float(self._val) * other.value())
        else:
            return W_Integer(self._val * other.value())

    def div(self, other):
        if isinstance(other, W_Float):
            return W_Float(float(self._val) / other.value())
        else:
            return W_Integer(self._val / other.value())

    def is_eq(self, other):
        if isinstance(other, W_Integer):
            return self._val == other.value()
        else:
            return False


class W_Float(W_Number):
    _imutable_fields_ = ["_val"]

    is_float = True

    def __init__(self, value):
        self._val = float(value)

    def value(self):
        return self._val

    def __str__(self):
        return str(self._val)

    def add(self, other):
        if isinstance(other, W_Integer):
            return W_Float(self._val + float(other.value()))
        else:
            return W_Float(self._val + other.value())

    def sub(self, other):
        if isinstance(other, W_Integer):
            return W_Float(self._val - float(other.value()))
        else:
            return W_Float(self._val - other.value())

    def mul(self, other):
        if isinstance(other, W_Integer):
            return W_Float(self._val * float(other.value()))
        else:
            return W_Float(self._val * other.value())

    def div(self, other):
        if isinstance(other, W_Integer):
            return W_Float(self._val / float(other.value()))
        else:
            return W_Float(self._val / other.value())

    def is_eq(self, other):
        if isinstance(other, W_Float):
            return self._val == other.value()
        else:
            return False


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

    def __str__(self):
        return "\"%s\"" % self._str

    def to_display(self):
        return self._str


class W_Callable(W_SlipObject):
    def call(self, args, env, cont):
        raise Exception("abstract base class")


class W_NativeFunction(W_Callable):
    def __init__(self, func):
        self._func = func

    def call(self, args, env, cont):
        return self._func(args, env, cont)

    def __str__(self):
        return "#<native>"


class W_Closure(W_Callable):
    def __init__(self, args, env, body):
        self._args = args
        self._env = env
        self._body = body

    def call(self, args, env, cont):
        # TODO: stuff like len calls could be optimized maybe?
        if len(args) != len(self._args):
            raise SlipException("Incorrect length of argument list")
        new_env = Env(previous=self._env)
        for sym, val in zip(self._args, args):
            new_env.add_var(sym, val)
        return self._body, new_env, cont

    def __str__(self):
        return "#<closure>"


class W_Continuation(W_Callable):
    def __init__(self, cont):
        self._cont = cont

    def call(self, args, env, cont):
        from slipy.interpreter import return_value_direct
        # TODO: deal with too much args etc
        if args:
            return return_value_direct(args[0], env, self._cont)
        else:
            return return_value_direct(w_void, env, self._cont)

    def __str__(self):
        return "#<continuation>"


class W_Undefined(W_SlipObject):
    def __str__(self):
        return "#<undefined>"


class W_Void(W_SlipObject):
    def __str__(self):
        return "#<void>"


# Default values

w_empty = W_Null()
w_true = W_Boolean(True)
w_false = W_Boolean(False)
w_undefined = W_Undefined()
w_void = W_Void()


def is_true(val):
    return not(isinstance(val, W_Boolean) and val is w_false)


def from_list(obj):
    list = []
    current = obj
    while isinstance(current, W_Pair):
        list.append(current.car())
        current = current.cdr()
    if not current is w_empty:
        raise SlipException("Malformed list")
    return list