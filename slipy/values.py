from rpython.rlib import jit
from slipy.exceptions import *
from slipy.continuation import *

# TODO: memoized constructors for numbers?

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
    # TODO: implement to_display

    def __init__(self, car, cdr):
        self._car = car
        self._cdr = cdr

    def car(self):
        return self._car

    def cdr(self):
        return self._cdr

    def set_car(self, val):
        self._car = val

    def set_cdr(self, val):
        self._cdr = val

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
    _immutable_fields_ = ["len"]

    def __init__(self, values, length):
        self._values = values
        self.len = length

    @staticmethod
    @jit.unroll_safe
    def make(length, val):
        vals = [val] * length
        return W_Vector(vals, length)

    def ref(self, idx):
        if idx >= self.len:
            raise SlipException("index out of bounds")
        return self._values[idx]

    def set(self, idx, val):
        if idx >= self.len:
            raise SlipException("index out of bounds")
        self._values[idx] = val

    def length(self):
        return self.len

    def __str__(self):
        vals = [None] * self.len
        for i, val in enumerate(self._values):
            vals[i] = val.to_string()
        vals = " ".join(vals)
        return "(vector %s)" % vals


class W_Null(W_SlipObject):
    def __str__(self):
        return "()"


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

    def lt(self, other):
        raise Exception("abstract method")

    def gt(self, other):
        raise Exception("abstract method")

    def le(self, other):
        raise Exception("abstract method")

    def ge(self, other):
        raise Exception("abstract method")


class W_Integer(W_Number):
    _immutable_fields_ = ["_val"]

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
            return other.is_eq(self)

    def lt(self, other):
        if isinstance(other, W_Integer):
            return self._val < other.value()
        else:
            return other.gt(self)

    def gt(self, other):
        if isinstance(other, W_Integer):
            return self._val > other.value()
        else:
            return other.lt(self)

    def le(self, other):
        if isinstance(other, W_Integer):
            return self._val <= other.value()
        else:
            return other.ge(self)

    def ge(self, other):
        if isinstance(other, W_Integer):
            return self._val >= other.value()
        else:
            return other.le(self)


class W_Float(W_Number):
    _immutable_fields_ = ["_val"]

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
            return self._val == float(other.value())

    def lt(self, other):
        if isinstance(other, W_Float):
            return self._val < other.value()
        else:
            return self._val < float(other.value())

    def gt(self, other):
        if isinstance(other, W_Float):
            return self._val > other.value()
        else:
            return self._val > float(other.value())

    def le(self, other):
        if isinstance(other, W_Float):
            return self._val <= other.value()
        else:
            return self._val <= float(other.value())

    def ge(self, other):
        if isinstance(other, W_Float):
            return self._val >= other.value()
        else:
            return self._val >= float(other.value())


class W_Boolean(W_SlipObject):
    _immutable_fields_ = ["_value"]

    def __init__(self, value):
        self._value = value

    @staticmethod
    def from_value(value):
        return w_true if value else w_false

    def __str__(self):
        return "#t" if self._value else "#f"


class W_Symbol(W_SlipObject):
    _immutable_fields_ = ["_str"]

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
    _immutable_fields_ = ["func"]

    def __init__(self, func):
        self._func = func

    def call(self, args, env, cont):
        return self._func(args, env, cont)

    def __str__(self):
        return "#<native>"


class W_Closure(W_Callable):
    _immutable_fields_ = ["args[*]", "vars[*]", "env", "body[*]"]

    def __init__(self, args, vars, env, body):
        from slipy.AST import Sequence
        self.args = args
        self.env = env
        self.body = Sequence(body)
        self.vars = vars

    @jit.unroll_safe
    def call(self, args, env, cont):
        from slipy.environment import Env
        if len(args) != len(self.args):
            raise SlipException("Incorrect length of argument list")
        new_env = Env(len(args)+len(self.vars), previous=self.env)

        # for sym, val in zip(self._args, args):
        #     new_env.add_var(sym, val)

        for i, val in enumerate(args):
            new_env.set_var(new_env.scope, i, val)

        return self.body, new_env, cont

    def __str__(self):
        return "#<closure>"


class W_Continuation(W_Callable):
    _immutable_fields_ = ["_cont"]

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


@jit.unroll_safe
def list_from_values(vals):
    if len(vals) == 0:
        return w_empty
    else:
        cur = w_empty
        for i in range(len(vals)-1, -1, -1):
            cur = W_Pair(vals[i], cur)
        return cur


def values_from_list(pair):
    result = []
    curr = pair
    while isinstance(curr, W_Pair):
        result.append(curr.car())
        curr = curr.cdr()
    if curr is w_empty:
        return result
    else:
        raise SlipException("Improper list")