import sys # For display
import time
from slipy.values import *


native_dict = {}


# TODO: fix with type checking etc
def declare_native(name, simple=True):
    def wrapper(func):
        def inner(args, env, cont):
            if simple:
                from slipy.interpreter import return_value_direct
                result = func(args)
                return return_value_direct(result, env, cont)
            else:
                return func(args, env, cont)

        sym = W_Symbol.from_string(name)
        native = W_NativeFunction(inner)
        native_dict[sym] = native
        return inner

    return wrapper


@declare_native("+")
def plus(args):
    accum = 0
    for arg in args:
        assert isinstance(arg, W_Number)
        accum += arg.value()
    return W_Number(accum)


@declare_native("-")
def minus(args):
    length = len(args)
    val = 0

    if length == 0:
        val = W_Number(0)
    elif length == 1:
        val = W_Number(-args[0].value())
    else:
        val = args[0].value()
        for arg in args[1:]:
            assert isinstance(arg, W_Number)
            val -= arg.value()

    return W_Number(val)


@declare_native("call/cc", simple=False)
def callcc(args, env, cont):
    assert isinstance(args[0], W_Callable)
    return args[0].call([W_Continuation(cont)], env, cont)


@declare_native("time")
def slip_time(args):
    assert len(args) == 0
    return W_Number(time.time())

@declare_native("display")
def display(args):
    sys.stdout.write(str(args[0]))
    return w_void

@declare_native("displayln")
def display(args):
    print str(args[0])
    return w_void