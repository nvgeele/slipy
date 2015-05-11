import sys # For display
import time
from slipy.values import *


native_dict = {}


# TODO: replace asserts with exceptions?
def declare_native(name, simple=True, arguments=False):
    def wrapper(func):
        def inner(args, env, cont):
            if isinstance(arguments, list):
                # TODO: Possibly optimizable
                assert len(args) == len(args)
                for arg, type in zip(args, arguments):
                    assert isinstance(arg, type)
            elif arguments:
                for arg in args:
                    assert isinstance(arg, arguments)

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


@declare_native("+", arguments=W_Number)
def plus(args):
    accum = 0
    for arg in args:
        accum += arg.value()
    return W_Number(accum)


@declare_native("-", arguments=W_Number)
def minus(args):
    accum = 0
    for arg in args:
        accum -= arg.value()
    return W_Number(accum)


@declare_native("call/cc", simple=False, arguments=[W_Callable])
def callcc(args, env, cont):
    return args[0].call([W_Continuation(cont)], env, cont)


@declare_native("time")
def slip_time(args):
    assert len(args) == 0
    return W_Number(time.time())

@declare_native("display")
def display(args):
    assert len(args) == 1
    sys.stdout.write(str(args[0]))
    return w_void

@declare_native("displayln")
def display(args):
    assert len(args) == 1
    print str(args[0])
    return w_void


@declare_native("cons")
def cons(args):
    assert len(args) == 2
    return W_Pair(args[0], args[1])

@declare_native("car", arguments=[W_Pair])
def car(args):
    assert len(args) == 1
    assert isinstance(args[0], W_Pair)
    return args[0].car()


@declare_native("cdr", arguments=[W_Pair])
def cdr(args):
    assert len(args) == 1
    assert isinstance(args[0], W_Pair)
    return args[0].cdr()