from slipy.values import *


native_dict = {}


# TODO: fix with type checking etc
def declare_native(name):
    def wrapper(func):
        sym = W_Symbol.from_string(name)
        native = W_NativeFunction(func)
        native_dict[sym] = native
        return func

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