import time
from slipy.exceptions import EvaluationFinished
from slipy.parse import parse_data, parse_ast
from slipy.read import read_string, expand_string
from slipy.values import *
from slipy.util import raw_input, write, zip


native_dict = {}

# TODO: Fix type checking for RPython inferencing
# TODO: replace asserts with exceptions!!!
def declare_native(name, simple=True):
    def wrapper(func):
        def inner(args, env, cont):
            if simple:
                from slipy.interpreter import return_value_direct
                result = func(args)
                return return_value_direct(result, env, cont)
            else:
                # TODO: without the assert, the inferencer tells us ret may be None
                # TODO: find out why! Probably due to eval and EvaluationFinished or sth
                ret = func(args, env, cont)
                assert ret
                return ret

        sym = W_Symbol.from_string(name)
        native = W_NativeFunction(inner)
        native_dict[sym] = native
        inner.func_name = "%s_wrapped" % name
        return inner

    return wrapper


@declare_native("+") #, arguments=W_Number)
def plus(args):
    if len(args) == 0:
        return W_Integer(0)
    elif len(args) == 1:
        assert isinstance(args[0], W_Number)
        return args[0]
    else:
        acc = args[0]
        for i in range(1, len(args)):
            assert isinstance(args[i], W_Number)
            acc = acc.add(args[i])
        return acc


@declare_native("-") #, arguments=W_Number)
def minus(args):
    if len(args) == 0:
        return W_Integer(0)
    elif len(args) == 1:
        assert isinstance(args[0], W_Number)
        return W_Integer(0).sub(args[0])
    else:
        acc = args[0]
        for i in range(1, len(args)):
            assert isinstance(args[i], W_Number)
            acc = acc.sub(args[i])
        return acc


@declare_native("*") #, arguments=W_Number)
def multiply(args):
    if len(args) == 0:
        return W_Integer(1)
    elif len(args) == 1:
        assert isinstance(args[0], W_Number)
        return args[0]
    else:
        acc = args[0]
        for i in range(1, len(args)):
            assert isinstance(args[i], W_Number)
            acc = acc.mul(args[i])
        return acc


@declare_native("/") #, arguments=W_Number)
def divide(args):
    if len(args) == 0:
        return W_Integer(1)
    elif len(args) == 1:
        assert isinstance(args[0], W_Number)
        return args[0]
    else:
        acc = args[0]
        for i in range(1, len(args)):
            assert isinstance(args[i], W_Number)
            acc = acc.div(args[i])
        return acc


@declare_native("=") #, arguments=W_Number)
def num_equal(args):
    assert len(args) >= 2
    i = 2
    v = True
    while i <= len(args):
        l, r = args[i-2], args[i-1]
        assert isinstance(l, W_Number)
        assert isinstance(r, W_Number)
        v = v and l.is_eq(r)
        if not v:
            return w_false
        i += 1
    return W_Boolean.from_value(v)


@declare_native("apply", simple=False) #, arguments=[W_Callable, W_Pair])
def apply(args, env, cont):
    fn = args[0]
    assert isinstance(fn, W_Callable)
    assert isinstance(args[1], W_Pair)
    try:
        actual_args = from_list(args[1])
    except SlipException:
        raise SlipException("apply: expected list")
    return fn.call(actual_args, env, cont)


@declare_native("call/cc", simple=False) #, arguments=[W_Callable])
def callcc(args, env, cont):
    fn = args[0]
    assert isinstance(fn, W_Callable)
    return fn.call([W_Continuation(cont)], env, cont)


@declare_native("time")
def slip_time(args):
    assert len(args) == 0
    return W_Float(time.time())


@declare_native("display")
def display(args):
    assert len(args) == 1
    write(args[0].to_string())
    return w_void


@declare_native("displayln")
def display(args):
    assert len(args) == 1
    print args[0].to_string()
    return w_void


@declare_native("cons")
def cons(args):
    assert len(args) == 2
    # TODO: We shouldn't need these assertions
    assert isinstance(args[0], W_SlipObject)
    assert isinstance(args[1], W_SlipObject)
    return W_Pair(args[0], args[1])


@declare_native("car") #, arguments=[W_Pair])
def car(args):
    assert len(args) == 1
    assert isinstance(args[0], W_Pair)
    return args[0].car()


@declare_native("cdr") #, arguments=[W_Pair])
def cdr(args):
    assert len(args) == 1
    assert isinstance(args[0], W_Pair)
    return args[0].cdr()


@declare_native("read")
def read(args):
    # TODO: multiline input
    # TODO: no more raw input
    # TODO: read string
    assert len(args) == 0
    input = raw_input('')
    data = read_string(input)
    return parse_data(data)


@declare_native("eval", simple=False) #, arguments=[W_Pair])
def eval(args, env, cont):
    from slipy.interpreter import interpret_with_env, return_value_direct
    form = args[0]
    # TODO: fix %s stuff
    expanded = expand_string("(%s)"%str(form))
    ast = parse_ast(expanded)
    return_value = interpret_with_env(ast, env)
    return return_value_direct(return_value, env, cont)