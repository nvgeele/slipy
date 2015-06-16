import time
from rpython.rlib import jit
from slipy.exceptions import EvaluationFinished
from slipy.read import read_string, expand_string
from slipy.values import *
from slipy.util import raw_input, write

native_dict = {}
simple_natives = []
_current_offset = 0

# TODO: replace asserts with exceptions!!!
# TODO: set-car!, set-cdr! + test with append if lists are copied properly

# TODO: Fix type checking for RPython inferencing
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
        global _current_offset
        native_dict[sym] = (_current_offset, native)
        _current_offset += 1
        if simple:
            simple_natives.append(sym)
        inner.func_name = "%s_wrapped" % func.func_name
        return inner
    return wrapper


@declare_native("not")
def is_not(args):
    assert len(args) == 1
    return W_Boolean.from_value(args[0] is w_false)


@declare_native("+") #, arguments=W_Number)
@jit.unroll_safe
def plus(args):
    if len(args) == 0:
        return W_Integer(0)
    elif len(args) == 1:
        assert isinstance(args[0], W_Number)
        return args[0]
    else:
        acc = args[0]
        for i in range(1, jit.promote(len(args))):
            assert isinstance(args[i], W_Number)
            acc = acc.add(args[i])
        return acc


@declare_native("-") #, arguments=W_Number)
@jit.unroll_safe
def minus(args):
    if len(args) == 0:
        return W_Integer(0)
    elif len(args) == 1:
        assert isinstance(args[0], W_Number)
        return W_Integer(0).sub(args[0])
    else:
        acc = args[0]
        for i in range(1, jit.promote(len(args))):
            assert isinstance(args[i], W_Number)
            acc = acc.sub(args[i])
        return acc


@declare_native("*") #, arguments=W_Number)
@jit.unroll_safe
def multiply(args):
    if len(args) == 0:
        return W_Integer(1)
    elif len(args) == 1:
        assert isinstance(args[0], W_Number)
        return args[0]
    else:
        acc = args[0]
        for i in range(1, jit.promote(len(args))):
            assert isinstance(args[i], W_Number)
            acc = acc.mul(args[i])
        return acc


@declare_native("/") #, arguments=W_Number)
@jit.unroll_safe
def divide(args):
    if len(args) == 0:
        return W_Integer(1)
    elif len(args) == 1:
        assert isinstance(args[0], W_Number)
        return args[0]
    else:
        acc = args[0]
        for i in range(1, jit.promote(len(args))):
            assert isinstance(args[i], W_Number)
            acc = acc.div(args[i])
        return acc


@declare_native("=") #, arguments=W_Number)
@jit.unroll_safe
def num_equal(args):
    assert len(args) >= 2
    i = 2
    v = True
    while i <= jit.promote(len(args)):
        l, r = args[i-2], args[i-1]
        assert isinstance(l, W_Number)
        assert isinstance(r, W_Number)
        v = v and l.is_eq(r)
        if not v:
            return w_false
        i += 1
    return W_Boolean.from_value(v)


@declare_native("<") #, arguments=W_Number)
@jit.unroll_safe
def num_lt(args):
    assert len(args) >= 2
    i = 2
    v = True
    while i <= jit.promote(len(args)):
        l, r = args[i-2], args[i-1]
        assert isinstance(l, W_Number)
        assert isinstance(r, W_Number)
        v = v and l.lt(r)
        if not v:
            return w_false
        i += 1
    return W_Boolean.from_value(v)


@declare_native(">") #, arguments=W_Number)
@jit.unroll_safe
def num_lt(args):
    assert len(args) >= 2
    i = 2
    v = True
    while i <= jit.promote(len(args)):
        l, r = args[i-2], args[i-1]
        assert isinstance(l, W_Number)
        assert isinstance(r, W_Number)
        v = v and l.gt(r)
        if not v:
            return w_false
        i += 1
    return W_Boolean.from_value(v)


@declare_native("exact->inexact")
def exact_inexact(args):
    assert len(args) == 1
    assert isinstance(args[0], W_Integer)
    return W_Float(args[0].value())


@declare_native("apply", simple=False) #, arguments=[W_Callable, W_Pair])
def apply(args, env, cont):
    fn = args[0]
    assert isinstance(fn, W_Callable)
    assert isinstance(args[1], W_Pair)
    try:
        # TODO: vals_from_list
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
    write(args[0].to_display())
    return w_void


@declare_native("displayln")
def displayln(args):
    assert len(args) == 1
    print args[0].to_display()
    return w_void


@declare_native("newline")
def newline(args):
    assert len(args) == 0
    print ""
    return w_void


@declare_native("void")
def void(args):
    return w_void


# TODO: move to appropriate place
@jit.unroll_safe
def list_from_values(vals):
    if len(vals) == 0:
        return w_empty
    else:
        cur = w_empty
        for i in range(len(vals)-1, -1, -1):
            cur = W_Pair(vals[i], cur)
        return cur


@declare_native("list")
def list(args):
    return list_from_values(args)


# TODO: move to appropriate place
# XXX: this unroll_safe annotation causes the compiler to segfault... yes.
# @jit.unroll_safe
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

# TODO: Support more than 2 args
@declare_native("append")
def append(args):
    assert len(args) == 2
    v1 = values_from_list(args[0])
    v2 = values_from_list(args[1])
    return list_from_values(v1+v2)


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


@declare_native("length")
@jit.unroll_safe
def list_length(args):
    assert len(args) == 1
    assert isinstance(args[0], W_Pair)
    length = 1
    cur = args[0]
    while True:
        cdr = cur.cdr()
        if isinstance(cdr, W_Pair):
            cur = cdr
            length += 1
        elif cdr is w_empty:
            return W_Integer(length)
        else:
            raise SlipException("Argument not a list!")


@declare_native("null?")
def is_null(args):
    assert len(args) == 1
    if args[0] is w_empty:
        return w_true
    else:
        return w_false


@declare_native("read")
def read(args):
    from slipy.parse import parse_data
    # TODO: multiline input
    # TODO: no more raw input
    # TODO: read string
    assert len(args) == 0
    input = raw_input('')
    data = read_string(input)
    return parse_data(data)


@declare_native("eval", simple=False) #, arguments=[W_Pair])
def eval(args, env, cont):
    from slipy.parse import parse_ast
    from slipy.interpreter import interpret_with_env, return_value_direct
    form = args[0]
    # TODO: fix %s stuff
    expanded = expand_string("(%s)" % form.to_string())
    ast = parse_ast(expanded)
    return_value = interpret_with_env(ast, env)
    return return_value_direct(return_value, env, cont)


@declare_native("vector")
def vector(args):
    return W_Vector(args, len(args))


@declare_native("make-vector")
def make_vector(args):
    assert len(args) == 2
    size = args[0]
    assert isinstance(size, W_Integer)
    return W_Vector.make(size.value(), args[1])


@declare_native("vector-length")
def vector_length(args):
    assert len(args) == 1
    assert isinstance(args[0], W_Vector)
    return W_Integer(args[0].length())


@declare_native("vector-ref")
def vector_ref(args):
    assert len(args) == 2
    idx = args[1]
    assert isinstance(args[0], W_Vector)
    assert isinstance(idx, W_Integer)
    return args[0].ref(idx.value())


@declare_native("vector-set!")
def vector_set(args):
    assert len(args) == 3
    idx = args[1]
    assert isinstance(args[0], W_Vector)
    assert isinstance(idx, W_Integer)
    args[0].set(idx.value(), args[2])
    return args[2]