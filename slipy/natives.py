import time
from math import sin, sqrt
from rpython.rlib import jit
from slipy.exceptions import *
from slipy.continuation import MapStartContinuation
from slipy.read import read_string, expand_string
from slipy.values import *
from slipy.util import raw_input, write

native_dict = {}
_current_offset = 0

# TODO: dedicated functions for error throwing

# TODO: test with append if lists are copied properly
# TODO: automatic type checkers in declare_native
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

        global _current_offset
        native = W_NativeFunction(inner)
        names = [name] if isinstance(name, str) else name
        for n in names:
            sym = W_Symbol.from_string(n)
            native_dict[sym] = (_current_offset, native)
            _current_offset += 1
        inner.func_name = "%s_wrapped" % func.func_name
        return inner
    return wrapper


@declare_native("eq?")
def is_eq(args):
    if not len(args) == 2:
        raise SlipException(arg_count_error % "eq?")
    # TODO: if we memoise W_Numbers, we would not need to do this
    if isinstance(args[0], W_Number) and isinstance(args[1], W_Number):
        return W_Boolean.from_value(args[0].is_eq(args[1]))
    else:
        return W_Boolean.from_value(args[0] is args[1])


@declare_native("pair?")
def is_pair(args):
    if not len(args) == 1:
        raise SlipException(arg_count_error % "pair?")
    return W_Boolean.from_value(isinstance(args[0], W_Pair))


@declare_native("not")
def is_not(args):
    if not len(args) == 1:
        raise SlipException(arg_count_error % "not")
    return W_Boolean.from_value(args[0] is w_false)


@declare_native("+")
@jit.unroll_safe
def plus(args):
    if len(args) == 0:
        return W_Integer(0)
    elif len(args) == 1:
        if not isinstance(args[0], W_Number):
            raise SlipException(arg_types_error % "+")
        return args[0]
    else:
        acc = args[0]
        for i in range(1, jit.promote(len(args))):
            if not isinstance(args[i], W_Number):
                raise SlipException(arg_types_error % "+")
            acc = acc.add(args[i])
        return acc


@declare_native("-")
@jit.unroll_safe
def minus(args):
    if len(args) == 0:
        return W_Integer(0)
    elif len(args) == 1:
        if not isinstance(args[0], W_Number):
            raise SlipException(arg_types_error % "-")
        return W_Integer(0).sub(args[0])
    else:
        acc = args[0]
        for i in range(1, jit.promote(len(args))):
            if not isinstance(args[i], W_Number):
                raise SlipException(arg_types_error % "-")
            acc = acc.sub(args[i])
        return acc


@declare_native("*")
@jit.unroll_safe
def multiply(args):
    if len(args) == 0:
        return W_Integer(1)
    elif len(args) == 1:
        if not isinstance(args[0], W_Number):
            raise SlipException(arg_types_error % "*")
        return args[0]
    else:
        acc = args[0]
        for i in range(1, jit.promote(len(args))):
            if not isinstance(args[i], W_Number):
                raise SlipException(arg_types_error % "*")
            acc = acc.mul(args[i])
        return acc


@declare_native("/")
@jit.unroll_safe
def divide(args):
    if len(args) == 0:
        return W_Integer(1)
    elif len(args) == 1:
        if not isinstance(args[0], W_Number):
            raise SlipException(arg_types_error % "/")
        return args[0]
    else:
        acc = args[0]
        for i in range(1, jit.promote(len(args))):
            if not isinstance(args[i], W_Number):
                raise SlipException(arg_types_error % "/")
            acc = acc.div(args[i])
        return acc


@declare_native("sqrt")
def num_sqrt(args):
    if not len(args) == 1:
        raise SlipException(arg_count_error % "sin")
    if not isinstance(args[0], W_Number):
        raise SlipException(arg_types_error % "sin")
    return W_Float(sqrt(args[0].value()))

@declare_native("sin")
def num_sin(args):
    if not len(args) == 1:
        raise SlipException(arg_count_error % "sin")
    if not isinstance(args[0], W_Number):
        raise SlipException(arg_types_error % "sin")
    return W_Float(sin(args[0].value()))


@declare_native("quotient")
def num_quotient(args):
    if not len(args) == 2:
        raise SlipException(arg_count_error % "quotient")
    if not isinstance(args[0], W_Number):
        raise SlipException(arg_types_error % "quotient")
    if not isinstance(args[1], W_Number):
        raise SlipException(arg_types_error % "quotient")

    return W_Integer(args[0].value()/args[1].value())


@declare_native("=")
@jit.unroll_safe
def num_equal(args):
    if not len(args) >= 2:
        raise SlipException(arg_count_error % "=")
    i = 2
    v = True
    while i <= jit.promote(len(args)):
        l, r = args[i-2], args[i-1]
        if not isinstance(l, W_Number):
            raise SlipException(arg_types_error % "=")
        if not isinstance(r, W_Number):
            raise SlipException(arg_types_error % "=")
        v = v and l.is_eq(r)
        if not v:
            return w_false
        i += 1
    return W_Boolean.from_value(v)


@declare_native("<")
@jit.unroll_safe
def num_lt(args):
    if not len(args) >= 2:
        raise SlipException(arg_count_error % "<")
    i = 2
    v = True
    while i <= jit.promote(len(args)):
        l, r = args[i-2], args[i-1]
        if not isinstance(l, W_Number):
            raise SlipException(arg_types_error % "<")
        if not isinstance(r, W_Number):
            raise SlipException(arg_types_error % "<")
        v = v and l.lt(r)
        if not v:
            return w_false
        i += 1
    return W_Boolean.from_value(v)


@declare_native(">")
@jit.unroll_safe
def num_gt(args):
    if not len(args) >= 2:
        raise SlipException(arg_count_error % ">")
    i = 2
    v = True
    while i <= jit.promote(len(args)):
        l, r = args[i-2], args[i-1]
        if not isinstance(l, W_Number):
            raise SlipException(arg_types_error % ">")
        if not isinstance(r, W_Number):
            raise SlipException(arg_types_error % ">")
        v = v and l.gt(r)
        if not v:
            return w_false
        i += 1
    return W_Boolean.from_value(v)


@declare_native("<=")
@jit.unroll_safe
def num_le(args):
    if not len(args) >= 2:
        raise SlipException(arg_count_error % "<=")
    i = 2
    v = True
    while i <= jit.promote(len(args)):
        l, r = args[i-2], args[i-1]
        if not isinstance(l, W_Number):
            raise SlipException(arg_types_error % "<=")
        if not isinstance(r, W_Number):
            raise SlipException(arg_types_error % "<=")
        v = v and l.le(r)
        if not v:
            return w_false
        i += 1
    return W_Boolean.from_value(v)


@declare_native(">=")
@jit.unroll_safe
def num_ge(args):
    if not len(args) >= 2:
        raise SlipException(arg_count_error % ">=")
    i = 2
    v = True
    while i <= jit.promote(len(args)):
        l, r = args[i-2], args[i-1]
        if not isinstance(l, W_Number):
            raise SlipException(arg_types_error % ">=")
        if not isinstance(r, W_Number):
            raise SlipException(arg_types_error % ">=")
        v = v and l.ge(r)
        if not v:
            return w_false
        i += 1
    return W_Boolean.from_value(v)


@declare_native("exact->inexact")
def exact_inexact(args):
    if not len(args) == 1:
        raise SlipException(arg_count_error % "exact->inexact")
    if not isinstance(args[0], W_Integer):
        raise SlipException(arg_types_error % "exact->inexact")
    return W_Float(args[0].value())


@declare_native(["error", "fatal-error"])
def throw_error(args):
    if not len(args) == 1:
        raise SlipException(arg_count_error % "error")
    raise SlipException("Program threw error with value: " % args[0].to_string())


@declare_native("map", simple=False)
def list_map(args, env, cont):
    if not len(args) == 2:
        raise SlipException(arg_count_error % "map")
    fn = args[0]
    if not isinstance(fn, W_Callable):
        raise SlipException(arg_types_error % "map")
    list = args[1]
    if not isinstance(list, W_Pair):
        raise SlipException(arg_types_error % "map")
    return do_map(fn, list, env, cont)

def do_map(fn, list, env, cont):
    from slipy.interpreter import return_value_direct
    if not isinstance(list, W_Pair):
        if list is not w_empty:
            raise SlipException("map: malformed list")
        return return_value_direct(w_empty, env, cont)
    return fn.call([list.car()], env, MapStartContinuation(fn, list.cdr(), cont))


@declare_native("apply", simple=False)
def apply(args, env, cont):
    if not len(args) == 2:
        raise SlipException(arg_count_error % "apply")
    fn = args[0]
    if not isinstance(fn, W_Callable):
        raise SlipException(arg_types_error % "apply")
    if not isinstance(args[1], W_Pair):
        raise SlipException(arg_types_error % "apply")

    try:
        actual_args = values_from_list(args[1])
    except SlipException:
        raise SlipException("apply: expected list")
    return fn.call(actual_args, env, cont)


@declare_native(["call/cc", "call-with-current-continuation"], simple=False)
def callcc(args, env, cont):
    if not len(args) == 1:
        raise SlipException(arg_count_error % "call/cc")
    fn = args[0]
    if not isinstance(fn, W_Callable):
        raise SlipException(arg_types_error % "call/cc")
    return fn.call([W_Continuation(cont)], env, cont)


@declare_native("time")
def slip_time(args):
    if not len(args) == 0:
        raise SlipException(arg_count_error % "time")
    return W_Float(time.time())


@declare_native("display")
def display(args):
    if not len(args) == 1:
        raise SlipException(arg_count_error % "display")
    write(args[0].to_display())
    return w_void


@declare_native("displayln")
def displayln(args):
    if not len(args) == 1:
        raise SlipException(arg_count_error % "displayln")
    print args[0].to_display()
    return w_void


@declare_native("newline")
def newline(args):
    if not len(args) == 0:
        raise SlipException(arg_count_error % "newline")
    print ""
    return w_void


@declare_native("void")
def void(args):
    return w_void


@declare_native("list")
def list(args):
    return list_from_values(args)


# TODO: Support more than 2 args
@declare_native("append")
def append(args):
    if not len(args) == 2:
        raise SlipException(arg_count_error % "append")
    try:
        v1 = values_from_list(args[0])
        v2 = values_from_list(args[1])
        return list_from_values(v1+v2)
    except SlipException:
        raise SlipException("append: expected proper lists as arguments")


@declare_native("cons")
def cons(args):
    if not len(args) == 2:
        raise SlipException(arg_count_error % "cons")
    assert isinstance(args[0], W_SlipObject)
    assert isinstance(args[1], W_SlipObject)
    return W_Pair(args[0], args[1])


@declare_native("car")
def car(args):
    if not len(args) == 1:
        raise SlipException(arg_count_error % "car")
    if not isinstance(args[0], W_Pair):
        raise SlipException(arg_types_error % "car")
    return args[0].car()


@declare_native("cdr")
def cdr(args):
    if not len(args) == 1:
        raise SlipException(arg_count_error % "cdr")
    if not isinstance(args[0], W_Pair):
        raise SlipException(arg_types_error % "cdr")
    return args[0].cdr()


@declare_native("set-car!")
def set_car(args):
    if not len(args) == 2:
        raise SlipException(arg_count_error % "set-car!")
    if not isinstance(args[0], W_Pair):
        raise SlipException(arg_types_error % "set-car!")
    args[0].set_car(args[1])
    return args[1]


@declare_native("set-cdr!")
def set_cdr(args):
    if not len(args) == 2:
        raise SlipException(arg_count_error % "set-cdr!")
    if not isinstance(args[0], W_Pair):
        raise SlipException(arg_types_error % "set-cdr!")
    args[0].set_cdr(args[1])
    return args[1]


@declare_native("length")
@jit.unroll_safe
def list_length(args):
    if not len(args) == 1:
        raise SlipException(arg_count_error % "length")
    if not isinstance(args[0], W_Pair):
        raise SlipException(arg_types_error % "length")
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
    if not len(args) == 1:
        raise SlipException(arg_count_error % "null?")
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
    if not len(args) == 0:
        raise SlipException(arg_count_error % "read")
    input = raw_input('')
    data = read_string(input)
    return parse_data(data)


@declare_native("eval", simple=False)
def eval(args, env, cont):
    from slipy.parse import parse_ast
    from slipy.interpreter import interpret_with_env, return_value_direct
    if not len(args) == 1:
        raise SlipException(arg_count_error % "eval")
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
    if not len(args) == 2:
        raise SlipException(arg_count_error % "make-vector")
    size = args[0]
    if not isinstance(size, W_Integer):
        raise SlipException(arg_types_error % "make-vector")
    return W_Vector.make(size.value(), args[1])


@declare_native("vector-length")
def vector_length(args):
    if not len(args) == 1:
        raise SlipException(arg_count_error % "vector-length")
    if not isinstance(args[0], W_Vector):
        raise SlipException(arg_types_error % "vector-length")
    return W_Integer(args[0].length())


@declare_native("vector-ref")
def vector_ref(args):
    if not len(args) == 2:
        raise SlipException(arg_count_error % "vector-ref")
    idx = args[1]
    if not isinstance(args[0], W_Vector):
        raise SlipException(arg_types_error % "vector-ref")
    if not isinstance(idx, W_Integer):
        raise SlipException(arg_types_error % "vector-ref")
    return args[0].ref(idx.value())


@declare_native("vector-set!")
def vector_set(args):
    if not len(args) == 3:
        raise SlipException(arg_count_error % "vector-set!")
    idx = args[1]
    if not isinstance(args[0], W_Vector):
        raise SlipException(arg_types_error % "vector-set!")
    if not isinstance(idx, W_Integer):
        raise SlipException(arg_types_error % "vector-set!")
    args[0].set(idx.value(), args[2])
    return args[2]