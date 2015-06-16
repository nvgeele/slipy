from slipy.AST import Application
from slipy.continuation import empty_continuation
from slipy.environment import *
from slipy.exceptions import EvaluationFinished
from rpython.rlib import jit


def get_printable_location(ast, prev):
    return ast.to_string()


driver = jit.JitDriver(reds=["env", "cont"], greens=["ast", "prev"],
                       get_printable_location=get_printable_location)


def initialize_global_env():
    from slipy.natives import native_dict
    env = Env(len(native_dict))
    for sym, tup in native_dict.iteritems():
        offset, native = tup
        env.set_var(0, offset, native)
    # return env
    set_global_env(env)

def return_value_direct(value, env, cont):
    return cont.cont(value, env)


def _interpret(ast, env, cont):
    # from slipy.util import write
    prev = ast
    while True:
        driver.jit_merge_point(ast=ast, prev=prev,
                               env=env, cont=cont)
        # write(str(cont.depth())+", ")
        # print "pre: %s" % ast
        prev = ast
        ast, env, cont = ast.eval(env, cont)
        # print "post: %s" % ast
        if isinstance(ast, Application):
            driver.can_enter_jit(ast=ast, prev=prev,
                                 env=env, cont=cont)


def interpret_with_env(ast, env):
    try:
        cont = empty_continuation
        _interpret(ast, env, cont)
    except EvaluationFinished, e:
        return e.value


def interpret_with_global(ast):
    return interpret_with_env(ast, get_global_env())
