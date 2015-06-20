from slipy.AST import Application
from slipy.continuation import empty_continuation
from slipy.environment import Env
from slipy.exceptions import EvaluationFinished
from rpython.rlib import jit


def get_printable_location(ast, prev):
    return ast.to_string()


driver = jit.JitDriver(reds=["env", "cont"], greens=["ast", "prev"],
                       get_printable_location=get_printable_location)


def initialize_global_env():
    from slipy.natives import native_dict
    env = Env()
    for sym, native in native_dict.iteritems():
        env.add_var(sym, native)
    return env


def return_value_direct(value, env, cont):
    return cont.cont(value, env)


def _interpret(ast, env, cont):
    # from slipy.util import write
    prev = ast
    while True:
        driver.jit_merge_point(ast=ast, prev=prev,
                               env=env, cont=cont)
        # write(str(cont.depth())+", ")
        prev = ast
        ast, env, cont = ast.eval(env, cont)
        if isinstance(ast, Application):
            driver.can_enter_jit(ast=ast, prev=prev,
                                 env=env, cont=cont)


def interpret_with_env(ast, env):
    try:
        cont = empty_continuation
        _interpret(ast, env, cont)
    except EvaluationFinished, e:
        return e.value


def interpret_program(prog):
    env = initialize_global_env()
    return interpret_with_env(prog, env)
