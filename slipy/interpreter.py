from slipy.continuation import EmptyContinuation
from slipy.environment import Env


def initialize_global_env():
    from slipy.natives import native_dict
    env = Env()
    for sym, native in native_dict.iteritems():
        env.add_var(sym, native)
    return env


def return_value_direct(value, env, cont):
    return cont.cont(value, env)


def _interpret(ast, env, cont):
    while True:
        ast, env, cont = ast.eval(env, cont)


def interpret_with_env(ast, env):
    cont = EmptyContinuation()
    _interpret(ast, env, cont)


def interpret_program(prog):
    env = initialize_global_env()
    interpret_with_env(prog, env)
