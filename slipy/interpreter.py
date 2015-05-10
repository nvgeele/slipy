from slipy.continuation import EmptyContinuation
from slipy.environment import Env
from slipy.natives import native_dict


def initialize_global_env():
    env = Env()
    for sym, native in native_dict.iteritems():
        env.add_var(sym, native)
    return env


def return_value_direct(value, env, cont):
    return cont.cont(value, env)


def interpret_program(prog):
    env = initialize_global_env()
    cont = EmptyContinuation()
    ast = prog

    while True:
        ast, env, cont = ast.eval(env, cont)