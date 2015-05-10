from slipy.environment import Env
from slipy.exceptions import EvaluationFinished


class Continuation(object):
    def cont(self, val, env):
        # Should return ast, env, cont
        raise Exception("abstract base class")


class EmptyContinuation(Continuation):
    def cont(self, val, env):
        raise EvaluationFinished(val)


class LetContinuation(Continuation):
    def __init__(self, prev, var, body):
        self._prev = prev
        self._var = var
        self._body = body

    def cont(self, val, env):
        new_env = Env(previous=env)
        new_env.add_var(self._var, val)
        cont = RestoreEnvContinuation(self._prev, env)
        return self._body, new_env, cont


class RestoreEnvContinuation(Continuation):
    def __init__(self, prev, env):
        # TODO: Give environment a prev field so we do not need env as an argument
        self._prev = prev
        self._env = env

    def cont(self, val, env):
        from slipy.interpreter import return_value_direct
        # TODO: make sure this always works, because I'm not sure
        return return_value_direct(val, self._env, self._prev)


class SequenceContinuation(Continuation):
    def __init__(self, exprs, prev):
        self._exprs = exprs
        self._prev = prev

    def cont(self, val, env):
        # TODO: Use an index instead of manipulating lists?
        if len(self._exprs) == 0:
            from slipy.interpreter import return_value_direct
            return return_value_direct(val, env, self._prev)
        else:
            cont = SequenceContinuation(self._exprs[1:], self._prev)
            return self._exprs[0], env, cont