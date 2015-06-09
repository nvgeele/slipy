from slipy.environment import Env
from slipy.exceptions import EvaluationFinished
from slipy.values import *


class Continuation(object):
    def cont(self, val, env):
        # Should return ast, env, cont
        raise Exception("abstract base class")


class EmptyContinuation(Continuation):
    def cont(self, val, env):
        raise EvaluationFinished(val)


empty_continuation = EmptyContinuation()


class LetContinuation(Continuation):
    def __init__(self, prev, var, body, env):
        self._prev = prev
        self._var = var
        self._body = body
        self._env = env

    def cont(self, val, env):
        # assert isinstance(val, W_SlipObject)
        # assert isinstance(self._var, W_Symbol)
        # assert isinstance(self._env, Env)
        new_env = Env(previous=self._env)
        # TODO: Restore me you shit
        new_env.add_var(self._var, val)
        return self._body, new_env, self._prev


class SequenceContinuation(Continuation):
    def __init__(self, exprs, env, prev):
        self._exprs = exprs
        self._prev = prev
        self._env = env

    def cont(self, val, env):
        # TODO: Use an index instead of manipulating lists?
        if len(self._exprs) == 1:
            # from slipy.interpreter import return_value_direct
            # return return_value_direct(val, self._env, self._prev)
            return self._exprs[0], self._env, self._prev
        else:
            cont = SequenceContinuation(self._exprs[1:], self._env, self._prev)
            return self._exprs[0], self._env, cont