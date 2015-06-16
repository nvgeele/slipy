from slipy.environment import Env
from slipy.exceptions import EvaluationFinished
from slipy.values import *


class Continuation(object):
    def cont(self, val, env):
        # Should return ast, env, cont
        raise Exception("abstract base class")

    def depth(self):
        raise Exception("abstract base class")


class EmptyContinuation(Continuation):
    def cont(self, val, env):
        raise EvaluationFinished(val)

    def depth(self):
        return 1


empty_continuation = EmptyContinuation()


class LetContinuation(Continuation):
    _immutable_fields_ = ["let", "prev", "cont"]

    def __init__(self, let, i, cont, env):
        self.let = let
        self.i = i
        self.prev = cont
        self.env = env

    # TODO: promote self.let?
    def cont(self, val, env):
        # self.env.add_var(self.let.vars[self.i], val)
        self.env.set_var(self.env.scope, self.i, val)
        self.i += 1
        if len(self.let.vars) == self.i:
            if len(self.let.body) == 1:
                return self.let.body[0], self.env, self.prev
            else:
                cont = SequenceContinuation(self.let.body, 1, self.env, self.prev)
                return self.let.body[0], self.env, cont
        else:
            cont = LetContinuation(self.let, self.i, self.prev, self.env)
            return self.let.vals[self.i], self.env, cont

    def depth(self):
        return 1 + self.prev.depth()


# Note how we always restore the environment
class SequenceContinuation(Continuation):
    _immutable_fields_ = ["exprs[*]", "env", "prev", "i", "len"]

    def __init__(self, exprs, i, env, prev):
        self.exprs = exprs
        self.prev = prev
        self.env = env
        self.i = i
        self.len = len(exprs)

    def cont(self, val, env):
        # TODO: Use an index instead of manipulating lists?
        if self.len - 1 == self.i:
            # from slipy.interpreter import return_value_direct
            # return return_value_direct(val, self._env, self._prev)
            return self.exprs[self.i], self.env, self.prev
        else:
            cont = SequenceContinuation(self.exprs, self.i+1, self.env, self.prev)
            return self.exprs[self.i], self.env, cont

    def depth(self):
        return 1 + self.prev.depth()