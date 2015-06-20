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

    def cont(self, val, env):
        self.env.set_var(self.env.scope, self.i, val)
        self.i += 1
        if len(self.let.vars) == self.i:
            return self.let.body, self.env, self.prev
        else:
            cont = LetContinuation(self.let, self.i, self.prev, self.env)
            return self.let.vals[self.i], self.env, cont

    def depth(self):
        return 1 + self.prev.depth()


class SequenceContinuation(Continuation):
    _immutable_fields_ = ["seq", "env", "prev", "i"]

    def __init__(self, seq, i, env, prev):
        self.seq = seq
        self.prev = prev
        self.env = env
        self.i = i

    def cont(self, val, env):
        return self.seq.make_cont(self.env, self.prev, self.i)

    def depth(self):
        return 1 + self.prev.depth()