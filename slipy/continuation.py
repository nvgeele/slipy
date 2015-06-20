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
    _immutable_fields_ = ["let", "prev", "cont", "i"]

    def __init__(self, let, i, env, cont):
        self.let = let
        self.i = i
        self.prev = cont
        self.env = env

    def cont(self, val, env):
        self.env.set_var(self.env.scope, self.i, val)
        return self.let.make_cont(self.env, self.prev, self.i + 1)

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


class MapStartContinuation(Continuation):
    _immutable_fields_ = ["fn", "list", "prev"]

    def __init__(self, fn, list, prev):
        self.fn = fn
        self.list = list
        self.prev = prev

    def cont(self, val, env):
        from slipy.natives import do_map
        cont = MapBuildContinuation(val, self.prev)
        return do_map(self.fn, self.list, env, cont)

    def depth(self):
        return 1 + self.prev.depth()


class MapBuildContinuation(Continuation):
    _immutable_fields_ = ["val", "prev"]

    def __init__(self, val, prev):
        self.val = val
        self.prev = prev

    def cont(self, val, env):
        from slipy.interpreter import return_value_direct
        return return_value_direct(W_Pair(self.val, val), env, self.prev)

    def depth(self):
        return 1 + self.prev.depth()