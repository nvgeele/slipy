from rpython.rlib import jit
from slipy.continuation import *
from slipy.environment import Env, get_global_env
from slipy.exceptions import SlipException
from slipy.util import zip
from slipy.values import *


class AST(object):
    simple = False

    def eval(self, env, cont):
        if self.simple:
            from slipy.interpreter import return_value_direct
            return return_value_direct(self.eval_simple(env), env, cont)
        raise Exception("abstract base class")

    def eval_simple(self, env):
        raise Exception("abstract base class")

    def __str__(self):
        return "<AST>"

    def to_string(self):
        return self.__str__()


class Application(AST):
    _immutable_fields_ = ["_operator", "_operands[*]"]

    def __init__(self, operator, operands):
        assert operator.simple
        for o in operands:
            assert o.simple
        self._operator = operator
        self._operands = operands

    @jit.unroll_safe
    def eval(self, env, cont):
        # Operator & operands are aexps, thus simple
        operator = self._operator.eval_simple(env)
        operands = [None] * len(self._operands)
        for i, op in enumerate(self._operands):
            operands[i] = op.eval_simple(env)
        if not isinstance(operator, W_Callable):
            raise SlipException("Operator not a callable instance")
        else:
            return operator.call(operands, env, cont)

    def __str__(self):
        rator_str = self._operator.to_string()
        rands_str = [None] * len(self._operands)
        for i, op in enumerate(self._operands):
            rands_str[i] = op.to_string()
        rands_str = " ".join(rands_str)
        return "(%s %s)" % (rator_str, rands_str)


class If(AST):
    _immutable_fields_ = ["test", "consequent", "alternative"]

    def __init__(self, test, consequent, alternative):
        self.test = test
        self.consequent = consequent
        self.alternative = alternative

    def eval(self, env, cont):
        # Test is an aexp, i.e. simple
        test = self.test.eval_simple(env)
        if is_true(test):
            return self.consequent, env, cont
        else:
            return self.alternative, env, cont

    def __str__(self):
        return "(if %s %s %s)" % \
               (self.test.to_string(), self.consequent.to_string(),
                self.alternative.to_string())


class Lambda(AST):
    _immutable_fields_ = ["args[*]", "body[*]", "vars[*]"]
    simple = True

    def __init__(self, args, vars, body):
        self.args = args
        self.body = body
        self.vars = vars

    def eval_simple(self, env):
        return W_Closure(self.args, self.vars, env, self.body)

    def __str__(self):
        args = [None] * len(self.args)
        for i, arg in enumerate(self.args):
            args[i] = arg.to_string()
        vars = [None] * len(self.vars)
        for i, var in enumerate(self.vars):
            vars[i] = var.to_string()
        body = [None] * len(self.body)
        for i, exp in enumerate(self.body):
            body[i] = exp.to_string()
        return "(lambda (%s) (%s) %s)" % (" ".join(args),
                                          " ".join(vars),
                                          " ".join(body))


class Let(AST):
    _immutable_fields_ = ["vars[*]", "vals[*]", "decls[*]", "body"]

    def __init__(self, vars, vals, decls, body):
        self.vars = vars
        self.vals = vals
        self.decls = decls
        self.body = Sequence(body)

    def eval(self, env, cont):
        new_env = Env(len(self.vars) + len(self.decls), previous=env)
        return self.make_cont(new_env, cont)

    def make_cont(self, env, prev, i=0):
        # jit.promote(self)
        # jit.promote(i)
        if i == len(self.vars):
            return self.body, env, prev
        else:
            return self.vals[i], env, LetContinuation(self, i, env, prev)

    def __str__(self):
        vars = [None] * len(self.vars)
        for i, t in enumerate(zip(self.vars, self.vals)):
            var, val = t
            vars[i] = "[%s %s]" % (var.to_string(), val.to_string())
        decls = [None] * len(self.decls)
        for i, var in enumerate(self.decls):
            decls[i] = var.to_string()
        body = [None] * len(self.body.body)
        for i, exp in enumerate(self.body.body):
            body[i] = exp.to_string()
        return "(let (%s) (%s) %s)" % ("".join(vars),
                                       " ".join(decls),
                                       " ".join(body))


class SetBang(AST):
    # We do not need a continuation as the val is an aexp
    simple = True
    _immutable_fields_ = ["sym", "scope", "offset", "val"]

    def __init__(self, sym, scope, offset, val):
        self.sym = sym
        self.val = val
        self.scope = int(scope)
        self.offset = int(offset)

    def eval_simple(self, env):
        val = self.val.eval_simple(env)
        env.set_var(self.scope, self.offset, val)
        return val

    def __str__(self):
        return "(set! %s %s)" % (self.sym.to_string(), self.val.to_string())


class Sequence(AST):
    _immutable_fields_ = ["body[*]"]

    def __init__(self, exprs):
        assert len(exprs) > 0, "Sequence body needs at least one expression"
        self.body = exprs

    def eval(self, env, cont):
        return self.make_cont(env, cont)

    def make_cont(self, env, prev, i=0):
        jit.promote(self)
        jit.promote(i)
        if i == len(self.body) - 1:
            return self.body[i], env, prev
        else:
            return self.body[i], env, SequenceContinuation(self, i+1, env, prev)

    def __str__(self):
        exprs = [None] * len(self.body)
        for i, exp in enumerate(self.body):
            exprs[i] = exp.to_string()
        exprs = " ".join(exprs)
        return "(begin %s)" % exprs


class VarRef(AST):
    _immutable_fields_ = ["sym", "scope", "offset"]
    simple = True

    def __init__(self, sym, scope, offset):
        self.sym = sym
        self.scope = int(scope)
        self.offset = int(offset)

    def eval_simple(self, env):
        cell = env.get_var(self.scope, self.offset)
        val = cell.get_value()
        if val is w_undefined:
            raise SlipException("Variable referenced before definition")
        return val

    def __str__(self):
        return self.sym.to_string()


class Program(AST):
    _immutable_fields_ = ["vars[*]", "body"]

    def __init__(self, vars, body):
        self.vars = vars
        self.body = Sequence(body)

    def eval(self, env, cont):
        env = Env(len(self.vars), previous=get_global_env())
        return self.body, env, cont

    def __str__(self):
        vars = [None] * len(self.vars)
        for i, var in enumerate(self.vars):
            vars[i] = var.to_string()
        body = [None] * len(self.body.body)
        for i, exp in enumerate(self.body.body):
            body[i] = exp.to_string()

        return "((%s) %s)" % (" ".join(vars), " ".join(body))

# TODO: if val is a list, return copy
class Quote(AST):
    _immutable_fields_ = ["_val"]
    simple = True

    def __init__(self, val):
        self._val = val

    def eval_simple(self, env):
        return self._val

    def __str__(self):
        if isinstance(self._val, W_Symbol) or isinstance(self._val, W_Pair):
            return "'%s" % self._val.to_string()
        else:
            return self._val.to_string()