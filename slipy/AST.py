from slipy.continuation import *
from slipy.environment import Env
from slipy.exceptions import SlipException
from slipy.values import *


# TODO: Immutable field declarations etc


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
    def __init__(self, operator, operands):
        assert operator.simple
        for o in operands:
            assert o.simple
        self._operator = operator
        self._operands = operands

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
    def __init__(self, test, consequent, alternative):
        self._test = test
        self._consequent = consequent
        self._alternative = alternative

    def eval(self, env, cont):
        # Test is an aexp, i.e. simple
        test = self._test.eval_simple(env)
        if is_true(test):
            return self._consequent, env, cont
        else:
            return self._alternative, env, cont

    def __str__(self):
        return "(if %s %s %s)" % \
               (self._test.to_string(), self._consequent.to_string(),
                self._alternative.to_string())


class Lambda(AST):
    simple = True

    def __init__(self, args, body):
        self._args = args
        self._body = body

    def eval_simple(self, env):
        return W_Closure(self._args, env, self._body)

    def __str__(self):
        args = [None] * len(self._args)
        for i, arg in enumerate(self._args):
            args[i] = arg.to_string()
        args = " ".join(args)
        return "(lambda (%s) %s)" % (args, self._body.to_string())

class Let(AST):
    def __init__(self, sym, val, body):
        self._sym = sym
        self._val = val
        self._body = body

    def eval(self, env, cont):
        cont = LetContinuation(cont, self._sym, self._body, env)
        return self._val, env, cont

    def __str__(self):
        return "(let ([%s %s]) %s)" %\
               (self._sym.to_string(), self._val.to_string(), self._body.to_string())


class SetBang(AST):
    # We do not need a continuation as the val is an aexp
    simple = True

    def __init__(self, sym, val):
        self._sym = sym
        self._val = val

    def eval_simple(self, env):
        val = self._val.eval_simple(env)
        cell = env.get_var(self._sym)
        cell.set_value(val)
        return val

    def __str__(self):
        return "(set! %s %s)" % (self._sym.to_string(), self._val.to_string())


class Sequence(AST):
    def __init__(self, exprs):
        self._exprs = exprs

    def eval(self, env, cont):
        if len(self._exprs) == 1:
            return self._exprs[0], env, cont
        else:
            cont = SequenceContinuation(self._exprs[1:], env, cont)
            return self._exprs[0], env, cont

    def __str__(self):
        exprs = [None] * len(self._exprs)
        for i, exp in enumerate(self._exprs):
            exprs[i] = exp.to_string()
        exprs = " ".join(exprs)
        return "(begin %s)" % exprs


class VarLet(AST):
    def __init__(self, vars, exprs):
        self._vars = vars
        self._body = exprs

    def eval(self, env, cont):
        new_env = Env(previous=env)
        for var in self._vars:
            assert isinstance(var, W_Symbol)
            new_env.add_var(var, w_undefined)
        if len(self._body) == 1:
            return self._body[0], new_env, cont
        else:
            cont = SequenceContinuation(self._body[1:], new_env, cont)
            return self._body[0], new_env, cont

    def __str__(self):
        decls = [None] * len(self._vars)
        for i, var in enumerate(self._vars):
            decls[i] = var.to_string()
        exprs = [None] * len(self._body)
        for i, exp in enumerate(self._body):
            exprs[i] = exp.to_string()
        decls = " ".join(decls)
        exprs = " ".join(exprs)
        return "(let-var (%s) %s)" % (decls, exprs)


class VarRef(AST):
    simple = True

    def __init__(self, sym):
        self._sym = sym

    def eval_simple(self, env):
        cell = env.get_var(self._sym)
        val = cell.get_value()
        if val is w_undefined:
            raise SlipException("Variable referenced before definition")
        return val

    def __str__(self):
        return self._sym.to_string()


class Quote(AST):
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