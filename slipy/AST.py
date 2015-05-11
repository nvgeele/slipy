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


class Application(AST):
    def __init__(self, operator, operands):
        self._operator = operator
        self._operands = operands

    def eval(self, env, cont):
        # Operator & operands are aexps, thus simple
        operator = self._operator.eval_simple(env)
        operands = [op.eval_simple(env) for op in self._operands]
        if not isinstance(operator, W_Callable):
            raise SlipException("Operator not a callable instance")
        return operator.call(operands, env, cont)

    def __str__(self):
        rator_str = str(self._operator)
        rands_str = " ".join(map(str, self._operands))
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
        return "(if %s %s %s)" % (self._test, self._consequent, self._alternative)


class Lambda(AST):
    simple = True

    def __init__(self, args, body):
        self._args = args
        self._body = body

    def eval_simple(self, env):
        return W_Closure(self._args, env, self._body)

    def __str__(self):
        args = " ".join(map(str, self._args))
        return "(lambda (%s) %s)" % (args, self._body)

class Let(AST):
    def __init__(self, sym, val, body):
        self._sym = sym
        self._val = val
        self._body = body

    def eval(self, env, cont):
        cont = LetContinuation(cont, self._sym, self._body)
        return self._val, env, cont

    def __str__(self):
        return "(let ([%s %s]) %s)" % (self._sym, self._val, self._body)


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
        return "(set! %s %s)" % (self._sym, self._val)


class Sequence(AST):
    def __init__(self, exprs):
        self._exprs = exprs

    def eval(self, env, cont):
        cont = SequenceContinuation(self._exprs[1:], cont)
        return self._exprs[0], env, cont

    def __str__(self):
        exprs = " ".join(map(str, self._exprs))
        return "(begin %s)" % exprs


class VarLet(AST):
    def __init__(self, vars, exprs):
        self._vars = vars
        self._body = Sequence(exprs)

    def eval(self, env, cont):
        new_env = Env(previous=env)
        for var in self._vars:
            new_env.add_var(var, w_undefined)
        cont = RestoreEnvContinuation(cont, env)
        return self._body, new_env, cont

    def __str__(self):
        decls = " ".join(map(str, self._vars))
        return "(let-var (%s) %s)" % (decls, str(self._body))


class VarRef(AST):
    simple = True

    def __init__(self, sym):
        self._sym = sym

    def eval_simple(self, env):
        cell = env.get_var(self._sym)
        return cell.get_value()

    def __str__(self):
        return str(self._sym)


class Quote(AST):
    simple = True

    def __init__(self, val):
        self._val = val

    def eval_simple(self, env):
        return self._val

    def __str__(self):
        if isinstance(self._val, W_Symbol) or isinstance(self._val, W_Pair):
            return "'%s" % str(self._val)
        else:
            return str(self._val)