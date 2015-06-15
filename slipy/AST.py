from rpython.rlib import jit
from slipy.continuation import *
from slipy.environment import Env, get_global_env
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
    def __init__(self, test, consequent, alternative):
        self.test = test
        self.consequent = consequent
        self.alternative = alternative
        # TODO: move this "optimization" to parse?
        # if isinstance(consequent, VarLet) and \
        #         len(consequent.vars) == 0 and \
        #         len(consequent.body) == 1:
        #     self._consequent = consequent.body[0]
        # else:
        #     self._consequent = consequent
        # if isinstance(alternative, VarLet) and \
        #         len(alternative.vars) == 0 and \
        #         len(alternative.body) == 1:
        #     self._alternative = alternative.body[0]
        # else:
        #     self._alternative = alternative

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
    simple = True

    def __init__(self, args, vars, body):
        self.args = args
        self.body = body
        self.vars = vars

    def eval_simple(self, env):
        return W_Closure(self.args, self.vars, env, self.body)

    def __str__(self):
        # TODO: re-implement me
        # args = [None] * len(self.args)
        # for i, arg in enumerate(self.args):
        #     args[i] = arg.to_string()
        # args = " ".join(args)
        # return "(lambda (%s) %s)" % (args, self.body.to_string())
        return "(lambda ...)"


class Let(AST):
    def __init__(self, vars, vals, decls, body):
        self.vars = vars
        self.vals = vals
        self.decls = decls
        self.body = body

    def eval(self, env, cont):
        new_env = Env(len(self.vars) + len(self.decls), previous=env)
        if len(self.vars) == 0:
            return Sequence(self.body), new_env, cont
        else:
            l_cont = LetContinuation(self, 0, cont, new_env)
            return self.vals[0], new_env, l_cont

    def __str__(self):
        # TODO: re-implement me
        # bs = [None] * len(self._body)
        # for i, e in enumerate(self._body):
        #     bs[i] = e.to_string()
        # bs = " ".join(bs)
        # return "(let ([%s %s]) %s)" %\
        #        (self._sym.to_string(), self._val.to_string(), bs)
        return "(let ...)"


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
        # cell = env.get_var(self.scope, self.offset)
        # cell.set_value(val)
        env.set_var(self.scope, self.offset, val)
        return val

    def __str__(self):
        return "(set! %s %s)" % (self.sym.to_string(), self.val.to_string())


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


# class VarLet(AST):
#     def __init__(self, vars, exprs):
#         self.vars = vars
#         self.body = exprs
#
#     @jit.unroll_safe
#     def eval(self, env, cont):
#         new_env = Env(previous=env)
#         for var in self.vars:
#             assert isinstance(var, W_Symbol)
#             new_env.add_var(var, w_undefined)
#         if len(self.body) == 1:
#             return self.body[0], new_env, cont
#         else:
#             cont = SequenceContinuation(self.body[1:], new_env, cont)
#             return self.body[0], new_env, cont
#
#     def __str__(self):
#         decls = [None] * len(self.vars)
#         for i, var in enumerate(self.vars):
#             decls[i] = var.to_string()
#         exprs = [None] * len(self.body)
#         for i, exp in enumerate(self.body):
#             exprs[i] = exp.to_string()
#         decls = " ".join(decls)
#         exprs = " ".join(exprs)
#         return "(let-var (%s) %s)" % (decls, exprs)


class VarRef(AST):
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


# class LexVar(AST):
#     simple = True
#
#     def __init__(self, scope, sym):
#         self.scope = scope
#         self.sym = sym
#
#     def eval_simple(self, env):
#         cell = env.lex_var(self.scope, self.sym)
#         return cell.get_value()
#
#
# class LexSetBang(AST):
#     simple = True
#
#     def __init__(self, scope, sym, val):
#         pass


class Program(AST):
    def __init__(self, vars, body):
        self.vars = vars
        self.body = body

    @jit.unroll_safe
    def eval(self, env, cont):
        env = Env(len(self.vars), previous=get_global_env())
        if len(self.body) == 1:
            return self.body[0], env, cont
        else:
            cont = SequenceContinuation(self.body[1:], env, cont)
            return self.body[0], env, cont

    def __str__(self):
        # TODO: implement me
        return "<program>"


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