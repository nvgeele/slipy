import json
from slipy.AST import *
from slipy.values import *


# As indicated in read.py: normally this file would be responsible for
# expanding W_SlipObject data to an AST.


def _parse_value(val):
    type = val['type']
    if type == 'number':
        return W_Number(val['val'])
    elif type == 'bool':
        return W_Boolean.from_value(val['val'])
    elif type == 'symbol':
        return W_Symbol.from_string(val['val'])
    elif type == 'string':
        return W_String(val['val'])
    elif type == 'char':
        # TODO: Or are they supported?
        raise Exception("chars not supported")
    else:
        # TODO: remove eventually
        raise Exception("_parse_value exception")


def _parse_list(values):
    def get_value(val):
        if isinstance(val, list):
            return helper(val)
        else:
            return _parse_value(val)

    def helper(vals):
        if not vals:
            return w_empty
        car = get_value(vals[0])
        cdr = helper(vals[1:])
        return W_Pair(car, cdr)

    return helper(values)


def _parse_dict(dict):
    type = dict['type']

    if type == 'lambda':
        # TODO: Support lambda's like (lambda x x)
        args = map(W_Symbol.from_string, dict['vars'])
        body = _parse_dict(dict['body'])
        return Lambda(args, body)
    elif type == 'quoted-list':
        # TODO: is list part of AST or a value that can be mutated?
        return Quote(_parse_list(dict['val']))
    elif type == 'symbol':
        obj = W_Symbol.from_string(dict['val'])
        return Quote(obj)
    elif type == 'number':
        return Quote(W_Number(dict['val']))
    elif type == 'bool':
        obj = W_Boolean.from_value(dict['val'])
        return Quote(obj)
    elif type == 'string':
        obj = W_String(dict['val'])
        return Quote(obj)
    elif type == 'char':
        # TODO: Or are they supported?
        raise Exception("chars not supported")
    elif type == 'var':
        sym = W_Symbol.from_string(dict['val'])
        return VarRef(sym)
    elif type == 'if':
        condition = _parse_dict(dict['test'])
        consequent = _parse_dict(dict['consequent'])
        alternative = _parse_dict(dict['alternative'])
        return If(condition, consequent, alternative)
    elif type == 'set':
        target = W_Symbol.from_string(dict['target'])
        return SetBang(target, _parse_dict(dict['val']))
    elif type == 'apl':
        operator = _parse_dict(dict['operator'])
        operands = map(_parse_dict, dict['operands'])
        return Application(operator, operands)
    elif type == 'let':
        sym = W_Symbol.from_string(dict['var'])
        val = _parse_dict(dict['val'])
        body = _parse_dict(dict['body'])
        return Let(sym, val, body)
    elif type == 'var-let':
        vars = map(W_Symbol.from_string, dict['vars'])
        body = map(_parse_dict, dict['body'])
        return VarLet(vars, body)
    else:
        # TODO: remove once we're finished
        raise Exception("Invalid key")


def _parse_program(program):
    vars = map(W_Symbol.from_string, program['vars'])
    exprs = map(_parse_dict, program['exps'])
    return VarLet(vars, exprs)


def parse_ast(json):
    return _parse_program(json)


def parse_data(data):
    if data['type'] == 'quoted-list':
        return _parse_list(data['val'])
    else:
        return _parse_value(data)