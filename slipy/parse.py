from slipy.AST import *
from slipy.natives import native_dict
from slipy.values import *


def _parse_num(obj):
    assert obj['val'].is_num
    assert obj['int'].is_bool
    if obj['int'].bool_value():
        return W_Integer(obj['val'].num_value())
    else:
        return W_Float(obj['val'].num_value())


def _parse_value(val):
    assert val['type'].is_string
    type = val['type'].string_value()
    if type == 'number':
        return _parse_num(val)
    elif type == 'bool':
        assert val['val'].is_bool
        return W_Boolean.from_value(val['val'].bool_value())
    elif type == 'symbol':
        assert val['val'].is_string
        return W_Symbol.from_string(val['val'].string_value())
    elif type == 'string':
        assert val['val'].is_string
        return W_String(val['val'].string_value())
    elif type == 'char':
        # TODO: Or are they supported?
        raise Exception("chars not supported")
    else:
        # TODO: remove eventually
        raise Exception("_parse_value exception")


class _ListBuilder(object):
    @staticmethod
    def _get_value(val):
        if val.is_list:
            return _ListBuilder._helper(val.list_value())
        else:
            assert val.is_object
            return _parse_value(val.object_value())

    @staticmethod
    def _helper(vals):
        if not vals:
            return w_empty
        car = _ListBuilder._get_value(vals[0])
        cdr = _ListBuilder._helper(vals[1:])
        return W_Pair(car, cdr)

    @staticmethod
    def build_list(vals):
        return _ListBuilder._helper(vals)


def _parse_list(values):
    return _ListBuilder.build_list(values)


def _vars_to_syms(vars):
    syms = [None] * len(vars)
    for i, var in enumerate(vars):
        assert var.is_string
        syms[i] = W_Symbol.from_string(var.string_value())
    return syms


def _parse_exp_list(lst):
    exprs = []
    for exp in lst:
        assert exp.is_object
        exprs.append(_parse_dict(exp.object_value()))
    return exprs


def _parse_dict(dict):
    assert dict['type'].is_string
    type = dict['type'].string_value()

    if type == 'lambda':
        # TODO: Support lambda's like (lambda x x)
        assert dict['params'].is_list
        assert dict['vars'].is_list
        assert dict['body'].is_list
        args = _vars_to_syms(dict['params'].list_value())
        vars = _vars_to_syms(dict['vars'].list_value())
        body = _parse_exp_list(dict['body'].list_value())
        return Lambda(args, vars, body)
    elif type == 'begin':
        assert dict['body'].is_list
        body = _parse_exp_list(dict['body'].list_value())
        if len(body) == 0:
            raise SlipException("Empty begin form is not allowed!")
        return Sequence(body)
    elif type == 'quoted-list':
        # TODO: is list part of AST or a value that can be mutated?
        assert dict['val'].is_list
        list = _parse_list(dict['val'].list_value())
        return Quote(list)
    elif type == 'symbol':
        assert dict['val'].is_string
        obj = W_Symbol.from_string(dict['val'].string_value())
        return Quote(obj)
    elif type == 'number':
        obj = _parse_num(dict)
        return Quote(obj)
    elif type == 'bool':
        assert dict['val'].is_bool
        obj = W_Boolean.from_value(dict['val'].bool_value())
        return Quote(obj)
    elif type == 'string':
        assert dict['val'].is_string
        obj = W_String(dict['val'].string_value())
        return Quote(obj)
    elif type == 'char':
        # TODO: Or are they supported?
        raise Exception("chars not supported")
    # elif type == 'var':
    #     assert dict['val'].is_string
    #     sym = W_Symbol.from_string(dict['val'].string_value())
    #     return VarRef(sym)
    elif type == 'nat-ref':
        assert dict['symbol'].is_string
        symbol = W_Symbol.from_string(dict['symbol'].string_value())
        offset, _ = native_dict[symbol]
        return VarRef(symbol, 0, offset)
    elif type == 'lex-ref':
        assert dict['symbol'].is_string
        assert dict['scope'].is_num
        assert dict['offset'].is_num
        symbol = W_Symbol.from_string(dict['symbol'].string_value())
        scope = dict['scope'].num_value()
        offset = dict['offset'].num_value()
        return VarRef(symbol, scope, offset)
    elif type == 'if':
        assert dict['test'].is_object
        assert dict['consequent'].is_object
        assert dict['alternative'].is_object
        condition = _parse_dict(dict['test'].object_value())
        consequent = _parse_dict(dict['consequent'].object_value())
        alternative = _parse_dict(dict['alternative'].object_value())
        return If(condition, consequent, alternative)
    elif type == 'set':
        assert dict['target'].is_object
        assert dict['val'].is_object
        # target = W_Symbol.from_string(dict['target'].string_value())
        # return SetBang(target, _parse_dict(dict['val'].object_value()))
        target = _parse_dict(dict['target'].object_value())
        val = _parse_dict(dict['val'].object_value())
        assert isinstance(target, VarRef)
        return SetBang(target.sym, target.scope, target.offset, val)
    elif type == 'apl':
        assert dict['operator'].is_object
        assert dict['operands'].is_list
        operator = _parse_dict(dict['operator'].object_value())
        operands = _parse_exp_list(dict['operands'].list_value())
        return Application(operator, operands)
    elif type == 'let':
        assert dict['vars'].is_list
        assert dict['vals'].is_list
        assert dict['body'].is_list
        assert dict['decls'].is_list
        # sym = W_Symbol.from_string(dict['var'].string_value())
        # val = _parse_dict(dict['val'].object_value())
        # body = _parse_exp_list(dict['body'].list_value())
        # return Let(sym, val, body)
        vars = _vars_to_syms(dict['vars'].list_value())
        decls = _vars_to_syms(dict['decls'].list_value())
        body = _parse_exp_list(dict['body'].list_value())
        vals = _parse_exp_list(dict['vals'].list_value())
        return Let(vars, vals, decls, body)
    else:
        # TODO: remove once we're finished
        raise Exception("Invalid key")


def _parse_program(program):
    # print program
    # if program.is_string:
    #     print program.string_value()
    assert program.is_object
    program = program.object_value()
    assert program['vars'].is_list
    assert program['exps'].is_list
    vars = _vars_to_syms(program['vars'].list_value())
    exprs = _parse_exp_list(program['exps'].list_value())
    return Program(vars, exprs)


def parse_ast(json):
    return _parse_program(json)


def parse_data(data):
    assert data.is_object
    data = data.object_value()
    assert data['type'].is_string
    if data['type'].string_value() == 'quoted-list':
        assert data['val'].is_list
        return _parse_list(data['val'].list_value())
    else:
        return _parse_value(data)