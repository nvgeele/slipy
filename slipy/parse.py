from slipy.AST import *
from slipy.values import *


def _parse_value(val):
    assert val['type'].is_string
    type = val['type'].string_value()
    if type == 'number':
        assert val['val'].is_num
        return W_Number(val['val'].num_value())
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
    # def get_value(val):
    #     if val.is_list:
    #         return helper(val.list_value())
    #     else:
    #         assert val.is_object
    #         return _parse_value(val.object_value())
    #
    # def helper(vals):
    #     if not vals:
    #         return w_empty
    #     car = get_value(vals[0])
    #     cdr = helper(vals[1:])
    #     return W_Pair(car, cdr)
    #
    # return helper(values)
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

        # args = map(W_Symbol.from_string, dict['vars'])
        # body = _parse_dict(dict['body'])

        assert dict['vars'].is_list
        assert dict['body'].is_object
        args = _vars_to_syms(dict['vars'].list_value())
        body = _parse_dict(dict['body'].object_value())

        return Lambda(args, body)
    elif type == 'quoted-list':
        # TODO: is list part of AST or a value that can be mutated?

        assert dict['val'].is_list
        list = _parse_list(dict['val'].list_value())

        return Quote(list)
    elif type == 'symbol':
        # obj = W_Symbol.from_string(dict['val'])
        # return Quote(obj)

        assert dict['val'].is_string
        obj = W_Symbol.from_string(dict['val'].string_value())
        return Quote(obj)
    elif type == 'number':
        # return Quote(W_Number(dict['val']))

        assert dict['val'].is_num
        return Quote(W_Number(dict['val'].num_value()))
    elif type == 'bool':
        # obj = W_Boolean.from_value(dict['val'])
        # return Quote(obj)

        assert dict['val'].is_bool
        obj = W_Boolean.from_value(dict['val'].bool_value())
        return Quote(obj)
    elif type == 'string':
        # obj = W_String(dict['val'])
        # return Quote(obj)

        assert dict['val'].is_string
        obj = W_String(dict['val'].string_value())
        return Quote(obj)
    elif type == 'char':
        # TODO: Or are they supported?
        raise Exception("chars not supported")
    elif type == 'var':
        # sym = W_Symbol.from_string(dict['val'])
        # return VarRef(sym)

        assert dict['val'].is_string
        sym = W_Symbol.from_string(dict['val'].string_value())
        return VarRef(sym)
    elif type == 'if':
        # condition = _parse_dict(dict['test'])
        # consequent = _parse_dict(dict['consequent'])
        # alternative = _parse_dict(dict['alternative'])
        # return If(condition, consequent, alternative)

        assert dict['test'].is_object
        assert dict['consequent'].is_object
        assert dict['alternative'].is_object
        condition = _parse_dict(dict['test'].object_value())
        consequent = _parse_dict(dict['consequent'].object_value())
        alternative = _parse_dict(dict['alternative'].object_value())
        return If(condition, consequent, alternative)
    elif type == 'set':
        # target = W_Symbol.from_string(dict['target'])
        # return SetBang(target, _parse_dict(dict['val']))

        assert dict['target'].is_string
        assert dict['val'].is_object
        target = W_Symbol.from_string(dict['target'].string_value())
        return SetBang(target, _parse_dict(dict['val'].object_value()))
    elif type == 'apl':
        # operator = _parse_dict(dict['operator'])
        # operands = map(_parse_dict, dict['operands'])
        # return Application(operator, operands)

        assert dict['operator'].is_object
        assert dict['operands'].is_list
        operator = _parse_dict(dict['operator'].object_value())
        operands = _parse_exp_list(dict['operands'].list_value())
        return Application(operator, operands)
    elif type == 'let':
        # sym = W_Symbol.from_string(dict['var'])
        # val = _parse_dict(dict['val'])
        # body = _parse_dict(dict['body'])
        # return Let(sym, val, body)

        assert dict['var'].is_string
        assert dict['val'].is_object
        assert dict['body'].is_object
        sym = W_Symbol.from_string(dict['var'].string_value())
        val = _parse_dict(dict['val'].object_value())
        body = _parse_dict(dict['body'].object_value())
        return Let(sym, val, body)
    elif type == 'var-let':
        # vars = map(W_Symbol.from_string, dict['vars'])
        # body = map(_parse_dict, dict['body'])
        # return VarLet(vars, body)

        assert dict['vars'].is_list
        assert dict['body'].is_list
        vars = _vars_to_syms(dict['vars'].list_value())
        body = _parse_exp_list(dict['body'].list_value())
        return VarLet(vars, body)
    else:
        # TODO: remove once we're finished
        raise Exception("Invalid key")


def _parse_program(program):
    assert program.is_object
    program = program.object_value()
    assert program['vars'].is_list
    assert program['exps'].is_list
    vars = _vars_to_syms(program['vars'].list_value())
    exprs = _parse_exp_list(program['exps'].list_value())
    return VarLet(vars, exprs)


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