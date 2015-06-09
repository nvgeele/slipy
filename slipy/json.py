from rpython.rlib.parsing.pypackrat import PackratParser
from rpython.rlib.parsing.makepackrat import BacktrackException, Status


def unquote(s):
    str_lst = []
    pos = 1
    last = len(s)-1
    while pos < last:
        ch = s[pos]
        if ch == '\\':
            pos += 1
            ch = s[pos]
            if ch == '\\' or ch == '\"':
                str_lst.append(ch)
            else:
                raise Exception("String unquote error")
        else:
            str_lst.append(ch)

        pos += 1

    return ''.join(str_lst)


class J_Base(object):
    is_string = is_bool = is_list = is_object = is_num = False

    def string_value(self):
        raise TypeError

    def bool_value(self):
        raise TypeError

    def object_value(self):
        raise TypeError

    def list_value(self):
        raise TypeError

    def num_value(self):
        raise TypeError


class J_Simple(J_Base):
    def __init__(self, val):
        self._val = val


class J_String(J_Simple):
    is_string = True

    def string_value(self):
        return self._val


class J_Bool(J_Simple):
    is_bool = True

    def bool_value(self):
        return self._val

class Entry(object):
    def __init__(self, key, val):
        assert isinstance(key, J_String)
        self._key = key.string_value()
        self._val = val

    def key(self):
        return self._key

    def val(self):
        return self._val


class J_Object(J_Base):
    is_object = True

    def __init__(self, entries):
        self._dict = {}
        for entry in entries:
            self._dict[entry.key()] = entry.val()

    def object_value(self):
        return self._dict


class J_List(J_Base):
    is_list = True

    def __init__(self, values):
        self._list = []
        for v in values:
            assert isinstance(v, J_Base)
            self._list.append(v)

    def list_value(self):
        return self._list


class J_Num(J_Base):
    is_num = True

    def __init__(self, str):
        try:
            self._val = int(str)
        except:
            try:
                self._val = float(str)
            except:
                raise Exception("Number type not supported")

    def num_value(self):
        return self._val


j_true = J_Bool(True)
j_false = J_Bool(False)


class JSONParser(PackratParser):
    """
    IGNORE:
        ` |\n|\t`;

    STRING:
        IGNORE*
        c = `\\"[^\\\\"]*\\"`
        IGNORE*
        return {J_String(unquote(c))};

    NUMBER:
        IGNORE*
        c = `\-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][\+\-]?[0-9]+)?`
        IGNORE*
        return {J_Num(c)};

    TRUE:
        'true'
        return {j_true};

    FALSE:
        'false'
        return {j_false};

    array_values:
        r = array_values
        IGNORE*
        ','
        IGNORE*
        v = value
        return {r+[v]}
        | v = value
          IGNORE*
          return {[v]}
        | return {[]};

    array:
        IGNORE*
        '['
        IGNORE*
        c = array_values
        ']'
        return {J_List(c)};

    entry:
        s = STRING
        IGNORE*
        ':'
        IGNORE*
        v = value
        return {Entry(s, v)};

    entries:
        e = entry
        ','
        IGNORE*
        r = entries
        return {[e] + r}
        | e = entry
          return {[e]};

    obj:
        IGNORE*
        '{'
        IGNORE*
        e = entries
        IGNORE*
        '}'
        return {J_Object(e)};

    value:
        STRING
        | NUMBER
        | TRUE
        | FALSE
        | obj
        | array;
    """


def loads(str):
    p = JSONParser(str)
    try:
        v = p.value()
        return v
    except:
        raise Exception("Could not parse JSON")