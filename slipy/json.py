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
    pass


class J_Simple(J_Base):
    def __init__(self, val):
        self._val = val

    def val(self):
        return self._val


class J_String(J_Simple):
    pass


class J_Bool(J_Simple):
    pass


class J_Entry(J_Base):
    def __init__(self, key, val):
        self._key = key
        self._val = val

    def key(self):
        return self._key

    def val(self):
        return self._val


class J_Dict(J_Base):
    def __init__(self, entries):
        self._dict = {}
        for entry in entries:
            self._dict[entry.key()] = entry.val()

    def val(self):
        return self._dict


class J_List(J_Base):
    def __init__(self, values):
        self._list = []
        for v in values:
            assert isinstance(v, J_Base)
            self._list.append(v.val())

    def val(self):
        return self._list


class J_Num(J_Base):
    def __init__(self, str):
        try:
            self._val = int(str)
        except:
            try:
                self._val = float(str)
            except:
                raise Exception("Number type not supported")

    def val(self):
        return self._val


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
        return {J_Bool(True)};

    FALSE:
        'false'
        return {J_Bool(False)};

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
        return {J_Entry(s.val(), v.val())};

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
        return {J_Dict(e)};

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
        #assert isinstance(v, J_Base)
        #v = v.val()
        return v
    except:
        raise Exception("Could not parse JSON")