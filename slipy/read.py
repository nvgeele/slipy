import os
from slipy.json import loads
from rpython.rlib.objectmodel import we_are_translated
from rpython.rlib import streamio
from rpython.rlib.rfile import create_popen_file


def _open_reader_python():
    "NOT_RPYTHON"
    from subprocess import Popen, PIPE
    file_path = os.path.dirname(os.path.realpath(__file__))
    reader_path = os.path.join(file_path, "read.rkt")
    cmd = "racket -l racket/base -t %s -e \"(read-loop)\"" % (reader_path)
    process = Popen(cmd, shell=True, stdin=PIPE, stdout=PIPE, stderr=PIPE)
    return process


class ReaderPython(object):
    def __init__(self):
        "NOT_RPYTHON"
        self.proc = _open_reader_python()

    def _restart(self):
        "NOT_RPYTHON"
        self.proc = _open_reader_python()

    def _assure_running(self):
        "NOT_RPYTHON"
        if not(self.is_running()):
            self._restart()
            self._assure_running()

    def _try_read(self, str):
        "NOT_RPYTHON"
        self._assure_running()

        self.proc.stdin.write(str.encode("utf-8"))
        self.proc.stdin.write("\n\0\n")
        self.proc.stdin.flush()

        if not(self.is_running()):
            raise Exception("Reader died whilst reading")

        return self.proc.stdout.readline()

    def is_running(self):
        "NOT_RPYTHON"
        self.proc.poll()
        return not(self.proc.returncode)

    # TODO: Fix code duplication
    # TODO: Maybe those %s need to be between double quotes
    def read(self, str):
        "NOT_RPYTHON"
        raw = self._try_read("(read %s)" % str)
        data = loads(raw)
        assert data.is_object
        data = data.object_value()
        if data['success']:
            return data['content']
        else:
            raise Exception("Read error: %s" % data['content'])

    def expand(self, str):
        "NOT_RPYTHON"
        raw = self._try_read("(expand %s)" % str)
        data = loads(raw)
        assert data.is_object
        data = data.object_value()
        if data['success']:
            return data['content']
        else:
            raise Exception("Read error: %s" % data['content'])

    def terminate(self):
        "NOT_RPYTHON"
        self.proc.terminate()


class ReaderRPython(object):
    def _call_reader_rpython(self, modus, input):
        tmp_file = os.tmpnam()
        cmd = "racket -l racket/base -t %s -e '(do-read \"%s\")'" % ("read.rkt", tmp_file)
        if not os.access("read.rkt", os.R_OK):
            raise Exception("Racket reader can not be accessed")
        pipe = create_popen_file(cmd, "w")
        input = "(%s %s)" % (modus, input)
        # TODO: might go wrong when dealing with UNICODE
        pipe.write(input)
        pipe.write("\n\0\n")
        pipe.flush()
        err = os.WEXITSTATUS(pipe.close())
        if err != 0:
            raise Exception("Reader produced an unexpected error")
        return tmp_file

    def _parse_json_file(self, path):
        f = streamio.open_file_as_stream(path)
        s = f.readall()
        f.close()
        os.remove(path)
        json_data = loads(s)
        assert json_data.is_object
        json_data = json_data.object_value()
        if json_data['success']:
            return json_data['content']
        else:
            raise Exception("Read error")

    def expand(self, str):
        json_file = self._call_reader_rpython("expand", str)
        json_data = self._parse_json_file(json_file)
        return json_data

    def read(self, str):
        json_file = self._call_reader_rpython("read", str)
        json_data = self._parse_json_file(json_file)
        return json_data


# We can't modify global vars, so store mutable global vars in a class
class Reader(object):
    def __init__(self):
        self.reader = None
_reader = Reader()


def read_string(str):
    if we_are_translated():
        assert isinstance(_reader.reader, ReaderRPython)
    return _reader.reader.read(str)


def expand_string(str):
    if we_are_translated():
        assert isinstance(_reader.reader, ReaderRPython)
    return _reader.reader.expand(str)


def expand_file(path):
    if we_are_translated():
        assert isinstance(_reader.reader, ReaderRPython)
    if not os.access(path, os.R_OK):
        raise Exception("Can not read file")
    f = streamio.open_file_as_stream(path)
    s = f.readall()
    f.close()
    return _reader.reader.expand(s)


def init_reader():
    if we_are_translated():
        _reader.reader = ReaderRPython()
    else:
        _reader.reader = ReaderPython()