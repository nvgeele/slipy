import json, os
from rpython.rlib.objectmodel import we_are_translated
from slipy import AST


# TODO: Fix it conceptually
# The *real* reader should pass a string to Racket and get
# Scheme data in the form of JSON back. Then that JSON data
# should be transformed to Python data by SliPy.
# When data has to be evaluated, it should be passed to
# another Racket function; the expander.
# Now the reader does BOTH in ONE STEP.


def _open_reader_python():
    from subprocess import Popen, PIPE
    file_path = os.path.dirname(os.path.realpath(__file__))
    reader_path = os.path.join(file_path, "read.rkt")
    cmd = "racket -l racket/base -t %s -e \"(read-loop)\"" % (reader_path)
    process = Popen(cmd, shell=True, stdin=PIPE, stdout=PIPE, stderr=PIPE)
    return process


def _open_reader_rpython():
    from rpython.rlib.rfile import create_popen_file
    cmd = "racket -l racket/base -t %s -e \"(read-loop)\"" % "read.rkt"
    if not os.access("read.rkt", os.R_OK):
        raise Exception("Racket reader can not be accessed")
    pipe = create_popen_file(cmd, "w")
    return pipe


class ReaderPython(object):
    def __init__(self):
        self.proc = _open_reader_python()

    def _restart(self):
        self.proc = _open_reader_python()

    def _assure_running(self):
        if not(self.is_running()):
            self._restart()
            self._assure_running()

    def _try_read(self, str):
        self._assure_running()

        self.proc.stdin.write(str.encode("utf-8"))
        self.proc.stdin.write("\n\0\n")
        self.proc.stdin.flush()

        if not(self.is_running()):
            raise Exception("Reader died whilst reading")

        return self.proc.stdout.readline()

    def is_running(self):
        self.proc.poll()
        return not(self.proc.returncode)

    def read(self, str):
        raw = self._try_read(str)
        data = json.loads(raw)
        if data['success']:
            return data['content']
        else:
            raise Exception("Read error: %s" % data['content'])

    def terminate(self):
        self.proc.terminate()


# TODO: Fix this code ASAP
class ReaderRPython(object):
    def __init__(self):
        self._pipe = _open_reader_rpython()

    def _try_read(self, str):
        self._pipe.write(str.encode("utf-8"))
        self._pipe.write("\n\0\n")
        self._pipe.flush()
        return self._pipe.read()

    def read(self, str):
        raw = self._try_read(str)
        data = json.loads(raw)
        if data['success']:
            return data['content']
        else:
            raise Exception("Read error")


_reader = ReaderRPython() if we_are_translated() else ReaderPython()


def read_string(str):
    json_data = _reader.read(str)
    return json_data