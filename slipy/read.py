import os
from slipy.json import loads
from rpython.rlib.objectmodel import we_are_translated


# def _open_reader_python():
#     from subprocess import Popen, PIPE
#     file_path = os.path.dirname(os.path.realpath(__file__))
#     reader_path = os.path.join(file_path, "read.rkt")
#     cmd = "racket -l racket/base -t %s -e \"(read-loop)\"" % (reader_path)
#     process = Popen(cmd, shell=True, stdin=PIPE, stdout=PIPE, stderr=PIPE)
#     return process
#
#
# class ReaderPython(object):
#     def __init__(self):
#         self.proc = _open_reader_python()
#
#     def _restart(self):
#         self.proc = _open_reader_python()
#
#     def _assure_running(self):
#         if not(self.is_running()):
#             self._restart()
#             self._assure_running()
#
#     def _try_read(self, str):
#         self._assure_running()
#
#         self.proc.stdin.write(str.encode("utf-8"))
#         self.proc.stdin.write("\n\0\n")
#         self.proc.stdin.flush()
#
#         if not(self.is_running()):
#             raise Exception("Reader died whilst reading")
#
#         return self.proc.stdout.readline()
#
#     def is_running(self):
#         self.proc.poll()
#         return not(self.proc.returncode)
#
#     # TODO: Maybe those %s need to be between double quotes
#     def read(self, str):
#         raw = self._try_read("(read %s)" % str)
#         data = json.loads(raw)
#         if data['success']:
#             return data['content']
#         else:
#             raise Exception("Read error: %s" % data['content'])
#
#     def expand(self, str):
#         raw = self._try_read("(expand %s)" % str)
#         data = json.loads(raw)
#         if data['success']:
#             return data['content']
#         else:
#             raise Exception("Read error: %s" % data['content'])
#
#     def terminate(self):
#         self.proc.terminate()


_reader = None#ReaderRPython() #if we_are_translated() else ReaderPython()


from rpython.rlib import streamio
from rpython.rlib.rfile import create_popen_file


def _call_reader_rpython(modus, input):
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


def _parse_json_file(path):
    f = streamio.open_file_as_stream(path)
    s = f.readall()
    f.close()
    os.remove(path)
    json_data = loads(s)

    # if not json_data.is_object:
    #     raise Exception("Read error")
    # else:
    #     json_data = json_data.value_object()

    assert json_data.is_object
    json_data = json_data.object_value()

    if json_data['success']:
        return json_data['content']
    else:
        raise Exception("Read error")

def read_string(str):
    json_file = _call_reader_rpython("read", str)
    json_data = _parse_json_file(json_file)
    return json_data


def expand_string(str):
    json_file = _call_reader_rpython("expand", str)
    json_data = _parse_json_file(json_file)
    return json_data