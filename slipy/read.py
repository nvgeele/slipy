import json, os
from subprocess import Popen, PIPE

def _open_reader_process():
    file_path = os.path.dirname(os.path.realpath(__file__))
    reader_path = os.path.join(file_path, "read.rkt")
    cmd = "racket -l racket/base -t %s -e \"(read-loop)\"" % (reader_path)
    process = Popen(cmd, shell=True, stdin=PIPE, stdout=PIPE, stderr=PIPE)
    return process

class Reader(object):
    def __init__(self):
        self.proc = _open_reader_process()

    def _restart(self):
        self.proc = _open_reader_process()

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
            raise Exception("Read error")

    def terminate(self):
        self.proc.terminate()
