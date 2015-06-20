from rpython.rlib.objectmodel import we_are_translated


class SlipException(Exception):
    def __init__(self, msg):
        if not we_are_translated():
            super(SlipException, self).__init__(msg)
        self.message = msg


class EvaluationFinished(Exception):
    def __init__(self, val):
        if not we_are_translated():
            msg = "Evaluation finished with: %s" % val.to_string()
            super(EvaluationFinished, self).__init__(msg)
        self.value = val