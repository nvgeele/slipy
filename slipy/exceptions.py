from rpython.rlib.objectmodel import we_are_translated


class SlipException(Exception):
    def __init__(self, msg):
        if not we_are_translated():
            super(SlipException, self).__init__(msg)
        self.message = msg


class EvaluationFinished(Exception):
    # TODO: Fix me maybe
    def __init__(self, val):
        # msg = "Evaluation finished with: %s" % val
        # super(EvaluationFinished, self).__init__(msg)
        # self.msg = msg
        self.value = val