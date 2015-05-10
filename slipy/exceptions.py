class SlipException(Exception):
    def __init__(self, msg):
        # In Pycket they use we_are_translated for some reason
        super(SlipException, self).__init__(msg)
        self.msg = msg


class EvaluationFinished(Exception):
    # TODO: Fix me maybe
    def __init__(self, val):
        msg = "Evaluation finished with: %s" % val
        super(EvaluationFinished, self).__init__(msg)
        self.msg = msg
        self.val = val