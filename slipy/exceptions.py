from rpython.rlib.objectmodel import we_are_translated

#
# Standard error messages
#

arg_count_error = "%s: the correct amount of arguments was not supplied"
arg_types_error = "%s: one or more operators have an incorrect type"

#
# Exceptions for use in Slipy
#

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