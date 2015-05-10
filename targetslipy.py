from slipy.parse import parse
from slipy.exceptions import *
from slipy.interpreter import interpret_program
from slipy.read import read_string
import os # For REPL
import traceback # For temporary REPL


def main(argv):
    #stdin_fd = 0
    #fd = os.fdopen(stdin_fd)
    while True:
        str = raw_input(">>> ")
        #str = fd.readline()
        data = read_string(str)
        #print "Data: "
        #print data
        ast = parse(data)
        #print "AST:"
        #print ast
        try:
            interpret_program(ast)
        except EvaluationFinished as e:
            print e.val
        except SlipException as e:
            print "Slip error: %s" % e.msg
        except Exception as e:
            print traceback.format_exc()

    return 0


def target(*args):
    return main, None


if __name__ == '__main__':
    import sys
    main(sys.argv)