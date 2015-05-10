from slipy.parse import parse
from slipy.exceptions import *
from slipy.interpreter import interpret_with_env, initialize_global_env
from slipy.read import read_string
import os # For REPL
import sys # Temporary
import traceback # For temporary REPL


def main(argv):
    # TODO: Make let-var env merging possible for REPL
    #stdin_fd = 0
    #fd = os.fdopen(stdin_fd)
    env = initialize_global_env()
    while True:
        str = raw_input(">>> ")
        #str = fd.readline()
        data = read_string(str)
        #print "Data: "
        #print data
        ast = parse(data)
        #print "AST:"
        #print ast
        sys.stdout.write("AST: %s\n" % ast)
        try:
            interpret_with_env(ast, env)
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