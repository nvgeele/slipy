from slipy.parse import parse_ast
from slipy.interpreter import interpret_with_env, initialize_global_env
from slipy.read import expand_string
from slipy.util import raw_input


def main(argv):
    # TODO: Expand file
    # TODO: Top-level env join with varlet
    env = initialize_global_env()
    # while True:
    #     str = raw_input(">>> ")
    #     #str = fd.readline()
    #     data = expand_string(str)
    #     #print "Data: "
    #     #print data
    #     ast = parse_ast(data)
    #     #print "AST:"
    #     #print ast
    #     #sys.stdout.write("AST: %s\n" % ast)
    #     try:
    #         val = interpret_with_env(ast, env)
    #         #sys.stdout.write("-> %s\n" % val)
    #         print "-> %s" % val
    #     except SlipException as e:
    #         # TODO: FIXME
    #         print "Slip error: %s" % "FIXME" #e.msg
    #     except Exception as e:
    #         print e

    # input = raw_input(">>> ")
    # input = "((displayln \"WOOO\"))"
    # input = "((define(repl)(display \">>> \")(let((input(read)))(displayln(eval input)))(repl))(repl))"
    # input = "((read))"
    input = "((+ (read) (read)))"
    data = expand_string(input)
    # print data
    ast = parse_ast(data)
    # print ast
    # print env
    print interpret_with_env(ast, env).to_string()

    return 0


def target(*args):
    return main, None


if __name__ == '__main__':
    import sys
    main(sys.argv)
