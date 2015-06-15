from slipy.exceptions import SlipException
from slipy.parse import parse_ast
from slipy.interpreter import interpret_with_global, initialize_global_env
from slipy.read import expand_string, expand_file, init_reader
from rpython.rlib.objectmodel import we_are_translated


# TODO: Cache ASTs of expanded files to disk?


def main(argv):
    # TODO: Top-level env join with varlet

    if not len(argv) == 2:
        print "Please provide a file as argument!"
        return 1

    try:
        # from slipy.natives import simple_natives
        # for s in simple_natives:
        #     print s.to_string()

        init_reader()
        initialize_global_env()

        # input = "((define(repl)(display \">>> \")(let((input(read)))(displayln(eval input)))(repl))(repl))"
        # input = "((+ 1 2)))"
        # data = expand_string(input)

        data = expand_file(argv[1])
        ast = parse_ast(data)
        print "<< SliPy >>"

        # print ">>> Old"
        # from slipy.mb_old import mb_ast
        # ast = mb_ast
        # print interpret_with_env(ast, env).to_string()
        #
        # print ">>> New"
        # from slipy.mb_new import mb_ast
        # ast = mb_ast
        # print interpret_with_env(ast, env).to_string()
        #
        # print ">>> Newer"
        # from slipy.mb import mb_ast
        # ast = mb_ast

        # print interpret_with_env(ast, env).to_string()
        print interpret_with_global(ast).to_string()
    except SlipException, e:
        print "Slip error: %s" % e.message
        raise
    except Exception, e:
        print "Caught an exception!"
        raise
        # if we_are_translated():
        #     raise
        # else:
        #     print e

    return 0


def target(*args):
    return main, None


if __name__ == '__main__':
    import sys
    main(sys.argv)