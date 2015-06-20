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
        init_reader()
        initialize_global_env()
        data = expand_file(argv[1])
        ast = parse_ast(data)
        print "<< SliPy >>"
        print interpret_with_global(ast).to_string()
    except SlipException, e:
        print "Slip error: %s" % e.message
        raise
    except Exception, e:
        print "Caught an exception!"
        raise

    return 0


def target(*args):
    return main, None


if __name__ == '__main__':
    import sys
    main(sys.argv)