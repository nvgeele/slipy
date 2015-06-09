from slipy.parse import parse_ast
from slipy.interpreter import interpret_with_env, initialize_global_env
from slipy.read import expand_string
from slipy.util import raw_input


def main(argv):
    # TODO: Expand file
    # TODO: Top-level env join with varlet
    env = initialize_global_env()
    # input = "((define(repl)(display \">>> \")(let((input(read)))(displayln(eval input)))(repl))(repl))"
    input = "((+ (read) (read)))"
    data = expand_string(input)
    ast = parse_ast(data)
    print interpret_with_env(ast, env).to_string()
    return 0


def target(*args):
    return main, None


if __name__ == '__main__':
    import sys
    main(sys.argv)
