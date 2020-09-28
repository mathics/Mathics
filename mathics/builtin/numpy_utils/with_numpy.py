#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
A couple of helper functions for doing numpy-like stuff with numpy.
"""

from mathics.core.expression import Expression
from functools import reduce
import numpy
import ast
import inspect
import sys


#
# INTERNAL FUNCTIONS
#


def _promote(x, shape):
    if isinstance(x, (int, float)):
        data = numpy.ndarray(shape)
        data.fill(x)
        return data
    else:
        return x


def _is_scalar(x):
    return not isinstance(x, (list, tuple, numpy.ndarray))


#
# ARRAY CREATION AND REORGANIZATION: STACK, UNSTACK, CONCAT, ...
#


def array(a):
    return numpy.array(a)


def unstack(a):
    a = array(a)
    b = array(numpy.split(a, a.shape[-1], axis=-1))
    if b.shape[-1] == 1:
        b = b.reshape(b.shape[:-1])
    return b


def stack(*a):
    # numpy.stack with axis=-1 stacks arrays along the most inner axis:

    # e.g. numpy.stack([ [1, 2], [3, 4] ], axis=-1)
    # gives: array([ [1, 3], [2, 4] ])

    # e.g. numpy.stack([ [[1, 2], [3, 4]], [[4, 5], [6, 7]] ], axis=-1)
    # gives: array([[[1, 4], [2, 5]], [[3, 6], [4, 7]]])

    a = [array(x) for x in a]
    b = numpy.stack(a, axis=-1)
    return b


def stacked(f, a):
    a = array(a)
    unwrap = False
    if len(a.shape) == 1:
        a = array([a])
        unwrap = True
    components = unstack(a)
    result = f(*components)
    result = stack(*result)
    if unwrap:
        result = result[0]
    return result


def concat(*a):
    a = [array(x) for x in a]
    a = [x for x in a if x.shape[0]]  # skip empty
    return numpy.concatenate(a, axis=-1)


def vectorize(a, depth, f):
    return f(a)


#
# MATHEMATICAL OPERATIONS
#


def clip(a, t0, t1):
    return numpy.clip(array(a), t0, t1)


def dot_t(u, v):
    return numpy.dot(array(u), array(v).T)


def mod(a, b):
    return numpy.mod(a, b)


def sin(a):
    return numpy.sin(array(a))


def cos(a):
    return numpy.cos(array(a))


def arctan2(y, x):
    return numpy.arctan2(array(y), array(x))


def sqrt(a):
    return numpy.sqrt(array(a))


def floor(a):
    return numpy.floor(array(a))


def maximum(*a):
    return reduce(numpy.maximum, [array(x) for x in a])


def minimum(*a):
    return reduce(numpy.minimum, [array(x) for x in a])


#
# PUBLIC HELPER FUNCTIONS
#


def is_numpy_available():
    return True


def allclose(a, b):
    return numpy.allclose(array(a), array(b))


def errstate(**kwargs):
    return numpy.errstate(**kwargs)


def instantiate_elements(a, new_element, d=1):
    # given a numpy array 'a' and a python element constructor 'new_element', generate a python array of the
    # same shape as 'a' with python elements constructed through 'new_element'. 'new_element' will get called
    # if an array of dimension 'd' is reached.

    if len(a.shape) == d:
        leaves = [new_element(x) for x in a]
    else:
        leaves = [instantiate_elements(e, new_element, d) for e in a]
    return Expression("List", *leaves)


#
# CONDITIONALS AND PROGRAM FLOW
#

# @conditional is an annotation that basically invoked a mini compiler in order to compile numpy
# conditional expressions from code that would run as regular Python code in the no-numpy case.
# the idea is to have readable Python code that states what happens in both numpy and no-numpy
# cases. the alternative would be using wrapper functions that are kind of ugly and hard to read
# for the numpy case.
#
# as an example, take the following code:
#
# @conditional
# def f(a):
#    if a > 10:
#        return a + 1.5
#    elif a > 9:
#        return 7
#    else:
#        return 2
#
# if numpy is not available, f() will just take one scalar and work as expected. if numpy is
# available, @conditional will recompile the function into something that allows "a" (or any
# other parameters of f) to be numpy arrays.
#
# this is necessary, as for numpy arrays, there is no single execution branch in conditionals
# as above, since the conditional has to be evaluated for each element of the numpy array.
# internally, in the numpy case, the function above will be transformed to something like:
#
#   a[a > 10] = a + 1.5
#   ...
#
# in general, and to make this transformation as simple as possible, @conditional expects the
# function that is annotated with it to be of the following restrained form:
#
# if [binary comparisons or variable]:
#   return [expression1]
# elif [binary comparisons or variable]:  # optional
#   return [expression2]
# else:  # optional
#   return [expression3]
#
# all relevant rules for @conditional functions are:
#   - all "if" branches must exit immediately with "return".
#   - "if"s must rely on simple binary comparisons, e.g. "b < 4" or "4 > b", or variables
#   - the occurence of "elif" is optional, as is the occurence of "else"
#   - if "else" is not provided, the provided "if" cases must cover all possible cases,
#     otherwise there will be undefined results.
#   - code in @conditional must not reference global variables.
#
# if a function does not adhere to the rules described above, a MalformedConditional is thrown when
# constructing the @vectorized function.


def choose(i, *options):
    assert options
    dim = len(options[0])
    columns = [[o[d] for o in options] for d in range(dim)]
    if isinstance(i, (int, float)):
        return [column[int(i)] for column in columns]  # int cast needed for PyPy
    else:
        assert len(options) < 256
        i_int = array(i).astype(numpy.uint8)
        return [numpy.choose(i_int, column) for column in columns]


_else_case_id = "else_case"


def _numpy_conditional(shape_id, *paths):
    # called during runtime when we actually evaluate a conditional.

    if _is_scalar(shape_id):
        for test, args, comp in paths:
            if test == _else_case_id or test():
                return comp(*args)
        assert False  # one case must be true

    shape = shape_id.shape
    result = numpy.ndarray(shape)

    has_else = paths[-1][0] == _else_case_id
    if_paths = paths[:-1] if has_else else paths
    masks = [path[0]() for path in if_paths]

    def efficient_masks():
        rest = ~masks[0]
        yield masks[0]
        for mask in masks[1:]:
            yield rest & mask
            rest &= ~mask

    if has_else:
        if len(paths) == 2:  # simple case: just 1 if, and 1 else
            masks.append(~masks[0])
        else:  # n ifs, and 1 else
            masks.append(numpy.ones(masks[0].shape, dtype=bool))
            masks = list(efficient_masks())
    else:
        masks = list(efficient_masks())

    # process in reverse order in order to reflect order that is written
    # down in Python code. for example, writing
    #
    # if a > 5:
    #   b = True
    # elif a > 4:
    #   b = False
    #
    # in Python, will always set b to True, if a > 5. if we evaluated the
    # statements above in non-reversed order for numpy, for some element
    # a > 5, we would first set b = True, and then b = False, which would
    # be wrong.

    for mask, path in zip(reversed(masks), reversed(paths)):
        test, args, comp = path
        result[mask] = comp(*[x if _is_scalar(x) else x[mask] for x in args])
    return result


class MalformedConditional(Exception):
    def __init__(self, func, node, error):
        Exception.__init__(
            self, "in function %s in line %d: %s" % (func.__name__, node.lineno, error)
        )


class _NameCollector(ast.NodeVisitor):
    def __init__(self):
        self.names = set()

    def visit_Name(self, node):
        assert isinstance(node.ctx, ast.Load)
        self.names.add(node.id)


def _create_ast_lambda(names, body):
    if sys.version_info >= (3, 0):  # change in AST structure for Python 3
        inner_args = [ast.arg(arg=name, annotation=None) for name in names]
        if sys.version_info >= (3, 8):
            args = ast.arguments(
                args=inner_args,
                posonlyargs=[],
                vararg=None,
                kwonlyargs=[],
                kw_defaults=[],
                kwarg=None,
                defaults=[],
            )
        else:
            args = ast.arguments(
                args=inner_args,
                vararg=None,
                kwonlyargs=[],
                kw_defaults=[],
                kwarg=None,
                defaults=[],
            )
    else:
        args = ast.arguments([ast.Name(id=name, ctx=ast.Load()) for name in names])

    return ast.Lambda(args=args, body=body)


def _expression_lambda(node):
    # convert some expression, e.g. b + a * 2 + b * 7, into a lambda with with all values used in the
    # expression as parameters, e.g. lambda a, b: b + a * 2 + b * 7

    collector = _NameCollector()
    collector.visit(node)
    names = list(collector.names)
    elements = [ast.Name(id=name, ctx=ast.Load()) for name in names]

    # return (1) a tuple of all the variable names in the expression, (2) a lambda that takes values
    # for these variables as parameters and evaluates the expression.
    return ast.Tuple(elts=elements, ctx=ast.Load()), _create_ast_lambda(names, node)


class _ConditionalTransformer(ast.NodeTransformer):
    def __init__(self, f):
        self._func = f

    def transform(self):
        tree = ast.parse(inspect.getsource(self._func))
        self._expect(tree, "function not a Module", type(tree), ast.Module)
        self._expect(tree, "Module body too large", len(tree.body), 1)
        self._expect(tree, "FunctionDef not found", type(tree.body[0]), ast.FunctionDef)

        func_def = tree.body[0]
        func_name = func_def.name
        self._expect(func_def, "FunctionDef body too large", len(func_def.body), 1)
        self._expect(
            func_def, 'function must start with "if"', type(func_def.body[0]), ast.If
        )

        tree = self.visit(tree)
        tree = ast.fix_missing_locations(tree)
        code = compile(tree, "<conditional:%s>" % func_name, "exec")

        data = {}
        eval(code, globals(), data)
        return data[func_name]

    def _expect(self, node, error, value, expected):
        if value != expected:
            raise MalformedConditional(
                self._func, node, "%s (%s != %s)" % (error, expected, value)
            )

    def visit_FunctionDef(self, node):
        assert len(node.decorator_list) == 1  # we expect that we are the only decorator
        assert isinstance(node.decorator_list[0], ast.Name)
        assert node.decorator_list[0].id == "conditional"
        body = [self.visit(x) for x in node.body]
        funcdef = ast.FunctionDef(
            name=node.name, args=node.args, body=body, decorator_list=[]
        )
        if "posonlyargs" in node.args._fields:
            funcdef.posonlyargs = []
        return funcdef

    def visit_If(self, node):
        blocks = []
        tests = []
        shapes = []

        while True:
            body = node.body
            self._expect(
                node, '"if" code body must contain exactly 1 element', len(body), 1
            )
            blocks.append(body[0])

            test = node.test
            tests.append(test)

            if type(test) == ast.Name:
                shapes.append(test)
            elif type(test) == ast.Compare:
                if isinstance(test.left, ast.Name):
                    shapes.append(test.left)
                elif isinstance(test.right, ast.Name):
                    shapes.append(test.right)
                else:
                    MalformedConditional(
                        self._func, test, "expected variable in comparison"
                    )
            else:
                self._expect(
                    test, "expected single comparison or name", type(test), ast.Compare
                )

            or_elses = node.orelse
            if not or_elses:
                break

            self._expect(
                node, '"else" code body must contain 1 element', len(or_elses), 1
            )
            or_else = or_elses[0]

            if isinstance(or_else, ast.If):
                node = or_else
            else:
                blocks.append(or_else)
                tests.append(_else_case_id)
                break

        for block in blocks:
            self._expect(
                block, '"if" blocks must exit with "return"', type(block), ast.Return
            )

        # now build a call to _numpy_conditional() using cond_args as arguments, that will handle
        # the runtime evaluation of the conditional.

        cond_args = [shapes[0]]
        for test, value in zip(tests, (block.value for block in blocks)):
            elements = []
            if test == _else_case_id:
                elements.append(ast.Str(s=test))
            else:
                elements.append(_create_ast_lambda([], test))

            elements.extend(_expression_lambda(value))
            cond_args.append(ast.Tuple(elts=elements, ctx=ast.Load()))

        return ast.Return(
            value=ast.Call(
                func=ast.Name(id="_numpy_conditional", ctx=ast.Load()),
                keywords=[],
                args=cond_args,
            )
        )


def conditional(*args, **kwargs):
    if len(args) == 1 and callable(args[0]):
        f = args[0]  # @conditional without arguments?
    else:
        return lambda f: conditional(f)  # with arguments

    if not inspect.isfunction(f):
        raise Exception("@conditional can only be applied to functions")

    transformer = _ConditionalTransformer(f)
    f_transformed = transformer.transform()

    def wrapper(*a):
        a = [numpy.array(x) if isinstance(x, (list, tuple)) else x for x in a]
        return f_transformed(*a)

    return wrapper
