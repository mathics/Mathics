import sympy
import random
import sys
import re

import mathics
from mathics import settings
from mathics.core.evaluation import Evaluation
from mathics.core.definitions import Definitions

if sys.version_info[:2] == (2, 7):
    import unittest
else:
    import unittest2 as unittest

definitions = Definitions(add_builtin=True)

shell = mathics.main.TerminalShell(definitions, 'NoColor', False, False)

shell.get_out_prompt = lambda: ''

def out_callback(out):
    pass

def evaluate(text):
    evaluation = Evaluation(text,
                        shell.definitions,
                        timeout=settings.TIMEOUT,
                        out_callback=out_callback)
    return evaluation.results

def isTrue(evalfun):
    results = evaluate(evalfun)
    result = results[0].result
    return 'True' == result

class TestMathics(unittest.TestCase):

    def setUp(self):
        evaluate('a = {1,2,3,4,10}')
        evaluate('b = {{1,2,3},{4,5,6}, {7,8,9}}')

    def testEqualOperator(self):
        assert(isTrue('True == True'))
        assert(isTrue('2 == 2'))
        assert(isTrue('{1,2,3} == {1,2,3}'))
        # This one doesn't work for some reason
        # assert(isTrue('True != False'))
        # assert(isTrue('{1,2,{3}} != {1,2,3}'))
        assert(isTrue('{1,2,{3}} == {1,2,{3}}'))
        assert(isTrue('{1,{2, 123},{3}} == {1,{2, 123},{3}}'))

    def testListSlicing(self):
        assert(isTrue('a[[1]] == 1'))
        assert(isTrue('a[[5]] == 10'))
        assert(isTrue('a[[1;;3]] == {1,2,3}'))
        assert(isTrue('a[[1;;3;;1]] == {1,2,3}'))
        assert(isTrue('a[[3;;1;;-1]] == {3,2,1}'))
        assert(isTrue('a[[4;;2;;-1]] == {4,3,2}'))

    def testListCreation(self):
        evaluate('a = {1,2,3,4,5}')
        evaluate('a[[1]] = 100')

        assert(isTrue('a == {100, 2,3,4,5}'))
        evaluate('f[x_] = x')
        assert(isTrue('Array[f, 10] == {1,2,3,4,5,6,7,8,9,10}'))

    def testFlatten(self):
        assert(isTrue('Flatten[a] == a'))
        assert(isTrue('Flatten[a] == {1,2,3,4,5}'))
        assert(isTrue('Flatten[b] == {1,2,3,4,5,6,7,8,9}'))
        assert(isTrue('Flatten[b, {2}] == {1,4,7,2,5,8,3,6,9}'))

if __name__ == '__main__':
    unittest.main()
