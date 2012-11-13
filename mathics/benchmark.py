# -*- coding: utf8 -*-

u"""
    Mathics: a general-purpose computer algebra system
    Copyright (C) 2011 Jan Pöschko

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
"""

# How many times each benchmark is performed
TESTS_PER_BENCHMARK = 1000

# Mathics expressions to benchmark
BENCHMARKS = {
    'Arithmetic': ['1 + 2', '5 * 3'],
    'Trig': ['Sin[RandomReal[]]']
}

import sys
import time
from argparse import ArgumentParser

from mathics.core.parser import parse, TranslateError
from mathics.core.definitions import Definitions
from mathics.core.expression import Evaluation
from mathics import get_version_string, settings

definitions = Definitions(add_builtin=True)
evaluation = None

def timeit(expr, repeats=TESTS_PER_BENCHMARK):
        print "  '{}'".format(str(expr))
        start = time.clock()
        for i in xrange(repeats):
            expr.evaluate(evaluation)
        stop = time.clock()
        average_time = (stop - start) / repeats
        print "    {} loops, avg: {}s per loop, best: {}s per loop".format(repeats, average_time, 1)
        return

def benchmark_parse(section_name):
    pass
    #TODO

def benchmark_format(section_name):
    pass
    #TODO

def benchmark_expression(expression_string):
    expr = parse(expression_string)
    timeit(expr)
    return

def benchmark_section(section_name):
    print "\n{}".format(section_name)
    for benchmark in BENCHMARKS.get(section_name):
        benchmark_expression(benchmark)
    return

def benchmark_all():
    for section_name in BENCHMARKS.keys():
        benchmark_section(section_name)

def main():
    global evaluation 
    parser = ArgumentParser(description="Mathics benchmark suite.", add_help=False)
    parser.add_argument('--help', '-h', help='show this help message and exit', action='help')
    parser.add_argument('--version', '-v', action='version', version='%(prog)s ' + settings.VERSION)
    parser.add_argument('--section', '-s', dest="section", metavar="SECTION", help="only test SECTION")
    parser.add_argument('--expression', '-e', dest="expression", metavar="EXPRESSION", help="benchmark a valid Mathics expression")
    args = parser.parse_args()
    
    try:
        evaluation = Evaluation("", definitions, catch_interrupt=False)
    except Exception, exc:
        print "Exception {}".format(exc)
        info = sys.exc_info()
        sys.excepthook(*info)
        sys.exit(-1)

    if args.expression:
        benchmark_expression(args.expression)
    elif args.section:
        benchmark_section(args.section)
    else:
        benchmark_all()

if __name__ == '__main__':
    main()
