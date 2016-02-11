#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import unicode_literals
from __future__ import print_function

# Default number of times to repeat each benchmark. None -> Automatic
from __future__ import absolute_import
from six.moves import map
from six.moves import range
TESTS_PER_BENCHMARK = None

# Mathics expressions to benchmark
BENCHMARKS = {
    'Arithmetic': ['1 + 2', '5 * 3'],
    'Plot': [
        'Plot[0, {x, -3, 3}]',
        'Plot[x^2 + x + 1, {x, -3, 3}]',
        'Plot[Sin[Cos[x^2]], {x, -3, 3}]',
        'Plot[Sin[100 x], {x, -3, 3}]'],
    'Plot3D': [
        'Plot3D[0, {x, -1, 1}, {y, -1, 1}]',
        'Plot3D[x + y^2, {x, -3, 3}, {y, -2, 2}]',
        'Plot3D[Sin[x + y^2], {x, -3, 3}, {y, -3, 3}]',
        'Plot3D[Sin[100 x + 100 y ^ 2], {x, 0, 1}, {y, 0, 1}]'],
    'DensityPlot': [
        'DensityPlot[x + y^2, {x, -3, 3}, {y, -2, 2}]'],
    'Trig': [
        'Sin[RandomReal[]]', 'ArcTan[RandomReal[]]'],
    'Random': [
        'RandomInteger[{-100, 100}, 100]',
        'RandomInteger[10, {10, 10}]',
        'RandomInteger[{0,1}, {5, 5, 5}]',
        'RandomReal[1, 100]', 'RandomReal[{-1, 1}, 100]',
        'RandomComplex[2 + I, 50]',
        'RandomComplex[{-1 - I, 1 + I}, {10, 10}]'],
    'Expand': [
        'Expand[(a1+a2)^200]', 'Expand[(a1+a2+a3)^25]',
        'Expand[(a1+a2+a3+a4+a5+a6+a7)^3]'],
    'Matrix': [
        'RandomInteger[{0,1}, {10,10}] . RandomInteger[{0,1}, {10,10}]',
        'RandomInteger[{0,10}, {10,10}] + RandomInteger[{0,10}, {10,10}]'],
}

PARSING_BENCHMARKS = [
    "+".join(map(str, range(1, 1000))),
    ";".join(map(str, range(1, 1000))),
    "/".join(map(str, range(1, 1000))),
    "^".join(map(str, range(1, 1000))),
    "! " * 1000 + 'expr',
    "!" * 1000 + 'expr',
    'expr' + "& " * 1000,
    "Sin[" * 1000 + '0.5' + "]" * 1000,
]

import sys
import time
from argparse import ArgumentParser

import mathics
from mathics.core.parser import parse
from mathics.core.definitions import Definitions
from mathics.core.evaluation import Evaluation

definitions = Definitions(add_builtin=True)
evaluation = None


def format_time_units(seconds):
    if seconds < 1e-6:
        return "{0:4.3g} ns".format(seconds * 1e9)
    elif seconds < 1e-3:
        return "{0:4.3g} us".format(seconds * 1e6)
    elif seconds < 1:
        return "{0:4.3g} ms".format(seconds * 1e3)
    else:
        return "{0:4.3g} s ".format(seconds)


def timeit(func, repeats=None):
    if repeats is None:
        global TESTS_PER_BENCHMARK
        repeats = TESTS_PER_BENCHMARK

    times = []
    if repeats is not None:
        # Fixed number of repeats
        for i in range(repeats):
            times.append(time.clock())
            func()
    else:
        # Automatic number of repeats
        repeats = 10000
        for i in range(repeats):
            times.append(time.clock())
            func()
            if any(i == j for j in (5, 10, 100, 1000, 5000)):
                if times[-1] > times[0] + 1:
                    repeats = i
                    break

    times.append(time.clock())

    average_time = format_time_units((times[-1] - times[0]) / repeats)
    best_time = format_time_units(
        min([times[i + 1] - times[i] for i in range(repeats)]))
    print("    {0:5n} loops, avg: {1} per loop, best: {2} per loop".format(
        repeats, average_time, best_time))


def truncate_line(string):
    if len(string) > 70:
        return string[:70] + "..."
    return string


def benchmark_parse(expression_string):
    print("  '{0}'".format(truncate_line(expression_string)))
    timeit(lambda: parse(expression_string, definitions))


def benchmark_format(expression_string):
    print("  '{0}'".format(expression_string))
    expr = parse(expression_string, definitions)
    timeit(lambda: expr.default_format(evaluation, "FullForm"))


def benchmark_expression(expression_string):
    print("  '{0}'".format(expression_string))
    expr = parse(expression_string, definitions)
    timeit(lambda: expr.evaluate(evaluation))


def benchmark_section(section_name):
    print(section_name)
    for benchmark in BENCHMARKS.get(section_name):
        benchmark_expression(benchmark)
    print()


def benchmark_all():
    print("EVALUATION BENCHMARKS:")
    for section_name in sorted(BENCHMARKS.keys()):
        benchmark_section(section_name)
    print("PARSING BENCHMARKS:")
    for expression_string in PARSING_BENCHMARKS:
        benchmark_parse(expression_string)


def main():
    global evaluation, TESTS_PER_BENCHMARK
    parser = ArgumentParser(
        description="Mathics benchmark suite.", add_help=False)

    parser.add_argument(
        '--help', '-h', help='show this help message and exit', action='help')

    parser.add_argument(
        '--version', '-v', action='version',
        version='%(prog)s ' + mathics.__version__)

    parser.add_argument(
        '--section', '-s', dest="section", metavar="SECTION",
        help="only test SECTION")

    parser.add_argument(
        '--expression', '-e', dest="expression", metavar="EXPRESSION",
        help="benchmark a valid Mathics expression")

    parser.add_argument(
        '--number', '-n', dest="repeat", metavar="REPEAT",
        help="loop REPEAT number of times")

    args = parser.parse_args()

    try:
        evaluation = Evaluation("", definitions, catch_interrupt=False)
    except Exception as exc:
        print("Exception {0}".format(exc))
        info = sys.exc_info()
        sys.excepthook(*info)
        sys.exit(-1)

    if args.repeat is not None:
        TESTS_PER_BENCHMARK = int(args.repeat)

    if args.expression:
        benchmark_expression(args.expression)
    elif args.section:
        benchmark_section(args.section)
    else:
        benchmark_all()

if __name__ == '__main__':
    main()
