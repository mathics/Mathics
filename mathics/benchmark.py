#!/usr/bin/env python3
# -*- coding: utf-8 -*-


import time
from argparse import ArgumentParser


try:
    from statistics import mean
    from statistics import median_low as median
except ImportError:
    mean = lambda l: sum(l) / len(l)
    median = lambda l: sorted(l)[len(l) // 2]


import mathics
from mathics.core.parser import parse, MathicsMultiLineFeeder, MathicsSingleLineFeeder
from mathics.core.definitions import Definitions
from mathics.core.evaluation import Evaluation


# Default number of times to repeat each benchmark. None -> Automatic
TESTS_PER_BENCHMARK = None


# Mathics expressions to benchmark
BENCHMARKS = {
    "Arithmetic": ["1 + 2", "5 * 3"],
    "Plot": [
        "Plot[0, {x, -3, 3}]",
        "Plot[x^2 + x + 1, {x, -3, 3}]",
        "Plot[Sin[Cos[x^2]], {x, -3, 3}]",
        "Plot[Sin[100 x], {x, -3, 3}]",
    ],
    "Plot3D": [
        "Plot3D[0, {x, -1, 1}, {y, -1, 1}]",
        "Plot3D[x + y^2, {x, -3, 3}, {y, -2, 2}]",
        "Plot3D[Sin[x + y^2], {x, -3, 3}, {y, -3, 3}]",
        "Plot3D[Sin[100 x + 100 y ^ 2], {x, 0, 1}, {y, 0, 1}]",
    ],
    "DensityPlot": ["DensityPlot[x + y^2, {x, -3, 3}, {y, -2, 2}]"],
    "Trig": ["Sin[RandomReal[]]", "ArcTan[RandomReal[]]"],
    "Random": [
        "RandomInteger[{-100, 100}, 100]",
        "RandomInteger[10, {10, 10}]",
        "RandomInteger[{0,1}, {5, 5, 5}]",
        "RandomReal[1, 100]",
        "RandomReal[{-1, 1}, 100]",
        "RandomComplex[2 + I, 50]",
        "RandomComplex[{-1 - I, 1 + I}, {10, 10}]",
    ],
    "Expand": [
        "Expand[(a1+a2)^200]",
        "Expand[(a1+a2+a3)^25]",
        "Expand[(a1+a2+a3+a4+a5+a6+a7)^3]",
    ],
    "Matrix": [
        "RandomInteger[{0,1}, {10,10}] . RandomInteger[{0,1}, {10,10}]",
        "RandomInteger[{0,10}, {10,10}] + RandomInteger[{0,10}, {10,10}]",
    ],
}

DEPTH = 300

PARSING_BENCHMARKS = [
    "+".join(map(str, range(1, DEPTH))),
    ";".join(map(str, range(1, DEPTH))),
    "/".join(map(str, range(1, DEPTH))),
    "^".join(map(str, range(1, DEPTH))),
    "! " * DEPTH + "expr",
    "!" * DEPTH + "expr",
    "expr" + "& " * DEPTH,
    "Sin[" * DEPTH + "0.5" + "]" * DEPTH,
]

definitions = Definitions(add_builtin=True)
evaluation = Evaluation(definitions=definitions, catch_interrupt=False)


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
            times.append(time.process_time())
            func()
    else:
        # Automatic number of repeats
        repeats = 10000
        for i in range(repeats):
            times.append(time.process_time())
            func()
            if (i + 1) in (5, 10, 100, 1000, 5000):
                if times[-1] > times[0] + 1:
                    repeats = i + 1
                    break

    times.append(time.process_time())

    times = [times[i + 1] - times[i] for i in range(repeats)]

    average_time = format_time_units(mean(times))
    best_time = format_time_units(min(times))
    median_time = format_time_units(median(times))

    print(
        "    {0:5n} loops, avg: {1}, best: {2}, median: {3} per loop".format(
            repeats, average_time, best_time, median_time
        )
    )


def truncate_line(string):
    if len(string) > 70:
        return string[:70] + "..."
    return string


def benchmark_parse(expression_string):
    print("  '{0}'".format(truncate_line(expression_string)))
    timeit(lambda: parse(definitions, MathicsSingleLineFeeder(expression_string)))


def benchmark_parse_file(fname):
    try:
        import urllib.request
    except ImportError:
        print("install urllib for Combinatorica parsing test")
        return
    print("  '{0}'".format(truncate_line(fname)))
    with urllib.request.urlopen(fname) as f:
        code = f.read().decode("utf-8")

    def do_parse():
        feeder = MathicsMultiLineFeeder(code)
        while not feeder.empty():
            parse(definitions, feeder)

    timeit(do_parse)


def benchmark_parser():
    print("PARSING BENCHMARKS:")
    for expression_string in PARSING_BENCHMARKS:
        benchmark_parse(expression_string)
    benchmark_parse_file(
        "http://www.cs.uiowa.edu/~sriram/Combinatorica/NewCombinatorica.m"
    )


def benchmark_format(expression_string):
    print("  '{0}'".format(expression_string))
    expr = parse(definitions, MathicsSingleLineFeeder(expression_string))
    timeit(lambda: expr.default_format(evaluation, "FullForm"))


def benchmark_expression(expression_string):
    print("  '{0}'".format(expression_string))
    expr = parse(definitions, MathicsSingleLineFeeder(expression_string))
    timeit(lambda: expr.evaluate(evaluation))


def benchmark_section(section_name):
    print(section_name)
    for benchmark in BENCHMARKS.get(section_name):
        benchmark_expression(benchmark)
    print()


def benchmark_all_sections():
    print("EVALUATION BENCHMARKS:")
    for section_name in sorted(BENCHMARKS.keys()):
        benchmark_section(section_name)


def main():
    global evaluation, TESTS_PER_BENCHMARK
    parser = ArgumentParser(description="Mathics benchmark suite.", add_help=False)

    parser.add_argument(
        "--help", "-h", help="show this help message and exit", action="help"
    )

    parser.add_argument(
        "--version", "-v", action="version", version="%(prog)s " + mathics.__version__
    )

    parser.add_argument(
        "--section", "-s", dest="section", metavar="SECTION", help="only test SECTION"
    )

    parser.add_argument("-p", "--parser", action="store_true", help="only test parser")

    parser.add_argument(
        "--expression",
        "-e",
        dest="expression",
        metavar="EXPRESSION",
        help="benchmark a valid Mathics expression",
    )

    parser.add_argument(
        "--number",
        "-n",
        dest="repeat",
        metavar="REPEAT",
        help="loop REPEAT number of times",
    )

    args = parser.parse_args()

    if args.repeat is not None:
        TESTS_PER_BENCHMARK = int(args.repeat)

    if args.expression:
        benchmark_expression(args.expression)
    elif args.section:
        benchmark_section(args.section)
    elif args.parser:
        benchmark_parser()
    else:
        benchmark_all_sections()
        benchmark_parser()


if __name__ == "__main__":
    main()
