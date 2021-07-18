#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# FIXME: combine with same thing in Mathics core
"""
Does 2 things which can either be done independently or
as a pipeline:

1. Extracts tests and runs them from static mdoc files and docstrings from Mathics built-in functions
2. Creates/updates internal documentation data
"""

import os
import pickle
import re
import sys

from argparse import ArgumentParser
from datetime import datetime

import mathics

from mathics.core.definitions import Definitions
from mathics.core.evaluation import Evaluation, Output
from mathics.core.parser import MathicsSingleLineFeeder
from mathics.builtin import builtins_dict

from mathics import version_string
from mathics import settings
from mathics.doc.common_doc import MathicsMainDocumentation

builtins = builtins_dict()


class TestOutput(Output):
    def max_stored_size(self, settings):
        return None


sep = "-" * 70 + "\n"

# Global variables
definitions = None
documentation = MathicsMainDocumentation()
check_partial_enlapsed_time = False
logfile = None


MAX_TESTS = 100000  # Number than the total number of tests


def print_and_log(*args):
    a = [a.decode("utf-8") if isinstance(a, bytes) else a for a in args]
    string = "".join(a)
    print(string)
    if logfile:
        logfile.write(string)


def compare(result, wanted):
    if result == wanted:
        return True
    if result is None or wanted is None:
        return False
    result = result.splitlines()
    wanted = wanted.splitlines()
    if result == [] and wanted == ["#<--#"]:
        return True
    if len(result) != len(wanted):
        return False
    for r, w in zip(result, wanted):
        wanted_re = re.escape(w.strip())
        wanted_re = wanted_re.replace("\\.\\.\\.", ".*?")
        wanted_re = "^%s$" % wanted_re
        if not re.match(wanted_re, r.strip()):
            return False
    return True


stars = "*" * 10


def test_case(test, tests, index=0, subindex=0, quiet=False, section=None):
    global check_partial_enlapsed_time
    test, wanted_out, wanted = test.test, test.outs, test.result

    def fail(why):
        part, chapter, section = tests.part, tests.chapter, tests.section
        print_and_log(
            f"""{sep}Test failed: {section} in {part} / {chapter}
{part}
{why}
""".encode(
                "utf-8"
            )
        )
        return False

    if not quiet:
        if section:
            print(f"{stars} {tests.chapter} / {section} {stars}".encode("utf-8"))
        print(f"{index:4d} ({subindex:2d}): TEST {test}".encode("utf-8"))

    feeder = MathicsSingleLineFeeder(test, "<test>")
    evaluation = Evaluation(definitions, catch_interrupt=False, output=TestOutput())
    try:
        time_parsing = datetime.now()
        query = evaluation.parse_feeder(feeder)
        if check_partial_enlapsed_time:
            print("   parsing took", datetime.now() - time_parsing)
        if query is None:
            # parsed expression is None
            result = None
            out = evaluation.out
        else:
            result = evaluation.evaluate(query)
            if check_partial_enlapsed_time:
                print("   evaluation took", datetime.now() - time_parsing)
            out = result.out
            result = result.result
    except Exception as exc:
        fail("Exception %s" % exc)
        info = sys.exc_info()
        sys.excepthook(*info)
        return False

    time_comparing = datetime.now()
    comparison_result = compare(result, wanted)
    if check_partial_enlapsed_time:
        print("   comparison took ", datetime.now() - time_comparing)
    if not comparison_result:
        print("result =!=wanted")
        fail_msg = "Result: %s\nWanted: %s" % (result, wanted)
        if out:
            fail_msg += "\nAdditional output:\n"
            fail_msg += "\n".join(str(o) for o in out)
        return fail(fail_msg)
    output_ok = True
    time_comparing = datetime.now()
    if len(out) != len(wanted_out):
        output_ok = False
    else:
        for got, wanted in zip(out, wanted_out):
            if not got == wanted:
                output_ok = False
                break
    if check_partial_enlapsed_time:
        print("   comparing messages took ", datetime.now() - time_comparing)
    if not output_ok:
        return fail(
            "Output:\n%s\nWanted:\n%s"
            % ("\n".join(str(o) for o in out), "\n".join(str(o) for o in wanted_out))
        )
    return True


def test_tests(
    tests,
    index,
    quiet=False,
    stop_on_failure=False,
    start_at=0,
    max_tests=MAX_TESTS,
    excludes=[],
):
    definitions.reset_user_definitions()
    total = failed = skipped = 0
    failed_symbols = set()
    section = tests.section
    if section in excludes:
        return total, failed, len(tests.tests), failed_symbols, index
    count = 0
    for subindex, test in enumerate(tests.tests):
        index += 1
        if test.ignore:
            continue
        if index < start_at:
            skipped += 1
            continue
        elif count >= max_tests:
            break

        total += 1
        count += 1
        if not test_case(test, tests, index, subindex + 1, quiet, section):
            failed += 1
            failed_symbols.add((tests.part, tests.chapter, tests.section))
            if stop_on_failure:
                break

        section = None
    return total, failed, skipped, failed_symbols, index


# FIXME: move this to common routine
def create_output(tests, doc_data, format="tex"):
    definitions.reset_user_definitions()
    for test in tests.tests:
        if test.private:
            continue
        key = test.key
        evaluation = Evaluation(
            definitions, format=format, catch_interrupt=True, output=TestOutput()
        )
        try:
            result = evaluation.parse_evaluate(test.test)
        except:  # noqa
            result = None
        if result is None:
            result = []
        else:
            result = [result.get_data()]
        doc_data[key] = {
            "query": test.test,
            "results": result,
        }


def test_chapters(
    chapters: set,
    quiet=False,
    stop_on_failure=False,
    generate_output=False,
    reload=False,
):
    failed = 0
    index = 0
    chapter_names = ", ".join(chapters)
    print(f"Testing chapter(s): {chapter_names}")
    output_data = load_doc_data() if reload else {}
    prev_key = []
    for tests in documentation.get_tests():
        if tests.chapter in chapters:
            for test in tests.tests:
                key = list(test.key)[1:-1]
                if prev_key != key:
                    prev_key = key
                    print(f'Testing section: {" / ".join(key)}')
                    index = 0
                if test.ignore:
                    continue
                index += 1
                if not test_case(test, tests, index, quiet=quiet):
                    failed += 1
                    if stop_on_failure:
                        break
            if generate_output and failed == 0:
                create_output(tests, output_data)

    print()
    if index == 0:
        print_and_log(f"No chapters found named {chapter_names}.")
    elif failed > 0:
        print_and_log("%d test%s failed." % (failed, "s" if failed != 1 else ""))
    else:
        print_and_log("All tests passed.")


def test_sections(
    sections: set,
    quiet=False,
    stop_on_failure=False,
    generate_output=False,
    reload=False,
):
    failed = 0
    index = 0
    section_names = ", ".join(sections)
    print(f"Testing section(s): {section_names}")
    sections |= {"$" + s for s in sections}
    output_data = load_doc_data() if reload else {}
    prev_key = []
    for tests in documentation.get_tests():
        if tests.section in sections:
            for test in tests.tests:
                key = list(test.key)[1:-1]
                if prev_key != key:
                    prev_key = key
                    print(f'Testing section: {" / ".join(key)}')
                    index = 0
                if test.ignore:
                    continue
                index += 1
                if not test_case(test, tests, index, quiet=quiet):
                    failed += 1
                    if stop_on_failure:
                        break
            if generate_output and failed == 0:
                create_output(tests, output_data)

    print()
    if index == 0:
        print_and_log(f"No sections found named {section_names}.")
    elif failed > 0:
        print_and_log("%d test%s failed." % (failed, "s" if failed != 1 else ""))
    else:
        print_and_log("All tests passed.")
    if generate_output and (failed == 0):
        save_doc_data(output_data)


def open_ensure_dir(f, *args, **kwargs):
    try:
        return open(f, *args, **kwargs)
    except (IOError, OSError):
        d = os.path.dirname(f)
        if d and not os.path.exists(d):
            os.makedirs(d)
        return open(f, *args, **kwargs)


def test_all(
    quiet=False,
    generate_output=True,
    stop_on_failure=False,
    start_at=0,
    count=MAX_TESTS,
    texdatafolder=None,
    doc_even_if_error=False,
    excludes=[],
):
    if not quiet:
        print(f"Testing {version_string}")

    if generate_output:
        if texdatafolder is None:
            texdatafolder = settings.DOC_DATA_PATH
    try:
        index = 0
        total = failed = skipped = 0
        failed_symbols = set()
        output_data = {}
        for tests in documentation.get_tests():
            sub_total, sub_failed, sub_skipped, symbols, index = test_tests(
                tests,
                index,
                quiet=quiet,
                stop_on_failure=stop_on_failure,
                start_at=start_at,
                max_tests=count,
                excludes=excludes,
            )
            if generate_output:
                create_output(tests, output_data)
            total += sub_total
            failed += sub_failed
            skipped += sub_skipped
            failed_symbols.update(symbols)
            if sub_failed and stop_on_failure:
                break
            if total >= count:
                break
        builtin_total = len(builtins)
    except KeyboardInterrupt:
        print("\nAborted.\n")
        return

    if failed > 0:
        print(sep)
    if count == MAX_TESTS:
        print_and_log(
            "%d Tests for %d built-in symbols, %d passed, %d failed, %d skipped."
            % (total, builtin_total, total - failed - skipped, failed, skipped)
        )
    else:
        print_and_log(
            "%d Tests, %d passed, %d failed, %d skipped."
            % (total, total - failed, failed, skipped)
        )
    if failed_symbols:
        if stop_on_failure:
            print_and_log("(not all tests are accounted for due to --stop-on-failure)")
        print_and_log("Failed:")
        for part, chapter, section in sorted(failed_symbols):
            print_and_log("  - %s in %s / %s" % (section, part, chapter))

    if generate_output and (failed == 0 or doc_even_if_error):
        save_doc_data(output_data)
        return True

    if failed == 0:
        print("\nOK")
    else:
        print("\nFAILED")
        return sys.exit(1)  # Travis-CI knows the tests have failed


def load_doc_data():
    print(f"Loading internal document data from {settings.DOC_DATA_PATH}")
    with open_ensure_dir(settings.DOC_DATA_PATH, "rb") as doc_data_file:
        return pickle.load(doc_data_file)


def save_doc_data(output_data):
    print(f"Writing internal document data to {settings.DOC_DATA_PATH}")
    with open_ensure_dir(settings.DOC_DATA_PATH, "wb") as output_file:
        pickle.dump(output_data, output_file, 4)


def extract_doc_from_source(quiet=False, reload=False):
    """
    Write internal (pickled) doc files and example data in docstrings.
    """
    if not quiet:
        print(f"Extracting internal doc data for {version_string}")
        print("This may take a while...")

    try:
        output_data = load_doc_data() if reload else {}
        for tests in documentation.get_tests():
            create_output(tests, output_data)
    except KeyboardInterrupt:
        print("\nAborted.\n")
        return

    print("done.\n")
    save_doc_data(output_data)


def main():
    global definitions
    global logfile
    global check_partial_enlapsed_time
    definitions = Definitions(add_builtin=True)

    parser = ArgumentParser(description="Mathics test suite.", add_help=False)
    parser.add_argument(
        "--help", "-h", help="show this help message and exit", action="help"
    )
    parser.add_argument(
        "--version", "-v", action="version", version="%(prog)s " + mathics.__version__
    )
    parser.add_argument(
        "--chapters",
        "-c",
        dest="chapters",
        metavar="CHAPTER",
        help="only test CHAPTER(s). "
        "You can list multiple chapters by adding a comma (and no space) in between chapter names.",
    )
    parser.add_argument(
        "--sections",
        "-s",
        dest="sections",
        metavar="SECTION",
        help="only test SECTION(s). "
        "You can list multiple sections by adding a comma (and no space) in between section names.",
    )
    parser.add_argument(
        "--exclude",
        "-X",
        default="",
        dest="exclude",
        metavar="SECTION",
        help="excude SECTION(s). "
        "You can list multiple sections by adding a comma (and no space) in between section names.",
    )
    parser.add_argument(
        "--logfile",
        "-f",
        dest="logfilename",
        metavar="LOGFILENAME",
        help="stores the output in [logfilename]. ",
    )
    parser.add_argument(
        "--pymathics",
        "-l",
        dest="pymathics",
        action="store_true",
        help="also checks pymathics modules.",
    )
    parser.add_argument(
        "--time-each",
        "-d",
        dest="enlapsed_times",
        action="store_true",
        help="check the time that take each test to parse, evaluate and compare.",
    )

    parser.add_argument(
        "--output",
        "-o",
        dest="output",
        action="store_true",
        help="generate pickled internal document data",
    )
    parser.add_argument(
        "--doc-only",
        dest="doc_only",
        action="store_true",
        help="generate pickled internal document data without running tests; Can't be used with --section or --reload.",
    )
    parser.add_argument(
        "--reload",
        "-r",
        dest="reload",
        action="store_true",
        help="reload pickled internal document data, before possibly adding to it",
    )
    parser.add_argument(
        "--quiet", "-q", dest="quiet", action="store_true", help="hide passed tests"
    )
    parser.add_argument(
        "--keep-going",
        "-k",
        dest="keep_going",
        action="store_true",
        help="create documentation even if there is a test failure",
    )
    parser.add_argument(
        "--stop-on-failure", "-x", action="store_true", help="stop on failure"
    )
    parser.add_argument(
        "--skip",
        metavar="N",
        dest="skip",
        type=int,
        default=0,
        help="skip the first N tests",
    )
    parser.add_argument(
        "--count",
        metavar="N",
        dest="count",
        type=int,
        default=MAX_TESTS,
        help="run only  N tests",
    )

    global logfile

    args = parser.parse_args()

    if args.enlapsed_times:
        check_partial_enlapsed_time = True
    # If a test for a specific section is called
    # just test it
    if args.logfilename:
        logfile = open(args.logfilename, "wt")

    if args.sections:
        sections = set(args.sections.split(","))
        if args.pymathics:  # in case the section is in a pymathics module...
            documentation.load_pymathics_doc()

        test_sections(
            sections,
            stop_on_failure=args.stop_on_failure,
            generate_output=args.output,
            reload=args.reload,
        )
    elif args.chapters:
        chapters = set(args.chapters.split(","))
        if args.pymathics:  # in case the section is in a pymathics module...
            documentation.load_pymathics_doc()

        test_chapters(
            chapters, stop_on_failure=args.stop_on_failure, reload=args.reload
        )
    else:
        # if we want to check also the pymathics modules
        if args.pymathics:
            print("Building pymathics documentation object")
            documentation.load_pymathics_doc()
        elif args.doc_only:
            extract_doc_from_source(
                quiet=args.quiet,
                reload=args.reload,
            )
        else:
            excludes = set(args.exclude.split(","))
            start_at = args.skip + 1
            start_time = datetime.now()
            test_all(
                quiet=args.quiet,
                generate_output=args.output,
                stop_on_failure=args.stop_on_failure,
                start_at=start_at,
                count=args.count,
                doc_even_if_error=args.keep_going,
                excludes=excludes,
            )
            end_time = datetime.now()
            print("Tests took ", end_time - start_time)
    if logfile:
        logfile.close()


if __name__ == "__main__":
    main()
