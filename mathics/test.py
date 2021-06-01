#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import re
import pickle
import os
from argparse import ArgumentParser
from datetime import datetime
import mathics


from mathics.core.definitions import Definitions
from mathics.core.evaluation import Evaluation, Output
from mathics.core.parser import MathicsSingleLineFeeder
from mathics.builtin import builtins_dict

from mathics import version_string
from mathics import settings

builtins = builtins_dict()


class TestOutput(Output):
    def max_stored_size(self, settings):
        return None


sep = "-" * 70 + "\n"

# Global variables
definitions = None
documentation = None
check_partial_enlapsed_time = False
logfile = None


MAX_TESTS = 100000  # Number than the total number of tests


def print_and_log(*args):
    global logfile
    string = "".join(args)
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
            "%sTest failed: %s in %s / %s\n%s\n%s\n"
            % (sep, section, part, chapter, test, why)
        )
        return False

    if not quiet:
        if section:
            print("%s %s / %s %s" % (stars, tests.chapter, section, stars))
        print("%4d (%2d): TEST %s" % (index, subindex, test))

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
    tests, index, quiet=False, stop_on_failure=False, start_at=0, max_tests=MAX_TESTS
):
    definitions.reset_user_definitions()
    total = failed = skipped = 0
    failed_symbols = set()
    section = tests.section
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


def create_output(tests, output_tex):
    definitions.reset_user_definitions()
    for test in tests.tests:
        if test.private:
            continue
        key = test.key
        evaluation = Evaluation(
            definitions, format="tex", catch_interrupt=False, output=TestOutput()
        )
        result = evaluation.parse_evaluate(test.test)
        if result is None:
            result = []
        else:
            result = [result.get_data()]
        output_tex[key] = {
            "query": test.test,
            "results": result,
        }


def test_section(
    sections: set,
    quiet=False,
    stop_on_failure=False,
    generate_output=False,
    reload=False,
):
    failed = 0
    index = 0
    print("Testing section(s): %s" % ", ".join(sections))
    sections |= {"$" + s for s in sections}
    output_tex = load_doc_data() if reload else {}
    for tests in documentation.get_tests():
        if tests.section in sections:
            for test in tests.tests:
                if test.ignore:
                    continue
                index += 1
                if not test_case(test, tests, index, quiet=quiet):
                    failed += 1
                    if stop_on_failure:
                        break
            if generate_output and failed == 0:
                create_output(tests, output_tex)

    print()
    if failed > 0:
        print_and_log("%d test%s failed." % (failed, "s" if failed != 1 else ""))
    else:
        print_and_log("OK")


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
    generate_output=False,
    stop_on_failure=False,
    start_at=0,
    count=MAX_TESTS,
    xmldatafolder=None,
    texdatafolder=None,
    doc_even_if_error=False,
):
    global documentation
    if not quiet:
        print("Testing %s" % version_string)

    if generate_output:
        if texdatafolder is None:
            texdatafolder = settings.DOC_TEX_DATA_PATH
    try:
        index = 0
        total = failed = skipped = 0
        failed_symbols = set()
        output_xml = {}
        output_tex = {}
        for tests in documentation.get_tests():
            sub_total, sub_failed, sub_skipped, symbols, index = test_tests(
                tests,
                index,
                quiet=quiet,
                stop_on_failure=stop_on_failure,
                start_at=start_at,
                max_tests=count,
            )
            if generate_output:
                create_output(tests, output_tex)
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
        save_doc_data(output_tex)
        return True

    if failed == 0:
        print("\nOK")
    else:
        print("\nFAILED")
        return sys.exit(1)  # Travis-CI knows the tests have failed


def load_doc_data():
    print(f"Loading TeX from {settings.DOC_TEX_DATA_PATH}")
    with open_ensure_dir(settings.DOC_TEX_DATA_PATH, "rb") as tex_data_file:
        return pickle.load(tex_data_file)


def save_doc_data(output_tex):
    print(f"Writing TeX to {settings.DOC_TEX_DATA_PATH}")
    with open_ensure_dir(settings.DOC_TEX_DATA_PATH, "wb") as output_file:
        pickle.dump(output_tex, output_file, 4)


def make_doc(quiet=False, reload=False):
    """
    Write TeX doc examples.
    """
    if not quiet:
        print("Extracting doc %s" % version_string)

    try:
        output_tex = load_doc_data() if reload else {}
        for tests in documentation.get_tests():
            create_output(tests, output_tex)
    except KeyboardInterrupt:
        print("\nAborted.\n")
        return

    save_doc_data(output_tex)


def write_latex():
    print(f"Load data {settings.DOC_TEX_DATA_PATH}")
    with open_ensure_dir(settings.DOC_TEX_DATA_PATH, "rb") as output_file:
        output_tex = pickle.load(output_file)

    print(f"Write LaTeX {settings.DOC_LATEX_FILE}")
    with open_ensure_dir(settings.DOC_LATEX_FILE, "wb") as doc:
        content = documentation.latex(output_tex)
        content = content.encode("utf-8")
        doc.write(content)


def main():
    from mathics.doc import documentation as main_mathics_documentation

    global definitions
    global documentation
    global logfile
    global check_partial_enlapsed_time
    definitions = Definitions(add_builtin=True)
    documentation = main_mathics_documentation

    parser = ArgumentParser(description="Mathics test suite.", add_help=False)
    parser.add_argument(
        "--help", "-h", help="show this help message and exit", action="help"
    )
    parser.add_argument(
        "--version", "-v", action="version", version="%(prog)s " + mathics.__version__
    )
    parser.add_argument(
        "--sections",
        "-s",
        dest="section",
        metavar="SECTION",
        help="only test SECTION(s). "
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
        help="generate TeX and XML output data",
    )
    parser.add_argument(
        "--doc-only",
        dest="doc_only",
        action="store_true",
        help="generate TeX output data without running tests",
    )
    parser.add_argument(
        "--reload",
        "-r",
        dest="reload",
        action="store_true",
        help="reload TeX data",
    )
    parser.add_argument(
        "--tex",
        "-t",
        dest="tex",
        action="store_true",
        help="generate TeX documentation file",
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
    args = parser.parse_args()

    if args.enlapsed_times:
        check_partial_enlapsed_time = True
    # If a test for a specific section is called
    # just test it
    if args.logfilename:
        logfile = open(args.logfilename, "wt")

    if args.section:
        sections = set(args.section.split(","))
        if args.pymathics:  # in case the section is in a pymathics module...
            documentation.load_pymathics_doc()

        test_section(sections, stop_on_failure=args.stop_on_failure, reload=args.reload)
    else:
        # if we want to check also the pymathics modules
        if args.pymathics:
            print("Building pymathics documentation object")
            documentation.load_pymathics_doc()
        elif args.doc_only:
            make_doc(
                quiet=args.quiet,
                reload=args.reload,
            )
        else:
            start_at = args.skip + 1
            start_time = datetime.now()
            test_all(
                quiet=args.quiet,
                generate_output=args.output,
                stop_on_failure=args.stop_on_failure,
                start_at=start_at,
                count=args.count,
                doc_even_if_error=args.keep_going,
            )
            end_time = datetime.now()
            print("Tests took ", end_time - start_time)
    # If TeX output requested, try to build it:
    if args.tex:
        write_latex()
    if logfile:
        logfile.close()


if __name__ == "__main__":
    main()
