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
from mathics.core.parser import SingleLineFeeder
from mathics.builtin import builtins
from mathics.doc import documentation
from mathics import version_string
from mathics import settings

definitions = Definitions(add_builtin=True)


class TestOutput(Output):
    def max_stored_size(self, settings):
        return None


sep = '-' * 70 + '\n'


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
        wanted_re = wanted_re.replace('\\.\\.\\.', '.*?')
        wanted_re = '^%s$' % wanted_re
        if not re.match(wanted_re, r.strip()):
            return False
    return True


def test_case(test, tests, index=0, quiet=False):
    test, wanted_out, wanted = test.test, test.outs, test.result
    part, chapter, section = tests.part, tests.chapter, tests.section

    def fail(why):
        print("%sTest failed: %s in %s / %s\n%s\n%s\n" % (
            sep, section, part, chapter, test, why))
        return False

    if not quiet:
        print('%4d. TEST %s' % (index, test))

    feeder = SingleLineFeeder(test, '<test>')
    evaluation = Evaluation(definitions, catch_interrupt=False, output=TestOutput())
    try:
        query = evaluation.parse_feeder(feeder)
        if query is None:
            # parsed expression is None
            result = None
            out = evaluation.out
        else:
            result = evaluation.evaluate(query)
            out = result.out
            result = result.result
    except Exception as exc:
        fail("Exception %s" % exc)
        info = sys.exc_info()
        sys.excepthook(*info)
        return False

    if not compare(result, wanted):
        fail_msg = "Result: %s\nWanted: %s" % (result, wanted)
        if out:
            fail_msg += "\nAdditional output:\n"
            fail_msg += '\n'.join(str(o) for o in out)
        return fail(fail_msg)
    output_ok = True
    if len(out) != len(wanted_out):
        output_ok = False
    else:
        for got, wanted in zip(out, wanted_out):
            if not got == wanted:
                output_ok = False
                break
    if not output_ok:
        return fail("Output:\n%s\nWanted:\n%s" % (
            '\n'.join(str(o) for o in out),
            '\n'.join(str(o) for o in wanted_out)))
    return True


def test_tests(tests, index, quiet=False, stop_on_failure=False, start_at=0):
    definitions.reset_user_definitions()
    count = failed = skipped = 0
    failed_symbols = set()
    for test in tests.tests:
        if test.ignore:
            continue
        count += 1
        index += 1
        if index < start_at:
            skipped += 1
            continue
        if not test_case(test, tests, index, quiet):
            failed += 1
            failed_symbols.add((tests.part, tests.chapter, tests.section))
            if stop_on_failure:
                break
    return count, failed, skipped, failed_symbols, index


def create_output(tests, output_xml, output_tex):
    for format, output in [('xml', output_xml), ('tex', output_tex)]:
        definitions.reset_user_definitions()
        for test in tests.tests:
            if test.private:
                continue
            key = test.key
            evaluation = Evaluation(definitions, format=format, catch_interrupt=False, output=TestOutput())
            result = evaluation.parse_evaluate(test.test)
            if result is None:
                result = []
            else:
                result = [result.get_data()]
            output[key] = {
                'query': test.test,
                'results': result,
            }


def test_section(section, quiet=False, stop_on_failure=False):
    failed = 0
    index = 0
    print('Testing section %s' % section)
    for tests in documentation.get_tests():
        if tests.section == section or tests.section == '$' + section:
            for test in tests.tests:
                if test.ignore:
                    continue
                index += 1
                if not test_case(test, tests, index, quiet=quiet):
                    failed += 1
                    if stop_on_failure:
                        break

    print()
    if failed > 0:
        print('%d test%s failed.' % (failed, 's' if failed != 1 else ''))
    else:
        print('OK')


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

    try:
        index = 0
        count = failed = skipped = 0
        failed_symbols = set()
        output_xml = {}
        output_tex = {}
        for tests in documentation.get_tests():
            sub_count, sub_failed, sub_skipped, symbols, index = test_tests(
                tests, index, quiet=quiet, stop_on_failure=stop_on_failure,
                start_at=start_at)
            if generate_output:
                create_output(tests, output_xml, output_tex)
            count += sub_count
            failed += sub_failed
            skipped += sub_skipped
            failed_symbols.update(symbols)
            if sub_failed and stop_on_failure:
                break
        builtin_count = len(builtins)
    except KeyboardInterrupt:
        print("\nAborted.\n")
        return

    if failed > 0:
        print(sep)
    print("%d Tests for %d built-in symbols, %d passed, %d failed, %d skipped." % (
        count, builtin_count, count - failed - skipped, failed, skipped))
    if failed_symbols:
        if stop_on_failure:
            print("(not all tests are accounted for due to --stop-on-failure)")
        print("Failed:")
        for part, chapter, section in sorted(failed_symbols):
            print('  - %s in %s / %s' % (section, part, chapter))

    if failed == 0:
def make_doc(quiet=False):
    """
    Write XML and TeX doc examples.
    """
    if not quiet:
        print("Extracting doc %s" % version_string)

    try:
        output_xml = {}
        output_tex = {}
        for tests in documentation.get_tests():
            create_output(tests, output_xml, output_tex)
        builtin_count = len(builtins)
    except KeyboardInterrupt:
        print("\nAborted.\n")
        return

    print('Save XML')
    with open_ensure_dir(settings.DOC_XML_DATA, 'wb') as output_file:
        pickle.dump(output_xml, output_file, 0)
        print('\nOK')

        if generate_output:
            print('Save XML')
            with open_ensure_dir(settings.DOC_XML_DATA, 'wb') as output_file:
                pickle.dump(output_xml, output_file, 0)

            print('Save TEX')
            with open_ensure_dir(settings.DOC_TEX_DATA, 'wb') as output_file:
                pickle.dump(output_tex, output_file, 0)
    else:
        print('\nFAILED')
        return sys.exit(1)      # Travis-CI knows the tests have failed

    print('Save TeX')
    with open_ensure_dir(settings.DOC_TEX_DATA, 'wb') as output_file:
        pickle.dump(output_tex, output_file, 0)

def make_doc(quiet=False, generate_output=False, stop_on_failure=False,
             start_at=0):
    if not quiet:
        print("Extracting doc %s" % version_string)

    try:
        index = 0
        count = failed = skipped = 0
        failed_symbols = set()
        output_xml = {}
        output_tex = {}
        for tests in documentation.get_tests():
            if generate_output:
                create_output(tests, output_xml, output_tex)
        builtin_count = len(builtins)
    except KeyboardInterrupt:
        print("\nAborted.\n")
        return

    print('Save XML')
    with open_ensure_dir(settings.DOC_XML_DATA, 'wb') as output_file:
        pickle.dump(output_xml, output_file, 0)

    print('Save TEX')
    with open_ensure_dir(settings.DOC_TEX_DATA, 'wb') as output_file:
        pickle.dump(output_tex, output_file, 0)

def write_latex():
    print("Load data")
    with open_ensure_dir(settings.DOC_TEX_DATA, 'rb') as output_file:
        output_tex = pickle.load(output_file)

    print('Print documentation')
    with open_ensure_dir(settings.DOC_LATEX_FILE, 'wb') as doc:
        content = documentation.latex(output_tex)
        content = content.encode('utf-8')
        doc.write(content)


def main():
    parser = ArgumentParser(description="Mathics test suite.", add_help=False)
    parser.add_argument(
        '--help', '-h', help='show this help message and exit', action='help')
    parser.add_argument(
        "--version", "-v", action="version", version="%(prog)s " + mathics.__version__
    )
    parser.add_argument(
        "--section", "-s", dest="section", metavar="SECTION", help="only test SECTION"
    )
    parser.add_argument(
        "--pymathics",
        "-l",
        dest="pymathics",
        action="store_true",
        help="also checks pymathics modules.",
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
        help="generate TeX and XML output data without running tests",
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
        "--keep-going", "-k", dest="keep_going", action="store_true", help="create documentation even if there is a test failure"
    )
    parser.add_argument(
        "--stop-on-failure", action="store_true", help="stop on failure"
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

    if args.tex:
        write_latex()
    else:
        # if we want to check also the pymathics modules
        if args.pymathics:
            print("Building pymathics documentation object")
            documentation.load_pymathics_doc()
        elif args.doc_only:
            make_doc(
                quiet=args.quiet,
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
            print("Tests took ", end_time-start_time)
    # If it was asked for tex output, try to build it:
    if args.tex:
        write_latex()

if __name__ == '__main__':
    main()
