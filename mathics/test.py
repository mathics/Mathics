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

from __future__ import with_statement

import sys
import re
import pickle
import os
from optparse import OptionParser

from mathics.core.parser import parse, TranslateError
from mathics.core.definitions import Definitions
from mathics.core.expression import Evaluation
from mathics.builtin import modules, builtins_by_module, get_module_doc, builtins
from mathics.doc import documentation
from mathics import get_version_string

from mathics import settings

definitions = Definitions(add_builtin=True)

sep = '-' * 70 + '\n'

def compare(result, wanted):
    if result == wanted:
        return True
    if result is None or wanted is None:
        return False
    result = result.splitlines()
    wanted = wanted.splitlines()
    if len(result) != len(wanted):
        return False
    for r, w in zip(result, wanted):
        wanted_re = re.escape(w.strip())
        wanted_re = wanted_re.replace('\\.\\.\\.', '.*?')
        wanted_re = '^%s$' % wanted_re
        if not re.match(wanted_re, r.strip()):
            return False
    return True

def test_case(test, tests, index=0):
    test, wanted_out, wanted, part, chapter, section = test.test, test.outs, test.result, tests.part, tests.chapter, tests.section
    
    def fail(why):
        print u"%sTest failed: %s in %s / %s\n%s\n%s\n" % (sep, section, part, chapter, test, why)
        return False
    
    print '%4d. TEST %s' % (index, test)
    try:
        evaluation = Evaluation(test, definitions, catch_interrupt=False)
    except Exception, exc:
        fail(u"Exception %s" % exc)
        info = sys.exc_info()
        sys.excepthook(*info)
        return False
    
    if evaluation.results:
        if len(evaluation.results) > 1:
            return fail(u"Too many results: %s" % evaluation.results)
        result = evaluation.results[0].result
        out = evaluation.results[0].out
    else:
        result = None
        out = []
    if not compare(result, wanted):
        return fail(u"Result: %s\nWanted: %s" % (result, wanted))
    output_ok = True
    if len(out) != len(wanted_out):
        output_ok = False
    else:
        for got, wanted in zip(out, wanted_out):
            if not got == wanted:
                output_ok = False
                break
    if not output_ok:
        return fail(u"Output:\n%s\nWanted:\n%s" % (u'\n'.join(unicode(o) for o in out),
            u'\n'.join(unicode(o) for o in wanted_out)))
    return True

def test_tests(tests, index):
    #print tests
    definitions.reset_user_definitions()
    count = failed = 0
    failed_symbols = set()
    for test in tests.tests:   
        count += 1
        index += 1
        if not test_case(test, tests, index):
            failed += 1
            failed_symbols.add((tests.part, tests.chapter, tests.section))
    return count, failed, failed_symbols, index

def create_output(tests, output_xml, output_tex):
    for format, output in [('xml', output_xml), ('tex', output_tex)]:
        definitions.reset_user_definitions()
        for test in tests.tests:
            key = test.key
            result = Evaluation(test.test, definitions, format=format, catch_interrupt=False)
            output[key] = {
                'query': test.test,
                'results': [r.get_data() for r in result.results],
            }
            
def test_section(section):
    failed = 0
    index = 0
    print 'Testing section %s' % section
    for tests in documentation.get_tests():
        if tests.section == section or tests.section == '$' + section:
            for test in tests.tests:
                index += 1
                if not test_case(test, tests, index):
                    failed += 1
        
    print ''
    if failed > 0:
        print '%d test%s failed.' % (failed, 's' if failed != 1 else '')
    else:
        print 'OK'
        
def open_ensure_dir(f, *args, **kwargs):
    try:
        return open(f, *args, **kwargs)
    except IOError, OSError:        
        d = os.path.dirname(f)
        if d and not os.path.exists(d):
            os.makedirs(d)
        return open(f, *args, **kwargs)

def test_all():
    print "Testing %s" % get_version_string(False)
      
    try:
        index = 0
        count = failed = 0
        failed_symbols = set()
        output_xml = {}
        output_tex = {}
        for tests in documentation.get_tests():
            sub_count, sub_failed, symbols, index = test_tests(tests, index)
            create_output(tests, output_xml, output_tex)
            count += sub_count
            failed += sub_failed
            failed_symbols.update(symbols)
        builtin_count = len(builtins)
    except KeyboardInterrupt:
        print "\nAborted.\n"
        return
        
    if failed > 0:
        print '%s' % sep
    print "%d Tests for %d built-in symbols, %d passed, %d failed." % (count, builtin_count, count - failed, failed)
    if failed_symbols:
        print "Failed:"
        for part, chapter, section in sorted(failed_symbols):
            print '  - %s in %s / %s' % (section, part, chapter)
    
    if failed == 0:
        print '\nOK'
        
        print 'Save XML'
        with open_ensure_dir(settings.DOC_XML_DATA, 'w') as output_xml_file:
            pickle.dump(output_xml, output_xml_file, 0)
            
        print 'Save TEX'
        with open_ensure_dir(settings.DOC_TEX_DATA, 'w') as output_tex_file:
            pickle.dump(output_tex, output_tex_file, 0)
    else:
        print '\nFAILED'
        
def write_latex():
    print "Load data"
    with open_ensure_dir(settings.DOC_TEX_DATA, 'r') as output_tex_file:
        output_tex = pickle.load(output_tex_file)
        
    print 'Print documentation'
    with open_ensure_dir(settings.DOC_LATEX_FILE, 'w') as doc:
        content = documentation.latex(output_tex)
        content = content.encode('utf-8')
        doc.write(content)

def main():
    parser = OptionParser(version='%prog ' + settings.VERSION,
        description="Mathics test suite.")
    parser.add_option("-s", "--section", dest="section", metavar="SECTION",
        help="only test SECTION")
    parser.add_option("-t", "--tex", dest="tex", action="store_true",
        help="generate TeX file")
    options, args = parser.parse_args()
    
    if options.tex:
        write_latex()
    else:
        if options.section:
            test_section(options.section)
        else:
            test_all()

if __name__ == '__main__':
    main()
