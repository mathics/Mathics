#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import absolute_import
from __future__ import unicode_literals

import os
import sys
import pexpect
import unittest
from six.moves import range


class ConsoleTest(unittest.TestCase):
    def setUp(self):
        os.environ["TERM"] = "dumb"
        self.console = pexpect.spawn(sys.executable + ' mathics/main.py --color NOCOLOR')

    def readline(self):
        return self.console.readline().decode('utf-8')

    def testLaunch(self):
        cons = self.console

        self.assertRegexpMatches(self.readline(), '\r\n')

        self.assertRegexpMatches(
            self.readline(), 'Mathics \\d\\.\\d.*\r\n')
        self.assertRegexpMatches(
            self.readline(), 'on (CPython|PyPy) \\d+.\\d+.\\d+.*\\(.+\\) ?\r\n')
        self.assertRegexpMatches(
            self.readline(), 'using ([a-zA-Z]+ [\\.\\d]+(, |\r\n$))+')

        self.assertRegexpMatches(self.readline(), '\r\n')

        self.assertRegexpMatches(
            self.readline(),
            'Copyright \\(C\\) 2011\-20\\d\\d The Mathics Team.\r\n')

        self.assertEqual(
            ''.join(self.readline() for i in range(7)),
            'This program comes with ABSOLUTELY NO WARRANTY.\r\n'
            'This is free software, and you are welcome to redistribute it\r\n'
            'under certain conditions.\r\n'
            'See the documentation for the full license.\r\n'
            '\r\n'
            'Quit by pressing CONTROL-D\r\n'
            '\r\n')

    def testPrompt(self):
        cons = self.console
        cons.expect('Quit by pressing CONTROL-D\r\n\r\n')

        self.lineno = 1

        def check(query, result):
            inprompt = 'In[{0}]:= '.format(self.lineno)
            self.assertEqual(cons.read(len(inprompt)).decode('utf-8'), inprompt)
            cons.sendline(query.encode('utf-8'))
            self.assertEqual(self.readline(), '{0}\r\n'.format(query))

            outprompt = 'Out[{0}]= {1}\r\n'.format(self.lineno, result)

            self.assertEqual(self.readline(), outprompt)
            self.assertEqual(self.readline(), '\r\n')
            self.lineno += 1

        check('1 + 1', '2')
        check('2 * 3', '6')

    def tearDown(self):
        self.console.close()


if __name__ == "__main__":
    unittest.main()
