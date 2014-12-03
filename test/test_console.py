import os
import sys
import pexpect

if sys.version_info[:2] == (2, 7):
    import unittest
else:
    import unittest2 as unittest


class ConsoleTest(unittest.TestCase):
    def setUp(self):
        os.environ["TERM"] = "dumb"
        self.console = pexpect.spawn('python2 mathics/main.py --color NOCOLOR')

    def testLaunch(self):
        cons = self.console

        self.assertRegexpMatches(cons.readline(), '.*\r\n')

        self.assertRegexpMatches(
            cons.readline(), 'Mathics \\d\\.\\d.*\r\n')
        self.assertRegexpMatches(
            cons.readline(), 'on (CPython|PyPy) \\d.\\d.\\d \\(.+\\) ?\r\n')
        self.assertRegexpMatches(
            cons.readline(), 'using ([a-zA-Z]+ [\\.\\d]+(, |\r\n$))+')

        self.assertRegexpMatches(cons.readline(), '\r\n')

        self.assertRegexpMatches(
            cons.readline(),
            'Copyright \\(C\\) 2011\-20\\d\\d The Mathics Team.\r\n')

        self.assertEqual(
            ''.join(cons.readline() for i in range(7)),
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
            self.assertEqual(
                cons.read(len(inprompt)), inprompt)
            cons.sendline(query)
            self.assertEqual(

                cons.readline(), '{0}\r\n'.format(query))

            outprompt = 'Out[{0}]= {1}\r\n'.format(self.lineno, result)

            self.assertEqual(cons.readline(), outprompt)
            self.assertEqual(cons.readline(), '\r\n')
            self.lineno += 1

        check('1 + 1', '2')
        check('2 * 3', '6')

    def tearDown(self):
        self.console.close()


if __name__ == "__main__":
    unittest.main()
