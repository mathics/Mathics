# -*- coding: utf8 -*-

"""
File Operations
"""

from __future__ import with_statement
import os
import io
import shutil
import hashlib
import zlib
import base64
import tempfile
import time

from mathics.core.expression import (Expression, String, Symbol, from_python,
                                     BoxError)
from mathics.builtin.base import (Builtin, Predefined, BinaryOperator,
                                  PrefixOperator)
from mathics.settings import ROOT_DIR

STREAMS = {}
INITIAL_DIR = os.getcwd()
HOME_DIR = os.path.expanduser('~')
SYS_ROOT_DIR = '/' if os.name == 'posix' else '\\'
TMP_DIR = tempfile.gettempdir()
DIRECTORY_STACK = [INITIAL_DIR]
INPUT_VAR = ""
INPUTFILE_VAR = ""
PATH_VAR = [HOME_DIR, os.path.join(ROOT_DIR, 'data'),
            os.path.join(ROOT_DIR, 'packages')]


class mathics_open:
    def __init__(self, filename, mode='r'):
        self.filename = filename
        self.mode = mode
        self.file = None

    def __enter__(self):
        path = path_search(self.filename)

        encoding = 'utf-8' if 'b' not in self.mode else None

        if path is not None:
            self.file = io.open(path, self.mode, encoding=encoding)
        elif self.mode == 'w':
            self.file = io.open(self.filename, self.mode, encoding=encoding)
        else:
            raise IOError
        return self

    def __exit__(self, type, value, traceback):
        if self.file is not None:
            self.file.close()

    def read(self, *args):
        return self.file.read(*args)

    def write(self, *args):
        return self.file.write(*args)

    def readline(self):
        return self.file.readline()

    def readlines(self):
        return self.file.readlines()

    def seek(self, *args):
        return self.file.seek(*args)

    def tell(self):
        return self.file.tell()

    def close(self):
        self.file.close()

    @property
    def closed(self):
        return self.file.closed

    @property
    def seekable(self):
        return self.file.seekable


def path_search(filename):
    # For names of the form "name`", search for name.mx and name.m
    if filename[-1] == '`':
        filename = filename[:-1].replace('`', os.path.sep)
        for ext in ['.mx', '.m']:
            result = path_search(filename + ext)
            if result is not None:
                filename = None
                break

    if filename is not None:
        result = None
        for p in PATH_VAR + ['']:
            path = os.path.join(p, filename)
            if os.path.exists(path):
                result = path
                break

    # If FindFile resolves to a dir, search within for Kernel/init.m and init.m
    if result is not None and os.path.isdir(result):
        for ext in [os.path.join('Kernel', 'init.m'), 'init.m']:
            tmp = os.path.join(result, ext)
            if os.path.isfile(tmp):
                return tmp
    return result


def _put_stream(stream):
    global STREAMS, _STREAMS, NSTREAMS

    try:
        _STREAMS
    except NameError:
        STREAMS = {}    # Python repr
        _STREAMS = {}   # Mathics repr
        NSTREAMS = 0    # Max stream number

    NSTREAMS += 1
    STREAMS[NSTREAMS] = stream
    return NSTREAMS


class InitialDirectory(Predefined):
    """
    <dl>
    <dt>'$InitialDirectory'
      <dd>returns the directory from which \Mathics was started.
    </dl>

    >> $InitialDirectory
     = ...
    """

    name = '$InitialDirectory'

    def evaluate(self, evaluation):
        global INITIAL_DIR
        return String(INITIAL_DIR)


class InstallationDirectory(Predefined):
    """
    <dl>
    <dt>'$InstallationDirectory'
      <dd>returns the directory in which \Mathics was installed.
    </dl>

    >> $InstallationDirectory
     = ...
    """

    name = '$InstallationDirectory'

    def evaluate(self, evaluation):
        global ROOT_DIR
        return String(ROOT_DIR)


class HomeDirectory(Predefined):
    """
    <dl>
    <dt>'$HomeDirectory'
      <dd>returns the users HOME directory.
    </dl>

    >> $HomeDirectory
     = ...
    """

    name = '$HomeDirectory'

    attributes = ('Protected')

    def evaluate(self, evaluation):
        global HOME_DIR
        return String(HOME_DIR)


class RootDirectory(Predefined):
    """
    <dl>
    <dt>'$RootDirectory'
      <dd>returns the system root directory.
    </dl>

    >> $RootDirectory
     = ...
    """

    name = '$RootDirectory'

    attributes = ('Protected')

    def evaluate(self, evaluation):
        global SYS_ROOT_DIR
        return String(SYS_ROOT_DIR)


class TemporaryDirectory(Predefined):
    """
    <dl>
    <dt>'$TemporaryDirectory'
      <dd>returns the directory used for temporary files.
    </dl>

    >> $TemporaryDirectory
     = ...
    """

    name = '$TemporaryDirectory'

    def evaluate(self, evaluation):
        return String(TMP_DIR)


class Input(Predefined):
    """
    <dl>
    <dt>'$Input'
      <dd>is the name of the stream from which input is currently being read.
    </dl>

    >> $Input
     = 
    """

    attributes = ('Protected', 'ReadProtected')
    name = '$Input'

    def evaluate(self, evaluation):
        global INPUT_VAR
        return String(INPUT_VAR)


class InputFileName(Predefined):
    """
    <dl>
    <dt>'$InputFileName'
      <dd>is the name of the file from which input is currently being read.
    </dl>

    While in interactive mode, '$InputFileName' is "".
    >> $InputFileName
     = 
    """

    name = '$InputFileName'

    def evaluate(self, evaluation):
        global INPUTFILE_VAR
        return String(INPUTFILE_VAR)


class PathnameSeparator(Predefined):
    """
    <dl>
    <dt>'$PathnameSeparator'
      <dd>returns a string for the seperator in paths.
    </dl>

    >> $PathnameSeparator
     = ...
    """

    name = '$PathnameSeparator'

    def evaluate(self, evaluation):
        return String(os.sep)


class Path(Predefined):
    """
    <dl>
    <dt>'$Path'
      <dd>returns the list of directories to search when looking for a file.
    </dl>

    >> $Path
     = ...
    """

    attributes = ('Protected')
    name = '$Path'

    def evaluate(self, evaluation):
        return Expression('List', *[String(p) for p in PATH_VAR])


class OperatingSystem(Predefined):
    """
    <dl>
    <dt>'$OperatingSystem'
      <dd>gives the type of operating system running Mathics.
    </dl>

    >> $OperatingSystem
     = ...
    """

    attributes = ('Locked', 'Protected')
    name = '$OperatingSystem'

    def evaluate(self, evaluation):
        if os.name == 'posix':
            return String('Unix')
        elif os.name == 'nt':
            return String('Windows')
        elif os.name == 'os2':
            return String('MacOSX')
        else:
            return String('Unknown')


class Read(Builtin):
    """
    <dl>
    <dt>'Read[stream]'
      <dd>reads the input stream and returns one expression.
    <dt>'Read[stream, type]'
      <dd>reads the input stream and returns an object of the given type.
    </dl>

    ## Malformed InputString
    #> Read[InputStream[String], {Word, Number}]
     : InputStream[String] is not string, InputStream[], or OutputStream[]
     = Read[InputStream[String], {Word, Number}]

    ## Correctly formed InputString but not open
    #> Read[InputStream[String, -1], {Word, Number}]
     : InputStream[String, -1] is not open.
     = Read[InputStream[String, -1], {Word, Number}]

    ## String
    >> str = StringToStream["abc123"];
    >> Read[str, String]
     = abc123
    #> Read[str, String]
     = EndOfFile
    #> Close[str];

    ## Word
    >> str = StringToStream["abc 123"];
    >> Read[str, Word]
     = abc
    >> Read[str, Word]
     = 123
    #> Read[str, Word]
     = EndOfFile
    #> Close[str];
    #> str = StringToStream[""];
    #> Read[str, Word]
     = EndOfFile
    #> Read[str, Word]
     = EndOfFile
    #> Close[str];

    ## Number
    >> str = StringToStream["123, 4"];
    >> Read[str, Number]
     = 123
    >> Read[str, Number]
     = 4
    #> Read[str, Number]
     = EndOfFile
    #> Close[str];
    #> str = StringToStream["123xyz 321"];
    #> Read[str, Number]
     = 123
    #> Quiet[Read[str, Number]]
     = $Failed

    ## Real
    #> str = StringToStream["123, 4abc"];
    #> Read[str, Real]
     = 123.
    #> Read[str, Real]
     = 4.
    #> Quiet[Read[str, Number]]
     = $Failed

    #> Close[str];
    #> str = StringToStream["1.523E-19"]; Read[str, Real]
     = 1.523*^-19
    #> Close[str];
    #> str = StringToStream["-1.523e19"]; Read[str, Real]
     = -1.523*^19
    #> Close[str];
    #> str = StringToStream["3*^10"]; Read[str, Real]
     = 3.*^10
    #> Close[str];
    #> str = StringToStream["3.*^10"]; Read[str, Real]
     = 3.*^10
    #> Close[str];

    ## Expression
    #> str = StringToStream["x + y Sin[z]"]; Read[str, Expression]
     = x + y Sin[z]
    #> Close[str];
    ## #> str = Quiet[StringToStream["Sin[1 123"]; Read[str, Expression]]
    ##  = $Failed

    ## Multiple types
    >> str = StringToStream["123 abc"];
    >> Read[str, {Number, Word}]
     = {123, abc}
    #> Read[str, {Number, Word}]
     = EndOfFile
    #> Close[str];

    #> str = StringToStream["123 abc"];
    #> Quiet[Read[str, {Word, Number}]]
     = $Failed
    #> Close[str];

    #> str = StringToStream["123 123"];  Read[str, {Real, Number}]
     = {123., 123}
    #> Close[str];

    ## TODO Replace the following test with this one:
    ## >> Read[str, {Real}]
    ##  : InputStream[String, ...] is not open.
    ##  = Read[InputStream[String, ...], {Real}]
    ## Quick check
    #> Quiet[Head[Read[str, {Real}]]]
     = Read
    """

    messages = {
        'openx': '`1` is not open.',
        'readf': '`1` is not a valid format specification.',
        'readn': 'Invalid real number found when reading from `1`.',
        'readt': 'Invalid input found when reading `1` from `2`.',
        'intnm': ('Non-negative machine-sized integer expected at '
                  'position 3 in `1`.'),
    }

    rules = {
        'Read[stream_]': 'Read[stream, Expression]',
    }

    options = {
        'NullRecords': 'False',
        'NullWords': 'False',
        'RecordSeparators': '{"\r\n", "\n", "\r"}',
        'TokenWords': '{}',
        'WordSeparators': '{" ", "\t"}',
    }

    attributes = ('Protected')

    def check_options(self, options):
        ## Options:
        # TODO: Proper error messages

        result = {}
        keys = options.keys()

        # AnchoredSearch
        if 'AnchoredSearch' in keys:
            anchored_search = options['AnchoredSearch'].to_python()
            assert anchored_search in [True, False]
            result['AnchoredSearch'] = anchored_search

        # IgnoreCase
        if 'IgnoreCase' in keys:
            ignore_case = options['IgnoreCase'].to_python()
            assert ignore_case in [True, False]
            result['IgnoreCase'] = ignore_case

        # WordSearch
        if 'WordSearch' in keys:
            word_search = options['WordSearch'].to_python()
            assert word_search in [True, False]
            result['WordSearch'] = word_search

        # RecordSeparators
        if 'RecordSeparators' in keys:
            record_separators = options['RecordSeparators'].to_python()
            assert isinstance(record_separators, list)
            assert all(isinstance(s, basestring) and s[
                       0] == s[-1] == '"' for s in record_separators)
            record_separators = [s[1:-1] for s in record_separators]
            result['RecordSeparators'] = record_separators

        # WordSeparators
        if 'WordSeparators' in keys:
            word_separators = options['WordSeparators'].to_python()
            assert isinstance(word_separators, list)
            assert all(isinstance(s, basestring) and s[
                       0] == s[-1] == '"' for s in word_separators)
            word_separators = [s[1:-1] for s in word_separators]
            result['WordSeparators'] = word_separators

        # NullRecords
        if 'NullRecords' in keys:
            null_records = options['NullRecords'].to_python()
            assert null_records in [True, False]
            result['NullRecords'] = null_records

        # NullWords
        if 'NullWords' in keys:
            null_words = options['NullWords'].to_python()
            assert null_words in [True, False]
            result['NullWords'] = null_words

        # TokenWords
        if 'TokenWords' in keys:
            token_words = options['TokenWords'].to_python()
            assert token_words == []
            result['TokenWords'] = token_words

        return result

    def apply(self, name, n, types, evaluation, options):
        'Read[InputStream[name_, n_], types_, OptionsPattern[Read]]'
        global STREAMS

        stream = STREAMS.get(n.to_python())

        if stream is None or stream.closed:
            evaluation.message('Read', 'openx', Expression(
                'InputStream', name, n))
            return

        types = types.to_python()
        if not isinstance(types, list):
            types = [types]

        READ_TYPES = ['Byte', 'Character', 'Expression',
                      'Number', 'Real', 'Record', 'String', 'Word']

        for typ in types:
            if not (isinstance(typ, basestring) and typ in READ_TYPES):
                evaluation.message('Read', 'readf', from_python(typ))
                return Symbol('$Failed')

        ## Options:
        # TODO: Implement extra options
        py_options = self.check_options(options)
        # null_records = py_options['NullRecords']
        # null_words = py_options['NullWords']
        record_separators = py_options['RecordSeparators']
        # token_words = py_options['TokenWords']
        word_separators = py_options['WordSeparators']

        name = name.to_python()

        result = []

        def reader(stream, word_separators, accepted=None):
            while True:
                word = ''
                while True:
                    tmp = stream.read(1)

                    if tmp == '':
                        if word == '':
                            raise EOFError
                        yield word

                    if tmp in word_separators:
                        if word == '':
                            break
                        if stream.seekable():
                            # stream.seek(-1, 1) #Python3
                            stream.seek(stream.tell() - 1)
                        yield word

                    if accepted is not None and tmp not in accepted:
                        yield word

                    word += tmp

        read_word = reader(stream, word_separators)
        read_record = reader(stream, record_separators)
        read_number = reader(stream, word_separators + record_separators,
                             ['+', '-', '.'] + [str(i) for i in range(10)])
        read_real = reader(
            stream, word_separators + record_separators,
            ['+', '-', '.', 'e', 'E', '^', '*'] + [str(i) for i in range(10)])
        for typ in types:
            try:
                if typ == 'Byte':
                    tmp = stream.read(1)
                    if tmp == '':
                        raise EOFError
                    result.append(ord(tmp))
                elif typ == 'Character':
                    tmp = stream.read(1)
                    if tmp == '':
                        raise EOFError
                    result.append(tmp)
                elif typ == 'Expression':
                    tmp = read_record.next()
                    try:
                        try:
                            expr = parse(tmp)
                        except NameError:
                            from mathics.core.parser import parse, ParseError
                            expr = parse(tmp)
                    except ParseError:
                        expr = None
                    if expr is None:
                        evaluation.message('Read', 'readt', tmp, Expression(
                            'InputSteam', name, n))
                        return Symbol('$Failed')
                    result.append(tmp)
                elif typ == 'Number':
                    tmp = read_number.next()
                    try:
                        tmp = int(tmp)
                    except ValueError:
                        try:
                            tmp = float(tmp)
                        except ValueError:
                            evaluation.message('Read', 'readn', Expression(
                                'InputSteam', name, n))
                            return Symbol('$Failed')
                    result.append(tmp)

                elif typ == 'Real':
                    tmp = read_real.next()
                    tmp = tmp.replace('*^', 'E')
                    try:
                        tmp = float(tmp)
                    except ValueError:
                        evaluation.message('Read', 'readn', Expression(
                            'InputSteam', name, n))
                        return Symbol('$Failed')
                    result.append(tmp)
                elif typ == 'Record':
                    result.append(read_record.next())
                elif typ == 'String':
                    tmp = stream.readline()
                    if len(tmp) == 0:
                        raise EOFError
                    result.append(tmp.rstrip('\n'))
                elif typ == 'Word':
                    result.append(read_word.next())

            except EOFError:
                return Symbol('EndOfFile')

        if len(result) == 1:
            return from_python(*result)

        return from_python(result)

    def apply_nostream(self, arg1, arg2, evaluation):
        'Read[arg1_, arg2_]'
        evaluation.message('General', 'stream', arg1)
        return


class Write(Builtin):
    """
    <dl>
    <dt>'Write[$channel$, $expr1$, $expr2$, ...]'
      <dd>writes the expressions to the output channel followed by a newline.
    </dl>

    >> str = OpenWrite[]
     = ...
    >> Write[str, 10 x + 15 y ^ 2]
    >> Write[str, 3 Sin[z]]
    >> Close[str]
     = ...
    >> str = OpenRead[%];
    >> ReadList[str]
     = {10 x + 15 y ^ 2, 3 Sin[z]}
    #> Close[str];
    """

    attributes = ('Protected')

    def apply(self, name, n, expr, evaluation):
        'Write[OutputStream[name_, n_], expr___]'
        global STREAMS
        stream = STREAMS.get(n.to_python())

        if stream is None or stream.closed:
            evaluation.message('General', 'openx', name)
            return

        expr = expr.get_sequence()
        expr = Expression('Row', Expression('List', *expr))

        evaluation.format = 'text'
        text = evaluation.format_output(from_python(expr))
        stream.write(unicode(text) + u'\n')
        return Symbol('Null')


class WriteString(Builtin):
    """
    <dl>
    <dt>'WriteString[$stream$, $str1, $str2$, ... ]'
      <dd>writes the strings to the output stream.
    </dl>

    >> str = OpenWrite[];
    >> WriteString[str, "This is a test 1"]
    >> WriteString[str, "This is also a test 2"]
    >> Close[str]
     = ...
    >> FilePrint[%]
     | This is a test 1This is also a test 2

    >> str = OpenWrite[];
    >> WriteString[str, "This is a test 1", "This is also a test 2"]
    >> Close[str]
     = ...
    >> FilePrint[%]
     | This is a test 1This is also a test 2

    #> str = OpenWrite[];
    #> WriteString[str, 100, 1 + x + y, Sin[x  + y]]
    #> Close[str]
     = ...
    #> FilePrint[%]
     | 1001 + x + ySin[x + y]
    """

    messages = {
        'strml': ('`1` is not a string, stream, '
                  'or list of strings and streams.'),
    }

    attributes = ('Protected')

    def apply(self, name, n, expr, evaluation):
        'WriteString[OutputStream[name_, n_], expr___]'
        global STREAMS
        stream = STREAMS.get(n.to_python())

        if stream is None or stream.closed:
            evaluation.message('General', 'openx', name)
            return

        exprs = []
        for expri in expr.get_sequence():
            result = expri.format(evaluation, "OutputForm")
            try:
                result = result.boxes_to_text(evaluation=evaluation)
            except BoxError:
                return evaluation.message(
                    'General', 'notboxes', String('%s' % result))
            exprs.append(result)

        stream.write(u''.join(exprs))
        return Symbol('Null')


class _OpenAction(Builtin):

    messages = {
        'argx': 'OpenRead called with 0 arguments; 1 argument is expected.',
        'fstr': ('File specification `1` is not a string of '
                 'one or more characters.'),
    }

    attributes = ('Protected')

    def apply(self, path, evaluation):
        '%(name)s[path_]'

        if not (isinstance(path, String) and len(path.to_python()) > 2):
            evaluation.message(self.__class__.__name__, 'fstr', path)
            return

        path_string = path.to_python()[1:-1]

        tmp = path_search(path_string)
        if tmp is None:
            if self.mode in ['a', 'r']:
                evaluation.message('General', 'noopen', path)
                return
        else:
            path_string = tmp

        try:
            stream = mathics_open(path_string, mode=self.mode).__enter__()
        except IOError:
            evaluation.message('General', 'noopen', path)
            return

        n = _put_stream(stream)
        result = Expression(self.stream_type, path, n)
        global _STREAMS
        _STREAMS[n] = result

        return result

    def apply_empty(self, evaluation):
        '%(name)s[]'

        if self.mode == 'r':
            evaluation.message(self.__class__.__name__, 'argx')
            return

        global TMP_DIR

        tmpf = tempfile.NamedTemporaryFile(dir=TMP_DIR)
        path_string = tmpf.name
        tmpf.close()

        try:
            stream = mathics_open(path_string, mode='w').__enter__()
        except IOError:
            evaluation.message('General', 'noopen', String(path_string))
            return

        n = _put_stream(stream)
        result = Expression(self.stream_type, String(path_string), n)
        global _STREAMS
        _STREAMS[n] = result

        return result


class OpenRead(_OpenAction):
    """
    <dl>
    <dt>'OpenRead["file"]'
      <dd>opens a file and returns an InputStream.
    </dl>

    >> OpenRead["ExampleData/EinsteinSzilLetter.txt"]
     = InputStream[...]
    #> Close[%];

    #> OpenRead[]
     : OpenRead called with 0 arguments; 1 argument is expected.
     = OpenRead[]

    #> OpenRead[y]
     : File specification y is not a string of one or more characters.
     = OpenRead[y]

    #> OpenRead[""]
     : File specification  is not a string of one or more characters.
     = OpenRead[]

    #> OpenRead["MathicsNonExampleFile"]
     : Cannot open MathicsNonExampleFile.
     = OpenRead[MathicsNonExampleFile]
    """

    mode = 'r'
    stream_type = 'InputStream'


class OpenWrite(_OpenAction):
    """
    <dl>
    <dt>'OpenWrite["file"]'
      <dd>opens a file and returns an OutputStream.
    </dl>

    >> OpenWrite[]
     = OutputStream[...]
    #> Close[%];
    """

    mode = 'w'
    stream_type = 'OutputStream'


class OpenAppend(_OpenAction):
    """
    <dl>
    <dt>'OpenAppend["file"]'
      <dd>opens a file and returns an OutputStream to which writes are appended.
    </dl>

    >> OpenAppend[]
     = OutputStream[...]
    #> Close[%];

    #> OpenAppend["MathicsNonExampleFile"]
     : Cannot open MathicsNonExampleFile.
     = OpenAppend[MathicsNonExampleFile]
    """

    mode = 'a'
    stream_type = 'OutputStream'


class Get(PrefixOperator):
    r"""
    <dl>
    <dt>'<<name'
      <dd>reads a file and evaluates each expression, returning only the last one.
    </dl>

    >> Put[x + y, "example_file"]
    >> <<"example_file"
     = x + y

    >> Put[x + y, 2x^2 + 4z!, Cos[x] + I Sin[x], "example_file"]
    >> <<"example_file"
     = Cos[x] + I Sin[x]
    #> DeleteFile["example_file"]

    >> 40! >> "fourtyfactorial"
    >> FilePrint["fourtyfactorial"]
     | 815915283247897734345611269596115894272000000000
    >> <<"fourtyfactorial"
     = 815915283247897734345611269596115894272000000000
    #> DeleteFile["fourtyfactorial"]

    ## TODO: Requires EndPackage implemented
    ## 'Get' can also load packages:
    ## >> << "VectorAnalysis`"

    #> Get["SomeTypoPackage`"]
     : Cannot open SomeTypoPackage`.
     = $Failed

    ## Parser Tests
    #> Hold[<< ~/some_example/dir/] // FullForm
     = Hold[Get["~/some_example/dir/"]]
    #> Hold[<<`/.\-_:$*~?] // FullForm
     = Hold[Get["`/.\\-_:$*~?"]]
    """

    operator = '<<'
    precedence = 720
    attributes = ('Protected')

    def apply(self, path, evaluation):
        'Get[path_?StringQ]'
        pypath = path.get_string_value()
        try:
            with mathics_open(pypath, 'r') as f:
                result = f.readlines()
        except IOError:
            evaluation.message('General', 'noopen', path)
            return Symbol('$Failed')

        try:
            parse
            ParseError
        except NameError:
            from mathics.core.parser import parse

        from mathics.main import wait_for_line

        total_input = ""
        syntax_error_count = 0
        expr = Symbol('Null')

        for lineno, tmp in enumerate(result):
            total_input += ' ' + tmp
            if wait_for_line(total_input):
                continue
            try:
                expr = parse(total_input)
            except:  # FIXME: something weird is going on here
                syntax_error_count += 1
                if syntax_error_count <= 4:
                    print "Syntax Error (line {0} of {1})".format(
                        lineno + 1, pypath)
                if syntax_error_count == 4:
                    print "Supressing further syntax errors in {0}".format(
                        pypath)
            else:
                if expr is not None:
                    expr = expr.evaluate(evaluation)
                total_input = ""

        if total_input != "":
            # TODO:
            # evaluation.message('Syntax', 'sntue', 'line {0} of
            # {1}'.format(lineno, pypath))
            print 'Unexpected end of file (probably unfinished expression)'
            print '    (line {0} of "{1}").'.format(lineno, pypath)
            return Symbol('Null')

        return expr

    def apply_default(self, filename, evaluation):
        'Get[filename_]'
        expr = Expression('Get', filename)
        evaluation.message('General', 'stream', filename)
        return expr


class Put(BinaryOperator):
    """
    <dl>
    <dt>'$expr$ >> $filename$'
      <dd>write $expr$ to a file.
    <dt>'Put[$expr1$, $expr2$, ..., $"filename"$]'
      <dd>write a sequence of expressions to a file.
    </dl>

    >> 40! >> "fourtyfactorial"
    >> FilePrint["fourtyfactorial"]
     | 815915283247897734345611269596115894272000000000
    #> 40! >> fourtyfactorial
    #> FilePrint["fourtyfactorial"]
     | 815915283247897734345611269596115894272000000000

    #> Put[40!, fourtyfactorial]
     : fourtyfactorial is not string, InputStream[], or OutputStream[]
     = 815915283247897734345611269596115894272000000000 >> fourtyfactorial
    ## FIXME: final line should be
    ## = Put[815915283247897734345611269596115894272000000000, fourtyfactorial]
    #> DeleteFile["fourtyfactorial"]

    >> Put[50!, "fiftyfactorial"]
    >> FilePrint["fiftyfactorial"]
     | 30414093201713378043612608166064768844377641568960512000000000000
    #> DeleteFile["fiftyfactorial"]

    >> Put[10!, 20!, 30!, "factorials"]
    >> FilePrint["factorials"]
     | 3628800
     | 2432902008176640000
     | 265252859812191058636308480000000

    #> DeleteFile["factorials"]
     =

    #> Put[x + y, 2x^2 + 4z!, Cos[x] + I Sin[x], "example_file"]
    #> FilePrint["example_file"]
     | x + y
     | 2*x^2 + 4*z!
     | Cos[x] + I*Sin[x]
    #> DeleteFile["example_file"]
    """

    operator = '>>'
    precedence = 30

    def apply(self, exprs, filename, evaluation):
        'Put[exprs___, filename_?StringQ]'
        instream = Expression('OpenWrite', filename).evaluate(evaluation)
        name, n = instream.leaves
        result = self.apply_input(exprs, name, n, evaluation)
        Expression('Close', instream).evaluate(evaluation)
        return result

    def apply_input(self, exprs, name, n, evaluation):
        'Put[exprs___, OutputStream[name_, n_]]'
        global STREAMS
        stream = STREAMS.get(n.to_python())

        if stream is None or stream.closed:
            evaluation.message('Put', 'openx', Expression(
                'OutputSteam', name, n))
            return

        text = [evaluation.format_output(Expression(
            'InputForm', expr)) for expr in exprs.get_sequence()]
        text = u'\n'.join(text) + u'\n'
        text.encode('ascii')

        stream.write(text)

        return Symbol('Null')

    def apply_default(self, exprs, filename, evaluation):
        'Put[exprs___, filename_]'
        expr = Expression('Put', exprs, filename)
        evaluation.message('General', 'stream', filename)
        return expr

    def parse(self, args):
        if isinstance(args[2], Symbol):
            ptokens = args[2].parse_tokens
            args[2] = String(args[2])
            args[2].parse_tokens = ptokens

        return super(Put, self).parse(args)


class PutAppend(BinaryOperator):
    """
    <dl>
    <dt>'$expr$ >>> $filename$'
      <dd>append $expr$ to a file.
    <dt>'PutAppend[$expr1$, $expr2$, ..., $"filename"$]'
      <dd>write a sequence of expressions to a file.
    </dl>

    >> Put[50!, "factorials"]
    >> FilePrint["factorials"]
     | 30414093201713378043612608166064768844377641568960512000000000000

    >> PutAppend[10!, 20!, 30!, "factorials"]
    >> FilePrint["factorials"]
     | 30414093201713378043612608166064768844377641568960512000000000000
     | 3628800
     | 2432902008176640000
     | 265252859812191058636308480000000

    >> 60! >>> "factorials"
    >> FilePrint["factorials"]
     | 30414093201713378043612608166064768844377641568960512000000000000
     | 3628800
     | 2432902008176640000
     | 265252859812191058636308480000000
     | 8320987112741390144276341183223364380754172606361245952449277696409600000000000000

    >> "string" >>> factorials
    >> FilePrint["factorials"]
     | 30414093201713378043612608166064768844377641568960512000000000000
     | 3628800
     | 2432902008176640000
     | 265252859812191058636308480000000
     | 8320987112741390144276341183223364380754172606361245952449277696409600000000000000
     | "string"
    #> DeleteFile["factorials"];
    """

    operator = '>>>'
    precedence = 30
    attributes = ('Protected')

    def apply(self, exprs, filename, evaluation):
        'PutAppend[exprs___, filename_?StringQ]'
        instream = Expression('OpenAppend', filename).evaluate(evaluation)
        name, n = instream.leaves
        result = self.apply_input(exprs, name, n, evaluation)
        Expression('Close', instream).evaluate(evaluation)
        return result

    def apply_input(self, exprs, name, n, evaluation):
        'PutAppend[exprs___, OutputStream[name_, n_]]'
        global STREAMS
        stream = STREAMS.get(n.to_python())

        if stream is None or stream.closed:
            evaluation.message('Put', 'openx', Expression(
                'OutputSteam', name, n))
            return

        text = [unicode(e.do_format(
            evaluation, 'OutputForm').__str__()) for e in exprs.get_sequence()]
        text = u'\n'.join(text) + u'\n'
        text.encode('ascii')

        stream.write(text)

        return Symbol('Null')

    def apply_default(self, exprs, filename, evaluation):
        'PutAppend[exprs___, filename_]'
        expr = Expression('PutAppend', exprs, filename)
        evaluation.message('General', 'stream', filename)
        return expr

    def parse(self, args):
        if isinstance(args[2], Symbol):
            ptokens = args[2].parse_tokens
            args[2] = String(args[2])
            args[2].parse_tokens = ptokens

        return super(PutAppend, self).parse(args)


class FindFile(Builtin):
    """
    <dl>
    <dt>'FindFile[$name$]'
      <dd>searches '$Path' for the given filename.
    </dl>

    >> FindFile["ExampleData/sunflowers.jpg"]
     = ...

    >> FindFile["VectorAnalysis`"]
     = ...

    >> FindFile["VectorAnalysis`VectorAnalysis`"]
     = ...

    #> FindFile["SomeTypoPackage`"]
     = $Failed
    """

    attributes = ('Protected')

    messages = {
        'string': 'String expected at position 1 in `1`.',
    }

    def apply(self, name, evaluation):
        'FindFile[name_]'

        py_name = name.to_python()

        if not (isinstance(py_name, basestring) and
                py_name[0] == py_name[-1] == '"'):
            evaluation.message(
                'FindFile', 'string', Expression('FindFile', name))
            return
        py_name = py_name[1:-1]

        result = path_search(py_name)

        if result is None:
            return Symbol('$Failed')

        return String(os.path.abspath(result))


class FileNameSplit(Builtin):
    """
    <dl>
    <dt>'FileNameSplit["$filenams$"]'
      <dd>splits a $filename$ into a list of parts.
    </dl>

    >> FileNameSplit["example/path/file.txt"]
     = {example, path, file.txt}

    #> FileNameSplit["example/path", OperatingSystem -> x]
     : The value of option OperatingSystem -> x must be one of "MacOSX", "Windows", or "Unix".
     = {example, path}
    """

    attributes = ('Protected')

    options = {
        'OperatingSystem': '$OperatingSystem',
    }

    messages = {
        'ostype': ('The value of option OperatingSystem -> `1` '
                   'must be one of "MacOSX", "Windows", or "Unix".'),
    }

    def apply(self, filename, evaluation, options):
        'FileNameSplit[filename_?StringQ, OptionsPattern[FileExtension]]'

        path = filename.to_python()[1:-1]

        operating_system = options[
            'OperatingSystem'].evaluate(evaluation).to_python()

        if operating_system not in ['"MacOSX"', '"Windows"', '"Unix"']:
            evaluation.message('FileNameSplit', 'ostype', options[
                               'OperatingSystem'])
            if os.name == 'posix':
                operating_system = 'Unix'
            elif os.name == 'nt':
                operating_system = 'Windows'
            elif os.name == 'os2':
                operating_system = 'MacOSX'
            else:
                return

        # TODO Implement OperatingSystem Option

        result = []
        while path not in ['', SYS_ROOT_DIR]:
            path, ext = os.path.split(path)
            if ext != '':
                result.insert(0, ext)

        return from_python(result)


class FileNameJoin(Builtin):
    """
    <dl>
    <dt>'FileNameJoin[{"$dir_1$", "$dir_2$", ...}]'
      <dd>joins the $dir_i$ togeather into one path.
    </dl>

    >> FileNameJoin[{"dir1", "dir2", "dir3"}]
     = ...

    >> FileNameJoin[{"dir1", "dir2", "dir3"}, OperatingSystem -> "Unix"]
     = dir1/dir2/dir3

    ## TODO
    ## #> FileNameJoin[{"dir1", "dir2", "dir3"}, OperatingSystem -> "Windows"]
    ##  = dir1\dir2\dir3
    """

    attributes = ('Protected')

    options = {
        'OperatingSystem': '$OperatingSystem',
    }

    messages = {
        'ostype': ('The value of option OperatingSystem -> `1` '
                   'must be one of "MacOSX", "Windows", or "Unix".'),
    }

    def apply(self, pathlist, evaluation, options):
        'FileNameJoin[pathlist_?ListQ, OptionsPattern[FileNameJoin]]'

        py_pathlist = pathlist.to_python()
        if not all(isinstance(p, basestring) and p[0] == p[-1] == '"'
                   for p in py_pathlist):
            return
        py_pathlist = [p[1:-1] for p in py_pathlist]

        operating_system = options[
            'OperatingSystem'].evaluate(evaluation).to_python()

        if operating_system not in ['"MacOSX"', '"Windows"', '"Unix"']:
            evaluation.message('FileNameSplit', 'ostype', options[
                               'OperatingSystem'])
            if os.name == 'posix':
                operating_system = 'Unix'
            elif os.name == 'nt':
                operating_system = 'Windows'
            elif os.name == 'os2':
                operating_system = 'MacOSX'
            else:
                return

        # TODO Implement OperatingSystem Option

        result = os.path.join(*py_pathlist)

        return from_python(result)


class FileExtension(Builtin):
    """
    <dl>
    <dt>'FileExtension["$file$"]'
      <dd>gives the extension for the specified file name.
    </dl>

    >> FileExtension["file.txt"]
     = txt

    >> FileExtension["file.tar.gz"]
     = gz

    #> FileExtension["file."]
     = 
    #> FileExtension["file"]
     = 
    """

    attributes = ('Protected')

    options = {
        'OperatingSystem': '$OperatingSystem',
    }

    def apply(self, filename, evaluation, options):
        'FileExtension[filename_?StringQ, OptionsPattern[FileExtension]]'
        path = filename.to_python()[1:-1]
        filename_base, filename_ext = os.path.splitext(path)
        filename_ext = filename_ext.lstrip('.')
        return from_python(filename_ext)


class FileBaseName(Builtin):
    """
    <dl>
    <dt>'FileBaseName["$file$"]'
      <dd>gives the base name for the specified file name.
    </dl>

    >> FileBaseName["file.txt"]
     = file

    >> FileBaseName["file.tar.gz"]
     = file.tar

    #> FileBaseName["file."]
     = file

    #> FileBaseName["file"]
     = file
    """

    attributes = ('Protected')

    options = {
        'OperatingSystem': '$OperatingSystem',
    }

    def apply(self, filename, evaluation, options):
        'FileBaseName[filename_?StringQ, OptionsPattern[FileBaseName]]'
        path = filename.to_python()[1:-1]

        filename_base, filename_ext = os.path.splitext(path)
        return from_python(filename_base)


class DirectoryName(Builtin):
    """
    <dl>
    <dt>'DirectoryName["$name$"]'
      <dd>extracts the directory name from a filename.
    </dl>

    >> DirectoryName["a/b/c"]
     = a/b

    >> DirectoryName["a/b/c", 2]
     = a

    #> DirectoryName["a/b/c", 3] // InputForm
     = ""
    #> DirectoryName[""] // InputForm
     = ""

    #> DirectoryName["a/b/c", x]
     : Positive machine-sized integer expected at position 2 in DirectoryName[a/b/c, x].
     = DirectoryName[a/b/c, x]

    #> DirectoryName["a/b/c", -1]
     : Positive machine-sized integer expected at position 2 in DirectoryName[a/b/c, -1].
     = DirectoryName[a/b/c, -1]

    #> DirectoryName[x]
     : String expected at position 1 in DirectoryName[x].
     = DirectoryName[x]
    """

    attributes = ('Protected')

    options = {
        'OperatingSystem': '$OperatingSystem',
    }

    messages = {
        'string': 'String expected at position 1 in `1`.',
        'intpm': ('Positive machine-sized integer expected at '
                  'position 2 in `1`.'),
    }

    def apply(self, name, n, evaluation, options):
        'DirectoryName[name_, n_, OptionsPattern[DirectoryName]]'

        if n is None:
            expr = Expression('DirectoryName', name)
            py_n = 1
        else:
            expr = Expression('DirectoryName', name, n)
            py_n = n.to_python()

        if not (isinstance(py_n, (int, long)) and py_n > 0):
            evaluation.message('DirectoryName', 'intpm', expr)
            return

        py_name = name.to_python()
        if not (isinstance(py_name, basestring) and
                py_name[0] == py_name[-1] == '"'):
            evaluation.message('DirectoryName', 'string', expr)
            return
        py_name = py_name[1:-1]

        result = py_name
        for i in range(py_n):
            (result, tmp) = os.path.split(result)

        return String(result)

    def apply1(self, name, evaluation, options):
        'DirectoryName[name_, OptionsPattern[DirectoryName]]'
        return self.apply(name, None, evaluation, options)


class FileNameDepth(Builtin):
    """
    <dl>
    <dt>'FileNameDepth["$name$"]'
      <dd>gives the number of path parts in the given filename.
    </dl>

    >> FileNameDepth["a/b/c"]
     = 3

    >> FileNameDepth["a/b/c/"]
     = 3

    #> FileNameDepth[x]
     = FileNameDepth[x]

    #> FileNameDepth[$RootDirectory]
     = 0
    """

    attributes = ('Protected')

    options = {
        'OperatingSystem': '$OperatingSystem',
    }

    rules = {
        'FileNameDepth[name_?StringQ]': 'Length[FileNameSplit[name]]',
    }


class AbsoluteFileName(Builtin):
    """
    <dl>
    <dt>'AbsoluteFileName["$name$"]'
      <dd>returns the absolute version of the given filename.
    </dl>

    >> AbsoluteFileName["ExampleData/sunflowers.jpg"]
     = ...

    #> AbsoluteFileName["Some/NonExistant/Path.ext"]
     : File not found during AbsoluteFileName[Some/NonExistant/Path.ext].
     = $Failed
    """

    attributes = ('Protected')

    messages = {
        'fstr': (
            'File specification x is not a string of one or more characters.'),
        'nffil': 'File not found during `1`.',
    }

    def apply(self, name, evaluation):
        'AbsoluteFileName[name_]'

        py_name = name.to_python()

        if not (isinstance(py_name, basestring) and
                py_name[0] == py_name[-1] == '"'):
            evaluation.message('AbsoluteFileName', 'fstr', name)
            return
        py_name = py_name[1:-1]

        result = path_search(py_name)

        if result is None:
            evaluation.message('AbsoluteFileName', 'nffil',
                               Expression('AbsoluteFileName', name))
            return Symbol('$Failed')

        return String(os.path.abspath(result))


class ExpandFileName(Builtin):
    """
    <dl>
    <dt>'ExpandFileName["$name$"]'
      <dd>expands $name$ to an absolute filename for your system.
    </dl>

    >> ExpandFileName["ExampleData/sunflowers.jpg"]
     = ...
    """

    attributes = ('Protected')

    messages = {
        'string': 'String expected at position 1 in `1`.',
    }

    def apply(self, name, evaluation):
        'ExpandFileName[name_]'

        py_name = name.to_python()

        if not (isinstance(py_name, basestring) and
                py_name[0] == py_name[-1] == '"'):
            evaluation.message('ExpandFileName', 'string',
                               Expression('ExpandFileName', name))
            return
        py_name = py_name[1:-1]

        return String(os.path.abspath(py_name))


class ReadList(Read):
    """
    <dl>
    <dt>'ReadList["$file$"]'
      <dd>Reads all the expressions until the end of file.
    <dt>'ReadList["$file$", $type$]'
      <dd>Reads objects of a specified type until the end of file.
    <dt>'ReadList["$file$", {$type1$, $type2$, ...}]'
      <dd>Reads a sequence of specified types until the end of file.
    </dl>

    >> ReadList[StringToStream["a 1 b 2"], {Word, Number}]
     = {{a, 1}, {b, 2}}

    >> str = StringToStream["abc123"];
    >> ReadList[str]
     = {abc123}
    >> InputForm[%]
     = {"abc123"}

    #> ReadList[str, "Invalid"]
     : "Invalid" is not a valid format specification.
     = ReadList[..., Invalid]
    #> Close[str];


    #> ReadList[StringToStream["a 1 b 2"], {Word, Number}, 1]
     = {{a, 1}}
    """

    # TODO
    """
    #> ReadList[StringToStream["a 1 b 2"], {Word, Number}, -1]
     : Non-negative machine-sized integer expected at position 3 in ReadList[InputStream[String, ...], {Word, Number}, -1].
     = ReadList[InputStream[String, ...], {Word, Number}, -1]
    """

    # TODO: Expression type
    """
    #> ReadList[StringToStream["123 45 x y"], Expression]
     = {5535 x y}
    """

    # TODO: Accept newlines in input
    """
    >> ReadList[StringToStream["123\nabc"]]
     = {123, abc}
    >> InputForm[%]
     = {123, abc}
    """

    rules = {
        'ReadList[stream_]': 'ReadList[stream, Expression]',
    }

    attributes = ('Protected')

    options = {
        'NullRecords': 'False',
        'NullWords': 'False',
        'RecordSeparators': '{"\r\n", "\n", "\r"}',
        'TokenWords': '{}',
        'WordSeparators': '{" ", "\t"}',
    }

    def apply(self, name, n, types, evaluation, options):
        'ReadList[InputStream[name_, n_], types_, OptionsPattern[ReadList]]'

        # Options
        # TODO: Implement extra options
        # py_options = self.check_options(options)
        # null_records = py_options['NullRecords']
        # null_words = py_options['NullWords']
        # record_separators = py_options['RecordSeparators']
        # token_words = py_options['TokenWords']
        # word_separators = py_options['WordSeparators']

        result = []
        while True:
            tmp = super(ReadList, self).apply(
                name, n, types, evaluation, options)

            if tmp == Symbol('$Failed'):
                return

            if tmp.to_python() == 'EndOfFile':
                break
            result.append(tmp)
        return from_python(result)

    def apply_m(self, name, n, types, m, evaluation, options):
        'ReadList[InputStream[name_,n_], types_, m_, OptionsPattern[ReadList]]'

        # Options
        # TODO: Implement extra options
        # py_options = self.check_options(options)
        # null_records = py_options['NullRecords']
        # null_words = py_options['NullWords']
        # record_separators = py_options['RecordSeparators']
        # token_words = py_options['TokenWords']
        # word_separators = py_options['WordSeparators']

        py_m = m.get_int_value()
        if py_m < 0:
            evaluation.message('ReadList', 'intnm', Expression(
                'ReadList', Expression('InputStream', name, n), types, m))
            return

        result = []
        for i in range(py_m):
            tmp = super(ReadList, self).apply(
                name, n, types, evaluation, options)

            if tmp == Symbol('$Failed'):
                return

            if tmp.to_python() == 'EndOfFile':
                break
            result.append(tmp)
        return from_python(result)


class FilePrint(Builtin):
    """
    <dl>
    <dt>'FilePrint[$file$]'
      <dd>prints the raw contents of $file$.
    </dl>

    #> exp = Sin[1];
    #> FilePrint[exp]
     : File specification Sin[1] is not a string of one or more characters.
     = FilePrint[Sin[1]]

    ## Return $Failed on special files
    #> FilePrint["/dev/zero"]
     = $Failed
    #> FilePrint["/dev/random"]
     = $Failed
    #> FilePrint["/dev/null"]
     = $Failed

    #> FilePrint["somenonexistantpath_h47sdmk^&h4"]
     : Cannot open somenonexistantpath_h47sdmk^&h4.
     = FilePrint[somenonexistantpath_h47sdmk^&h4]

    #> FilePrint[""]
     : File specification  is not a string of one or more characters.
     = FilePrint[]
    """

    messages = {
        'fstr': ('File specification `1` is not a string of '
                 'one or more characters.'),
    }

    options = {
        'CharacterEncoding': '$CharacterEncoding',
        'RecordSeparators': '{"\r\n", "\n", "\r"}',
        'WordSeparators': '{" ", "\t"}',
    }

    attributes = ('Protected')

    def apply(self, path, evaluation, options):
        'FilePrint[path_ OptionsPattern[FilePrint]]'
        pypath = path.to_python()
        if not (isinstance(pypath, basestring) and
                pypath[0] == pypath[-1] == '"' and len(pypath) > 2):
            evaluation.message('FilePrint', 'fstr', path)
            return
        pypath = path_search(pypath[1:-1])

        # Options
        record_separators = options['RecordSeparators'].to_python()
        assert isinstance(record_separators, list)
        assert all(isinstance(s, basestring) and s[
                   0] == s[-1] == '"' for s in record_separators)
        record_separators = [s[1:-1] for s in record_separators]

        if pypath is None:
            evaluation.message('General', 'noopen', path)
            return

        if not os.path.isfile(pypath):
            return Symbol("$Failed")

        try:
            with mathics_open(pypath, 'r') as f:
                result = f.read()
        except IOError:
            evaluation.message('General', 'noopen', path)
            return

        result = [result]
        for sep in record_separators:
            result = [item for res in result for item in res.split(sep)]

        if result[-1] == '':
            result = result[:-1]

        for res in result:
            evaluation.print_out(from_python(res))

        return Symbol('Null')


class Close(Builtin):
    """
    <dl>
    <dt>'Close[$stream$]'
      <dd>closes an input or output stream.
    </dl>

    >> Close[StringToStream["123abc"]]
     = String

    >> Close[OpenWrite[]]
     = ...
    """

    attributes = ('Protected')

    def apply_input(self, name, n, evaluation):
        'Close[InputStream[name_, n_]]'
        global STREAMS
        stream = STREAMS.get(n.to_python())

        if stream is None or stream.closed:
            evaluation.message('General', 'openx', name)
            return

        stream.close()
        return name

    def apply_output(self, name, n, evaluation):
        'Close[OutputStream[name_, n_]]'
        global STREAMS
        stream = STREAMS.get(n.to_python())

        if stream is None or stream.closed:
            evaluation.message('General', 'openx', name)
            return

        stream.close()
        return name

    def apply_default(self, stream, evaluation):
        'Close[stream_]'
        evaluation.message('General', 'stream', stream)
        return


class StreamPosition(Builtin):
    """
    <dl>
    <dt>'StreamPosition[$stream$]'
      <dd>returns the current position in a stream as an integer.
    </dl>

    >> str = StringToStream["Mathics is cool!"]
     = ...

    >> Read[str, Word]
     = Mathics

    >> StreamPosition[str]
     = 7
    """

    attributes = ('Protected')

    def apply_input(self, name, n, evaluation):
        'StreamPosition[InputStream[name_, n_]]'
        global STREAMS
        stream = STREAMS.get(n.to_python())

        if stream is None or stream.closed:
            evaluation.message('General', 'openx', name)
            return

        return from_python(stream.tell())

    def apply_output(self, name, n, evaluation):
        'StreamPosition[OutputStream[name_, n_]]'
        global STREAMS
        stream = STREAMS.get(n.to_python())

        if stream is None or stream.closed:
            evaluation.message('General', 'openx', name)
            return

        return from_python(stream.tell())

    def apply_default(self, stream, evaluation):
        'StreamPosition[stream_]'
        evaluation.message('General', 'stream', stream)
        return


class SetStreamPosition(Builtin):
    """
    <dl>
    <dt>'SetStreamPosition[$stream$, $n$]'
      <dd>sets the current position in a stream.
    </dl>

    >> str = StringToStream["Mathics is cool!"]
     = ...

    >> SetStreamPosition[str, 8]
     = 8

    >> Read[str, Word]
     = is

    #> SetStreamPosition[str, -5]
     : Python2 cannot handle negative seeks.
     = 10

    >> SetStreamPosition[str, Infinity]
     = 16
    """

    # TODO: Seeks beyond stream should return stmrng message
    """
    #> SetStreamPosition[str, 40]
     = ERROR_MESSAGE_HERE
    """

    messages = {
        'int': 'Integer expected at position 2 in `1`.',
        'stmrng': (
            'Cannot set the current point in stream `1` to position `2`. The '
            'requested position exceeds the number of characters in the file'),
        'python2': 'Python2 cannot handle negative seeks.',  # FIXME: Python3?
    }

    attributes = ('Protected')

    def apply_input(self, name, n, m, evaluation):
        'SetStreamPosition[InputStream[name_, n_], m_]'
        global STREAMS
        stream = STREAMS.get(n.to_python())

        if stream is None or stream.closed:
            evaluation.message('General', 'openx', name)
            return

        if not stream.seekable:
            raise NotImplementedError

        seekpos = m.to_python()
        if not (isinstance(seekpos, int) or seekpos == float('inf')):
            evaluation.message('SetStreamPosition', 'stmrng',
                               Expression('InputStream', name, n), m)
            return

        try:
            if seekpos == float('inf'):
                stream.seek(0, 2)
            else:
                if seekpos < 0:
                    stream.seek(seekpos, 2)
                else:
                    stream.seek(seekpos)
        except IOError:
            evaluation.message('SetStreamPosition', 'python2')

        return from_python(stream.tell())

    def apply_output(self, name, n, m, evaluation):
        'SetStreamPosition[OutputStream[name_, n_], m_]'
        return self.apply_input(name, n, m, evaluation)

    def apply_default(self, stream, evaluation):
        'SetStreamPosition[stream_]'
        evaluation.message('General', 'stream', stream)
        return


class Skip(Read):
    """
    <dl>
    <dt>'Skip[$stream$, $type$]'
      <dd>skips ahead in an input steream by one object of the specified $type$.
    <dt>'Skip[$stream$, $type$, $n$]'
      <dd>skips ahead in an input steream by $n$ objects of the specified $type$.
    </dl>

    >> str = StringToStream["a b c d"];
    >> Read[str, Word]
     = a
    >> Skip[str, Word]
    >> Read[str, Word]
     = c
    #> Close[str];

    >> str = StringToStream["a b c d"];
    >> Read[str, Word]
     = a
    >> Skip[str, Word, 2]
    >> Read[str, Word]
     = d
    #> Skip[str, Word]
     = EndOfFile
    #> Close[str];
    """

    rules = {
        'Skip[InputStream[name_, n_], types_]':
        'Skip[InputStream[name, n], types, 1]',
    }

    messages = {
        'intm':
        'Non-negative machine-sized integer expected at position 3 in `1`',
    }

    options = {
        'AnchoredSearch': 'False',
        'IgnoreCase': 'False',
        'WordSearch': 'False',
        'RecordSeparators': '{"\r\n", "\n", "\r"}',
        'WordSeparators': '{" ", "\t"}',
    }

    attributes = ('Protected')

    def apply(self, name, n, types, m, evaluation, options):
        'Skip[InputStream[name_, n_], types_, m_, OptionsPattern[Skip]]'

        # Options
        # TODO Implement extra options
        # py_options = self.check_options(options)
        # null_records = py_options['NullRecords']
        # null_words = py_options['NullWords']
        # record_separators = py_options['RecordSeparators']
        # token_words = py_options['TokenWords']
        # word_separators = py_options['WordSeparators']

        py_m = m.to_python()
        if not (isinstance(py_m, int) and py_m > 0):
            evaluation.message('Skip', 'intm', Expression(
                'Skip', Expression('InputStream', name, n), types, m))
            return
        for i in range(py_m):
            result = super(Skip, self).apply(
                name, n, types, evaluation, options)
            if result.to_python() == 'EndOfFile':
                return Symbol('EndOfFile')
        return Symbol('Null')


class Find(Read):
    """
    <dl>
    <dt>'Find[$stream$, $text$]'
      <dd>find the first line in $stream$ that contains $text$.
    </dl>

    >> str = OpenRead["ExampleData/EinsteinSzilLetter.txt"];
    >> Find[str, "uranium"]
     = in manuscript, leads me to expect that the element uranium may be turned into
    >> Find[str, "uranium"]
     = become possible to set up a nuclear chain reaction in a large mass of uranium,
    >> Close[str]
     = ...

    >> str = OpenRead["ExampleData/EinsteinSzilLetter.txt"];
    >> Find[str, {"energy", "power"} ]
     = a new and important source of energy in the immediate future. Certain aspects
    >> Find[str, {"energy", "power"} ]
     = by which vast amounts of power and large quantities of new radium-like
    >> Close[str]
     = ...
    """

    attributes = ('Protected')

    options = {
        'AnchoredSearch': 'False',
        'IgnoreCase': 'False',
        'WordSearch': 'False',
        'RecordSeparators': '{"\r\n", "\n", "\r"}',
        'WordSeparators': '{" ", "\t"}',
    }

    def apply(self, name, n, text, evaluation, options):
        'Find[InputStream[name_, n_], text_, OptionsPattern[Find]]'

        # Options
        # TODO Implement extra options
        # py_options = self.check_options(options)
        # anchored_search = py_options['AnchoredSearch']
        # ignore_case = py_options['IgnoreCase']
        # word_search = py_options['WordSearch']
        # record_separators = py_options['RecordSeparators']
        # word_separators = py_options['WordSeparators']

        py_text = text.to_python()

        if not isinstance(py_text, list):
            py_text = [py_text]

        if not all(isinstance(t, basestring) and
                   t[0] == t[-1] == '"' for t in py_text):
            evaluation.message('Find', 'unknown', Expression(
                'Find', Expression('InputStream', name, n), text))
            return

        py_text = [t[1:-1] for t in py_text]

        while True:
            tmp = super(Find, self).apply(name, n, Symbol(
                'Record'), evaluation, options)
            py_tmp = tmp.to_python()[1:-1]

            if py_tmp == 'EndOfFile':
                evaluation.message('Find', 'notfound', Expression(
                    'Find', Expression('InputStream', name, n), text))
                return Symbol("$Failed")

            for t in py_text:
                if py_tmp.find(t) != -1:
                    return from_python(py_tmp)


class FindList(Builtin):
    """
    <dl>
    <dt>'FindList[$file$, $text$]'
      <dd>returns a list of all lines in $file$ that contain $text$.
    <dt>'FindList[$file$, {$text1$, $text2$, ...}]'
      <dd>returns a list of all lines in $file$ that contain any of the specified string.
    <dt>'FindList[{$file1$, $file2$, ...}, ...]'
      <dd>returns a list of all lines in any of the $filei$ that contain the specified strings.
    </dl>

    >> str = FindList["ExampleData/EinsteinSzilLetter.txt", "uranium"];
    #> Length[str]
     = 7

    >> FindList["ExampleData/EinsteinSzilLetter.txt", "uranium", 1]
     = {in manuscript, leads me to expect that the element uranium may be turned into}

    #> FindList["ExampleData/EinsteinSzilLetter.txt", "project"]
     = {}

    #> FindList["ExampleData/EinsteinSzilLetter.txt", "uranium", 0]
     = $Failed
    """

    messages = {
        'strs':
        'String or non-empty list of strings expected at position `1` in `2`.',
        'intnm':
        'Non-negative machine-sized integer expected at position `1` in `2`.',
    }

    attributes = ('Protected')

    options = {
        'AnchoredSearch': 'False',
        'IgnoreCase': 'False',
        'RecordSeparators': '{"\r\n", "\n", "\r"}',
        'WordSearch': 'False',
        'WordSeparators': '{" ", "\t"}',
    }

    # TODO: Extra options AnchoredSearch, IgnoreCase RecordSeparators,
    # WordSearch, WordSeparators this is probably best done with a regex

    def apply_without_n(self, filename, text, evaluation, options):
        'FindList[filename_, text_, OptionsPattern[FindList]]'
        return self.apply(filename, text, None, evaluation, options)

    def apply(self, filename, text, n, evaluation, options):
        'FindList[filename_, text_, n_, OptionsPattern[FindList]]'
        py_text = text.to_python()
        py_name = filename.to_python()
        if n is None:
            py_n = None
            expr = Expression('FindList', filename, text)
        else:
            py_n = n.to_python()
            expr = Expression('FindList', filename, text, n)

        if not isinstance(py_text, list):
            py_text = [py_text]

        if not isinstance(py_name, list):
            py_name = [py_name]

        if not all(isinstance(t, basestring) and
                   t[0] == t[-1] == '"' for t in py_name):
            evaluation.message('FindList', 'strs', '1', expr)
            return Symbol('$Failed')

        if not all(isinstance(t, basestring) and
                   t[0] == t[-1] == '"' for t in py_text):
            evaluation.message('FindList', 'strs', '2', expr)
            return Symbol('$Failed')

        if not ((isinstance(py_n, int) and py_n >= 0) or py_n is None):
            evaluation.message('FindList', 'intnm', '3', expr)
            return Symbol('$Failed')

        if py_n == 0:
            return Symbol('$Failed')

        py_text = [t[1:-1] for t in py_text]
        py_name = [t[1:-1] for t in py_name]

        results = []
        for path in py_name:
            try:
                with mathics_open(path, 'r') as f:
                    lines = f.readlines()
            except IOError:
                evaluation.message('General', 'noopen', path)
                return

            result = []
            for line in lines:
                for t in py_text:
                    if line.find(t) != -1:
                        result.append(line[:-1])
            results.append(result)

        results = [r for result in results for r in result]

        if isinstance(py_n, int):
            results = results[:min(py_n, len(results))]

        return from_python(results)


class InputStream(Builtin):
    """
    <dl>
    <dt>'InputStream[$name$, $n$]'
      <dd>represents an input stream.
    </dl>

    >> str = StringToStream["Mathics is cool!"]
     = ...
    >> Close[str]
     = String
    """

    attributes = ('Protected')

    def apply(self, name, n, evaluation):
        'InputStream[name_, n_]'
        return


class OutputStream(Builtin):
    """
    <dl>
    <dt>'OutputStream[$name$, $n$]'
      <dd>represents an output stream.
    </dl>

    >> OpenWrite[]
     = ...
    >> Close[%]
     = ...
    """

    attributes = ('Protected')

    def apply(self, name, n, evaluation):
        'OutputStream[name_, n_]'
        return


class StringToStream(Builtin):
    """
    <dl>
    <dt>'StringToStream[$string$]'
      <dd>converts a $string$ to an open input stream.
    </dl>

    >> StringToStream["abc 123"]
     = ...
    #> Close[%]
     = String
    """

    attributes = ('Protected')

    def apply(self, string, evaluation):
        'StringToStream[string_]'
        pystring = string.to_python()[1:-1]
        stream = io.StringIO(unicode(pystring))
        n = _put_stream(stream)
        result = Expression('InputStream', from_python('String'), n)

        global _STREAMS
        _STREAMS[n] = result

        return result


class Streams(Builtin):
    """
    <dl>
    <dt>'Streams[]'
      <dd>returns a list of all open streams.
    </dl>

    >> Streams[]
     = ...
    """

    attributes = ('Protected')

    def apply(self, evaluation):
        'Streams[]'
        global STREAMS
        global _STREAMS
        global NSTREAMS

        try:
            _STREAMS
        except NameError:
            STREAMS = {}    # Python repr
            _STREAMS = {}   # Mathics repr
            NSTREAMS = 0    # Max stream number
        return Expression('List', *_STREAMS.values())


class Compress(Builtin):
    """
    <dl>
    <dt>'Compress[$expr$]'
      <dd>gives a compressed string representation of $expr$.
    </dl>

    >> Compress[N[Pi, 10]]
     = ...
    """

    attributes = ('Protected')

    options = {
        'Method': '{}',
    }

    def apply(self, expr, evaluation, options):
        'Compress[expr_, OptionsPattern[Compress]]'

        string = expr.do_format(evaluation, 'FullForm').__str__()
        string = string.encode('utf-8')

        # TODO Implement other Methods
        result = zlib.compress(string)
        result = base64.encodestring(result)

        return from_python(result)


class Uncompress(Builtin):
    """
    <dl>
    <dt>'Uncompress["$string$"]'
      <dd>recovers an expression from a string generated by 'Compress'.
    </dl>

    >> Compress["Mathics is cool"]
     = eJxT8k0sychMLlbILFZIzs/PUQIANFwF1w==
    >> Uncompress[%]
     = Mathics is cool

    >> a = x ^ 2 + y Sin[x] + 10 Log[15];
    >> b = Compress[a];
    >> Uncompress[b]
     = x ^ 2 + y Sin[x] + 10 Log[15]
    """

    attributes = ('Protected')

    def apply(self, string, evaluation):
        'Uncompress[string_?StringQ]'
        string = string.to_python()[1:-1]
        string = base64.decodestring(string)
        tmp = zlib.decompress(string)
        tmp = tmp.decode('utf-8')

        try:
            expr = parse(tmp)
        except NameError:
            from mathics.core.parser import parse
            expr = parse(tmp)

        return expr


class FileByteCount(Builtin):
    """
    <dl>
    <dt>'FileByteCount[$file$]'
      <dd>returns the number of bytes in $file$.
    </dl>

    >> FileByteCount["ExampleData/sunflowers.jpg"]
     = 142286
    """

    messages = {
        'fstr':
        'File specification `1` is not a string of one or more characters.',
    }

    def apply(self, filename, evaluation):
        'FileByteCount[filename_]'
        py_filename = filename.to_python()
        if not (isinstance(py_filename, basestring) and
                py_filename[0] == py_filename[-1] == '"'):
            evaluation.message('FileByteCount', 'fstr', filename)
            return
        py_filename = py_filename[1:-1]

        try:
            with mathics_open(py_filename, 'rb') as f:
                count = 0
                tmp = f.read(1)
                while tmp != '':
                    count += 1
                    tmp = f.read(1)

        except IOError:
            evaluation.message('General', 'noopen', filename)
            return

        return from_python(count)


class FileHash(Builtin):
    """
    <dl>
    <dt>'FileHash[$file$]'
      <dd>returns an integer hash for the given $file$.
    <dt>'FileHash[$file$, $type$]'
      <dd>returns an integer hash of the specified $type$ for the given $file$.</dd>
      <dd>The types supported are "MD5", "Adler32", "CRC32", "SHA", "SHA224", "SHA256", "SHA384", and "SHA512".</dd>
    </dl>

    >> FileHash["ExampleData/sunflowers.jpg"]
     = 109937059621979839952736809235486742106

    >> FileHash["ExampleData/sunflowers.jpg", "MD5"]
     = 109937059621979839952736809235486742106

    >> FileHash["ExampleData/sunflowers.jpg", "Adler32"]
     = 1607049478

    >> FileHash["ExampleData/sunflowers.jpg", "SHA256"]
     = 111619807552579450300684600241129773909359865098672286468229443390003894913065

    #> FileHash["ExampleData/sunflowers.jpg", "CRC32"]
     = 933095683
    #> FileHash["ExampleData/sunflowers.jpg", "SHA"]
     = 851696818771101405642332645949480848295550938123
    #> FileHash["ExampleData/sunflowers.jpg", "SHA224"]
     = 8723805623766373862936267623913366865806344065103917676078120867011
    #> FileHash["ExampleData/sunflowers.jpg", "SHA384"]
     = 28288410602533803613059815846847184383722061845493818218404754864571944356226472174056863474016709057507799332611860
    #> FileHash["ExampleData/sunflowers.jpg", "SHA512"]
     = 10111462070211820348006107532340854103555369343736736045463376555356986226454343186097958657445421102793096729074874292511750542388324853755795387877480102

    #> FileHash["ExampleData/sunflowers.jpg", xyzsymbol]
     = FileHash[ExampleData/sunflowers.jpg, xyzsymbol]
    #> FileHash["ExampleData/sunflowers.jpg", "xyzstr"]
     = FileHash[ExampleData/sunflowers.jpg, xyzstr]
    #> FileHash[xyzsymbol]
     = FileHash[xyzsymbol]
    """

    rules = {
        'FileHash[filename_?StringQ]': 'FileHash[filename, "MD5"]',
    }

    attributes = ('Protected', 'ReadProtected')

    def apply(self, filename, hashtype, evaluation):
        'FileHash[filename_?StringQ, hashtype_?StringQ]'
        py_hashtype = hashtype.to_python()
        py_filename = filename.to_python()

        # TODO: MD2?
        supported_hashes = {
            'Adler32': zlib.adler32,
            'CRC32': zlib.crc32,
            'MD5': lambda s: int(hashlib.md5(s).hexdigest(), 16),
            'SHA': lambda s: int(hashlib.sha1(s).hexdigest(), 16),
            'SHA224': lambda s: int(hashlib.sha224(s).hexdigest(), 16),
            'SHA256': lambda s: int(hashlib.sha256(s).hexdigest(), 16),
            'SHA384': lambda s: int(hashlib.sha384(s).hexdigest(), 16),
            'SHA512': lambda s: int(hashlib.sha512(s).hexdigest(), 16),
        }

        py_hashtype = py_hashtype[1:-1]
        py_filename = py_filename[1:-1]

        hash_func = supported_hashes.get(py_hashtype)
        if hash_func is None:
            return

        try:
            with mathics_open(py_filename, 'rb') as f:
                dump = f.read()
        except IOError:
            evaluation.message('General', 'noopen', filename)
            return

        return from_python(hash_func(dump))


class FileDate(Builtin):
    """
    <dl>
    <dt>'FileDate[$file$, $types$]'
      <dd>returns the time and date at which the file was last modified.
    </dl>

    >> FileDate["ExampleData/sunflowers.jpg"]
     = ...

    >> FileDate["ExampleData/sunflowers.jpg", "Access"]
     = ...

    >> FileDate["ExampleData/sunflowers.jpg", "Creation"]
     = ...

    >> FileDate["ExampleData/sunflowers.jpg", "Change"]
     = ...

    >> FileDate["ExampleData/sunflowers.jpg", "Modification"]
     = ...

    >>  FileDate["ExampleData/sunflowers.jpg", "Rules"]
     = ...

    #>  FileDate["MathicsNonExistantExample"]
     : File not found during FileDate[MathicsNonExistantExample].
     = FileDate[MathicsNonExistantExample]
    #>  FileDate["MathicsNonExistantExample", "Modification"]
     : File not found during FileDate[MathicsNonExistantExample, Modification].
     = FileDate[MathicsNonExistantExample, Modification]

    #> FileDate["ExampleData/sunflowers.jpg", "Fail"]
     : Date type Fail should be "Access", "Modification", "Creation" (Windows only), "Change" (Macintosh and Unix only), or "Rules".
     = FileDate[ExampleData/sunflowers.jpg, Fail]
    """

    messages = {
        'nffil': 'File not found during `1`.',
        'datetype': ('Date type Fail should be "Access", "Modification", '
                     '"Creation" (Windows only), '
                     '"Change" (Macintosh and Unix only), or "Rules".'),
    }

    rules = {
        'FileDate[filepath_?StringQ, "Rules"]':
        '''{"Access" -> FileDate[filepath, "Access"],
            "Creation" -> FileDate[filepath, "Creation"],
            "Change" -> FileDate[filepath, "Change"],
            "Modification" -> FileDate[filepath, "Modification"]}''',
    }

    attributes = ('Protected')

    def apply(self, path, timetype, evaluation):
        'FileDate[path_, timetype_]'
        py_path = path_search(path.to_python()[1:-1])

        if py_path is None:
            if timetype is None:
                evaluation.message(
                    'FileDate', 'nffil', Expression('FileDate', path))
            else:
                evaluation.message('FileDate', 'nffil', Expression(
                    'FileDate', path, timetype))
            return

        if timetype is None:
            time_type = 'Modification'
        else:
            time_type = timetype.to_python()[1:-1]

        if time_type == 'Access':
            result = os.path.getatime(py_path)
        elif time_type == 'Creation':
            if os.name == 'posix':
                return Expression('Missing', 'NotApplicable')
            result = os.path.getctime(py_path)
        elif time_type == 'Change':
            if os.name != 'posix':
                return Expression('Missing', 'NotApplicable')
            result = os.path.getctime(py_path)
        elif time_type == 'Modification':
            result = os.path.getmtime(py_path)
        else:
            evaluation.message('FileDate', 'datetype')
            return

        # Offset for system epoch
        epochtime = Expression('AbsoluteTime', time.strftime(
            "%Y-%m-%d %H:%M",
            time.gmtime(0))).to_python(n_evaluation=evaluation)
        result += epochtime

        return Expression('DateList', from_python(result))

    def apply_default(self, path, evaluation):
        'FileDate[path_]'
        return self.apply(path, None, evaluation)


class SetFileDate(Builtin):
    """
    <dl>
    <dt>'SetFileDate["$file$"]'
      <dd>set the file access and modification dates of $file$ to the current date.
    <dt>'SetFileDate["$file$", $date$]'
      <dd>set the file access and modification dates of $file$ to the specified date list.
    <dt>'SetFileDate["$file$", $date$, "$type$"]'
      <dd>set the file date of $file$ to the specified date list.
      The "$type$" can be one of "$Access$", "$Creation$", "$Modification$", or 'All'.
    </dl>

    Create a temporary file (for example purposes)
    >> tmpfilename = $TemporaryDirectory <> "/tmp0";
    >> Close[OpenWrite[tmpfilename]];

    >> SetFileDate[tmpfilename, {2000, 1, 1, 0, 0, 0.}, "Access"];

    >> FileDate[tmpfilename, "Access"]
     = {2000, 1, 1, 0, 0, 0.}

    #> SetFileDate[tmpfilename, {2001, 1, 1, 0, 0, 0.}];
    #> FileDate[tmpfilename, "Access"]
     = {2001, 1, 1, 0, 0, 0.}

    #> SetFileDate[tmpfilename]
    #> FileDate[tmpfilename, "Access"]
     = {...}

    #> DeleteFile[tmpfilename]

    #> SetFileDate["MathicsNonExample"]
     : File not found during SetFileDate[MathicsNonExample].
     = $Failed
    """

    messages = {
        'fstr': ('File specification `1` is not a string of one or '
                 'more characters.'),
        'nffil': 'File not found during `1`.',
        'fdate': ('Date specification should be either the number of seconds '
                  'since January 1, 1900 or a {y, m, d, h, m, s} list.'),
        'datetype': ('Date type a should be "Access", "Modification", '
                     '"Creation" (Windows only), or All.'),
        'nocreationunix': ('The Creation date of a file cannot be set on '
                           'Macintosh or Unix.'),
    }

    attributes = ('Protected')

    def apply(self, filename, datelist, attribute, evaluation):
        'SetFileDate[filename_, datelist_, attribute_]'

        py_filename = filename.to_python()

        if datelist is None:
            py_datelist = Expression(
                'DateList').evaluate(evaluation).to_python()
            expr = Expression('SetFileDate', filename)
        else:
            py_datelist = datelist.to_python()

        if attribute is None:
            py_attr = 'All'
            if datelist is not None:
                expr = Expression('SetFileDate', filename, datelist)
        else:
            py_attr = attribute.to_python()
            expr = Expression('SetFileDate', filename, datelist, attribute)

        # Check filename
        if not (isinstance(py_filename, basestring) and
                py_filename[0] == py_filename[-1] == '"'):
            evaluation.message('SetFileDate', 'fstr', filename)
            return
        py_filename = path_search(py_filename[1:-1])

        if py_filename is None:
            evaluation.message('SetFileDate', 'nffil', expr)
            return Symbol('$Failed')

        # Check datelist
        if not (isinstance(py_datelist, list) and len(py_datelist) == 6 and
                all(isinstance(d, int) for d in py_datelist[:-1]) and
                isinstance(py_datelist[-1], float)):
            evaluation.message('SetFileDate', 'fdate', expr)

        # Check attribute
        if py_attr not in ['"Access"', '"Creation"', '"Modification"', 'All']:
            evaluation.message('SetFileDate', 'datetype')
            return

        epochtime = Expression('AbsoluteTime', time.strftime(
            "%Y-%m-%d %H:%M", time.gmtime(0))).evaluate(evaluation).to_python()

        stattime = Expression('AbsoluteTime', from_python(py_datelist))
        stattime = stattime.to_python(n_evaluation=evaluation)

        stattime -= epochtime

        try:
            os.stat(py_filename)
            if py_attr == '"Access"':
                os.utime(py_filename, (
                    stattime, os.path.getatime(py_filename)))
            if py_attr == '"Creation"':
                if os.name == 'posix':
                    evaluation.message('SetFileDate', 'nocreationunix')
                    return Symbol('$Failed')
                else:
                    # TODO: Note: This is windows only
                    return Symbol('$Failed')
            if py_attr == '"Modification"':
                os.utime(py_filename, (os.path.getatime(
                    py_filename), stattime))
            if py_attr == 'All':
                os.utime(py_filename, (stattime, stattime))
        except OSError as e:
            print e
            # evaluation.message(...)
            return Symbol('$Failed')

        return Symbol('Null')

    def apply_1arg(self, filename, evaluation):
        'SetFileDate[filename_]'
        return self.apply(filename, None, None, evaluation)

    def apply_2arg(self, filename, datelist, evaluation):
        'SetFileDate[filename_, datelist_]'
        return self.apply(filename, datelist, None, evaluation)


class CopyFile(Builtin):
    """
    <dl>
    <dt>'CopyFile["$file1$", "$file2$"]'
      <dd>copies $file1$ to $file2$.
    </dl>

    >> CopyFile["ExampleData/sunflowers.jpg", "MathicsSunflowers.jpg"]
     = MathicsSunflowers.jpg
    >> DeleteFile["MathicsSunflowers.jpg"]
    """

    messages = {
        'filex': 'Cannot overwrite existing file `1`.',
        'fstr': ('File specification `1` is not a string of '
                 'one or more characters.'),
        'nffil': 'File not found during `1`.',
    }

    attributes = ('Protected')

    def apply(self, source, dest, evaluation):
        'CopyFile[source_, dest_]'

        py_source = source.to_python()
        py_dest = dest.to_python()

        # Check filenames
        if not (isinstance(py_source, basestring) and
                py_source[0] == py_source[-1] == '"'):
            evaluation.message('CopyFile', 'fstr', source)
            return
        if not (isinstance(py_dest, basestring) and
                py_dest[0] == py_dest[-1] == '"'):
            evaluation.message('CopyFile', 'fstr', dest)
            return

        py_source = py_source[1:-1]
        py_dest = py_dest[1:-1]

        py_source = path_search(py_source)

        if py_source is None:
            evaluation.message('CopyFile', 'filex', source)
            return Symbol('$Failed')

        if os.path.exists(py_dest):
            evaluation.message('CopyFile', 'filex', dest)
            return Symbol('$Failed')

        try:
            shutil.copy(py_source, py_dest)
        except IOError:
            evaluation.message('CopyFile', 'nffil', Expression(
                'CopyFile', source, dest))
            return Symbol('$Failed')

        return dest


class RenameFile(Builtin):
    """
    <dl>
    <dt>'RenameFile["$file1$", "$file2$"]'
      <dd>renames $file1$ to $file2$.
    </dl>

    >> CopyFile["ExampleData/sunflowers.jpg", "MathicsSunflowers.jpg"]
     = MathicsSunflowers.jpg
    >> RenameFile["MathicsSunflowers.jpg", "MathicsSunnyFlowers.jpg"]
     = MathicsSunnyFlowers.jpg
    >> DeleteFile["MathicsSunnyFlowers.jpg"]
    """

    messages = {
        'filex': 'Cannot overwrite existing file `1`.',
        'fstr': ('File specification `1` is not a string of '
                 'one or more characters.'),
        'nffil': 'File not found during `1`.',
    }

    attributes = ('Protected')

    def apply(self, source, dest, evaluation):
        'RenameFile[source_, dest_]'

        py_source = source.to_python()
        py_dest = dest.to_python()

        # Check filenames
        if not (isinstance(py_source, basestring) and
                py_source[0] == py_source[-1] == '"'):
            evaluation.message('RenameFile', 'fstr', source)
            return
        if not (isinstance(py_dest, basestring) and
                py_dest[0] == py_dest[-1] == '"'):
            evaluation.message('RenameFile', 'fstr', dest)
            return

        py_source = py_source[1:-1]
        py_dest = py_dest[1:-1]

        py_source = path_search(py_source)

        if py_source is None:
            evaluation.message('RenameFile', 'filex', source)
            return Symbol('$Failed')

        if os.path.exists(py_dest):
            evaluation.message('RenameFile', 'filex', dest)
            return Symbol('$Failed')

        try:
            shutil.move(py_source, py_dest)
        except IOError:
            evaluation.message('RenameFile', 'nffil', dest)
            return Symbol('$Failed')

        return dest


class DeleteFile(Builtin):
    """
    <dl>
    <dt>'Delete["$file$"]'
      <dd>deletes $file$.
    <dt>'Delete[{"$file1$", "$file2$", ...}]'
      <dd>deletes a list of files.
    </dl>

    >> CopyFile["ExampleData/sunflowers.jpg", "MathicsSunflowers.jpg"];
    >> DeleteFile["MathicsSunflowers.jpg"]

    >> CopyFile["ExampleData/sunflowers.jpg", "MathicsSunflowers1.jpg"];
    >> CopyFile["ExampleData/sunflowers.jpg", "MathicsSunflowers2.jpg"];
    >> DeleteFile[{"MathicsSunflowers1.jpg", "MathicsSunflowers2.jpg"}]
    """

    messages = {
        'filex': 'Cannot overwrite existing file `1`.',
        'strs': ('String or non-empty list of strings expected at '
                 'position `1` in `2`.'),
        'nffil': 'File not found during `1`.',
    }

    attributes = ('Protected')

    def apply(self, filename, evaluation):
        'DeleteFile[filename_]'

        py_path = filename.to_python()
        if not isinstance(py_path, list):
            py_path = [py_path]

        py_paths = []
        for path in py_path:
            # Check filenames
            if not (isinstance(path, basestring) and
                    path[0] == path[-1] == '"'):
                evaluation.message('DeleteFile', 'strs', filename,
                                   Expression('DeleteFile', filename))
                return

            path = path[1:-1]
            path = path_search(path)

            if path is None:
                evaluation.message('DeleteFile', 'nffil', Expression(
                    'DeleteFile', filename))
                return Symbol('$Failed')
            py_paths.append(path)

        for path in py_paths:
            try:
                os.remove(path)
            except OSError:
                return Symbol('$Failed')

        return Symbol('Null')


class DirectoryStack(Builtin):
    """
    <dl>
    <dt>'DirectoryStack[]'
      <dd>returns the directory stack.
    </dl>

    >> DirectoryStack[]
    = ...
    """

    attributes = ('Protected')

    def apply(self, evaluation):
        'DirectoryStack[]'
        global DIRECTORY_STACK
        return from_python(DIRECTORY_STACK)


class Directory(Builtin):
    """
    <dl>
    <dt>'Directory[]'
      <dd>returns the current working directory.
    </dl>

    >> Directory[]
    = ...
    """

    attributes = ('Protected')

    def apply(self, evaluation):
        'Directory[]'
        result = os.getcwd()
        return String(result)


class ParentDirectory(Builtin):
    """
    <dl>
    <dt>'ParentDirectory[]'
      <dd>returns the parent of the current working directory.
    <dt>'ParentDirectory["$dir$"]'
      <dd>returns the parent $dir$.
    </dl>

    >> ParentDirectory[]
     = ...
    """

    rules = {
        'ParentDirectory[]': 'ParentDirectory[Directory[]]',
    }

    messages = {
        'fstr': ('File specification `1` is not a string of '
                 'one or more characters.'),
    }

    attributes = ('Protected')

    def apply(self, path, evaluation):
        'ParentDirectory[path_]'

        if not isinstance(path, String):
            evaluation.message('ParentDirectory', 'fstr', path)
            return

        pypath = path.to_python()[1:-1]

        result = os.path.abspath(os.path.join(pypath, os.path.pardir))
        return String(result)


class SetDirectory(Builtin):
    """
    <dl>
    <dt>'SetDirectory[$dir$]'
      <dd>sets the current working directory to $dir$.
    </dl>

    >> SetDirectory[]
    = ...

    #> SetDirectory["MathicsNonExample"]
     : Cannot set current directory to MathicsNonExample.
     = $Failed
    """

    rules = {
        'SetDirectory[]': 'SetDirectory[$HomeDirectory]',
    }

    messages = {
        'fstr': ('File specification `1` is not a string of '
                 'one or more characters.'),
        'cdir': 'Cannot set current directory to `1`.',
    }

    attributes = ('Protected')

    def apply(self, path, evaluation):
        'SetDirectory[path_]'

        if not isinstance(path, String):
            evaluation.message('SetDirectory', 'fstr', path)
            return

        py_path = path.__str__()[1:-1]
        py_path = path_search(py_path)

        if py_path is None:
            evaluation.message('SetDirectory', 'cdir', path)
            return Symbol('$Failed')

        os.chdir(py_path)
        DIRECTORY_STACK.append(os.getcwd())
        return String(os.getcwd())


class ResetDirectory(Builtin):
    """
    <dl>
    <dt>'ResetDirectory[]'
      <dd>pops a directory from the directory stack and returns it.
    </dl>

    >> ResetDirectory[]
    = ...
    """

    messages = {
        'dtop': 'Directory stack is empty.',
    }

    attributes = ('Protected')

    def apply(self, evaluation):
        'ResetDirectory[]'
        global DIRECTORY_STACK

        try:
            tmp = DIRECTORY_STACK.pop()
        except IndexError:
            tmp = os.getcwd()
            evaluation.message('ResetDirectory', 'dtop')
        else:
            os.chdir(tmp)
        return String(tmp)


class CreateDirectory(Builtin):
    """
    <dl>
    <dt>'CreateDirectory["$dir$"]'
      <dd>creates a directory called $dir$.
    <dt>'CreateDirectory[]'
      <dd>creates a temporary directory.
    </dl>

    >> dir = CreateDirectory[]
     = ...
    #> DirectoryQ[dir]
     = True
    #> DeleteDirectory[dir]
    """

    attributes = ('Listable', 'Protected')

    options = {
        'CreateIntermediateDirectories': 'True',
    }

    messages = {
        'fstr': ('File specification `1` is not a string of '
                 'one or more characters.'),
        'nffil': "File not found during `1`.",
        'filex': "`1` already exists.",
    }

    def apply(self, dirname, evaluation, options):
        'CreateDirectory[dirname_, OptionsPattern[CreateDirectory]]'

        expr = Expression('CreateDirectory', dirname)
        py_dirname = dirname.to_python()

        if not (isinstance(py_dirname, basestring) and
                py_dirname[0] == py_dirname[-1] == '"'):
            evaluation.message('CreateDirectory', 'fstr', dirname)
            return

        py_dirname = py_dirname[1:-1]

        if os.path.isdir(py_dirname):
            evaluation.message(
                'CreateDirectory', 'filex', os.path.abspath(py_dirname))
            return

        os.mkdir(py_dirname)

        if not os.path.isdir(py_dirname):
            evaluation.message('CreateDirectory', 'nffil', expr)
            return

        return String(os.path.abspath(py_dirname))

    def apply_empty(self, evaluation, options):
        'CreateDirectory[OptionsPattern[CreateDirectory]]'
        dirname = tempfile.mkdtemp(prefix='m', dir=TMP_DIR)
        return String(dirname)


class DeleteDirectory(Builtin):
    """
    <dl>
    <dt>'DeleteDirectory["$dir$"]'
      <dd>deletes a directory called $dir$.
    </dl>

    >> dir = CreateDirectory[]
     = ...
    >> DeleteDirectory[dir]
    >> DirectoryQ[dir]
     = False
    #> Quiet[DeleteDirectory[dir]]
     = $Failed
    """

    attributes = ('Protected')

    options = {
        'DeleteContents': 'False',
    }

    messages = {
        'strs': ('String or non-empty list of strings expected at '
                 'position 1 in `1`.'),
        'nodir': 'Directory `1` not found.',
        'dirne': 'Directory `1` not empty.',
        'optx': 'Unknown option `1` in `2`',
        'idcts': 'DeleteContents expects either True or False.',   # MMA Bug
    }

    def apply(self, dirname, evaluation, options):
        'DeleteDirectory[dirname_, OptionsPattern[DeleteDirectory]]'

        expr = Expression('DeleteDirectory', dirname)
        py_dirname = dirname.to_python()

        delete_contents = options['DeleteContents'].to_python()
        if not delete_contents in [True, False]:
            evaluation.message('DeleteDirectory', 'idcts')
            return

        if not (isinstance(py_dirname, basestring) and
                py_dirname[0] == py_dirname[-1] == '"'):
            evaluation.message('DeleteDirectory', 'strs', expr)
            return

        py_dirname = py_dirname[1:-1]

        if not os.path.isdir(py_dirname):
            evaluation.message('DeleteDirectory', 'nodir', dirname)
            return Symbol('$Failed')

        if delete_contents:
            shutil.rmtree(py_dirname)
        else:
            if os.listdir(py_dirname) != []:
                evaluation.message('DeleteDirectory', 'dirne', dirname)
                return Symbol('$Failed')
            os.rmdir(py_dirname)

        return Symbol('Null')


class CopyDirectory(Builtin):
    """
    <dl>
    <dt>'CopyDirectory["$dir1$", "$dir2$"]'
      <dd>copies directory $dir1$ to $dir2$.
    </dl>
    """

    attributes = ('Protected')

    messages = {
        'argr': 'called with `1` argument; 2 arguments are expected.',
        'fstr': ('File specification `1` is not a string of '
                 'one or more characters.'),
        'filex': 'Cannot overwrite existing file `1`.',
        'nodir': 'Directory `1` not found.',
    }

    def apply(self, dirs, evaluation):
        'CopyDirectory[dirs__]'

        seq = dirs.get_sequence()
        if len(seq) != 2:
            evaluation.message('CopyDirectory', 'argr', len(seq))
            return
        (dir1, dir2) = (s.to_python() for s in seq)

        if not (isinstance(dir1, basestring) and dir1[0] == dir1[-1] == '"'):
            evaluation.message('CopyDirectory', 'fstr', seq[0])
            return
        dir1 = dir1[1:-1]

        if not (isinstance(dir2, basestring) and dir2[0] == dir2[-1] == '"'):
            evaluation.message('CopyDirectory', 'fstr', seq[1])
            return
        dir2 = dir2[1:-1]

        if not os.path.isdir(dir1):
            evaluation.message('CopyDirectory', 'nodir', seq[0])
            return Symbol('$Failed')
        if os.path.isdir(dir2):
            evaluation.message('CopyDirectory', 'filex', seq[1])
            return Symbol('$Failed')

        shutil.copytree(dir1, dir2)

        return String(os.path.abspath(dir2))


class RenameDirectory(Builtin):
    """
    <dl>
    <dt>'RenameyDirectory["$dir1$", "$dir2$"]'
      <dd>renames directory $dir1$ to $dir2$.
    </dl>
    """

    attributes = ('Protected')

    messages = {
        'argr': 'called with `1` argument; 2 arguments are expected.',
        'fstr': ('File specification `1` is not a string of '
                 'one or more characters.'),
        'filex': 'Cannot overwrite existing file `1`.',
        'nodir': 'Directory `1` not found.',
    }

    def apply(self, dirs, evaluation):
        'RenameDirectory[dirs__]'

        seq = dirs.get_sequence()
        if len(seq) != 2:
            evaluation.message('RenameDirectory', 'argr', len(seq))
            return
        (dir1, dir2) = (s.to_python() for s in seq)

        if not (isinstance(dir1, basestring) and dir1[0] == dir1[-1] == '"'):
            evaluation.message('RenameDirectory', 'fstr', seq[0])
            return
        dir1 = dir1[1:-1]

        if not (isinstance(dir2, basestring) and dir2[0] == dir2[-1] == '"'):
            evaluation.message('RenameDirectory', 'fstr', seq[1])
            return
        dir2 = dir2[1:-1]

        if not os.path.isdir(dir1):
            evaluation.message('RenameDirectory', 'nodir', seq[0])
            return Symbol('$Failed')
        if os.path.isdir(dir2):
            evaluation.message('RenameDirectory', 'filex', seq[1])
            return Symbol('$Failed')

        shutil.move(dir1, dir2)

        return String(os.path.abspath(dir2))


class FileType(Builtin):
    """
    <dl>
    <dt>'FileType["$file$"]'
      <dd>returns the type of a file, from 'File', 'Directory' or 'None'.
    </dl>

    >> FileType["ExampleData/sunflowers.jpg"]
     = File
    >> FileType["ExampleData"]
     = Directory
    >> FileType["ExampleData/nonexistant"]
     = None

    #> FileType[x]
     : File specification x is not a string of one or more characters.
     = FileType[x]
    """

    messages = {
        'fstr': ('File specification `1` is not a string of '
                 'one or more characters.'),
    }

    attributes = ('Protected')

    def apply(self, filename, evaluation):
        'FileType[filename_]'
        if not isinstance(filename, String):
            evaluation.message('FileType', 'fstr', filename)
            return
        path = filename.to_python()[1:-1]

        path = path_search(path)

        if path is None:
            return Symbol('None')

        if os.path.isfile(path):
            return Symbol('File')
        else:
            return Symbol('Directory')


class FileExistsQ(Builtin):
    """
    <dl>
    <dt>'FileExistsQ["$file$"]'
      <dd>returns 'True' if $file$ exists and 'False' otherwise.
    </dl>

    >> FileExistsQ["ExampleData/sunflowers.jpg"]
     = True
    >> FileExistsQ["ExampleData/sunflowers.png"]
     = False
    """

    messages = {
        'fstr': ('File specification `1` is not a string of '
                 'one or more characters.'),
    }

    attributes = ('Protected')

    def apply(self, filename, evaluation):
        'FileExistsQ[filename_]'
        path = filename.to_python()
        if not (isinstance(path, basestring) and path[0] == path[-1] == '"'):
            evaluation.message('FileExistsQ', 'fstr', filename)
            return
        path = path[1:-1]

        path = path_search(path)

        if path is None:
            return Symbol('False')
        return Symbol('True')


class DirectoryQ(Builtin):
    """
    <dl>
    <dt>'DirectoryQ["$name$"]'
      <dd>returns 'True' if the directory called $name$ exists and 'False' otherwise.
    </dl>

    >> DirectoryQ["ExampleData/"]
     = True
    >> DirectoryQ["ExampleData/MythicalSubdir/"]
     = False

    #> DirectoryQ["ExampleData"]
     = True

    #> DirectoryQ["ExampleData/MythicalSubdir/NestedDir/"]
     = False
    """

    messages = {
        'fstr': ('File specification `1` is not a string of '
                 'one or more characters.'),
    }

    attributes = ('Protected')

    def apply(self, pathname, evaluation):
        'DirectoryQ[pathname_]'
        path = pathname.to_python()

        if not (isinstance(path, basestring) and path[0] == path[-1] == '"'):
            evaluation.message('DirectoryQ', 'fstr', pathname)
            return
        path = path[1:-1]

        path = path_search(path)

        if path is not None and os.path.isdir(path):
            return Symbol('True')
        return Symbol('False')


class Needs(Builtin):
    """
    <dl>'Needs["context`"]'
      <dd>loads the specified context if not already in '$Packages'.
    </dl>

    >> Needs["VectorAnalysis`"]
    #> Needs["VectorAnalysis`"]

    #> Needs["SomeFakePackageOrTypo`"]
     : Cannot open SomeFakePackageOrTypo`.
     : Context SomeFakePackageOrTypo` was not created when Needs was evaluated.
     = $Failed

    #> Needs["VectorAnalysis"]
     : Invalid context specified at position 1 in Needs[VectorAnalysis]. A context must consist of valid symbol names separated by and ending with `.
     = Needs[VectorAnalysis]

    ## --- VectorAnalysis ---

    #> Needs["VectorAnalysis`"]

    #> DotProduct[{1,2,3}, {4,5,6}]
     = 32
    #> DotProduct[{-1.4, 0.6, 0.2}, {0.1, 0.6, 1.7}]
     = 0.56

    #> CrossProduct[{1,2,3}, {4,5,6}]
     = {-3, 6, -3}
    #> CrossProduct[{-1.4, 0.6, 0.2}, {0.1, 0.6, 1.7}]
     = {0.9, 2.4, -0.9}

    #> ScalarTripleProduct[{-2,3,1},{0,4,0},{-1,3,3}]
     = -20
    #> ScalarTripleProduct[{-1.4,0.6,0.2}, {0.1,0.6,1.7}, {0.7,-1.5,-0.2}]
     = -2.79

    #> CoordinatesToCartesian[{2, Pi, 3}, Spherical]
     = {0, 0, -2}
    #> CoordinatesFromCartesian[%, Spherical]
     = {2, Pi, 0}
    #> CoordinatesToCartesian[{2, Pi, 3}, Cylindrical]
     = {-2, 0, 3}
    #> CoordinatesFromCartesian[%, Cylindrical]
     = {2, Pi, 3}
    ## Needs Sin/Cos exact value (PR #100) for these tests to pass
    ## #> CoordinatesToCartesian[{2, Pi / 4, Pi / 3}, Spherical]
    ##  = {Sqrt[2] / 2, Sqrt[6] / 2, Sqrt[2]}
    ## #> CoordinatesFromCartesian[%, Spherical]
    ##  = {2, Pi / 4, Pi / 3}
    ## #> CoordinatesToCartesian[{2, Pi / 4, -1}, Cylindrical]
    ##  = {Sqrt[2], Sqrt[2], -1}
    ## #> CoordinatesFromCartesian[%, Cylindrical]
    ##  = {2, Pi / 4, -1}
    #> CoordinatesToCartesian[{0.27, 0.51, 0.92}, Cylindrical]
     = {0.235641017064352841, 0.131807856658385023, 0.92}
    #> CoordinatesToCartesian[{0.27, 0.51, 0.92}, Spherical]
     = {0.0798518563676219116, 0.10486654429093224, 0.235641017064352841}

    #> Coordinates[]
     = {Xx, Yy, Zz}
    #> Coordinates[Spherical]
     = {Rr, Ttheta, Pphi}
    #> SetCoordinates[Cylindrical]
     = Cylindrical[Rr, Ttheta, Zz]
    #> Coordinates[]
     = {Rr, Ttheta, Zz}
    #> CoordinateSystem
     = Cylindrical
    #> Parameters[]
     = {}
    #> CoordinateRanges[]
    ## = {0 <= Rr < Infinity, -Pi < Ttheta <= Pi, -Infinity < Zz < Infinity}
     = {0 <= Rr && Rr < Infinity, -Pi < Ttheta && Ttheta <= Pi, -Infinity < Zz < Infinity}
    #> CoordinateRanges[Cartesian]
     = {-Infinity < Xx < Infinity, -Infinity < Yy < Infinity, -Infinity < Zz < Infinity}
    #> ScaleFactors[Cartesian]
     = {1, 1, 1}
    #> ScaleFactors[Spherical]
     = {1, Rr, Rr Sin[Ttheta]}
    #> ScaleFactors[Cylindrical]
     = {1, Rr, 1}
    #> ScaleFactors[{2, 1, 3}, Cylindrical]
     = {1, 2, 1}
    #> JacobianDeterminant[Cartesian]
     = 1
    #> JacobianDeterminant[Spherical]
     = Rr ^ 2 Sin[Ttheta]
    #> JacobianDeterminant[Cylindrical]
     = Rr
    #> JacobianDeterminant[{2, 1, 3}, Cylindrical]
     = 2
    #> JacobianMatrix[Cartesian]
     = {{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}
    #> JacobianMatrix[Spherical]
     = {{Cos[Pphi] Sin[Ttheta], Rr Cos[Pphi] Cos[Ttheta], -Rr Sin[Pphi] Sin[Ttheta]}, {Sin[Pphi] Sin[Ttheta], Rr Cos[Ttheta] Sin[Pphi], Rr Cos[Pphi] Sin[Ttheta]}, {Cos[Ttheta], -Rr Sin[Ttheta], 0}}
    #> JacobianMatrix[Cylindrical]
     = {{Cos[Ttheta], -Rr Sin[Ttheta], 0}, {Sin[Ttheta], Rr Cos[Ttheta], 0}, {0, 0, 1}}
    """

    messages = {
        'ctx': ('Invalid context specified at position `2` in `1`. '
                'A context must consist of valid symbol names separated by '
                'and ending with `3`.'),
        'nocont': 'Context `1` was not created when Needs was evaluated.',
    }

    def apply(self, context, evaluation):
        'Needs[context_String]'

        if context.get_string_value()[-1] != '`':
            evaluation.message('Needs', 'ctx', Expression(
                'Needs', context), 1, '`')
            return

        # TODO
        # if Expression('MemberQ', context, Symbol('$Packages')).is_true():
        #    # Already loaded
        #    return Symbol('Null')

        result = Expression('Get', context).evaluate(evaluation)

        if result == Symbol('$Failed'):
            evaluation.message('Needs', 'nocont', context)
            return Symbol('$Failed')

        return Symbol('Null')
