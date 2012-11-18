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
import sys
import tempfile
import time

from mathics.core.expression import Expression, String, Symbol, from_python
from mathics.builtin.base import Builtin, Predefined, BinaryOperator, PrefixOperator
from mathics.settings import ROOT_DIR

STREAMS = {}
INITIAL_DIR = os.getcwd()
HOME_DIR = os.path.expanduser('~')
TMP_DIR = tempfile.gettempdir()
DIRECTORY_STACK = [INITIAL_DIR]
INPUT_VAR = ""
INPUTFILE_VAR = ""
PATH_VAR = [HOME_DIR, os.path.join(ROOT_DIR, 'data')]

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
        return self

    def __exit__(self, type, value, traceback):
        if self.file is not None:
            self.file.close()

    def read(self, *args):
        return self.file.read(*args)

    def write(self, *args):
        return self.file.write(*args)

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
    if os.path.exists(filename):
        return filename
    for p in PATH_VAR:
        path = os.path.join(p, filename)
        if os.path.exists(path):
            return path
    return None

def _put_stream(stream):
    global STREAMS
    global _STREAMS
    global NSTREAMS

    try:
        _STREAMS
    except NameError:
        STREAMS = {}    # Python repr
        _STREAMS = {}   # Mathics repr
        NSTREAMS = 0    # Max stream number

    NSTREAMS += 1
    STREAMS[NSTREAMS] = stream
    return NSTREAMS

def _get_stream(n):
    global STREAMS
    return STREAMS[n]


class InitialDirectory(Predefined):
    """
    <dl>
    <dt>'$InitialDirectory'
      <dd>returns the directory from which \Mathics was started.'
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
      <dd>returns the directory in which \Mathics was installed.'
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
      <dd>returns the users HOME directory.'
    </dl>

    >> $HomeDirectory
     = ...
    """

    name = '$HomeDirectory'

    def evaluate(self, evaluation):
        global HOME_DIR
        return String(HOME_DIR)


class TemporaryDirectory(Predefined):
    """
    <dl>
    <dt>'$TemporaryDirectory'
      <dd>returns the directory used for temporary files.'
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
      <dd>is the name of the stream from which input it currently being read.
    </dl>
    
    >> $Input
     = 
    """

    name = '$Input'

    def evaluate(self, evaluation):
        global INPUT_VAR
        return String(INPUT_VAR)


class InputFileName(Predefined):
    """
    <dl>
    <dt>'$InputFileName'
      <dd>is the file name of the file from which input it currently being read.
    </dl>
    
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
      <dd>returns the list of directories to search when looking for an file.
    </dl>

    >> $Path
     = ...
    """

    name = '$Path'

    def evaluate(self, evaluation):
        return Expression('List', *[String(p) for p in PATH_VAR])


class Read(Builtin):
    """
    <dl>
    <dt>'Read[stream]'
        <dd>reads the input stream and returns one expression.
    <dt>'Read[stream, type]
        <dd>reads the input stream and returns object of the given type.
    </dl>

    ## Malformed InputString
    #> Read[InputStream[String], {Word, Number}]
     : InputStream[String] is not string, InputStream[], or OutputStream[]
     = Read[InputStream[String], {Word, Number}]

    ## Correctly formed InputString but not open
    #> Read[InputStream[String, -1], {Word, Number}]
     : InputSteam[String, -1] is not open
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
    ## #> Read[str, Number]
    ##  : Invalid real number found when reading from InputSteam["String", 6]
    ##  = $Failed

    ## Real
    #> str = StringToStream["123, 4abc"];
    #> Read[str, Real]
     = 123.
    #> Read[str, Real]
     = 4.
    ## #> Read[str, Number]
    ##  : Invalid real number found when reading from InputSteam["String", 6]
    ##  = $Failed
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
    ## #> str = StringToStream["Sin[1 123"]; Read[str, Expression]
    ##  : Invalid input found when reading Sin[1 123 from InputSteam["String", 12]
    ##  = $Failed

    ## Multiple types
    >> str = StringToStream["123 abc"];
    >> Read[str, {Number, Word}]
     = {123, abc}
    #> Read[str, {Number, Word}]
     = EndOfFile
    #> Close[str];

    ## #> str = StringToStream["123 abc"];
    ## #> Read[str, {Word, Number}]
    ##  : Invalid real number found when reading from InputSteam["String", 14]
    ##  = $Failed
    ## #> Close[str];

    #> str = StringToStream["123 123"];  Read[str, {Real, Number}]
     = {123., 123}
    #> Close[str];
    """

    messages = {
        'openx': '`1` is not open',
        'readf': '`1` is not a valid format specificiation',
        'readn': 'Invalid real number found when reading from `1`',
        'readt': 'Invalid input found when reading `1` from `2`',
    }

    rules = {
        'Read[stream_]': 'Read[stream, Expression]',
    }

    def apply(self, name, n, types, evaluation):
        'Read[InputStream[name_, n_], types_]'
        global STREAMS
    
        stream = STREAMS.get(n.to_python())

        if stream is None:
            evaluation.message('Read', 'openx', Expression('InputSteam', name, n))
            return
        
        types = types.to_python()
        if not isinstance(types, list):
            types = [types]
    
        READ_TYPES = ['Byte', 'Character', 'Expression', 'Number', 'Real', 'Record', 'String', 'Word']

        for typ in types:
            if not (isinstance(typ, basestring) and typ in READ_TYPES):
                evaluation.message('Read', 'readf', from_python(typ))
                return Symbol('$Failed')
        
        name = name.to_python()

        result = []

        #TODO: Implement these as options to Read
        word_separators = [' ', '\t']
        record_separators = ['\n', '\r\n', '\r']

        def reader(stream, word_separators, accepted = None):
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
                            stream.seek(stream.tell()-1)
                        yield word

                    if accepted is not None and tmp not in accepted:
                        yield word

                    word += tmp

        read_word = reader(stream, word_separators)
        read_record = reader(stream, record_separators)
        read_number = reader(stream, word_separators + record_separators, 
            ['+', '-', '.'] + [str(i) for i in range(10)])
        read_real = reader(stream, word_separators + record_separators,
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
                        evaluation.message('Read', 'readt', tmp, Expression('InputSteam', name, n))
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
                            evaluation.message('Read', 'readn', Expression('InputSteam', name, n))
                            return Symbol('$Failed')
                    result.append(tmp)
                        
                elif typ == 'Real':
                    tmp = read_real.next()
                    tmp = tmp.replace('*^', 'E')
                    try:
                        tmp = float(tmp)
                    except ValueError:
                        evaluation.message('Read', 'readn', Expression('InputSteam', name, n))
                        return Symbol('$Failed')
                    result.append(tmp)
                elif typ == 'Record':
                    result.append(read_record.next())
                elif typ == 'String':
                    tmp = stream.readline()
                    if len(tmp) == 0:
                        raise EOFError
                    result.append(tmp)
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
        <dd>writes the expressions to the output channel followed by a newline."
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

    def apply(self, name, n, expr, evaluation):
        'Write[OutputStream[name_, n_], expr___]'
        global STREAMS
        stream = STREAMS[n.to_python()]

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
        <dd>writes the strings to the output stream."
    </dl>

    >> str = OpenWrite[];
    >> WriteString[str, "This is a test 1"]
    >> WriteString[str, "This is also a test 2"]
    >> Close[str]
     = ...
    >> FilePrint[%]
     = This is a test 1This is also a test 2

    >> str = OpenWrite[];
    >> WriteString[str, "This is a test 1", "This is also a test 2"]
    >> Close[str]
     = ...
    >> FilePrint[%]
     = This is a test 1This is also a test 2
    """

    messages = {
        'strml': '`1` is not a string, stream, or list of strings and streams.',
    }

    def apply(self, name, n, expr, evaluation):
        'WriteString[OutputStream[name_, n_], expr___]'
        global STREAMS
        stream = STREAMS[n.to_python()]

        exprs = expr.get_sequence()
        for e in exprs:
            if not isinstance(e, String):
                evaluation.message('WriteString', 'strml', e) # Mathematica gets this message wrong
                return

        text = map(lambda x: x.to_python().strip('"'), exprs)
        text = unicode(''.join(text))
        stream.write(text)
        return Symbol('Null')


class Save(Builtin):
    pass


class _OpenAction(Builtin):

    messages = {
        'argx': 'OpenRead called with 0 arguments; 1 argument is expected.',
    }

    def apply(self, path, evaluation):
        '%(name)s[path_]'

        if not isinstance(path, String):
            #TODO: evaluation.message
            return

        path_string = path.to_python().strip('"')

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
            evaluation.message('OpenRead', 'argx')
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
    """

    mode = 'a'
    stream_type = 'OutputStream'


class Get(PrefixOperator):
    """
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
     = 815915283247897734345611269596115894272000000000
    >> <<"fourtyfactorial"
     = 815915283247897734345611269596115894272000000000
    #> DeleteFile["fourtyfactorial"]
    """

    operator = '<<'
    precedence = 290 #FIXME: what value is appropriate here?

    def apply(self, path, evaluation):
        'Get[path_?StringQ]'
        pypath = path.get_string_value()
        try:
            with mathics_open(pypath, 'r') as f:
                result = f.readlines()
        except IOError:
            evaluation.message('General', 'noopen', path)
            return
        for tmp in result:
            try:
                expr = parse(tmp)
            except NameError:
                from mathics.core.parser import parse, ParseError
                expr = parse(tmp)
        return expr

    def apply_default(self, exprs, filename, evaluation):
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

    >> 40! >> "fourtyfactorial"
    >> FilePrint["fourtyfactorial"]
     = 815915283247897734345611269596115894272000000000
    #> 40! >> fourtyfactorial
    #> FilePrint["fourtyfactorial"]
     = 815915283247897734345611269596115894272000000000

    #> Put[40!, fourtyfactorial]
     : fourtyfactorial is not string, InputStream[], or OutputStream[]
     = ...
    ## FIXME: final line should be
    ## = Put[815915283247897734345611269596115894272000000000, fourtyfactorial]

    >> Put[50!, "fiftyfactorial"]
    >> FilePrint["fiftyfactorial"]
     = 30414093201713378043612608166064768844377641568960512000000000000
    #> DeleteFile["fiftyfactorial"]
    
    >> Put[10!, 20!, 30!, "factorials"]
    >> FilePrint["factorials"]
     = 3628800
     . 2432902008176640000
     . 265252859812191058636308480000000

    #> DeleteFile["factorials"]
     =
    """

    operator = '>>'
    precedence = 290 #FIXME: what value is appropriate here?

    def apply(self, exprs, filename, evaluation):
        'Put[exprs___, filename_?StringQ]'
        instream = Expression('OpenWrite', filename).evaluate(evaluation)
        name, n = instream.leaves
        result = self.apply_input(exprs, name, n, evaluation)
        close = Expression('Close', instream).evaluate(evaluation)
        return result

    def apply_input(self, exprs, name, n, evaluation):
        'Put[exprs___, OutputStream[name_, n_]]'
        global STREAMS
        stream = STREAMS.get(n.to_python())

        if stream is None:
            evaluation.message('Put', 'openx', Expression('OutputSteam', name, n))
            return

        text = [unicode(e.do_format(evaluation, 'InputForm').__str__()) for e in exprs.get_sequence()]
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
      <dt>append $expr$ to a file.
    <dt>'PutAppend[$expr1$, $expr2$, ..., $"filename"$]'
      <dt>write a sequence of expressions to a file.

    >> Put[50!, "factorials"]
    >> FilePrint["factorials"]
     = 30414093201713378043612608166064768844377641568960512000000000000
    
    >> PutAppend[10!, 20!, 30!, "factorials"]
    >> FilePrint["factorials"]
     = 30414093201713378043612608166064768844377641568960512000000000000
     . 3628800
     . 2432902008176640000
     . 265252859812191058636308480000000

    >> 60! >>> "factorials"
    >> FilePrint["factorials"]
     = 30414093201713378043612608166064768844377641568960512000000000000
     . 3628800
     . 2432902008176640000
     . 265252859812191058636308480000000
     . 8320987112741390144276341183223364380754172606361245952449277696409600000000000000

    >> "string" >>> factorials
    >> FilePrint["factorials"]
     = 30414093201713378043612608166064768844377641568960512000000000000
     . 3628800
     . 2432902008176640000
     . 265252859812191058636308480000000
     . 8320987112741390144276341183223364380754172606361245952449277696409600000000000000
     . "string"
    #> DeleteFile["factorials"];
    """

    operator = '>>>'
    precedence = 290 #FIXME: what value is appropriate here?

    def apply(self, exprs, filename, evaluation):
        'PutAppend[exprs___, filename_?StringQ]'
        instream = Expression('OpenAppend', filename).evaluate(evaluation)
        name, n = instream.leaves
        result = self.apply_input(exprs, name, n, evaluation)
        close = Expression('Close', instream).evaluate(evaluation)
        return result

    def apply_input(self, exprs, name, n, evaluation):
        'PutAppend[exprs___, OutputStream[name_, n_]]'
        global STREAMS
        stream = STREAMS.get(n.to_python())

        if stream is None:
            evaluation.message('Put', 'openx', Expression('OutputSteam', name, n))
            return

        text = [unicode(e.do_format(evaluation, 'OutputForm').__str__()) for e in exprs.get_sequence()]
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


class FileExtension(Builtin):
    """
    <dl>
    <dt>'FileExtension["$file$"]'
        <dd>gives the extension for the specified file name.
    </dl>

    >> FileExtension["file.txt"]
     = txt

    #> FileExtension["file."]
     = 

    #> FileExtension["file"]
     = 

    #> FileExtension["file.tar.gz"]
     = gz
    """

    def apply(self, filename, evaluation):
        'FileExtension[filename_?StringQ]'
        path = filename.to_python().strip('"')

        filename_base, filename_ext  = os.path.splitext(path)
        filename_ext =  filename_ext.lstrip('.')
        return from_python(filename_ext)


class FileBaseName(Builtin):
    """
    <dl>
    <dt>'FileBaseName["$file$"]'
        <dd>gives the base name for the specified file name.
    </dl>

    >> FileBaseName["file.txt"]
     = file

    #> FileBaseName["file.tar.gz"]
     = file.tar
    """

    def apply(self, filename, evaluation):
        'FileBaseName[filename_?StringQ]'
        path = filename.to_python().strip('"')

        filename_base, filename_ext  = os.path.splitext(path)
        return from_python(filename_base)


class ReadList(Read):
    """
    <dl>
    <dt>'ReadList["file"]
        <dd>Reads all the expressions until the end of file.
    </dl>

    >> str = StringToStream["abc123"];
    >> ReadList[str]
     = {abc123}
    >> InputForm[%]
     = {"abc123"}
    #> Close[str];
    """

    #TODO: Accept newlines in input
    """
    >> ReadList[StringToStream["123\nabc"]]
     = {123, abc}
    >> InputForm[%]
     = {"123", "abc"}
    """

    rules = {
        'ReadList[stream_]': 'ReadList[stream, Expression]',
    }

    def apply(self, name, n, types, evaluation):
        'ReadList[InputStream[name_, n_], types_]'
        result = []
        while True:
            tmp = super(ReadList, self).apply(name, n, types, evaluation)
            if tmp.to_python() == 'EndOfFile':
                break
            result.append(tmp)
        return from_python(result)


class FilePrint(Builtin):
    """
    <dl>
    <dt>'FilePrint[$file$]
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
    """

    messages = {
        'fstr': 'File specification `1` is not a string of one or more characters.',
    }

    def apply(self, path, evaluation):
        'FilePrint[path_]'
        pypath = path.to_python()
        if not (isinstance(pypath, basestring) and pypath[0] == pypath[-1] == '"'):
            evaluation.message('FilePrint', 'fstr', path)
            return
        pypath = path_search(pypath.strip('"'))
        
        if pypath is None:
            evaluation.message('FilePrint', 'TODO', path)
            return

        if not os.path.isfile(pypath):
            return Symbol("$Failed")

        try:
            with mathics_open(pypath, 'r') as f:
                result = f.read()
        except IOError:
            evaluation.message('General', 'noopen', path)
            return

        return from_python(result)


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
     
    def apply_input(self, name, n, evaluation):
        'Close[InputStream[name_, n_]]'
        global STREAMS
        stream = STREAMS[n.to_python()]

        if stream.closed:
            evaluation.message('General', 'openx', name)
            return

        stream.close()
        return name

    def apply_output(self, name, n, evaluation):
        'Close[OutputStream[name_, n_]]'
        global STREAMS
        stream = STREAMS[n.to_python()]

        if stream.closed:
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

    def apply_input(self, name, n, evaluation):
        'StreamPosition[InputStream[name_, n_]]'
        global STREAMS
        stream = STREAMS[n.to_python()]

        if stream.closed:
            evaluation.message('General', 'openx', name)
            return
   
        return from_python(stream.tell())


    def apply_output(self, name, n, evaluation):
        'StreamPosition[OutputStream[name_, n_]]'
        global STREAMS
        stream = STREAMS[n.to_python()]

        if stream.closed:
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

    >> SetStreamPosition[str, 11]
     = 11

    >> Read[str, Word]
     = cool!
    """
    
    #TODO: This might work in py3
    """
    #> SetStreamPosition[str, -5]
     = 11

    #> Read[str, Word]
     = cool!
    """

    #TODO: Seeks beyond stream should return stmrng message
    """
    #> SetStreamPosition[str, 40]
     = ERROR_MESSAGE_HERE
    """

    messages = {
        'int': 'Integer expected at position 2 in `1`.',
        'stmrng': 'Cannot set the current point in stream `1` to position `2`. The requested position exceeds the number of characters in the file',
    }

    def apply_input(self, name, n, m, evaluation):
        'SetStreamPosition[InputStream[name_, n_], m_]'
        global STREAMS
        stream = STREAMS[n.to_python()]

        if stream.closed:
            evaluation.message('General', 'openx', name)
            return

        if not stream.seekable:
            raise NotImplementedError
   
        seekpos = m.to_python()
        if not (isinstance(seekpos, int) or seekpos == 'Infinity'):
            evaluation.message('SetStreamPosition', 'stmrng', Expression('InputStream', name, n), m)
            return

        if seekpos == 'Infinity':
            tmp = stream.seek(0, 2)
        else:
            if seekpos < 0:
                stream.seek(seekpos, 2)
            else:
                stream.seek(seekpos)

        return from_python(stream.tell())

    def apply_output(self, name, n, m, evaluation):
        'SetStreamPosition[OutputStream[name_, n_], m_]'
        global STREAMS
        stream = STREAMS[n.to_python()]

        if stream.closed:
            evaluation.message('General', 'openx', name)
            return

        if not stream.seekable:
            raise NotImplementedError

        seekpos = m.to_python()
        if not (isinstance(seekpos, int) or seekpos == 'Infinity'):
            evaluation.message('SetStreamPosition', 'stmrng', Expression('OutputStream', name, n), m)
            return

        if seekpos == 'Infinity':
            tmp = stream.seek(0, 2)
        else:
            if seekpos < 0:
                stream.seek(seekpos, 2)
            else:
                stream.seek(seekpos)

        return from_python(stream.tell())

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
        'Skip[InputStream[name_, n_], types_]': 'Skip[InputStream[name, n], types, 1]',
    }

    messages = {
        'intm': 'Non-negative machine-sized integer expected at position 3 in `1`',
    }

    def apply(self, name, n, types, m, evaluation):
        'Skip[InputStream[name_, n_], types_, m_]'
        py_m = m.to_python()
        if not (isinstance(py_m, int) and py_m > 0):
            evaluation.message('Skip', 'intm', Expression('Skip', Expression('InputStream', name, n), types, m))
            return
        for i in range(py_m):
            result = super(Skip, self).apply(name, n, types, evaluation)
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

    #TODO: Extra options AnchoredSearch, IgnoreCase RecordSeparators, WordSearch, WordSeparators
    # this is probably best done with a regex

    def apply(self, name, n, text, evaluation):
        'Find[InputStream[name_, n_], text_]'
        py_text = text.to_python()

        if not isinstance(py_text, list):
            py_text = [py_text]

        if not all(isinstance(t, basestring) and t[0] == t[-1] == '"' for t in py_text):
            evaluation.message('Find', 'unknown', Expression('Find', Expression('InputStream', name, n), text))
            return

        py_text = [t.strip('"') for t in py_text]

        while True:
            tmp = super(Find, self).apply(name, n, Symbol('Record'), evaluation)
            py_tmp = tmp.to_python().strip('"')

            if py_tmp == 'EndOfFile':
                evaluation.message('Find', 'notfound', Expression('Find', Expression('InputStream', name, n), text))
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
        'strs': 'String or non-empty list of strings expected at position `1` in `2`.',
        'intnm': 'Non-negative machine-sized integer expected at position `1` in `2`.',
    }

    #TODO: Extra options AnchoredSearch, IgnoreCase RecordSeparators, WordSearch, WordSeparators
    # this is probably best done with a regex

    def apply_without_n(self, filename, text, evaluation):
        'FindList[filename_, text_]'
        return self.apply(filename, text, None, evaluation)

    def apply(self, filename, text, n, evaluation):
        'FindList[filename_, text_, n_]'
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

        if not all(isinstance(t, basestring) and t[0] == t[-1] == '"' for t in py_name):
            evaluation.message('FindList', 'strs', '1', expr)
            return Symbol('$Failed')

        if not all(isinstance(t, basestring) and t[0] == t[-1] == '"' for t in py_text):
            evaluation.message('FindList', 'strs', '2', expr)
            return Symbol('$Failed')

        if not ((isinstance(py_n, int) and py_n >= 0) or py_n is None):
            evaluation.message('FindList', 'intnm', '3', expr)
            return Symbol('$Failed')

        if py_n == 0:
            return Symbol('$Failed')


        py_text = [t.strip('"') for t in py_text]
        py_name = [t.strip('"') for t in py_name]

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
    
    def apply(self, string, evaluation):
        'StringToStream[string_]'
        pystring = string.to_python().strip('"')
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

    def apply(self, evaluation):
        'Streams[]'
        global _STREAMS
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

    def apply(self, expr, evaluation):
        'Compress[expr_]'
        
        string = expr.do_format(evaluation, 'FullForm').__str__()
        string = string.encode('utf-8')
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

    def apply(self, string, evaluation):
        'Uncompress[string_?StringQ]'
        string = string.to_python().strip('"')
        string = base64.decodestring(string)
        tmp = zlib.decompress(string)
        tmp = tmp.decode('utf-8')
        
        try:
            expr = parse(tmp)
        except NameError:
            from mathics.core.parser import parse, ParseError
            expr = parse(tmp)

        return expr

# TODO: Find a better way to do this
#class ByteCount(Builtin):
#    """
#    <dl>
#    <dt>'ByteCount[$expr$]'
#      <dd>returns the number of bytes to store $expr$.
#    </dl>
#
#    >> ByteCount[{1, 2, 3, 4, 5}]
#     = 128
#    """
#
#    def apply(self, expr, evaluation):
#        'ByteCount[expr_]'
#        full = expr.do_format(evaluation, 'FullForm').__str__()
#        return from_python(sys.getsizeof(full))


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
        'fstr': 'File specification `1` is not a string of one or more characters.',
    }

    def apply(self, filename, evaluation):
        'FileByteCount[filename_]'
        py_filename = filename.to_python()
        if not (isinstance(py_filename, basestring) and py_filename[0] == py_filename[-1] == '"'):
            evaluation.message('FileByteCount', 'fstr', filename)
            return
        py_filename = py_filename.strip('"')

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
    <dt>'FileHash[$file$, $types$]'
      <dd>returns an integer hash of specified $type$ for the given $file$.
    </dl>

    >> FileHash["ExampleData/sunflowers.jpg"]
     = 109937059621979839952736809235486742106

    >> FileHash["ExampleData/sunflowers.jpg", "MD5"]
     = 109937059621979839952736809235486742106

    >> FileHash["ExampleData/sunflowers.jpg", "Adler32"]
     = 1607049478

    >> FileHash["ExampleData/sunflowers.jpg", "CRC32"]
     = 933095683

    >> FileHash["ExampleData/sunflowers.jpg", "SHA"]
     = 851696818771101405642332645949480848295550938123

    >> FileHash["ExampleData/sunflowers.jpg", "SHA224"]
     = 8723805623766373862936267623913366865806344065103917676078120867011

    >> FileHash["ExampleData/sunflowers.jpg", "SHA256"]
     = 111619807552579450300684600241129773909359865098672286468229443390003894913065

    >> FileHash["ExampleData/sunflowers.jpg", "SHA384"]
     = 28288410602533803613059815846847184383722061845493818218404754864571944356226472174056863474016709057507799332611860

    >> FileHash["ExampleData/sunflowers.jpg", "SHA512"]
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

    def apply(self, filename, hashtype, evaluation):
        'FileHash[filename_?StringQ, hashtype_?StringQ]'
        py_hashtype = hashtype.to_python()
        py_filename = filename.to_python()

        #TODO: MD2?
        supported_hashes = {
            'Adler32': zlib.adler32,
            'CRC32': zlib.crc32,
            'MD5': lambda s : int(hashlib.md5(s).hexdigest(), 16),
            'SHA': lambda s : int(hashlib.sha1(s).hexdigest(), 16),
            'SHA224': lambda s : int(hashlib.sha224(s).hexdigest(), 16),
            'SHA256': lambda s : int(hashlib.sha256(s).hexdigest(), 16),
            'SHA384': lambda s : int(hashlib.sha384(s).hexdigest(), 16),
            'SHA512': lambda s : int(hashlib.sha512(s).hexdigest(), 16),
        }

        py_hashtype = py_hashtype.strip('"')
        py_filename = py_filename.strip('"')

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
    """

    rules = {
        'FileDate[path_]': 'FileDate[path, "Modification"]',
    }

    def apply(self, path, timetype, evaluation):
        'FileDate[path_, timetype_]'
        py_path = path_search(path.to_python().strip('"'))

        if py_path is None:
            evaluation.message('FileDate', 'TODO1', path)
            return

        time_type = timetype.to_python().strip('"')
        if time_type == 'Access':
            result = os.path.getatime(py_path)
        elif time_type in ['Creation', 'Change']:   # TODO: Fixing this cross platform is difficult
            result = os.path.getctime(py_path)
        elif time_type == 'Modification':
            result = os.path.getmtime(py_path)
        else:
            return

        # Offset for system epoch
        epochtime = Expression('AbsoluteTime', time.strftime("%F %R", time.gmtime(0))).to_python(n_evaluation=evaluation)
        result += epochtime

        return Expression('DateList', from_python(result))


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

    #> DeleteFile[tmpfilename]
    """

    rules = {
        'SetFileDate[file_]': 'SetFileDate[file, DateList[], All]',
        'SetFileDate[file_, date]': 'SetFileDate[file, date, All]',
    }

    def apply(self, filename, datelist, attribute, evaluation):
        'SetFileDate[filename_, datelist_, attribute_]'
        
        py_filename = filename.to_python()
        py_datelist = datelist.to_python()
        py_attr = attribute.to_python()

        #Check filename
        if not (isinstance(py_filename, basestring) and py_filename[0] == py_filename[-1] == '"'):
            evaluation.message('SetFileDate', 'TODO0', filename)
            return
        py_filename = path_search(py_filename.strip('"'))

        if py_filename is None:
            evaluation.message('SetFileDate', 'TODO1', filename)
            return

        #Check datelist
        if not (isinstance(py_datelist, list) and len(py_datelist) == 6 and 
            all(isinstance(d, int) for d in py_datelist[:-1]) and isinstance(py_datelist[-1], float)):
            evaluation.message('SetFileDate', 'TODO2', datelist)

        #Check attribute
        if py_attr not in ['"Access"', '"Creation"', '"Modification"', 'All']:
            evaluation.message('SetFileDate', 'todo3', attribute)
            return

        epochtime = Expression('AbsoluteTime', time.strftime("%F %R", time.gmtime(0))).evaluate(evaluation).to_python()
        stattime = Expression('AbsoluteTime', datelist).to_python(n_evaluation=evaluation)
        stattime -= epochtime

        try:
            stat = os.stat(py_filename)
            if py_attr == '"Access"':
                os.utime(py_filename, (stattime, os.path.getatime(py_filename)))
            if py_attr == '"Creation"':
                #TODO: ???
                pass
            if py_attr == '"Modification"':
                os.utime(py_filename, (os.path.getatime(py_filename), stattime))
            if py_attr == 'All':
                os.utime(py_filename, (stattime, stattime))
        except OSError as e:
            print e
            #evaluation.message(...)
            return Symbol('$Failed')
    
        return Symbol('Null')


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
        'fstr': 'File specification `1` is not a string of one or more characters.',
        'nffil': 'File not found during `1`.',
    }

    def apply(self, source, dest, evaluation):
        'CopyFile[source_, dest_]'

        py_source = source.to_python()
        py_dest = dest.to_python()

        #Check filenames
        if not (isinstance(py_source, basestring) and py_source[0] == py_source[-1] == '"'):
            evaluation.message('CopyFile', 'fstr', source)
            return
        if not (isinstance(py_dest, basestring) and py_dest[0] == py_dest[-1] == '"'):
            evaluation.message('CopyFile', 'fstr', dest)
            return

        py_source = py_source.strip('"')
        py_dest = py_dest.strip('"')

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
            evaluation.message('CopyFile', 'nffil', Expression('CopyFile', source, dest))
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
        'fstr': 'File specification `1` is not a string of one or more characters.',
        'nffil': 'File not found during `1`.',
    }

    def apply(self, source, dest, evaluation):
        'RenameFile[source_, dest_]'

        py_source = source.to_python()
        py_dest = dest.to_python()

        #Check filenames
        if not (isinstance(py_source, basestring) and py_source[0] == py_source[-1] == '"'):
            evaluation.message('RenameFile', 'fstr', source)
            return
        if not (isinstance(py_dest, basestring) and py_dest[0] == py_dest[-1] == '"'):
            evaluation.message('RenameFile', 'fstr', dest)
            return

        py_source = py_source.strip('"')
        py_dest = py_dest.strip('"')

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
        'strs': 'String or non-empty list of strings expected at position `1` in `2`.',
        'nffil': 'File not found during `1`.',
    }

    def apply(self, filename, evaluation):
        'DeleteFile[filename_]'

        py_path = filename.to_python()
        if not isinstance(py_path, list):
            py_path = [py_path]

        py_paths = []
        for path in py_path:
            #Check filenames
            if not (isinstance(path, basestring) and path[0] == path[-1] == '"'):
                evaluation.message('DeleteFile', 'strs', filename, Expression('DeleteFile', filename))
                return

            path = path.strip('"')
            path = path_search(path)

            if path is None:
                evaluation.message('DeleteFile', 'nffil', Expression('DeleteFile', filename))
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
        'fstr': 'File specification `1` is not a string of one or more characters.',
    }

    def apply(self, path, evaluation):
        'ParentDirectory[path_]'

        if not isinstance(path, String):
            evaluation.message('ParentDirectory', 'fstr', path)
            return

        pypath = path.to_python().strip('"')

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
    """

    rules = {
        'SetDirectory[]': 'SetDirectory[$HomeDirectory]',
    }

    def apply(self, path, evaluation):
        'SetDirectory[path_]'

        if not isinstance(path, String):
            #evaluation.message() #TODO
            return

        py_path = path.__str__().strip('"')
        py_path = path_search(py_path)
    
        if py_path is None:
            #evaluation.message() #TODO
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
    
    def apply(self, evaluation):
        'ResetDirectory[]'
        global DIRECTORY_STACK

        try:
            tmp = DIRECTORY_STACK.pop()
        except IndexError:
            #evaluation.message #TODO
            return Symbol('$Failed')
        os.chdir(tmp)
        return String(tmp)


class FileType(Builtin):
    """
    <dl>
    <dt>'FileType["$file$"]
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
        'fstr': 'File specification `1` is not a string of one or more characters.',
    }

    def apply(self, filename, evaluation):
        'FileType[filename_]'
        if not isinstance(filename, String):
            evaluation.message('FileType', 'fstr', filename)
            return
        path = filename.to_python().strip('"')

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
    <dt>'FileExistsQ["$file$"]
      <dd>returns 'True' if $file$ exists and 'False' otherwise.
    </dl>

    >> FileExistsQ["ExampleData/sunflowers.jpg"]
     = True
    >> FileExistsQ["ExampleData/sunflowers.png"]
     = False
    """

    messages = {
        'fstr': 'File specification `1` is not a string of one or more characters.',
    }

    def apply(self, filename, evaluation):
        'FileExistsQ[filename_]'
        path = filename.to_python()
        if not (isinstance(path, basestring) and path[0] == path[-1] == '"'):
            evaluation.message('FileExistsQ', 'fstr', filename)
            return
        path = path.strip('"')

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
        'fstr': 'File specification `1` is not a string of one or more characters.',
    }

    def apply(self, pathname, evaluation):
        'DirectoryQ[pathname_]'
        path = pathname.to_python()

        if not (isinstance(path, basestring) and path[0] == path[-1] == '"'):
            evaluation.message('DirectoryQ', 'fstr', pathname)
            return
        path = path.strip('"')

        #path = path_search(path)
        #TODO

        if path.startswith('ExampleData'):
            path = ROOT_DIR + 'data/' + path

        if os.path.isdir(path):
            return Symbol('True')
        return Symbol('False')

