# -*- coding: utf8 -*-

"""
File Operations
"""

import io
from os.path import getatime, getmtime, getctime

from mathics.core.expression import Expression, String, Symbol, from_python
from mathics.builtin.base import Builtin, Predefined

STREAMS = {}

class ImportFormats(Predefined):
    """
    <dl>
    <dt>'$ImportFormats'
        <dd>returns a list of file formats supported by Import.
    </dl>
    
    >> $ImportFormats
     = {}
    """

    name = '$ImportFormats'

    def evaluate(self, evaluation):
        return Expression('List')

class ExportFormats(Predefined):
    """
    <dl>
    <dt>'$ExportFormats'
        <dd>returns a list of file formats supported by Export.
    </dl>
    
    >> $ExportFormats
     = {}
    """

    name = '$ExportFormats'

    def evaluate(self, evaluation):
        return Expression('List')

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
                return from_python('EndOfFile')

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

    ##TODO: Need USER_DIR to store temp files like this
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
    def apply(self, path, evaluation):
        '%(name)s[path_]'

        path_string = path.to_python().strip('"')

        try:
            stream = io.open(path_string, mode=self.mode)
        except IOError:
            evaluation.message('General', 'noopen', path)
            return

        n = _put_stream(stream)
        result = Expression(self.stream_type, path, n)
        global _STREAMS
        _STREAMS[n] = result

        return result

class OpenRead(_OpenAction):
    """
    <dl>
    <dt>'OpenRead["file"]'
        <dd>opens a file and returns an InputStream. 
    </dl>
    """
    mode = 'r'
    stream_type = 'InputStream'

class OpenWrite(_OpenAction):
    """
    <dl>
    <dt>'OpenWrite["file"]'
        <dd>opens a file and returns an OutputStream. 
    </dl>
    """

    rules = {
        'OpenWrite[]': 'OpenWrite["/tmp/mathics.write_test"]',
    }
    
    mode = 'w'
    stream_type = 'OutputStream'


class OpenAppend(_OpenAction):
    """
    <dl>
    <dt>'OpenAppend["file"]'
        <dd>opens a file and returns an OutputStream to which writes are appended. 
    </dl>
    """

    mode = 'a'
    stream_type = 'OutputStream'

class Import(Builtin):
    pass

class Export(Builtin):
    pass

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
            if tmp.to_python() == '"EndOfFile"':
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
    """

    #TODO: Handle strange unix files - WARNING: These may crash your computer
    """
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
        pypath = pypath.strip('"')

        try:
            f = open(pypath, 'r')
            result = f.read()
            f.close()
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

    #TODO: Close by "name"
     
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
            if result.to_python() == '"EndOfFile"':
                return result
        return Symbol('Null')

class Find(Read):
    """
    <dl>
    <dt>'Find[$stream$, $text$"]'
        <dd>find the first line in $stream$ that contains $text$
    </dl>

    >> str = OpenRead["/home/angus/const.txt"];
    >> Find[str, "electors"]
     = choice of electors for President and Vice-President of the United States,
    >> Find[str, "electors"]
     = have one vote. The electors in each State shall have the qualifications
    >> Close[str]
     = ...

    >> str = OpenRead["/home/angus/const.txt"];
    >> Find[str, {"electors", "people"}]
     = of the press; or the right of the people peaceably to assemble, and to petition
    >> Close[str]
     = ...
    """

    def apply(self, name, n, text, evaluation):
        'Find[InputStream[name_, n_], text_]'
        py_text = text.to_python()

        if not isinstance(py_text, list):
            py_text = [py_text]

        if not all(isinstance(t, basestring) and t[0] == t[-1] == '"' for t in py_text):
            evaluation.message('Find', 'unknown', Expression('Find', Expression('InputStream', name, n), text))
            return

        py_text = [t.strip('"') for t in py_text]
        #print py_text

        while True:
            tmp = super(Find, self).apply(name, n, Symbol('Record'), evaluation)
            py_tmp = tmp.to_python().strip('"')

            #print py_tmp

            if py_tmp == 'EndOfFile':
                evaluation.message('Find', 'notfound', Expression('Find', Expression('InputStream', name, n), text))
                return Symbol("$Failed")

            for t in py_text:
                if py_tmp.find(t) != -1:
                    return from_python(py_tmp)

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
        stream = io.StringIO(initial_value=unicode(pystring))
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

class FileDate(Builtin):
    """
    <dl>
    <dt>'FileDate[$file$, $types$]'
        <dd>returns the time and date at which the file was last modified.
    </dl>
    """

    #TODO: Test different properties of some example data

    rules = {
        'FileDate[path_]': 'FileDate[path, "Modification"]',
    }

    def apply(self, path, timetype, evaluation):
        'FileDate[path_, timetype_]'
        path = path.to_python().strip('"')
        time_type = timetype.to_python().strip('"')
        if time_type == 'Access':
            time = getatime(path)
        elif time_type in ['Creation', 'Change']:   # TODO: Fixing this cross platform is difficult
            time = getctime(path)
        elif time_type == 'Modification':
            time = getmtime(path)
        else:
            return

        # Mathematica measures epoch from Jan 1 1900, while python is from Jan 1 1970!
        return Expression('DateList', from_python(time + 2208988800))

