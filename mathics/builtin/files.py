# -*- coding: utf8 -*-

"""
File Operations
"""

import io
import shutil
import hashlib
import os
from zlib import adler32, crc32

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
    <dt>'Find[$stream$, $text$]'
        <dd>find the first line in $stream$ that contains $text$.
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

    >> str = FindList["/home/angus/const.txt", "electors"];
    #> Length[str]
     = 6

    >> str = FindList["/home/angus/const.txt", "electors", 1]
     = {choice of electors for President and Vice-President of the United States,}

    >> str = FindList["/home/angus/const.txt", "democracy"]
     = {}
    """

    rules = {
        'FindList[file_, text_]': 'FindList[file, text, All]',
    }

    #TODO: Extra options AnchoredSearch, IgnoreCase RecordSeparators, WordSearch, WordSeparators
    # this is probably best done with a regex

    def apply(self, filename, text, n, evaluation):
        'FindList[filename_, text_, n_]'
        py_text = text.to_python()
        py_name = filename.to_python()
        py_n = n.to_python()

        if not isinstance(py_text, list):
            py_text = [py_text]

        if not isinstance(py_name, list):
            py_name = [py_name]

        if not all(isinstance(t, basestring) and t[0] == t[-1] == '"' for t in py_text):
            evaluation.message('FindList', 'todo', text)
            return

        if not all(isinstance(t, basestring) and t[0] == t[-1] == '"' for t in py_name):
            evaluation.message('FindList', 'todo', filename)
            return

        if not ((isinstance(py_n, int) and py_n > 0) or py_n == 'All'):
            evaluation.message('FindList', 'todo', n)
            return

        py_text = [t.strip('"') for t in py_text]
        py_name = [t.strip('"') for t in py_name]

        results = []
        for path in py_name:
            try:
                f = open(path, 'r')
                lines = f.readlines()
                f.close()
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

# File Properties

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
            time = os.path.getatime(path)
        elif time_type in ['Creation', 'Change']:   # TODO: Fixing this cross platform is difficult
            time = os.path.getctime(path)
        elif time_type == 'Modification':
            time = os.path.getmtime(path)
        else:
            return

        # Mathematica measures epoch from Jan 1 1900, while python is from Jan 1 1970!
        return Expression('DateList', from_python(time + 2208988800))

class FileHash(Builtin):
    """
    <dl>
    <dt>'FileHash[$file$]'
      <dd>returns an integer hash for the given $file$.
    <dt>'FileHash[$file$, $types$]'
      <dd>returns an integer hash of specified $type$ for the given $file$.
    </dl>

    >> FileHash["/home/angus/const.txt"]
     = 327543094924296211684012457845472528194

    >> FileHash["/home/angus/const.txt", "MD5"]
     = 327543094924296211684012457845472528194

    >> FileHash["/home/angus/const.txt", "Adler32"]
     = 379970077

    >> FileHash["/home/angus/const.txt", "CRC32"]
     = -714117466

    >> FileHash["/home/angus/const.txt", "SHA"]
     = 373198480298074987456387719402557487603665831619

    >> FileHash["/home/angus/const.txt", "SHA224"]
     = 3917722155990863301063131219313332812450825714047709114184660657040

    >> FileHash["/home/angus/const.txt", "SHA256"]
     = 11644666593438942629277351724048748939285751972475052522764329079189078028814

    >> FileHash["/home/angus/const.txt", "SHA384"]
     = 30483718751041705495695586237041127244740940313861521100269551019906506917011401631803235020535267688078407100882707

    >> FileHash["/home/angus/const.txt", "SHA512"]
     = 1326437464183143781001018855370051677613062767836549469526136224930996101542674475615428678343701377300870189417449686598400481332250256827946277566015805
    """

    rules = {
        'FileHash[filename_]': 'FileHash[filename, "MD5"]',
    }

    def apply(self, filename, hashtype, evaluation):
        'FileHash[filename_, hashtype_]'
        py_hashtype = hashtype.to_python()
        py_filename = filename.to_python()

        #TODO: MD2?
        supported_hashes = ['Adler32', 'CRC32', 'MD5', 'SHA', 'SHA224', 'SHA256', 'SHA384', 'SHA512']

        # Check hashtype
        if not (isinstance(py_hashtype, basestring) and py_hashtype[0] == py_hashtype[-1] == '"'):
            evaluation.message('FileHash', 'todo1', hashtype)
            return
        py_hashtype = py_hashtype.strip('"')

        if py_hashtype not in supported_hashes:
            evaluation.message('FileHash', 'todo1', hashtype)
            return

        # Check filename
        if not (isinstance(py_filename, basestring) and py_filename[0] == py_filename[-1] == '"'):
            evaluation.message('FindList', 'todo2', filename)
            return
        py_filename = py_filename.strip('"')

        try:
            f = open(py_filename, 'rb')
            dump = f.read()
            f.close()
        except IOError:
            evaluation.message('General', 'noopen', filename)
            return

        if py_hashtype == 'Adler32':
            result = adler32(dump)
        if py_hashtype == 'CRC32':
            result = crc32(dump)
        if py_hashtype == 'MD5':
            result = int(hashlib.md5(dump).hexdigest(), 16)
        if py_hashtype == 'SHA':
            result = int(hashlib.sha1(dump).hexdigest(), 16)
        if py_hashtype == 'SHA224':
            result = int(hashlib.sha224(dump).hexdigest(), 16)
        if py_hashtype == 'SHA256':
            result = int(hashlib.sha256(dump).hexdigest(), 16)
        if py_hashtype == 'SHA384':
            result = int(hashlib.sha384(dump).hexdigest(), 16)
        if py_hashtype == 'SHA512':
            result = int(hashlib.sha512(dump).hexdigest(), 16)

        return from_python(result)


class FileByteCount(Builtin):
    """
    <dl>
    <dt>'FileByteCount[$file$]'
      <dd>returns the number of bytes in $file$.
    </dl>

    >> FileByteCount["/home/angus/const.txt"]
     = 45119
    """

    def apply(self, filename, evaluation):
        'FileByteCount[filename_]'
        py_filename = filename.to_python()
        if not (isinstance(py_filename, basestring) and py_filename[0] == py_filename[-1] == '"'):
            evaluation.message('FindList', 'todo2', filename)
            return
        py_filename = py_filename.strip('"')

        try:
            f = open(py_filename, 'rb')

            count = 0 
            tmp = f.read(1)
            while tmp != '':
                count += 1
                tmp = f.read(1)

            f.close()
        except IOError:
            evaluation.message('General', 'noopen', filename)
            return

        return from_python(count)

# TODO: This has to wait until the time branch has been merged
#
#class SetFileDate(Builtin):
#    """
#    <dl>
#    <dt>'SetFileDate["$file$"]'
#      <dd>set the file access and modification dates of $file$ to the current date.
#    <dt>'SetFileDate["$file$", $date$]'
#      <dd>set the file access and modification dates of $file$ to the specified date list.
#    <dt>'SetFileDate["$file$", $date$, "$type$"]'
#      <dd>set the file date of $file$ to the specified date list. 
#      The "$type$" can be one of "$Access$", "$Creation$", "$Modification$", or 'All'.
#    </dl>
#
#    >> SetFileDate["/home/angus/const.txt"]
#
#    """
#
#    rules = {
#        'SetFileDate[file_]': 'SetFileDate[file, DateList[], All]',
#        'SetFileDate[file_, date]': 'SetFileDate[file, date, All]',
#    }
#
#    def apply(self, filename, datelist, attribute, evaluation):
#        'SetFileDate[filename_, datelist_, attribute_]'
#        
#        py_filename = filename.to_python()
#        py_datelist = datelist.to_python()
#        py_attr = attribute.to_python()
#
#        #Check filename
#        if not (isinstance(py_filename, basestring) and py_filename[0] == py_filename[-1] == '"'):
#            evaluation.message('SetFileDate', 'todo1', filename)
#            return
#        py_filename = py_filename.strip('"')
#
#        #Check datelist
#        if not (isinstance(py_datelist, list) and len(pydatelist) == 6 and 
#            all(isinstance(d, int) for d in py_datelist[:-1]) and isinstance(py_datelist[-1], float)):
#            evaluation.message('SetFileDate', 'todo2', datelist)
#
#        #Check attribute
#        if py_attr not in ['"Access"', '"Creation"', '"Modification"', 'All']:
#            evaluation.message('SetFileDate', 'todo3', attribute)
#            return
#        try:
#            with open(py_filename, 'a'):
#                if py_attr == '"Access"':
#                    pass #TODO
#                if py_attr == '"Creation"':
#                    pass #TODO
#                if py_attr == '"Modification"':
#                    pass #TODO
#                if py_attr == 'All':
#                    pass #TODO
#        except IOError:
#            evaluation.message('General', 'noopen', filename)
#            return
#    
#        return Symbol('Null')
#

class CopyFile(Builtin):
    """
    <dl>
    <dt>'CopyFile["$file1$", "$file2$"]'
      <dd>copies $file1$ to $file2$.
    </dl>

    >> CopyFile["/home/angus/const.txt", "/home/angus/12.txt"]
     = /home/angus/12.txt
    >> DeleteFile["/home/angus/12.txt"]
    """

    def apply(self, source, dest, evaluation):
        'CopyFile[source_, dest_]'

        py_source = source.to_python()
        py_dest = dest.to_python()

        #Check filenames
        if not (isinstance(py_source, basestring) and py_source[0] == py_source[-1] == '"'):
            evaluation.message('CopyFile', 'todo1', source)
            return
        if not (isinstance(py_dest, basestring) and py_dest[0] == py_dest[-1] == '"'):
            evaluation.message('CopyFile', 'todo2', dest)
            return

        py_source = py_source.strip('"')
        py_dest = py_dest.strip('"')

        if not os.path.exists(py_source):
            evaluation.message('CopyFile', 'todo3', source)
            return Symbol('$Failed')
        if os.path.exists(py_dest):
            evaluation.message('CopyFile', 'todo4', dest)
            return Symbol('$Failed')

        try:
            shutil.copy(py_source, py_dest)
        except IOError:
            evaluation.message('CopyFile', 'todo5', dest)
            return Symbol('$Failed')

        return dest


class RenameFile(Builtin):
    """
    <dl>
    <dt>'RenameFile["$file1$", "$file2$"]'
      <dd>renames $file1$ to $file2$.
    </dl>

    >> CopyFile["/home/angus/const.txt", "/home/angus/12.txt"]
     = /home/angus/12.txt
    >> RenameFile["/home/angus/12.txt", "/home/angus/13.txt"]
     = /home/angus/13.txt
    >> DeleteFile["/home/angus/13.txt"]
    """

    def apply(self, source, dest, evaluation):
        'RenameFile[source_, dest_]'

        py_source = source.to_python()
        py_dest = dest.to_python()

        #Check filenames
        if not (isinstance(py_source, basestring) and py_source[0] == py_source[-1] == '"'):
            evaluation.message('RenameFile', 'todo1', source)
            return
        if not (isinstance(py_dest, basestring) and py_dest[0] == py_dest[-1] == '"'):
            evaluation.message('RenameFile', 'todo2', dest)
            return

        py_source = py_source.strip('"')
        py_dest = py_dest.strip('"')

        if not os.path.exists(py_source):
            evaluation.message('RenameFile', 'todo3', source)
            return Symbol('$Failed')
        if os.path.exists(py_dest):
            evaluation.message('RenameFile', 'todo4', dest)
            return Symbol('$Failed')

        try:
            shutil.move(py_source, py_dest)
        except IOError:
            evaluation.message('RenameFile', 'todo5', dest)
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

    >> CopyFile["/home/angus/const.txt", "/home/angus/12.txt"];
    >> DeleteFile["/home/angus/12.txt"]

    >> CopyFile["/home/angus/const.txt", "/home/angus/12.txt"];
    >> CopyFile["/home/angus/const.txt", "/home/angus/13.txt"];
    >> DeleteFile[{"/home/angus/12.txt", "/home/angus/13.txt"}]
    """

    def apply(self, filename, evaluation):
        'DeleteFile[filename_]'

        py_path = filename.to_python()
        if not isinstance(py_path, list):
            py_path = [py_path]

        for path in py_path:
            #Check filenames
            if not (isinstance(path, basestring) and path[0] == path[-1] == '"'):
                evaluation.message('DeleteFile', 'todo1', filename)
                return

            if not os.path.exists(path.strip('"')):
                evaluation.message('DeleteFile', 'todo3', from_python(path))
                return Symbol('$Failed')

        py_path = [path.strip('"') for path in py_path]
   
        for path in py_path:
            try:
                os.remove(path)
            except OSError:
                evaluation.message('DeleteFile', 'todo5', from_python(path))
                return Symbol('$Failed')

        return Symbol('Null')

class FileExistsQ(Builtin):
    """
    <dl>
    <dt>'FileExistsQ["$file$"]
      <dd>returns 'True' if $file$ exists and 'False' otherwise.
    </dl>

    >> FileExistsQ["/home/angus/const.txt"]
     = True
    >> FileExistsQ["/home/angus/13.txt"]
     = False
    """

    def apply(self, filename, evaluation):
        'FileExistsQ[filename_]'
        path = filename.to_python()
        if not (isinstance(path, basestring) and path[0] == path[-1] == '"'):
            evaluation.message('FileExistsQ', 'todo1', filename)
            return

        if os.path.exists(path.strip('"')):
            return Symbol('True')
        return Symbol('False')

