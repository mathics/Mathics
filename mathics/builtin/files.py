# -*- coding: utf8 -*-

"""
File Operations
"""

import io

from mathics.core.expression import Expression, String, Symbol, from_python
from mathics.builtin.base import Builtin, Predefined

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
    """

    rules = {
        'Read[stream_]': 'Read[stream, Expression]',
    }

    def apply(self, name, n, types, evaluation):
        'Read[InputStream[name_, n_], types_]'
        global STREAMS
    
        stream = STREAMS[n.to_python()]
        
        types = types.to_python()
        if not isinstance(types, list):
            types = [types]
        
        name = name.to_python()

        result = []

        for typ in types:
            if typ == 'String':
                result.append(stream.readline())
            elif typ == 'Byte':
                result.append(ord(stream.read(1)))

        if len(result) == 1:
            return from_python(*result)

        return from_python(result)
                
class Write(Builtin):
    """
    <dl>
    <dt>'Write[channel, expr]'
        <dd>writes the expression to the output channel as a string."
    </dl>
    """

    def apply(self, name, n, expr, evaluation):
        'WriteString[OutputStream[name_, n_], expr___]'
        global STREAMS
        stream = STREAMS[n.to_python()]

        expr = expr.get_sequence()
        expr = Expression('Row', Expression('List', *expr))

        evaluation.format = 'text'
        text = evaluation.format_output(from_python(expr))
        stream.write(text)
        return Symbol('Null')

class WriteString(Builtin):
    """
    <dl>
    <dt>'Write[stream, expr1, expr2, ... ]'
        <dd>writes the expressions to the output channel followed by a newline"
    </dl>
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
        text = ''.join(text)
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

class ReadList(Builtin):
    """
    <dl>
    <dt>'ReadList["file"]
        <dd>Reads all the expressions until the end of file.
    </dl>
    """

    rules = {
        'ReadList[stream_]': 'ReadList[stream, Expression]',
    }

    def apply(self, name, n, types, evaluation):
        'ReadList[InputStream[name_, n_], types_]'
        global STREAMS

        stream = STREAMS[n.to_python()]

        types = types.to_python()
        if not isinstance(types, list):
            types = [types]

        name = name.to_python()

        result = []

        for typ in types:
            if typ == 'String':
                result.append(stream.readlines())
            else:
                #TODO
                pass

        if len(result) == 1:
            return from_python(*result)

        return from_python(result)

class FilePrint(Builtin):
    """
    <dl>
    <dt>'FilePrint["file"]
        <dd>prints the raw contents of $file$.
    </dl>
    """

    def apply(self, path, evaluation):
        'FilePrint[path_]'
        path = path.to_python().strip('"')

        try:
            f = open(path, 'r')
            result = f.read()
            f.close()
        except IOError:
            evaluation.message('General', 'noopen', path)
            return

        return from_python(result)

class Close(Builtin):
    """
    <dl>
    <dt>'Close[stream]'
        <dd>closes an input or output stream.
    </dl>
    
    """
     
    def apply_input(self, name, n, evaluation):
        'Close[InputStream[name_, n_]]'
        global STREAMS
        stream = STREAMS[n.to_python()]

        if stream.closed:
            evaluation.message('General', 'openx', name)
            return

        stream.close()
        return Symbol('Null')

    def apply_output(self, name, n, evaluation):
        'Close[OutputStream[name_, n_]]'
        global STREAMS
        stream = STREAMS[n.to_python()]

        if stream.closed:
            evaluation.message('General', 'openx', name)
            return

        stream.close()
        return Symbol('Null')

    def apply_default(self, stream, evaluation):
        'Close[stream_]'
        evaluation.message('General', 'stream', stream)
        return

class InputStream(Builtin):
    """
    <dl>
    <dt>'InputStream["name", n]'
        <dd>represents an input stream.
    </dl>
    """

    def apply(self, name, n, evaluation):
        'InputStream[name_, n_]'
        return

class OutputStream(Builtin):
    """
    <dl>
    <dt>'OutputStream["name", n]'
        <dd>represents an output stream.
    </dl>
    """
    def apply(self, name, n, evaluation):
        'OutputStream[name_, n_]'
        return


class StringToStream(Builtin):
    """
    <dl>
    <dt>'StringToStream["string"]'
        <dd>converts a string to an open stream.
    </dl>

    >> StringToStream["abc 123"]
     = InputStream[String, 1]
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
    """

    def apply(self, evaluation):
        'Streams[]'
        global _STREAMS
        return Expression('List', *_STREAMS.values())

def _put_stream(stream):
    global STREAMS
    global _STREAMS
    global NSTREAMS

    try:
        STREAMS
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

