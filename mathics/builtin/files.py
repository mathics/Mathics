# -*- coding: utf8 -*-

"""
File Operations
"""

import io

from mathics.core.expression import Expression, String, from_python
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
    <dt>'Write[stream, expr1, expr2, ... ]'
        <dd>writes the expressions to the output channel followed by a newline"
    </dl>
    """

    def apply(self, channel, exprs, evaluation):
        'Write[channel_, exprs___]'
        #print exprs
        #TODO

class WriteString(Builtin):
    """
    <dl>
    <dt>'Write[channel, expr]'
        <dd>writes the expression to the output channel as a string."
    </dl>
    """
    #TODO: Multiple exprs

    def apply(self, name, n, expr, evaluation):
        'WriteString[OutputStream[name_, n_], expr_]'
        global STREAMS

        expr_str =  expr.to_python().strip('"')

        if not isinstance(expr_str, unicode):
            #TODO: Conversion to string
            return

        stream = STREAMS[n.to_python()]
        
        stream.write(expr_str)
        return String('')
        

class Save(Builtin):
    pass

class OpenRead(Builtin):
    """
    <dl>
    <dt>'OpenRead["file"]'
        <dd>opens a file and returns an InputStream. 
    </dl>
    """

    def apply(self, path, evaluation):
        'OpenRead[path_]'

        path_string = path.to_python().strip('"')

        try:
            stream = io.open(path_string, mode='r')
        except IOError:
            evaluation.message('General', 'noopen', path)
            return

        n = _put_stream(stream)
        result = Expression('InputStream', path, n)
        global _STREAMS
        _STREAMS[n] = result

        return result

class OpenWrite(Builtin):
    """
    <dl>
    <dt>'OpenWrite["file"]'
        <dd>opens a file and returns an OutputStream. 
    </dl>
    """

    def apply(self, path, evaluation):
        'OpenWrite[path_]'

        path_string = path.to_python().strip('"')

        try:
            stream = io.open(path_string, mode='w')
        except IOError:
            evaluation.message('General', 'noopen', path)
            return

        n = _put_stream(stream)
        result = Expression('OutputStream', path, n)
        global _STREAMS
        _STREAMS[n] = result

        return result

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
        return String('')

    def apply_output(self, name, n, evaluation):
        'Close[OutputStream[name_, n_]]'
        global STREAMS
        stream = STREAMS[n.to_python()]

        if stream.closed:
            evaluation.message('General', 'openx', name)
            return

        stream.close()
        return String('')

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

