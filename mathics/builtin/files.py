# -*- coding: utf8 -*-

"""
File Operations
"""

import io

from mathics.core.expression import Expression, String
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

    def apply(self, stream, types, evaluation):
        'Read[stream_, types_]'
        #TODO

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
    pass

class Save(Builtin):
    pass

class OpenRead(Builtin):
    pass

class OpenWrite(Builtin):
    pass

class Import(Builtin):
    pass

class Export(Builtin):
    pass

class ReadList(Builtin):
    pass

class FilePrint(Builtin):
    """
    <dl>
    <dt>'FilePrint["file"]
        <dd>prints the raw contents of $file$.
    </dl>
    """

    def apply(self, path, evaluation):
        'FilePrint[path_]'
        path = path.to_python().strip('"') #Bug in to_python()?

        try:
            f = open(path, 'r')
            result = f.read()
            f.close()
        except IOError:
            evaluation.message('General', 'noopen', path)
            return

        return Expression('String', result)

class InputStream(Builtin):
    """
    <dl>
    <dt>'Input["name", n]'
        <dd>represents an input stream.
    </dl>
    """

    def apply(self, name, n, evaluation):
        'InputStream[name_, n_]'
        return

class StringToStream(Builtin):
    """
    <dl>
    <dt>'StringToStream["string"]'
        <dd>converts a string to an open stream.
    </dl>

    >> StringToStream["abc 123"]
     = InputStream[String, 18]
    """
    
    def apply(self, string, evaluation):
        'StringToStream[string_]'
        pystring = string.to_python().strip('"')
        global NSTREAM
        global STREAMS
        if NSTREAM is None:
            NSTREAM = 0 
            STREAMS = {}
        NSTREAM += 1
        STREAMS[NSTREAM] = io.StringIO(initial_value=unicode(pystring))
        return Expression('InputStream', 'String', NSTREAM)


