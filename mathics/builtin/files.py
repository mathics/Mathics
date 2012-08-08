# -*- coding: utf8 -*-

"""
File Operations
"""

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
    pass

class Write(Builtin):
    pass

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
    pass

