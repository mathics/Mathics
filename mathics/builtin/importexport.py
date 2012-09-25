# -*- coding: utf8 -*-

"""
Importing and Exporting
"""

import os
import magic

from mathics.core.expression import Expression, from_python
from mathics.builtin.base import Builtin, Predefined, Symbol, String
from mathics.settings import ROOT_DIR


IMPORTFORMATS = ['Binary', 'BMP', 'GIF', 'JPEG', 'PDF', 'PNG', 'Text', 'TIFF', 'XML']
EXPORTFORMATS = []


class ImportFormats(Predefined):
    """
    <dl>
    <dt>'$ImportFormats'
        <dd>returns a list of file formats supported by Import.
    </dl>
    
    >> $ImportFormats
     = {...}
    """

    name = '$ImportFormats'

    def evaluate(self, evaluation):
        return from_python(IMPORTFORMATS)


class ExportFormats(Predefined):
    """
    <dl>
    <dt>'$ExportFormats'
        <dd>returns a list of file formats supported by Export.
    </dl>
    
    >> $ExportFormats
     = {...}
    """

    name = '$ExportFormats'

    def evaluate(self, evaluation):
        return from_python(EXPORTFORMATS)


class Import(Builtin):
    """
    <dl>
    <dt>'Import["$file$"]'
      <dd>imports data from a file.
    <dt>'Import["$file$", $elements$]'
      <dd>imports the specified elements from a file.
    <dt>'Import["http://$url$", ...]' and 'Import["ftp://$url$", ...]'
      <dd>imports from a URL.
    </dl>

    >> Import["ExampleData/BloodToilTearsSweat.txt", "Elements"]
     = {Data, Lines, Plaintext, String, Words}
    #> ListQ[Import["ExampleData/BloodToilTearsSweat.txt", "Data"]]
     = True
    #> ListQ[Import["ExampleData/BloodToilTearsSweat.txt", "Lines"]]
     = True
    #> StringQ[Import["ExampleData/BloodToilTearsSweat.txt", "Plaintext"]]
     = True
    #> StringQ[Import["ExampleData/BloodToilTearsSweat.txt", "String"]]
     = True
    #> ListQ[Import["ExampleData/BloodToilTearsSweat.txt", "Words"]]
     = True
    #> Import["ExampleData/BloodToilTearsSweat.txt", "Lines"] == Import["ExampleData/BloodToilTearsSweat.txt", "Data"]
     = True
    """

    #TODO: Images tests
    """
    >> Import["ExampleData/sunflowers.jpg"]
     = -Image-

    #> Import["ExampleData/sunflowers.jp"]
     : File not found during Import.
     = $Failed
    """

    messages = {
        'nffil': 'File not found during Import.',
        'chtype': 'First argument `1` is not a valid file, directory, or URL specification.',
    }

    def importer(self, filename, evaluation):

        path = filename.to_python()
        if not (isinstance(path, basestring) and path[0] == path[-1] == '"'):
            evaluation.message('Import', 'chtype', filename)
            return Symbol('$Failed')
        path = path.strip('"')

        if path.startswith("ExampleData"):
            path = ROOT_DIR + 'data/' + path

        if not os.path.exists(path):
            evaluation.message('Import', 'nffil')
            return None

        filetype = Expression('FileFormat', path).evaluate(evaluation=evaluation)

        assert isinstance(filetype, String)
        filetype = filetype.to_python().strip('"')
        assert filetype in IMPORTFORMATS

        result = {}
        if filetype == 'Text':
            with open(path, 'r') as f:
                plaintext = f.read()
                textlines = filter(lambda x: x != '', plaintext.split('\n'))
                textwords = filter(lambda x: x != '', plaintext.split())
                result['Plaintext'] = plaintext
                result['Lines'] = textlines
                result['Words'] = textwords
                result['String'] = plaintext
                result['Data'] = textlines

        return result

    def apply(self, filename, evaluation):
        'Import[filename_]'
        
        result = self.importer(filename, evaluation)

        if result is None:
            return Symbol('$Failed')

        return from_python(result['Data'])

    def apply_elements(self, filename, elements, evaluation):
        'Import[filename_, elements_]'

        elements = elements.to_python()
        if not (isinstance(elements, basestring) and elements[0] == elements[-1] == '"'):
            return Symbol('$Failed')
        elements = elements.strip('"')

        result = self.importer(filename, evaluation)

        if result is None:
            return Symbol('$Failed')

        if elements == "Elements":
            result = result.keys()
            result.sort()
            
            return from_python(result)
        return from_python(result[elements])

class Export(Builtin):
    """
    <dl>
    <dt>'Export["$file$.$ext$", $expr$]'
      <dd>exports $expr$ to a file, using the extension $ext$ to determine the format.
    <dt>'Export["$file$", $expr$, "$format$"]'
      <dd>exports $expr$ to a file in the specified format.
    <dt>'Export["$file$", $exprs$, $elems$]'
      <dd>exports $exprs$ to a file as elements specified by $elems$.
    </dl>
    """

    pass


class FileFormat(Builtin):
    """
    <dl>
    <dt>'FileFormat["$name$"]'
      <dd>attempts to determine what format 'Import' should use to import specified file.
    </dl>

    >> FileFormat["ExampleData/sunflowers.jpg"]
     = JPEG

    ## UTF-8 Unicode text
    >> FileFormat["ExampleData/EinsteinSzilLetter.txt"]
     = Text

    ## ASCII text
    >> FileFormat["ExampleData/BloodToilTearsSweat.txt"]
     = Text

    >> FileFormat["ExampleData/MadTeaParty.gif"]
     = GIF

    """

    messages = {
        'nffil': 'File not found during `1`.',
    }

    def apply(self, filename, evaluation):
        'FileFormat[filename_?StringQ]'

        path = filename.to_python().strip('"')

        if path.startswith("ExampleData/"):
            path = ROOT_DIR + 'data/' + path

        if not os.path.exists(path):
            evaluation.message('FileFormat', 'nffil', Expression('FileFormat', filename))
            return Symbol('$Failed')

        fileformat = magic.from_file(path)

        #TODO: Add more file formats

        if fileformat.startswith('PC bitmap,'):
            result = 'BMP'
        elif fileformat.startswith('GIF image data,'):
            result = 'GIF'
        elif fileformat.startswith('JPEG image data,'):
            result = 'JPEG'
        elif fileformat.startswith('PDF document,'):
            result = 'PDF'
        elif fileformat.startswith('PNG image data,'):
            result = 'PNG'
        elif fileformat.startswith('TIFF image data,'):
            result = 'TIFF'
        elif fileformat == 'XML  document text':
            result = 'XML'
        else:
            if 'text' in fileformat:
                result = 'Text'
            else:
                result = 'Binary'

        return from_python(result)

