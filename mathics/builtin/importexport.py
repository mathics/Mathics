# -*- coding: utf8 -*-

"""
Importing and Exporting
"""

import os

from mathics.core.expression import Expression, from_python
from mathics.builtin.base import Builtin, Predefined, Symbol, String
from mathics.settings import ROOT_DIR

from pymimesniffer import magic


IMPORTERS = {}
EXPORTERS = {}

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
        return from_python(IMPORTERS.keys())


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
        return from_python(EXPORTERS.keys())

#FIXME This should be private, that is accesed with ImportExport`RegisterImport
class RegisterImport(Builtin):
    """
    <dl>
    <dt>'RegisterImport["$format$", $defaultFunction$]'
      <dd>register '$defaultFunction$' as the default function used when importing from a file of type '"$format$"'.
    <dt>'RegisterImport["$format$", {"$elem1$" :> $conditionalFunction1$, "$elem2$" :> $conditionalFunction2$, ..., $defaultFunction$}]'
      <dd>registers multiple elements ($elem1$, ...) and their corresponding converter functions ($conditionalFunction1$, ...) in addition to the $defaultFunction$.
    <dt>'RegisterImport["$format$", {"$conditionalFunctions$, $defaultFunction$, "$elem3$" :> $postFunction3$, "$elem4$" :> $postFunction4$, ...}]'
      <dd>also registers additional elements ($elem3$, ...) whose converters ($postFunction3$, ...) act on output from the low-level funcions.
    </dl>

    First, define the default function used to import the data.
    >> ExampleFormat1Import[filename_String] := Module[{stream, head, data}, stream = OpenRead[filename]; head = ReadList[stream, "String", 2]; data = Partition[ReadList[stream, Number], 2]; Close[stream]; {"Header" -> head, "Data" -> data}]

    'RegisterImport' is then used to register the above function to a new data format.
    >> RegisterImport["ExampleFormat1", ExampleFormat1Import]

    >> FilePrint["ExampleData/ExampleData1.txt"]
     | Example File Format
     | Created by Angus
     | 0.629452        0.586355
     | 0.711009        0.687453
     | 0.246540        0.433973
     | 0.926871        0.887255
     | 0.825141        0.940900
     | 0.847035        0.127464
     | 0.054348        0.296494
     | 0.838545        0.247025
     | 0.838697        0.436220
     | 0.309496        0.833591

    >> Import["ExampleData/ExampleData1.txt", {"ExampleFormat1", "Elements"}]
     = {Data, Header}

    >> Import["ExampleData/ExampleData1.txt", {"ExampleFormat1", "Header"}]
     = {"Example File Format", "Created by Angus"}
    """

    attributes = ('Protected', 'ReadProtected')

    options = {
        'Path': 'Automatic',
        'FunctionChannels': '{"FileNames"}',
        'Sources': 'None',
        'DefaultElement': 'Automatic',
        'AvailableElements': 'None',
        'Options': '{}',
        'OriginalChannel': 'False',
        'BinaryFormat': 'False',
        'Encoding': 'False',
        'Extensions': '{}',
        'AlphaChannel': 'False',
    }

    def apply(self, formatname, function, evaluation, options):
        'RegisterImport[formatname_String, function_, OptionsPattern[RegisterImport]]'
        
        if not function.get_head() == 'List':
            function = Expression('List', function)

        leaves = function.get_leaves()
        isdefault = [not x.has_form('RuleDelayed', None) for x in leaves]

        # Only one default Importer is allowed
        if sum(isdefault) != 1:
            #TODO: Message
            return Symbol('$Failed')

        indx = isdefault.index(True)

        IMPORTERS[formatname.get_string_value()] = (
            {},                                     # Conditional Raw-Importers
            leaves[indx],                           # Default Importer
            {}                                      # Post-Importers
        )

        return Symbol('Null')

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

    #> Import["ExampleData/ExampleData1.tx"]
     : File not found during Import.
     = $Failed
    #> Import[x]
     : First argument x is not a valid file, directory, or URL specification.
     = $Failed

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
        'noelem': 'The Import element `1` is not present when importing as `2`.',
        'fmtnosup': '`1` is not a supported Import format.',
    }

    def importer(self, filename, evaluation):

        path = filename.to_python()
        if not (isinstance(path, basestring) and path[0] == path[-1] == '"'):
            evaluation.message('Import', 'chtype', filename)
            return None
        path = path.strip('"')

        if Expression('FindFile', filename).evaluate(evaluation) == Symbol('$Failed'):
            evaluation.message('Import', 'nffil')
            return None

        if self.filetype is None:
            self.filetype = Expression('FileFormat', path).evaluate(evaluation=evaluation).get_string_value()

        if self.filetype not in IMPORTERS.keys():
            evaluation.message('Import', 'fmtnosup', from_python(self.filetype))
            return None

        return IMPORTERS[self.filetype]
        #(conditional_function, default_function, post_functions) = IMPORTERS[filetype]
        #return (conditional_function, default_function, post_functions)

    def apply(self, filename, evaluation):
        'Import[filename_]'
        
        self.filetype = None
        result = self.importer(filename, evaluation)

        if result is None:
            return Symbol('$Failed')

        (conditional_function, default_function, post_functions) = result

    def apply_elements(self, filename, elements, evaluation):
        'Import[filename_, elements_]'

        elements = elements.to_python()
        if not isinstance(elements, list):
            elements = [elements] 

        for el in elements:
            if not (isinstance(el, basestring) and el[0] == el[-1] == '"'):
                evaluation.message('Import', 'noelem', from_python(el))
                return Symbol('$Failed')
    
        elements = [el[1:-1] for el in elements]

        self.filetype = None
        for el in elements:
            if el in IMPORTERS.keys():
                self.filetype = el
                elements.remove(el)
                break

        result = self.importer(filename, evaluation)

        if result is None:
            return Symbol('$Failed')

        (conditionals, default_function, posts) = result

        def get_defaults():
            defaults = Expression(from_python(default_function), filename).evaluate(evaluation)
            defaults = defaults.get_leaves()
            assert all(expr.has_form('Rule', None) for expr in defaults)
            return {a.get_string_value() : b for (a,b) in map(lambda x: x.get_leaves(), defaults)}

        defaults = None
        for el in elements:
            if el == "Elements":
                defaults = get_defaults()
                return from_python(sorted(conditionals.keys() + defaults.keys() + posts.keys()))
            else:
                if el in conditionals.keys():
                    return conditionals[el].evaluate(evaluation)
                elif el in posts.keys():
                    return posts[el].evaluate(evaluation)
                else:
                    if defaults is None:
                        defaults = get_defaults()
                    if el in defaults.keys():
                        return defaults[el]
                    else:
                        evaluation.message('Import', 'noelem', from_python(el), from_python(self.filetype))
                        return Symbol('$Failed')

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

    >> FileFormat["ExampleData/lena.tif"]
     = TIFF

    ## ASCII text
    #> FileFormat["ExampleData/BloodToilTearsSweat.txt"]
     = Text
    #> FileFormat["ExampleData/MadTeaParty.gif"]
     = GIF
    #> FileFormat["ExampleData/moon.tif"]
     = TIFF

    """

    messages = {
        'nffil': 'File not found during `1`.',
    }
    
    detector = None

    def apply(self, filename, evaluation):
        'FileFormat[filename_?StringQ]'

        path = filename.to_python().strip('"')

        if path.startswith("ExampleData/"):
            path = ROOT_DIR + 'data/' + path

        if not os.path.exists(path):
            evaluation.message('FileFormat', 'nffil', Expression('FileFormat', filename))
            return Symbol('$Failed')
        
        if not FileFormat.detector:
            loader = magic.MagicLoader()
            loader.load()
            FileFormat.detector = magic.MagicDetector(loader.mimetypes)
            
        mimetypes = FileFormat.detector.match(path)
        mimetypes = set(mimetypes)

        #TODO: Add more file formats
        
        result = 'Binary'
        if 'image/gif' in mimetypes:
            result = 'GIF'
        elif 'image/jpeg' in mimetypes:
            result = 'JPEG'
        elif 'application/pdf' in mimetypes:
            result = 'PDF'
        elif 'image/png' in mimetypes:
            result = 'PNG'
        elif 'image/tiff' in mimetypes:
            result = 'TIFF'
        else:
            for mimetype in mimetypes:
                if mimetype.startswith('text'):
                    result = 'Text'
                    break
                elif 'xml' in mimetype:
                    result = 'XML'
                    break
            else:
                # TODO: text file recognition is not perfect
                if path.lower().endswith('.txt'):
                    result = 'Text'

        return from_python(result)

