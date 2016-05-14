#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import unicode_literals
from __future__ import absolute_import
import six

"""
Importing and Exporting
"""

from mathics.core.expression import Expression, from_python
from mathics.builtin.base import Builtin, Predefined, Symbol, String

from .pymimesniffer import magic
import mimetypes


mimetypes.add_type('application/vnd.wolfram.mathematica.package', '.m')

# Seems that JSON is not registered on the mathics.net server, so we do it manually here.
# Keep in mind that mimetypes has system-dependent aspects (it inspects "/etc/mime.types" and other files).
mimetypes.add_type('application/json', '.json')

IMPORTERS = {}
EXPORTERS = {}


class ImportFormats(Predefined):
    """
    <dl>
    <dt>'$ImportFormats'
        <dd>returns a list of file formats supported by Import.
    </dl>

    >> $ImportFormats
     = {CSV, JSON, Text}
    """

    name = '$ImportFormats'

    def evaluate(self, evaluation):
        return Expression('List', *sorted(IMPORTERS.keys()))


class ExportFormats(Predefined):
    """
    <dl>
    <dt>'$ExportFormats'
        <dd>returns a list of file formats supported by Export.
    </dl>

    >> $ExportFormats
     = {CSV, SVG, Text}
    """

    name = '$ExportFormats'

    def evaluate(self, evaluation):
        return Expression('List', *sorted(EXPORTERS.keys()))


# FIXME This should be private, ImportExport`RegisterImport
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
    >> ExampleFormat1Import[filename_String] := Module[{stream, head, data}, stream = OpenRead[filename]; head = ReadList[stream, String, 2]; data = Partition[ReadList[stream, Number], 2]; Close[stream]; {"Header" -> head, "Data" -> data}]

    'RegisterImport' is then used to register the above function to a new data format.
    >> RegisterImport["ExampleFormat1", ExampleFormat1Import]

    >> FilePrint["ExampleData/ExampleData.txt"]
     | Example File Format
     | Created by Angus
     | 0.629452	0.586355
     | 0.711009	0.687453
     | 0.246540	0.433973
     | 0.926871	0.887255
     | 0.825141	0.940900
     | 0.847035	0.127464
     | 0.054348	0.296494
     | 0.838545	0.247025
     | 0.838697	0.436220
     | 0.309496	0.833591

    >> Import["ExampleData/ExampleData.txt", {"ExampleFormat1", "Elements"}]
     = {Data, Header}

    >> Import["ExampleData/ExampleData.txt", {"ExampleFormat1", "Header"}]
     = {Example File Format, Created by Angus}

    Conditional Importer:
    >> ExampleFormat2DefaultImport[filename_String] := Module[{stream, head}, stream = OpenRead[filename]; head = ReadList[stream, String, 2]; Close[stream]; {"Header" -> head}]

    >> ExampleFormat2DataImport[filename_String] := Module[{stream, data}, stream = OpenRead[filename]; Skip[stream, String, 2]; data = Partition[ReadList[stream, Number], 2]; Close[stream]; {"Data" -> data}]

    >> RegisterImport["ExampleFormat2", {"Data" :> ExampleFormat2DataImport, ExampleFormat2DefaultImport}]

    >> Import["ExampleData/ExampleData.txt", {"ExampleFormat2", "Elements"}]
     = {Data, Header}

    >> Import["ExampleData/ExampleData.txt", {"ExampleFormat2", "Header"}]
     = {Example File Format, Created by Angus}

    >> Import["ExampleData/ExampleData.txt", {"ExampleFormat2", "Data"}] // Grid
     = 0.629452   0.586355
     .
     . 0.711009   0.687453
     .
     . 0.24654    0.433973
     .
     . 0.926871   0.887255
     .
     . 0.825141   0.9409
     .
     . 0.847035   0.127464
     .
     . 0.054348   0.296494
     .
     . 0.838545   0.247025
     .
     . 0.838697   0.43622
     .
     . 0.309496   0.833591

    """

    attributes = ('Protected', 'ReadProtected')

    # XXX OptionsIssue
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

    rules = {
        'RegisterImport[formatname_String, function_]':
        'RegisterImport[formatname, function, {}]',
    }

    def apply(self, formatname, function, posts, evaluation, options):
        '''RegisterImport[formatname_String, function_, posts_,
               OptionsPattern[RegisterImport]]'''

        if function.has_form('List', None):
            leaves = function.get_leaves()
        else:
            leaves = [function]

        if not (len(leaves) >= 1 and isinstance(leaves[-1], Symbol) and
                all(x.has_form('RuleDelayed', None) for x in leaves[:-1])):
            # TODO: Message
            return Symbol('$Failed')

        conditionals = {
            elem.get_string_value(): expr for (elem, expr) in
            (x.get_leaves() for x in leaves[:-1])}
        default = leaves[-1]
        posts = {}

        IMPORTERS[formatname.get_string_value()] = (conditionals, default, posts, options)

        return Symbol('Null')


# FIXME This should be private, ImportExport`RegisterExport
class RegisterExport(Builtin):
    """
    <dl>
    <dt>'RegisterExport["$format$", $func$]'
      <dd>register '$func$' as the default function used when exporting from a file of type '"$format$"'.
    </dl>

    Simple text exporter
    >> ExampleExporter1[filename_, data_, opts___] := Module[{strm = OpenWrite[filename], char = data}, WriteString[strm, char]; Close[strm]]

    >> RegisterExport["ExampleFormat1", ExampleExporter1]

    >> Export["sample.txt", "Encode this string!", "ExampleFormat1"];

    >> FilePrint["sample.txt"]
     | Encode this string!

    #> DeleteFile["sample.txt"]

    Very basic encrypted text exporter
    >> ExampleExporter2[filename_, data_, opts___] := Module[{strm = OpenWrite[filename], char}, (* TODO: Check data *) char = FromCharacterCode[Mod[ToCharacterCode[data] - 84, 26] + 97]; WriteString[strm, char]; Close[strm]]

    >> RegisterExport["ExampleFormat2", ExampleExporter2]

    >> Export["sample.txt", "encodethisstring", "ExampleFormat2"];

    >> FilePrint["sample.txt"]
     | rapbqrguvffgevat

    #> DeleteFile["sample.txt"]
    """

    options = {
        'Path': 'Automatic',
        'FunctionChannels': '{"FileNames"}',
        'Sources': 'None',
        'DefaultElement': 'None',
        'AvailableElements': 'None',
        'Options': '{}',
        'OriginalChannel': 'False',
        'BinaryFormat': 'False',
        'Encoding': 'False',
        'Extensions': '{}',
        'AlphaChannel': 'False',
    }

    def apply(self, formatname, function, evaluation, options):
        'RegisterExport[formatname_String, function_, OptionsPattern[RegisterExport]]'
        EXPORTERS[formatname.get_string_value()] = function

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

    #> Import["ExampleData/ExampleData.tx"]
     : File not found during Import.
     = $Failed
    #> Import[x]
     : First argument x is not a valid file, directory, or URL specification.
     = $Failed

    ## CSV
    #> Import["ExampleData/numberdata.csv", "Elements"]
     = {Data, Grid}
    #> Import["ExampleData/numberdata.csv", "Data"]
    = {{0.88, 0.60, 0.94}, {0.76, 0.19, 0.51}, {0.97, 0.04, 0.26}, {0.33, 0.74, 0.79}, {0.42, 0.64, 0.56}}
    #> Import["ExampleData/numberdata.csv"]
    = {{0.88, 0.60, 0.94}, {0.76, 0.19, 0.51}, {0.97, 0.04, 0.26}, {0.33, 0.74, 0.79}, {0.42, 0.64, 0.56}}

    ## Text
    >> Import["ExampleData/ExampleData.txt", "Elements"]
     = {Data, Lines, Plaintext, String, Words}
    >> Import["ExampleData/ExampleData.txt", "Lines"]
     = ...

    ## JSON
    >> Import["ExampleData/colors.json"]
     = {colorsArray -> {{colorName -> black, rgbValue -> (0, 0, 0), hexValue -> #000000}, {colorName -> red, rgbValue -> (255, 0, 0), hexValue -> #FF0000}, {colorName -> green, rgbValue -> (0, 255, 0), hexValue -> #00FF00}, {colorName -> blue, rgbValue -> (0, 0, 255), hexValue -> #0000FF}, {colorName -> yellow, rgbValue -> (255, 255, 0), hexValue -> #FFFF00}, {colorName -> cyan, rgbValue -> (0, 255, 255), hexValue -> #00FFFF}, {colorName -> magenta, rgbValue -> (255, 0, 255), hexValue -> #FF00FF}, {colorName -> white, rgbValue -> (255, 255, 255), hexValue -> #FFFFFF}}}
    """

    # TODO: Images tests
    """
    >> Import["ExampleData/sunflowers.jpg"]
     = -Image-
    """

    messages = {
        'nffil': 'File not found during Import.',
        'chtype': ('First argument `1` is not a valid file, directory, '
                   'or URL specification.'),
        'noelem': (
            'The Import element `1` is not present when importing as `2`.'),
        'fmtnosup': '`1` is not a supported Import format.',
    }

    rules = {
        'Import[filename_]': 'Import[filename, {}]',
    }

    def apply(self, filename, elements, evaluation):
        'Import[filename_, elements_]'

        # Check filename
        path = filename.to_python()
        if not (isinstance(path, six.string_types) and path[0] == path[-1] == '"'):
            evaluation.message('Import', 'chtype', filename)
            return Symbol('$Failed')

        findfile = Expression('FindFile', filename).evaluate(evaluation)
        if findfile == Symbol('$Failed'):
            evaluation.message('Import', 'nffil')
            return findfile

        # Check elements
        if elements.has_form('List', None):
            elements = elements.get_leaves()
        else:
            elements = [elements]

        for el in elements:
            if not isinstance(el, String):
                evaluation.message('Import', 'noelem', el)
                return Symbol('$Failed')

        elements = [el.get_string_value() for el in elements]

        # Determine file type
        for el in elements:
            if el in IMPORTERS.keys():
                filetype = el
                elements.remove(el)
                break
        else:
            filetype = Expression('FileFormat', findfile).evaluate(
                evaluation=evaluation).get_string_value()

        if filetype not in IMPORTERS.keys():
            evaluation.message('Import', 'fmtnosup', filetype)
            return Symbol('$Failed')

        # Load the importer
        (conditionals, default_function, posts, importer_options) = IMPORTERS[filetype]

        function_channels = importer_options.get("System`FunctionChannels")
        if function_channels is None:
            # TODO message
            return Symbol('$Failed')

        default_element = importer_options.get("System`DefaultElement")
        if default_element is None:
            # TODO message
            return Symbol('$Failed')

        def get_results(tmp_function):
            if function_channels == Expression('List', String('FileNames')):
                tmp = Expression(tmp_function, findfile).evaluate(evaluation)
            elif function_channels == Expression('List', String('Streams')):
                stream = Expression('OpenRead', findfile).evaluate(evaluation)
                if stream.get_head_name() != 'System`InputStream':
                    evaluation.message('Import', 'nffil')
                    return None
                tmp = Expression(tmp_function, stream).evaluate(evaluation)
                Expression('Close', stream).evaluate(evaluation)
            else:
                # TODO message
                return Symbol('$Failed')
            tmp = tmp.get_leaves()
            if not all(expr.has_form('Rule', None) for expr in tmp):
                return None

            # return {a.get_string_value() : b for (a,b) in map(lambda x:
            # x.get_leaves(), tmp)}
            return dict((a.get_string_value(), b)
                        for (a, b) in [x.get_leaves() for x in tmp])

        # Perform the import
        defaults = None

        if elements == []:
            defaults = get_results(default_function)
            if defaults is None:
                return Symbol('$Failed')
            if default_element == Symbol("Automatic"):
                return Expression('List', *(
                    Expression('Rule', String(key), defaults[key])
                    for key in defaults.keys()))
            else:
                result = defaults.get(default_element.get_string_value())
                if result is None:
                    evaluation.message('Import', 'noelem', default_element,
                                       from_python(filetype))
                    return Symbol('$Failed')
                return result
        else:
            assert len(elements) == 1
            el = elements[0]
            if el == "Elements":
                defaults = get_results(default_function)
                if defaults is None:
                    return Symbol('$Failed')
                # Use set() to remove duplicates
                return from_python(sorted(set(
                    list(conditionals.keys()) + list(defaults.keys()) + list(posts.keys()))))
            else:
                if el in conditionals.keys():
                    result = get_results(conditionals[el])
                    if result is None:
                        return Symbol('$Failed')
                    if len(list(result.keys())) == 1 and list(result.keys())[0] == el:
                        return list(result.values())[0]
                elif el in posts.keys():
                    # TODO: allow use of conditionals
                    result = get_results(posts[el])
                    if result is None:
                        return Symbol('$Failed')
                else:
                    if defaults is None:
                        defaults = get_results(default_function)
                        if defaults is None:
                            return Symbol('$Failed')
                    if el in defaults.keys():
                        return defaults[el]
                    else:
                        evaluation.message('Import', 'noelem', from_python(el),
                                           from_python(filetype))
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

    ## Invalid Filename
    #> Export["abc.", 1+2]
     : Cannot infer format of file abc..
     = $Failed
    #> Export[".ext", 1+2]
     : Cannot infer format of file .ext.
     = $Failed
    #> Export[x, 1+2]
     : First argument x is not a valid file specification.
     = $Failed

    ## Explicit Format
    #> Export["abc.txt", 1+x, "JPF"]
     : {JPF} is not a valid set of export elements for the Text format.
     = $Failed
    #> Export["abc.txt", 1+x, {"JPF"}]
     : {JPF} is not a valid set of export elements for the Text format.
     = $Failed

    ## Empty elems
    #> Export["123.txt", 1+x, {}]
     = 123.txt
    #> Export["123.jcp", 1+x, {}]
     : Cannot infer format of file 123.jcp.
     = $Failed

    ## Compression
    ## #> Export["abc.txt", 1+x, "ZIP"]    (* MMA Bug - Export::type *)
    ##  : {ZIP} is not a valid set of export elements for the Text format.
    ##  = $Failed
    ## #> Export["abc.txt", 1+x, "BZIP"]   (* MMA Bug - General::stop *)
    ##  : {BZIP} is not a valid set of export elements for the Text format.
    ##  = $Failed
    ## #> Export["abc.txt", 1+x, {"BZIP", "ZIP", "Text"}]
    ##  = abc.txt
    ## #> Export["abc.txt", 1+x, {"GZIP", "Text"}]
    ##  = abc.txt
    ## #> Export["abc.txt", 1+x, {"BZIP2", "Text"}]
    ##  = abc.txt

    ## FORMATS

    ## Text
    #> Export["abc.txt", 1 + x + y]
     = abc.txt
    #> FilePrint[%]
     | 1 + x + y
    #> DeleteFile[%%]

    ## CSV
    #> Export["abc.csv", {{1, 2, 3}, {4, 5, 6}}]
     = abc.csv
    #> FilePrint[%]
     | 1,2,3
     | 4,5,6
    #> DeleteFile[%%]

    ## SVG
    #> Export["sine.svg", Plot[Sin[x], {x,0,1}]]
     = sine.svg
    #> FileFormat[%]
     = SVG
    #> DeleteFile[%%]
    """

    messages = {
        'chtype': "First argument `1` is not a valid file specification.",
        'infer': "Cannot infer format of file `1`.",
        'noelem': "`1` is not a valid set of export elements for the `2` format.",
    }

    _extdict = {
        'bmp': 'BMP',
        'gif': 'GIF',
        'jp2': 'JPEG2000',
        'jpg': 'JPEG',
        'pcx': 'PCX',
        'png': 'PNG',
        'ppm': 'PPM',
        'pbm': 'PBM',
        'pgm': 'PGM',
        'tif': 'TIFF',
        'txt': 'Text',
        'csv': 'CSV',
        'svg': 'SVG',
    }

    rules = {
        'Export[filename_, expr_, elems_?NotListQ]': (
            'Export[filename, expr, {elems}]'),
    }

    def apply_noelems(self, filename, expr, evaluation):
        "Export[filename_, expr_]"

        # Check filename
        if not self._check_filename(filename, evaluation):
            return Symbol('$Failed')

        # Determine Format
        form = self._infer_form(filename, evaluation)

        if form is None:
            evaluation.message('Export', 'infer', filename)
            return Symbol('$Failed')
        else:
            return self.apply(filename, expr, String(form), evaluation)

    def apply(self, filename, expr, elems, evaluation):
        "Export[filename_, expr_, elems_List]"

        # Check filename
        if not self._check_filename(filename, evaluation):
            return Symbol('$Failed')

        # Process elems {comp* format?, elem1*}
        leaves = elems.get_leaves()

        format_spec, elems_spec = [], []
        found_form = False
        for leaf in leaves[::-1]:
            leaf_str = leaf.get_string_value()

            if not found_form and leaf_str in EXPORTERS:
                found_form = True

            if found_form:
                format_spec.append(leaf_str)
            else:
                elems_spec.append(leaf)

        # Infer format if not present
        if not found_form:
            assert format_spec == []
            format_spec = self._infer_form(filename, evaluation)
            if format_spec is None:
                evaluation.message('Export', 'infer', filename)
                return Symbol('$Failed')
            format_spec = [format_spec]
        else:
            assert format_spec != []

        # First item in format_spec is the explicit format.
        # The other elements (if present) are compression formats

        if elems_spec != []:        # FIXME: support elems
            evaluation.message(
                'Export', 'noelem', elems, String(format_spec[0]))
            return Symbol('$Failed')

        # Load the exporter
        exporter_symbol = EXPORTERS[format_spec[0]]

        exporter_function = Expression(exporter_symbol, filename, expr)

        if exporter_function.evaluate(evaluation) == Symbol('Null'):
            return filename
        return Symbol('$Failed')

    def _check_filename(self, filename, evaluation):
        path = filename.to_python()
        if isinstance(path, six.string_types) and path[0] == path[-1] == '"':
            return True
        evaluation.message('Export', 'chtype', filename)
        return False

    def _infer_form(self, filename, evaluation):
        ext = Expression('FileExtension', filename).evaluate(evaluation)
        ext = ext.get_string_value()
        return self._extdict.get(ext)


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

    #> FileFormat["ExampleData/numberdata.csv"]
     = CSV

    #> FileFormat["ExampleData/EinsteinSzilLetter.txt"]
     = Text

    #> FileFormat["ExampleData/BloodToilTearsSweat.txt"]
     = Text

    #> FileFormat["ExampleData/benzene.xyz"]
     = XYZ

    #> FileFormat["ExampleData/colors.json"]
     = JSON

    #> FileFormat["ExampleData/some-typo.extension"]
     : File not found during FileFormat[ExampleData/some-typo.extension].
     = $Failed

    #> FileFormat["ExampleData/Testosterone.svg"]
     = SVG
    """

    # TODO: JSON example file
    """
    #> FileFormat["ExampleData/example.json"]
     = JSON
    """

    messages = {
        'nffil': 'File not found during `1`.',
    }

    detector = None

    def apply(self, filename, evaluation):
        'FileFormat[filename_String]'

        findfile = Expression('FindFile', filename).evaluate(evaluation)
        if findfile == Symbol('$Failed'):
            evaluation.message(
                'FileFormat', 'nffil', Expression('FileFormat', filename))
            return findfile

        path = findfile.get_string_value()

        if not FileFormat.detector:
            loader = magic.MagicLoader()
            loader.load()
            FileFormat.detector = magic.MagicDetector(loader.mimetypes)

        mime = set(FileFormat.detector.match(path))

        # If match fails match on extension only
        if mime == set([]):
            mime, encoding = mimetypes.guess_type(path)
            if mime is None:
                mime = set([])
            else:
                mime = set([mime])

        # TODO: Add more file formats

        typedict = {
            'application/dicom': 'DICOM',
            'application/dbase': 'DBF',
            'application/dbf': 'DBF',
            'application/eps': 'EPS',
            'application/fits': 'FITS',
            'application/json': 'JSON',
            'application/mathematica': 'NB',
            'application/mdb': 'MDB',
            'application/mbox': 'MBOX',
            'application/msaccess': 'MDB',
            'application/octet-stream': 'OBJ',
            'application/pdf': 'PDF',
            'application/pcx': 'PCX',
            'application/postscript': 'EPS',
            'application/rss+xml': 'RSS',
            'application/rtf': 'RTF',
            'application/sla': 'STL',
            'application/tga': 'TGA',
            'application/vnd.google-earth.kml+xml': 'KML',
            'application/vnd.ms-excel': 'XLS',
            'application/vnd.ms-pki.stl': 'STL',
            'application/vnd.oasis.opendocument.spreadsheet': 'ODS',
            'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet': 'XLSX',    # nopep8
            'application/vnd.sun.xml.calc': 'SXC',
            'application/vnd.msaccess': 'MDB',
            'application/vnd.wolfram.cdf': 'CDF',
            'application/vnd.wolfram.cdf.text': 'CDF',
            'application/vnd.wolfram.mathematica.package': 'Package',
            'application/xhtml+xml': 'XHTML',
            'application/xml': 'XML',
            'application/x-3ds': '3DS',
            'application/x-cdf': 'NASACDF',
            'application/x-eps': 'EPS',
            'application/x-flac': 'FLAC',
            'application/x-font-bdf': 'BDF',
            'application/x-hdf': 'HDF',
            'application/x-msaccess': 'MDB',
            'application/x-netcdf': 'NetCDF',
            'application/x-shockwave-flash': 'SWF',
            'application/x-tex': 'TeX',  # Also TeX
            'audio/aiff': 'AIFF',
            'audio/basic': 'AU',        # Also SND
            'audio/midi': 'MIDI',
            'audio/x-aifc': 'AIFF',
            'audio/x-aiff': 'AIFF',
            'audio/x-flac': 'FLAC',
            'audio/x-wav': 'WAV',
            'chemical/seq-na-genbank': 'GenBank',
            'chemical/seq-aa-fasta': 'FASTA',
            'chemical/seq-na-fasta': 'FASTA',
            'chemical/seq-na-fastq': 'FASTQ',
            'chemical/seq-na-sff': 'SFF',
            'chemical/x-cif': 'CIF',
            'chemical/x-daylight-smiles': 'SMILES',
            'chemical/x-hin': 'HIN',
            'chemical/x-jcamp-dx': 'JCAMP-DX',
            'chemical/x-mdl-molfile': 'MOL',
            'chemical/x-mdl-sdf': 'SDF',
            'chemical/x-mdl-sdfile': 'SDF',
            'chemical/x-mdl-tgf': 'TGF',
            'chemical/x-mmcif': 'CIF',
            'chemical/x-mol2': 'MOL2',
            'chemical/x-mopac-input': 'Table',
            'chemical/x-pdb': 'PDB',
            'chemical/x-xyz': 'XYZ',
            'image/bmp': 'BMP',
            'image/eps': 'EPS',
            'image/fits': 'FITS',
            'image/gif': 'GIF',
            'image/jp2': 'JPEG2000',
            'image/jpeg': 'JPEG',
            'image/pbm': 'PNM',
            'image/pcx': 'PCX',
            'image/pict': 'PICT',
            'image/png': 'PNG',
            'image/svg+xml': 'SVG',
            'image/tga': 'TGA',
            'image/tiff': 'TIFF',
            'image/vnd.dxf': 'DXF',
            'image/vnd.microsoft.icon': 'ICO',
            'image/x-3ds': '3DS',
            'image/x-dxf': 'DXF',
            'image/x-exr': 'OpenEXR',
            'image/x-icon': 'ICO',
            'image/x-ms-bmp': 'BMP',
            'image/x-pcx': 'PCX',
            'image/x-portable-anymap': 'PNM',
            'image/x-portable-bitmap': 'PBM',
            'image/x-portable-graymap': 'PGM',
            'image/x-portable-pixmap': 'PPM',
            'image/x-xbitmap': 'XBM',
            'model/x3d+xml': 'X3D',
            'model/vrml': 'VRML',
            'model/x-lwo': 'LWO',
            'model/x-pov': 'POV',
            'text/calendar': 'ICS',
            'text/comma-separated-values': 'CSV',
            'text/csv': 'CSV',
            'text/html': 'HTML',
            'text/mathml': 'MathML',
            'text/plain': 'Text',
            'text/rtf': 'RTF',
            'text/scriptlet': 'SCT',
            'text/tab-separated-values': 'TSV',
            'text/texmacs': 'Text',
            'text/vnd.graphviz': 'DOT',
            'text/x-csrc': 'C',
            'text/x-tex': 'TeX',
            'text/x-vcalendar': 'VCS',
            'text/x-vcard': 'VCF',
            'video/avi': 'AVI',
            'video/quicktime': 'QuickTime',
            'video/x-flv': 'FLV',
            # None: 'Binary',
        }

        result = []
        for key in typedict.keys():
            if key in mime:
                result.append(typedict[key])

        if len(result) == 0:
            result = 'Binary'
        elif len(result) == 1:
            result = result[0]
        else:
            return None

        return from_python(result)
