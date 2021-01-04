#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Importing and Exporting
"""

from mathics.core.expression import Expression, from_python, strip_context, Symbol, SymbolFailed
from mathics.builtin.base import Builtin, Predefined, String, Integer, get_option
from mathics.builtin.options import options_to_rules

from .pymimesniffer import magic
import mimetypes
import sys
from itertools import chain

import urllib

try:
    import urllib.request as urllib2
    from urllib.error import HTTPError, URLError
except ImportError:
    import urllib2
    from urllib2 import HTTPError, URLError

mimetypes.add_type('application/vnd.wolfram.mathematica.package', '.m')

# Seems that JSON is not registered on the mathics.net server, so we do it manually here.
# Keep in mind that mimetypes has system-dependent aspects (it inspects "/etc/mime.types" and other files).
mimetypes.add_type('application/json', '.json')

# TODO: Add more file formats

mimetype_dict = {
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
    'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet': 'XLSX',  # nopep8
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
    'audio/basic': 'AU',  # Also SND
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
    'text/xml': 'XML',
    'video/avi': 'AVI',
    'video/quicktime': 'QuickTime',
    'video/x-flv': 'FLV',
    # None: 'Binary',
}

IMPORTERS = {}
EXPORTERS = {}
EXTENSIONMAPPINGS = {"*.3ds" : "3DS", "*.aco" : "ACO", "*.aif" : "AIFF", "*.aiff" : "AIFF", "*.au" : "AU", "*.avi" : "AVI", "*.b64" : "BASE64", "*.bdf" : "BDF", "*.bmp" : "BMP", "*.dib" : "BMP", "*.bson" : "BSON", "*.byu" : "BYU", "*.bz2" : "BZIP2", "*.c" : "C", "*.cdf" : "CDF", "*.cif" : "CIF", "*.col" : "DIMACS", "*.col.b" : "DIMACS", "*.csv" : "CSV", "*.css" : "CSS", "*.cur" : "CUR", "*.dae" : "DAE", "*.dat" : "Table", "*.dcm" : "DICOM", "*.dic" : "DICOM", "*.dicm" : "DICOM", "*.dif" : "DIF", "*.dot" : "DOT", "*.dxf" : "DXF", "*.edf" : "EDF", "*.emf" : "EMF", "*.eml" : "EML", "*.enc" : "UUE", "*.ent" : "PDB", "*.eps" : "EPS", "*.epsf" : "EPS", "*.epsi" : "EPS", "*.fcs" : "FCS", "*.fsa" : "FASTA", "*.fasta" : "FASTA", "*.fa" : "FASTA", "*.mpfa" : "FASTA", "*.fq" : "FASTQ", "*.fastq" : "FASTQ", "*.fit" : "FITS", "*.fits" : "FITS", "*.flac" : "FLAC", "*.flv" : "FLV", "*.fmu" : "FMU", "*.g6" : "Graph6", "*.gif" : "GIF", "*.gml" : "Graphlet", "*.grd" : "SurferGrid", "*.grib" : "GRIB", "*.grb" : "GRIB", "*.gv" : "DOT", "*.gw" : "LEDA", "*.gxl" : "GXL", "*.graphml" : "GraphML", "*.gz" : "GZIP", "*.hdf" : "HDF", "*.hmm" : "HMMER", "*.htm" : "HTML", "*.html" : "HTML", "*.sds" : "HDF", "*.h5" : "HDF5", "*.icc" : "ICC", "*.icm" : "ICC", "*.icns" : "ICNS", "*.ico" : "ICO", "*.ics" : "ICS", "*.ini" : "INI", "*.jar" : "ZIP", "*.jp2" : "JPEG2000", "*.j2k" : "JPEG2000", "*.jpc" : "JPEG2000", "*.jpg" : "JPEG", "*.jpeg" : "JPEG", "*.jfif" : "JPEG", "*.jvx" : "JVX", "*.kml" : "KML", "*.kmz" : "KML", "*.lgr" : "LEDA", "*.lmd" : "FCS", "*.lwo" : "LWO", "*.m" : "Package", "*.m4a" : "M4A", "*.aac" : "M4A", "*.ma" : "Maya", "*.mat" : "MAT", "*.mbx" : "MBOX", "*.mbox" : "MBOX", "*.mesh" : "MESH", "*.mgf" : "MGF", "*.mid" : "MIDI", "*.mml" : "MathML", "*.mo" : "MO", "*.mol" : "MOL", "*.mol2" : "MOL2", "*.mov" : "QuickTime", "*.mp3" : "MP3", "*.mtx" : "MTX", "*.mulaw" : "AU", "*.mx" : "MX", "*.nb" : "NB", "*.nc" : "NETCDF", "*.ndk" : "NDK", "*.net" : "PAJEK", "*.nex" : "NEXUS", "*.noff" : "NOFF", "*.nxs" : "NEXUS", "*.obj" : "OBJ", "*.ods" : "ODS", "*.off" : "OFF", "*.oga" : "OGG", "*.ogg" : "OGG", "*.pcx" : "PCX", "*.pbm" : "PBM", "*.pgm" : "PGM", "*.ppm" : "PPM", "*.pnm" : "PNM", "*.png" : "PNG", "*.pdb" : "PDB", "*.pdf" : "PDF", "*.pic" : "PXR", "*.pic" : "PICT", "*.pict" : "PICT", "*.pct" : "PICT", "*.ply" : "PLY", "*.pov" : "POV", "*.properties" : "JavaProperties", "*.pxr" : "PXR", "*.qt" : "QuickTime", "*.raw" : "RawBitmap", "*.rib" : "RIB", "*.rtf" : "RTF", "*.sdf" : "SDF", "*.sct" : "SCT", "*.ch" : "SCT", "*.ct" : "SCT", "*.sff" : "SFF", "*.sp3" : "SP3", "*.stl" : "STL", "*.s6" : "Sparse6", "*.sma" : "SMA", "*.sme" : "SME", "*.smi" : "SMILES", "*.snd" : "SND", "*.svg" : "SVG", "*.svgz" : "SVGZ", "*.swf" : "SWF", "*.tar" : "TAR", "*.tex" : "TeX", "*.tga" : "TGA", "*.tgf" : "TGF", "*.tgz" : "GZIP", "*.tff" : "TIFF", "*.tif" : "TIFF", "*.tiff" : "TIFF", "*.tsv" : "TSV", "*.txt" : "Text", "*.uue" : "UUE", "*.w64" : "Wave64", "*.wav" : "WAV", "*.webp" : "WebP", "*.wdx" : "WDX", "*.wl" : "Package", "*.wls" : "Package", "*.wlnet" : "WLNet", "*.wmf" : "WMF", "*.wmlf" : "WMLF", "*.html" : "XHTML", "*.htm" : "XHTML", "*.xhtml" : "XHTML", "*.xbm" : "XBM", "*.xht" : "XHTML", "*.xml" : "XML", "*.xml" : "ExpressionML", "*.xml" : "XHTML", "*.xml" : "XHTMLMathML", "*.xls" : "XLS", "*.xlsx" : "XLSX", "*.wrl" : "VRML", "*.wxf" : "WXF", "*.vtk" : "VTK", "*.x3d" : "X3D", "*.xyz" : "XYZ", "*.zip" : "ZIP", "*.zpr" : "ZPR", "*.cha" : "HarwellBoeing", "*.che" : "HarwellBoeing", "*.cra" : "HarwellBoeing", "*.cre" : "HarwellBoeing", "*.csa" : "HarwellBoeing", "*.cse" : "HarwellBoeing", "*.cua" : "HarwellBoeing", "*.cue" : "HarwellBoeing", "*.cza" : "HarwellBoeing", "*.cze" : "HarwellBoeing", "*.pha" : "HarwellBoeing", "*.phe" : "HarwellBoeing", "*.pra" : "HarwellBoeing", "*.pre" : "HarwellBoeing", "*.psa" : "HarwellBoeing", "*.pse" : "HarwellBoeing", "*.pua" : "HarwellBoeing", "*.pue" : "HarwellBoeing", "*.pza" : "HarwellBoeing", "*.pze" : "HarwellBoeing", "*.rha" : "HarwellBoeing", "*.rhe" : "HarwellBoeing", "*.rra" : "HarwellBoeing", "*.rre" : "HarwellBoeing", "*.rsa" : "HarwellBoeing", "*.rse" : "HarwellBoeing", "*.rua" : "HarwellBoeing", "*.rue" : "HarwellBoeing", "*.rza" : "HarwellBoeing", "*.rze" : "HarwellBoeing", "*.json" : "JSON", "*.ubj" : "UBJSON", "*.geojson" : "GeoJSON", "*.bay" : "Raw", "*.bmq" : "Raw", "*.cr2" : "Raw", "*.crw" : "Raw", "*.cs1" : "Raw", "*.dc2" : "Raw", "*.dcr" : "Raw", "*.dng" : "Raw", "*.erf" : "Raw", "*.fff" : "Raw", "*.hdr" : "Raw", "*.k25" : "Raw", "*.kdc" : "Raw", "*.mdc" : "Raw", "*.mos" : "Raw", "*.mrw" : "Raw", "*.nef" : "Raw", "*.orf" : "Raw", "*.pef" : "Raw", "*.pxn" : "Raw", "*.raf" : "Raw", "*.raw" : "Raw", "*.rdc" : "Raw", "*.sr2" : "Raw", "*.srf" : "Raw", "*.x3f" : "Raw", "*.arw" : "Raw", "*.3fr" : "Raw", "*.cine" : "Raw", "*.ia" : "Raw", "*.kc2" : "Raw", "*.mef" : "Raw", "*.nrw" : "Raw", "*.qtk" : "Raw", "*.rw2" : "Raw", "*.sti" : "Raw", "*.rwl" : "Raw", "*.rle" : "RLE", "*.tcx" : "TECHEXPLORER", "*.tcx" : "TCX", "*.css" : "CSS"}


FORMATMAPPINGS = {"Agilent" : "AgilentMicroarray", "BZIP" : "BZIP2", "BZ2" : "BZIP2", "Excel" : "XLS", "MatrixMarket" : "MTX", "GraphWin" : "LEDA", "GZ" : "GZIP", "TGZ" : "GZIP", "vCard" : "VCF", "Metafile" : "WMF", "JPG" : "JPEG", "JCAMPDX" : "JCAMP-DX", "WAVE" : "WAV", "AIFC" : "AIFF", "MuLaw" : "AU", "Flash" : "SWF", "HTMLMathML" : "XHTMLMathML", "RichText" : "RTF", "JAR" : "ZIP", "WEBP" : "WebP", "RAW" : "Raw", "3DS" : "3DS", "ACO" : "ACO", "AFFYMETRIX" : "Affymetrix", "AGILENTMICROARRAY" : "AgilentMicroarray", "AIFF" : "AIFF", "APACHELOG" : "ApacheLog", "ARCGRID" : "ArcGRID", "AU" : "AU", "AVI" : "AVI", "BASE64" : "Base64", "BDF" : "BDF", "BINARY" : "Binary", "BIT" : "Bit", "BMP" : "BMP", "BSON" : "BSON", "BYTE" : "Byte", "BYU" : "BYU", "BZIP2" : "BZIP2", "CDED" : "CDED", "CDF" : "CDF", "CHARACTER16" : "Character16", "CHARACTER8" : "Character8", "CIF" : "CIF", "COMPLEX128" : "Complex128", "COMPLEX256" : "Complex256", "COMPLEX64" : "Complex64", "CSV" : "CSV", "CUR" : "CUR", "DAE" : "DAE", "DBF" : "DBF", "DICOM" : "DICOM", "DIF" : "DIF", "DIMACS" : "DIMACS", "DIRECTORY" : "Directory", "DOT" : "DOT", "DXF" : "DXF", "EDF" : "EDF", "EML" : "EML", "EPS" : "EPS", "EXPRESSIONJSON" : "ExpressionJSON", "EXPRESSIONML" : "ExpressionML", "FASTA" : "FASTA", "FASTQ" : "FASTQ", "FCS" : "FCS", "FITS" : "FITS", "FLAC" : "FLAC", "GENBANK" : "GenBank", "GEOJSON" : "GeoJSON", "GEOTIFF" : "GeoTIFF", "GIF" : "GIF", "GPX" : "GPX", "GRAPH6" : "Graph6", "GRAPHLET" : "Graphlet", "GRAPHML" : "GraphML", "GRIB" : "GRIB", "GTOPO30" : "GTOPO30", "GXL" : "GXL", "GZIP" : "GZIP", "HARWELLBOEING" : "HarwellBoeing", "HDF5" : "HDF5", "HDF" : "HDF", "HIN" : "HIN", "HTML" : "HTML", "HTTPREQUEST" : "HTTPRequest", "HTTPRESPONSE" : "HTTPResponse", "ICC" : "ICC", "ICNS" : "ICNS", "ICO" : "ICO", "ICS" : "ICS", "INI" : "Ini", "INTEGER128" : "Integer128", "INTEGER16" : "Integer16", "INTEGER24" : "Integer24", "INTEGER32" : "Integer32", "INTEGER64" : "Integer64", "INTEGER8" : "Integer8", "JAVAPROPERTIES" : "JavaProperties", "JAVASCRIPTEXPRESSION" : "JavaScriptExpression", "JCAMP-DX" : "JCAMP-DX", "JPEG2000" : "JPEG2000", "JPEG" : "JPEG", "JSON" : "JSON", "JVX" : "JVX", "KML" : "KML", "LATEX" : "LaTeX", "LEDA" : "LEDA", "LIST" : "List", "LWO" : "LWO", "M4A" : "M4A", "MATHML" : "MathML", "MAT" : "MAT", "MBOX" : "MBOX", "MCTT" : "MCTT", "MDB" : "MDB", "MESH" : "MESH", "MGF" : "MGF", "MIDI" : "MIDI", "MMCIF" : "MMCIF", "MO" : "MO", "MOL2" : "MOL2", "MOL" : "MOL", "MP3" : "MP3", "MPS" : "MPS", "MTP" : "MTP", "MTX" : "MTX", "MX" : "MX", "MXNET" : "MXNet", "NASACDF" : "NASACDF", "NB" : "NB", "NDK" : "NDK", "NETCDF" : "NetCDF", "NEXUS" : "NEXUS", "NOFF" : "NOFF", "OBJ" : "OBJ", "ODS" : "ODS", "OFF" : "OFF", "OGG" : "OGG", "OPENEXR" : "OpenEXR", "PACKAGE" : "Package", "PAJEK" : "Pajek", "PBM" : "PBM", "PCAP" : "PCAP", "PCX" : "PCX", "PDB" : "PDB", "PDF" : "PDF", "PGM" : "PGM", "PHPINI" : "PHPIni", "PLY" : "PLY", "PNG" : "PNG", "PNM" : "PNM", "PPM" : "PPM", "PXR" : "PXR", "PYTHONEXPRESSION" : "PythonExpression", "QUICKTIME" : "QuickTime", "RAWBITMAP" : "RawBitmap", "RAW" : "Raw", "RAWJSON" : "RawJSON", "REAL128" : "Real128", "REAL32" : "Real32", "REAL64" : "Real64", "RIB" : "RIB", "RLE" : "RLE", "RSS" : "RSS", "RTF" : "RTF", "SCT" : "SCT", "SDF" : "SDF", "SDTSDEM" : "SDTSDEM", "SDTS" : "SDTS", "SFF" : "SFF", "SHP" : "SHP", "SMA" : "SMA", "SME" : "SME", "SMILES" : "SMILES", "SND" : "SND", "SP3" : "SP3", "SPARSE6" : "Sparse6", "STL" : "STL", "STRING" : "String", "SURFERGRID" : "SurferGrid", "SXC" : "SXC", "TABLE" : "Table", "TAR" : "TAR", "TERMINATEDSTRING" : "TerminatedString", "TEX" : "TeX", "TEXT" : "Text", "TGA" : "TGA", "TGF" : "TGF", "TIFF" : "TIFF", "TIGER" : "TIGER", "TLE" : "TLE", "TSV" : "TSV", "UBJSON" : "UBJSON", "UNSIGNEDINTEGER128" : "UnsignedInteger128", "UNSIGNEDINTEGER16" : "UnsignedInteger16", "UNSIGNEDINTEGER24" : "UnsignedInteger24", "UNSIGNEDINTEGER32" : "UnsignedInteger32", "UNSIGNEDINTEGER64" : "UnsignedInteger64", "UNSIGNEDINTEGER8" : "UnsignedInteger8", "USGSDEM" : "USGSDEM", "UUE" : "UUE", "VCF" : "VCF", "VCS" : "VCS", "VTK" : "VTK", "WARC" : "WARC", "WAVE64" : "Wave64", "WAV" : "WAV", "WDX" : "WDX", "WEBP" : "WebP", "WLNET" : "WLNet", "WMLF" : "WMLF", "WXF" : "WXF", "XBM" : "XBM", "XHTML" : "XHTML", "XHTMLMATHML" : "XHTMLMathML", "XLS" : "XLS", "XLSX" : "XLSX", "XML" : "XML", "XPORT" : "XPORT", "XYZ" : "XYZ", "ZIP" : "ZIP", "C" : "C", "EMF" : "EMF", "FLV" : "FLV", "FMU" : "FMU", "HTMLFRAGMENT" : "HTMLFragment", "MAYA" : "Maya", "PICT" : "PICT", "POV" : "POV", "SVG" : "SVG", "SWF" : "SWF", "TEXFRAGMENT" : "TeXFragment", "VIDEOFRAMES" : "VideoFrames", "VRML" : "VRML", "WMF" : "WMF", "X3D" : "X3D", "ZPR" : "ZPR", "AUDIO/AIFF" : "AIFF", "AUDIO/X-AIFF" : "AIFF", "AUDIO/BASIC" : "AU", "AUDIO/X-AU" : "AU", "AUDIO/X-ULAW" : "AU", "APPLICATION/X-TROFF-MSVIDEO" : "AVI", "VIDEO/AVI" : "AVI", "VIDEO/MSVIDEO" : "AVI", "VIDEO/X-MSVIDEO" : "AVI", "APPLICATION/BMP" : "BMP", "APPLICATION/X-BMP" : "BMP", "APPLICATION/X-WIN-BITMAP" : "BMP", "IMAGE/BITMAP" : "BMP", "IMAGE/BMP" : "BMP", "IMAGE/MS-BMP" : "BMP", "IMAGE/X-BITMAP" : "BMP", "IMAGE/X-BMP" : "BMP", "IMAGE/X-MS-BMP" : "BMP", "IMAGE/X-WIN-BITMAP" : "BMP", "IMAGE/X-WINDOWS-BITMAP" : "BMP", "APPLICATION/X-BZIP" : "BZIP2", "BZ2" : "BZIP2", "BZIP" : "BZIP2", "APPLICATION/VND.WOLFRAM.CDF.TEXT" : "CDF", "APPLICATION/DICOM" : "DICOM", "APPLICATION/ACAD" : "DXF", "APPLICATION/DXF" : "DXF", "APPLICATION/X-AUTOCAD" : "DXF", "APPLICATION/X-DXF" : "DXF", "IMAGE/DXF" : "DXF", "IMAGE/VND.DXF" : "DXF", "IMAGE/X-AUTOCAD" : "DXF", "IMAGE/X-DXF" : "DXF", "ZZ-APPLICATION/ZZ-WINASSOC-DXF" : "DXF", "APPLICATION/EMF" : "EMF", "APPLICATION/X-EMF" : "EMF", "ENHANCEDMETAFILE" : "EMF", "IMAGE/X-EMF" : "EMF", "IMAGE/X-MGX-EMF" : "EMF", "IMAGE/X-XBITMAP" : "EMF", "APPLICATION/EPS" : "EPS", "APPLICATION/POSTSCRIPT" : "EPS", "APPLICATION/X-EPS" : "EPS", "IMAGE/EPS" : "EPS", "IMAGE/X-EPS" : "EPS", "APPLICATION/FITS" : "FITS", "IMAGE/FITS" : "FITS", "VIDEO/X-FLV" : "FLV", "IMAGE/GIF" : "GIF", "APPLICATION/X-HDF" : "HDF", "APPLICATION/X-HDF5" : "HDF5", "APPLICATION/JPG" : "JPEG", "APPLICATION/X-JPG" : "JPEG", "IMAGE/JPEG" : "JPEG", "IMAGE/JPG" : "JPEG", "IMAGE/PJPEG" : "JPEG", "IMAGE/JP2" : "JPEG2000", "IMAGE/JPEG2000" : "JPEG2000", "IMAGE/JPEG2000-IMAGE" : "JPEG2000", "IMAGE/X-JPEG2000-IMAGE" : "JPEG2000", "AUDIO/AAC" : "M4A", "AUDIO/AACP" : "M4A", "AUDIO/3GPP" : "M4A", "AUDIO/3GPP2" : "M4A", "AUDIO/MP4" : "M4A", "AUDIO/MP4A-LATM" : "M4A", "AUDIO/MPEG4-GENERIC" : "M4A", "AUDIO/MPEG" : "MP3", "AUDIO/X-MPEG" : "MP3", "AUDIO/MP3" : "MP3", "AUDIO/X-MP3" : "MP3", "AUDIO/MPEG3" : "MP3", "AUDIO/X-MPEG3" : "MP3", "AUDIO/MPG" : "MP3", "AUDIO/X-MPG" : "MP3", "AUDIO/X-MPEGAUDIO" : "MP3", "APPLICATION/MATHEMATICA" : "NB", "APPLICATION/VND.WOLFRAM.MATHEMATICA" : "NB", "APPLICATION/VND.WOLFRAM.PLAYER" : "NB", "APPLICATION/VND.OASIS.OPENDOCUMENT.SPREADSHEET" : "ODS", "APPLICATION/X-VND.OASIS.OPENDOCUMENT.SPREADSHEET" : "ODS", "AUDIO/OGG" : "OGG", "AUDIO/VORBIS" : "OGG", "IMAGE/X-EXR" : "OpenEXR", "APPLICATION/VND.TCPDUMP.PCAP" : "PCAP", "APPLICATION/X-PCAPNG" : "PCAP", "APPLICATION/ACROBAT" : "PDF", "APPLICATION/PDF" : "PDF", "APPLICATION/VND.PDF" : "PDF", "APPLICATION/X-PDF" : "PDF", "TEXT/PDF" : "PDF", "TEXT/X-PDF" : "PDF", "APPLICATION/PNG" : "PNG", "APPLICATION/X-PNG" : "PNG", "IMAGE/PNG" : "PNG", "IMAGE/X-PNG" : "PNG", "IMAGE/X-PBM" : "PBM", "IMAGE/X-PORTABLE-BITMAP" : "PBM", "IMAGE/X-PGM" : "PGM", "IMAGE/X-PORTABLE-GRAYMAP" : "PGM", "IMAGE/X-PPM" : "PPM", "IMAGE/X-PORTABLE-PIXMAP" : "PPM", "IMAGE/X-PNM" : "PNM", "IMAGE/X-PORTABLE-ANYMAP" : "PNM", "APPLICATION/RTF" : "RTF", "APPLICATION/X-RTF" : "RTF", "RICHTEXT" : "RTF", "TEXT/RICHTEXT" : "RTF", "TEXT/RTF" : "RTF", "APPLICATION/X-SHOCKWAVE-FLASH" : "SWF", "FLASH" : "SWF", "APPLICATION/X-GZIP" : "GZIP", "APPLICATION/X-GZIP-COMPRESSED" : "GZIP", "MULTIPART/X-GZIP" : "GZIP", "APPLICATION/TAR" : "TAR", "APPLICATION/X-TAR" : "TAR", "MULTIPART/X-TAR" : "TAR", "APPLICATION/TIF" : "TIFF", "APPLICATION/TIFF" : "TIFF", "APPLICATION/X-TIF" : "TIFF", "APPLICATION/X-TIFF" : "TIFF", "IMAGE/TIF" : "TIFF", "IMAGE/TIFF" : "TIFF", "IMAGE/X-TIF" : "TIFF", "IMAGE/X-TIFF" : "TIFF", "APPLICATION/X-3DS" : "3DS", "IMAGE/X-3DS" : "3DS", "APPLICATION/VCARD" : "VCF", "TEXT/X-VCARD" : "VCF", "VCARD" : "VCF", "AUDIO/WAV" : "WAV", "AUDIO/WAVE" : "WAV", "AUDIO/X-WAV" : "WAV", "WAVE" : "WAV", "APPLICATION/WMF" : "WMF", "APPLICATION/X-MSMETAFILE" : "WMF", "APPLICATION/X-WMF" : "WMF", "IMAGE/WMF" : "WMF", "IMAGE/X-WIN-METAFILE" : "WMF", "IMAGE/X-WMF" : "WMF", "METAFILE" : "WMF", "WINDOWS/METAFILE" : "WMF", "ZZ-APPLICATION/ZZ-WINASSOC-WMF" : "WMF", "APPLICATION/EXCEL" : "XLS", "APPLICATION/MS-EXCEL" : "XLS", "APPLICATION/VND.MS-EXCEL" : "XLS", "APPLICATION/X-DOS_MS_EXCEL" : "XLS", "APPLICATION/X-EXCEL" : "XLS", "APPLICATION/X-MS-EXCEL" : "XLS", "APPLICATION/X-MSEXCEL" : "XLS", "APPLICATION/X-XLS" : "XLS", "ZZ-APPLICATION/ZZ-WINASSOC-XLS" : "XLS", "APPLICATION/EXCEL" : "XLSX", "APPLICATION/MS-EXCEL" : "XLSX", "APPLICATION/VND.MS-EXCEL" : "XLSX", "APPLICATION/X-DOS_MS_EXCEL" : "XLSX", "APPLICATION/X-EXCEL" : "XLSX", "APPLICATION/X-MS-EXCEL" : "XLSX", "APPLICATION/X-MSEXCEL" : "XLSX", "APPLICATION/X-XLS" : "XLSX", "ZZ-APPLICATION/ZZ-WINASSOC-XLS" : "XLSX", "APPLICATION/MSWORD" : "DOC", "APPLICATION/X-WINZIP" : "ZIP", "APPLICATION/X-ZIP" : "ZIP", "APPLICATION/X-ZIP-COMPRESSED" : "ZIP", "APPLICATION/ZIP" : "ZIP", "MULTIPART/X-ZIP" : "ZIP", "IMAGE/SVG-XML" : "SVG", "IMAGE/SVG+XML" : "SVG", "TEXT/CALENDAR" : "VCS", "TEXT/CALENDAR" : "ICS", "APPLICATION/TGA" : "TGA", "APPLICATION/X-TARGA" : "TGA", "APPLICATION/X-TGA" : "TGA", "IMAGE/TARGA" : "TGA", "IMAGE/TGA" : "TGA", "IMAGE/X-TARGA" : "TGA", "IMAGE/X-TGA" : "TGA", "APPLICATION/WARC" : "WARC", "TEXT/HTML" : "HTML", "APPLICATION/XHTML+XML" : "XHTML", "APPLICATION/XML" : "XML", "TEXT/XML" : "XML", "APPLICATION/X-TEX" : "TeX", "APPLICATION/CSV" : "CSV", "TEXT/COMMA-SEPARATED-VALUES" : "CSV", "TEXT/CSV" : "CSV", "TEXT/X-COMMA-SEPARATED-VALUES" : "CSV", "TEXT/TAB-SEPARATED-VALUES" : "TSV", "APPLICATION/VND.WOLFRAM.MATHEMATICA.PACKAGE" : "Package", "MESSAGE/RFC822" : "EML", "IMAGE/VND.MICROSOFT.ICON" : "ICO", "APPLICATION/JSON" : "JSON", "APPLICATION/UBJSON" : "UBJSON", "APPLICATION/GEO+JSON" : "GeoJSON", "APPLICATION/X-LATEX" : "LaTeX", "VIDEO/X-MATROSKA" : "MKV", "APPLICATION/PCX" : "PCX", "APPLICATION/X-PCX" : "PCX", "IMAGE/PCX" : "PCX", "IMAGE/X-PC-PAINTBRUCH" : "PCX", "IMAGE/X-PCX" : "PCX", "ZZ-APPLICATION/ZZ-WINASSOC-PCX" : "PCX", "IMAGE/PICT" : "PICT", "IMAGE/X-PICT" : "PICT", "MODEL/X-POV" : "POV", "VIDEO/QUICKTIME" : "QuickTime", "APPLICATION/SLA" : "STL", "XBITMAP" : "XBM", "IMAGE/XBM" : "XBM", "IMAGE/X-XBITMAP" : "XBM", "IMAGE/X-XBM" : "XBM", "APPLICATION/TXT" : "Text", "TEXT/PLAIN" : "Text"}


def _importer_exporter_options(available_options, options, builtin_name: str, evaluation):
    stream_options = []
    custom_options = []
    remaining_options = options.copy()

    if available_options and available_options.has_form('List', None):
        for name in available_options.leaves:
            if isinstance(name, String):
                py_name = name.get_string_value()
            elif isinstance(name, Symbol):
                py_name = strip_context(name.get_name())
            else:
                py_name = None

            if py_name:
                value = get_option(remaining_options, py_name, evaluation, pop=True)
                if value is not None:
                    expr = Expression('Rule', String(py_name), value)
                    if py_name == 'CharacterEncoding':
                        stream_options.append(expr)
                    else:
                        custom_options.append(expr)

    syntax_option = remaining_options.get("System`$OptionSyntax", None)
    if syntax_option and syntax_option !=  Symbol("System`Ignore"):
        # warn about unsupported options.
        for name, value in remaining_options.items():
            print(" es de acá....")
            evaluation.message(
                builtin_name,
                "optx",
                Expression('Rule', strip_context(name), value),
                strip_context(builtin_name))

    return stream_options, custom_options


class ImportFormats(Predefined):
    """
    <dl>
    <dt>'$ImportFormats'
        <dd>returns a list of file formats supported by Import.
    </dl>

    >> $ImportFormats
     = {...CSV,...JSON,...Text...}
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
     = {...CSV,...SVG,...Text...}
    """

    name = '$ExportFormats'

    def evaluate(self, evaluation):
        return Expression('List', *sorted(EXPORTERS.keys()))

class ConverterDumpsExtensionMappings(Predefined):
    """
    <dl>
    <dt>'$extensionMappings'
        <dd>Returns a list of associations between file extensions and file types. 
    </dl>
    
    """
    context = 'System`ConvertersDump`'
    name = "$extensionMappings"
    attributes = ['Unprotected']

    def evaluate(self, evaluation):
        return Expression('List',
                          Expression('List', *sorted(EXTENSIONMAPPINGS.keys())))


class ConverterDumpsFormatMappings(Predefined):
    """
    <dl>
    <dt>'$formatMappings'
        <dd>Returns a list of associations between file extensions and file types. 
    </dl>
    
    """
    context = 'System`ConvertersDump`'
    name = "$formatMappings"
    attributes = ['Unprotected']
    def evaluate(self, evaluation):
        return Expression('List', *sorted(FORMATMAPPINGS.keys()))


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
    >> ImportExport`RegisterImport["ExampleFormat1", ExampleFormat1Import]

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

    >> ImportExport`RegisterImport["ExampleFormat2", {"Data" :> ExampleFormat2DataImport, ExampleFormat2DefaultImport}]

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

    context = 'ImportExport`'

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
        'ImportExport`RegisterImport[formatname_String, function_]':
        'ImportExport`RegisterImport[formatname, function, {}]',
    }

    def apply(self, formatname, function, posts, evaluation, options):
        '''ImportExport`RegisterImport[formatname_String, function_, posts_,
                OptionsPattern[ImportExport`RegisterImport]]'''

        if function.has_form('List', None):
            leaves = function.get_leaves()
        else:
            leaves = [function]

        if not (len(leaves) >= 1 and isinstance(leaves[-1], Symbol) and
                all(x.has_form('RuleDelayed', None) for x in leaves[:-1])):
            # TODO: Message
            return SymbolFailed

        conditionals = {
            elem.get_string_value(): expr for (elem, expr) in
            (x.get_leaves() for x in leaves[:-1])}
        default = leaves[-1]
        posts = {}

        IMPORTERS[formatname.get_string_value()] = (conditionals, default, posts, options)

        return Symbol('Null')


class RegisterExport(Builtin):
    """
    <dl>
    <dt>'RegisterExport["$format$", $func$]'
      <dd>register '$func$' as the default function used when exporting from a file of type '"$format$"'.
    </dl>

    Simple text exporter
    >> ExampleExporter1[filename_, data_, opts___] := Module[{strm = OpenWrite[filename], char = data}, WriteString[strm, char]; Close[strm]]

    >> ImportExport`RegisterExport["ExampleFormat1", ExampleExporter1]

    >> Export["sample.txt", "Encode this string!", "ExampleFormat1"];

    >> FilePrint["sample.txt"]
     | Encode this string!

    #> DeleteFile["sample.txt"]

    Very basic encrypted text exporter
    >> ExampleExporter2[filename_, data_, opts___] := Module[{strm = OpenWrite[filename], char}, (* TODO: Check data *) char = FromCharacterCode[Mod[ToCharacterCode[data] - 84, 26] + 97]; WriteString[strm, char]; Close[strm]]

    >> ImportExport`RegisterExport["ExampleFormat2", ExampleExporter2]

    >> Export["sample.txt", "encodethisstring", "ExampleFormat2"];

    >> FilePrint["sample.txt"]
     | rapbqrguvffgevat

    #> DeleteFile["sample.txt"]
    """

    context = 'ImportExport`'

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
        '''ImportExport`RegisterExport[formatname_String, function_,
                OptionsPattern[ImportExport`RegisterExport]]'''
        EXPORTERS[formatname.get_string_value()] = (function, options)

        return Symbol('Null')


class URLFetch(Builtin):
    '''
    <dl>
    <dt>'URLFetch[$URL$]'
      <dd> Returns the content of $URL$ as a string.
    </dl>


    #> Quiet[URLFetch["https:////", {}]]
     = $Failed

    #> Quiet[URLFetch["https://www.example.com", {}]]
     = $Failed
    '''

    messages = {
        'httperr': '`1` could not be retrieved; `2`.',
    }

    def apply(self, url, elements, evaluation, options={}):
        'URLFetch[url_String, elements_, OptionsPattern[]]'

        import tempfile
        import os

        py_url = url.get_string_value()

        temp_handle, temp_path = tempfile.mkstemp(suffix='')
        try:
            # some pages need cookies or they will end up in an infinite redirect (i.e. HTTP 303)
            # loop, which prevents the page from getting loaded.
            f = urllib2.build_opener(urllib2.HTTPCookieProcessor).open(py_url)

            try:
                if sys.version_info >= (3, 0):
                    content_type = f.info().get_content_type()
                else:
                    content_type = f.headers['content-type']

                os.write(temp_handle, f.read())
            finally:
                f.close()

                # on some OS (e.g. Windows) all writers need to be closed before another
                # reader (e.g. Import._import) can access it. so close the file here.
                os.close(temp_handle)

            def determine_filetype():
                return mimetype_dict.get(content_type)

            result = Import._import(temp_path, determine_filetype, elements, evaluation, options)
        except HTTPError as e:
            evaluation.message(
                'URLFetch', 'httperr', url,
                'the server returned an HTTP status code of %s (%s)' % (e.code, str(e.reason)))
            return SymbolFailed
        except URLError as e:  # see https://docs.python.org/3/howto/urllib2.html
            if hasattr(e, 'reason'):
                evaluation.message('URLFetch', 'httperr', url, str(e.reason))
            elif hasattr(e, 'code'):
                evaluation.message('URLFetch', 'httperr', url, 'server returned %s' % e.code)
            return SymbolFailed
        except ValueError as e:
            evaluation.message('URLFetch', 'httperr', url, str(e))
            return SymbolFailed
        finally:
            os.unlink(temp_path)

        return result


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
    #> Import["ExampleData/numberdata.csv", "FieldSeparators" -> "."]
    = {{0, 88,0, 60,0, 94}, {0, 76,0, 19,0, 51}, {0, 97,0, 04,0, 26}, {0, 33,0, 74,0, 79}, {0, 42,0, 64,0, 56}}

    ## Text
    >> Import["ExampleData/ExampleData.txt", "Elements"]
     = {Data, Lines, Plaintext, String, Words}
    >> Import["ExampleData/ExampleData.txt", "Lines"]
     = ...
    #> Import["ExampleData/Middlemarch.txt"];
     : An invalid unicode sequence was encountered and ignored.
    #> StringTake[Import["ExampleData/Middlemarch.txt", CharacterEncoding -> "ISO8859-1"], {21, 69}]
     = Le sentiment de la fausseté des plaisirs présents

    ## JSON
    >> Import["ExampleData/colors.json"]
     = {colorsArray -> {{colorName -> black, rgbValue -> (0, 0, 0), hexValue -> #000000}, {colorName -> red, rgbValue -> (255, 0, 0), hexValue -> #FF0000}, {colorName -> green, rgbValue -> (0, 255, 0), hexValue -> #00FF00}, {colorName -> blue, rgbValue -> (0, 0, 255), hexValue -> #0000FF}, {colorName -> yellow, rgbValue -> (255, 255, 0), hexValue -> #FFFF00}, {colorName -> cyan, rgbValue -> (0, 255, 255), hexValue -> #00FFFF}, {colorName -> magenta, rgbValue -> (255, 0, 255), hexValue -> #FF00FF}, {colorName -> white, rgbValue -> (255, 255, 255), hexValue -> #FFFFFF}}}

    ## XML
    #> Import["ExampleData/InventionNo1.xml", "Tags"]
     = {accidental, alter, arpeggiate, ..., words}
    """

    messages = {
        'nffil': 'File not found during Import.',
        'chtype': ('First argument `1` is not a valid file, directory, '
                   'or URL specification.'),
        'noelem': (
            'The Import element `1` is not present when importing as `2`.'),
        'fmtnosup': '`1` is not a supported Import format.',
        'emptyfch': 'Function Channel not defined.'
    }

    rules = {
        'Import[filename_]': 'Import[filename, {}]',
    }

    options = {
        '$OptionSyntax': 'System`Ignore',
    }

    def apply(self, filename, evaluation, options={}):
        'Import[filename_, OptionsPattern[]]'
        return self.apply_elements(filename, Expression('List'), evaluation, options)

    def apply_element(self, filename, element, evaluation, options={}):
        'Import[filename_, element_String, OptionsPattern[]]'
        return self.apply_elements(filename, Expression('List', element), evaluation, options)

    def apply_elements(self, filename, elements, evaluation, options={}):
        'Import[filename_, elements_List?(AllTrue[#, NotOptionQ]&), OptionsPattern[]]'
        # Check filename
        path = filename.to_python()
        if not (isinstance(path, str) and path[0] == path[-1] == '"'):
            evaluation.message('Import', 'chtype', filename)
            return SymbolFailed

        # Load local file
        findfile = Expression('FindFile', filename).evaluate(evaluation)

        if findfile == SymbolFailed:
            evaluation.message('Import', 'nffil')
            return findfile

        def determine_filetype():
            return Expression('FileFormat', findfile).evaluate(
                evaluation=evaluation).get_string_value()

        return self._import(findfile, determine_filetype, elements, evaluation, options)

    @staticmethod
    def _import(findfile, determine_filetype, elements, evaluation, options, data = None):
        current_predetermined_out = evaluation.predetermined_out
        # Check elements
        if elements.has_form('List', None):
            elements = elements.get_leaves()
        else:
            elements = [elements]

        for el in elements:
            if not isinstance(el, String):

                evaluation.message('Import', 'noelem', el)
                evaluation.predetermined_out = current_predetermined_out
                return SymbolFailed

        elements = [el.get_string_value() for el in elements]

        # Determine file type
        for el in elements:
            if el in IMPORTERS.keys():
                filetype = el
                elements.remove(el)
                break
        else:
            filetype = determine_filetype()

        if filetype not in IMPORTERS.keys():
            evaluation.message('Import', 'fmtnosup', filetype)
            evaluation.predetermined_out = current_predetermined_out
            return SymbolFailed

        # Load the importer
        (conditionals, default_function, posts, importer_options) = IMPORTERS[filetype]

        stream_options, custom_options = _importer_exporter_options(
            importer_options.get("System`Options"), options, "System`Import", evaluation)

        function_channels = importer_options.get("System`FunctionChannels")

        if function_channels is None:
            # TODO message
            if data is None:
                evaluation.message('Import', 'emptyfch')
            else:
                evaluation.message('ImportString', 'emptyfch')
            evaluation.predetermined_out = current_predetermined_out
            return SymbolFailed


        default_element = importer_options.get("System`DefaultElement")
        if default_element is None:
            # TODO message
            evaluation.predetermined_out = current_predetermined_out
            return SymbolFailed

        def get_results(tmp_function, findfile):
            if function_channels == Expression('List', String('FileNames')):
                joined_options = list(chain(stream_options, custom_options))
                tmpfile = False
                if findfile is None:
                    tmpfile = True
                    stream = Expression('OpenWrite').evaluate(evaluation)
                    findfile = stream.leaves[0]
                    if not data is None:
                        Expression('WriteString', data).evaluate(evaluation)
                    else:
                        Expression('WriteString', String("")).evaluate(evaluation)
                    Expression('Close', stream).evaluate(evaluation)
                    stream = None
                tmp = Expression(tmp_function, findfile, *joined_options).evaluate(evaluation)
                if tmpfile:
                     Expression("DeleteFile", findfile).evaluate(evaluation)
            elif function_channels == Expression('List', String('Streams')):
                if findfile is None:
                    stream = Expression('StringToStream', data).evaluate(evaluation)
                else:
                    stream = Expression('OpenRead', findfile, *stream_options).evaluate(evaluation)
                if stream.get_head_name() != 'System`InputStream':
                    evaluation.message('Import', 'nffil')
                    evaluation.predetermined_out = current_predetermined_out
                    return None
                tmp = Expression(tmp_function, stream, *custom_options).evaluate(evaluation)
                Expression('Close', stream).evaluate(evaluation)
            else:
                # TODO message
                evaluation.predetermined_out = current_predetermined_out
                return SymbolFailed
            tmp = tmp.get_leaves()
            if not all(expr.has_form('Rule', None) for expr in tmp):
                evaluation.predetermined_out = current_predetermined_out
                return None

            # return {a.get_string_value() : b for (a,b) in map(lambda x:
            # x.get_leaves(), tmp)}
            evaluation.predetermined_out = current_predetermined_out
            return dict((a.get_string_value(), b)
                        for (a, b) in [x.get_leaves() for x in tmp])

        # Perform the import
        defaults = None

        if not elements:
            defaults = get_results(default_function, findfile)
            if defaults is None:
                evaluation.predetermined_out = current_predetermined_out
                return SymbolFailed
            if default_element == Symbol("Automatic"):
                evaluation.predetermined_out = current_predetermined_out
                return Expression('List', *(
                    Expression('Rule', String(key), defaults[key])
                    for key in defaults.keys()))
            else:
                result = defaults.get(default_element.get_string_value())
                if result is None:
                    evaluation.message('Import', 'noelem', default_element,
                                       from_python(filetype))
                    evaluation.predetermined_out = current_predetermined_out
                    return SymbolFailed
                evaluation.predetermined_out = current_predetermined_out
                return result
        else:
            assert len(elements) == 1
            el = elements[0]
            if el == "Elements":
                defaults = get_results(default_function, findfile)
                if defaults is None:
                    evaluation.predetermined_out = current_predetermined_out
                    return SymbolFailed
                # Use set() to remove duplicates
                evaluation.predetermined_out = current_predetermined_out
                return from_python(sorted(set(
                    list(conditionals.keys()) + list(defaults.keys()) + list(posts.keys()))))
            else:
                if el in conditionals.keys():
                    result = get_results(conditionals[el], findfile)
                    if result is None:
                        evaluation.predetermined_out = current_predetermined_out
                        return SymbolFailed
                    if len(list(result.keys())) == 1 and list(result.keys())[0] == el:
                        evaluation.predetermined_out = current_predetermined_out
                        return list(result.values())[0]
                elif el in posts.keys():
                    # TODO: allow use of conditionals
                    result = get_results(posts[el])
                    if result is None:
                        evaluation.predetermined_out = current_predetermined_out
                        return SymbolFailed
                else:
                    if defaults is None:
                        defaults = get_results(default_function, findfile)
                        if defaults is None:
                            evaluation.predetermined_out = current_predetermined_out
                            return SymbolFailed
                    if el in defaults.keys():
                        evaluation.predetermined_out = current_predetermined_out
                        return defaults[el]
                    else:
                        evaluation.message('Import', 'noelem', from_python(el),
                                           from_python(filetype))
                        evaluation.predetermined_out = current_predetermined_otu
                        return SymbolFailed


class ImportString(Import):
    """
    <dl>
    <dt>'ImportString["$data$", "$format$"]'
      <dd>imports data in the specified format from a string.
    <dt>'ImportString["$file$", $elements$]'
      <dd>imports the specified elements from a string.
    <dt>'ImportString["$data$"]'
      <dd>attempts to determine the format of the string from its content.
    </dl>


    #> ImportString[x]
     : First argument x is not a string.
     = $Failed

    ## CSV
    #> datastring = "0.88, 0.60, 0.94\\n.076, 0.19, .51\\n0.97, 0.04, .26";
    #> ImportString[datastring, "Elements"]
     = {Data, Lines, Plaintext, String, Words}
    #> ImportString[datastring, {"CSV","Elements"}]
     = {Data, Grid}
    #> ImportString[datastring, {"CSV", "Data"}]
    = {{0.88,  0.60,  0.94}, {.076,  0.19,  .51}, {0.97,  0.04,  .26}}
    #> ImportString[datastring]
    = 0.88, 0.60, 0.94
    .  .076, 0.19, .51
    .  0.97, 0.04, .26
    #> ImportString[datastring, "CSV","FieldSeparators" -> "."]
    = {{0, 88, 0, 60, 0, 94}, {076, 0, 19, , 51}, {0, 97, 0, 04, , 26}}

    ## Text
    >> str = "Hello!\\n    This is a testing text\\n";
    >> ImportString[str, "Elements"]
     = {Data, Lines, Plaintext, String, Words}
    >> ImportString[str, "Lines"]
     = ...
    """

    messages = {
        'string': 'First argument `1` is not a string.',
        'noelem': (
            'The Import element `1` is not present when importing as `2`.'),
        'fmtnosup': '`1` is not a supported Import format.',
    }

    rules = {
            }


    def apply(self, data, evaluation, options={}):
        'ImportString[data_, OptionsPattern[]]'
        return self.apply_elements(data, Expression('List'), evaluation, options)

    def apply_element(self, data, element, evaluation, options={}):
        'ImportString[data_, element_String, OptionsPattern[]]'

        return self.apply_elements(data, Expression('List', element), evaluation, options)


    def apply_elements(self, data, elements, evaluation, options={}):
        'ImportString[data_, elements_List?(AllTrue[#, NotOptionQ]&), OptionsPattern[]]'
        if not (isinstance(data, String)):
            evaluation.message('ImportString', 'string', data)
            return SymbolFailed

        def determine_filetype():
            if not FileFormat.detector:
                loader = magic.MagicLoader()
                loader.load()
                FileFormat.detector = magic.MagicDetector(loader.mimetypes)
            mime = set(FileFormat.detector.match("", data=data.to_python()))

            result = []
            for key in mimetype_dict.keys():
                if key in mime:
                    result.append(mimetype_dict[key])

            # the following fixes an extremely annoying behaviour on some (not all)
            # installations of Windows, where we end up classifying .csv files als XLS.
            if len(result) == 1 and result[0] == 'XLS' and path.lower().endswith('.csv'):
                return String('CSV')

            if len(result) == 0:
                result = 'Binary'
            elif len(result) == 1:
                result = result[0]
            else:
                return None

            return result

        return self._import(None, determine_filetype, elements, evaluation, options, data = data)





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

    #> Export["abc.txt", "ä", CharacterEncoding -> "ISOLatin1"];
    #> strm = OpenRead["abc.txt", BinaryFormat -> True];
    #> BinaryRead[strm]
     = 228
    #> Close[strm];
    #> DeleteFile["abc.txt"];

    #> Export["abc.txt", "ä", CharacterEncoding -> "UTF-8"];
    #> strm = OpenRead["abc.txt", BinaryFormat -> True];
    #> BinaryRead[strm]
     = 195
    #> Close[strm];
    #> DeleteFile["abc.txt"];

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
        'emptyfch': 'Function Channel not defined.',
        'nffil':  'File `1` could not be opened',
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

    options = {
        '$OptionSyntax': 'System`Ignore',
    }

    def apply(self, filename, expr, evaluation, options={}):
        "Export[filename_, expr_, OptionsPattern[]]"

        # Check filename
        if not self._check_filename(filename, evaluation):
            return SymbolFailed

        # Determine Format
        form = self._infer_form(filename, evaluation)

        if form is None:
            evaluation.message('Export', 'infer', filename)
            return SymbolFailed
        else:
            return self.apply_elements(filename, expr, String(form), evaluation, options)

    def apply_element(self, filename, expr, element, evaluation, options={}):
        'Export[filename_, expr_, element_String, OptionsPattern[]]'
        return self.apply_elements(filename, expr, Expression('List', element), evaluation, options)

    def apply_elements(self, filename, expr, elems, evaluation, options={}):
        "Export[filename_, expr_, elems_List?(AllTrue[#, NotOptionQ]&), OptionsPattern[]]"

        # Check filename
        if not self._check_filename(filename, evaluation):
            return SymbolFailed

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

        # Just to be sure that the following calls do not change the state of this property
        current_predetermined_out =  evaluation.predetermined_out
        # Infer format if not present
        if not found_form:
            assert format_spec == []
            format_spec = self._infer_form(filename, evaluation)
            if format_spec is None:
                evaluation.message('Export', 'infer', filename)
                evaluation.predetermined_out = current_predetermined_out
                return SymbolFailed
            format_spec = [format_spec]
        else:
            assert format_spec != []

        # First item in format_spec is the explicit format.
        # The other elements (if present) are compression formats

        if elems_spec != []:        # FIXME: support elems
            evaluation.message(
                'Export', 'noelem', elems, String(format_spec[0]))
            evaluation.predetermined_out = current_predetermined_out
            return SymbolFailed

        # Load the exporter
        exporter_symbol, exporter_options = EXPORTERS[format_spec[0]]
        function_channels = exporter_options.get("System`FunctionChannels")

        stream_options, custom_options = _importer_exporter_options(
            exporter_options.get("System`Options"), options, 'System`Export', evaluation)


        if function_channels is None:
            evaluation.message('Export', 'emptyfch')
            evaluation.predetermined_out = current_predetermined_out
            return SymbolFailed
        elif function_channels == Expression('List', String('FileNames')):
            exporter_function = Expression(
                exporter_symbol, filename, expr, *list(chain(stream_options, custom_options)))
            res = exporter_function.evaluate(evaluation)
        elif function_channels == Expression('List', String('Streams')):
            stream = Expression('OpenWrite', filename, *stream_options).evaluate(evaluation)
            if stream.get_head_name() != 'System`OutputStream':
                evaluation.message('Export', 'nffil')
                evaluation.predetermined_out = current_predetermined_out
                return Symbol("$Failed")
            exporter_function = Expression(
                exporter_symbol, stream, expr, *list(chain(stream_options, custom_options)))
            res = exporter_function.evaluate(evaluation)
            Expression('Close', stream).evaluate(evaluation)
        if res == Symbol('Null'):
            evaluation.predetermined_out = current_predetermined_out
            return filename
        evaluation.predetermined_out = current_predetermined_out
        return SymbolFailed

    def _check_filename(self, filename, evaluation):
        path = filename.to_python()
        if isinstance(path, str) and path[0] == path[-1] == '"':
            return True
        evaluation.message('Export', 'chtype', filename)
        return False

    def _infer_form(self, filename, evaluation):
        ext = Expression('FileExtension', filename).evaluate(evaluation)
        ext = ext.get_string_value().lower()
        return self._extdict.get(ext)


class ExportString(Builtin):
    """
    <dl>
    <dt>'ExportString[$expr$, $form$]'
      <dd>exports $expr$ to a string, in the format $form$.
    <dt>'Export["$file$", $exprs$, $elems$]'
      <dd>exports $exprs$ to a string as elements specified by $elems$.
    </dl>

    >> ExportString[{{1,2,3,4},{3},{2},{4}}, "CSV"]
     = 1,2,3,4
     . 3,
     . 2,
     . 4,

    >> ExportString[{1,2,3,4}, "CSV"]
     = 1,
     . 2,
     . 3,
     . 4,
    >> ExportString[Integrate[f[x],{x,0,2}], "SVG"]
     = ...
    """

    messages = {
        'noelem': "`1` is not a valid set of export elements for the `2` format.",
        'emptyfch': 'Function Channel not defined.',
    }

    rules = {
        'ExportString[expr_, elems_?NotListQ]': (
            'ExportString[expr, {elems}]'),
    }


    def apply_element(self, expr, element, evaluation, options={}):
        'ExportString[expr_, element_String, OptionsPattern[]]'
        return self.apply_elements(expr, Expression('List', element), evaluation, options)

    def apply_elements(self, expr, elems, evaluation, options={}):
        "ExportString[expr_, elems_List?(AllTrue[#, NotOptionQ]&), OptionsPattern[]]"
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

        # Just to be sure that the following evaluations do not change the value of this property
        current_predetermined_out =  evaluation.predetermined_out

        # Infer format if not present
        if format_spec is None:
            evaluation.message('ExportString', 'infer', filename)
            evaluation.predetermined_out = current_predetermined_out
            return SymbolFailed

        # First item in format_spec is the explicit format.
        # The other elements (if present) are compression formats

        if elems_spec != []:        # FIXME: support elems
            if format_spec != []:
                evaluation.message(
                    'ExportString', 'noelem', elems, String(format_spec[0]))
            else:
                evaluation.message(
                    'ExportString', 'noelem', elems, String("Unknown"))
            evaluation.predetermined_out = current_predetermined_out
            return SymbolFailed

        # Load the exporter
        exporter_symbol, exporter_options = EXPORTERS[format_spec[0]]
        function_channels = exporter_options.get("System`FunctionChannels")

        stream_options, custom_options = _importer_exporter_options(
            exporter_options.get("System`Options"), options, "System Options", evaluation)

        if function_channels is None:
            evaluation.message('ExportString', 'emptyfch')
            evaluation.predetermined_out = current_predetermined_out
            return SymbolFailed
        elif function_channels == Expression('List', String('FileNames')):
            # Generates a temporary file
            import tempfile
            tmpfile =  tempfile.NamedTemporaryFile(dir=tempfile.gettempdir())
            filename = String(tmpfile.name)
            tmpfile.close()
            exporter_function = Expression(
                exporter_symbol, filename, expr, *list(chain(stream_options, custom_options)))
            if exporter_function.evaluate(evaluation) != Symbol('Null'):
                evaluation.predetermined_out = current_predetermined_out
                return SymbolFailed
            else:
                try:
                    tmpstream = open(filename.value, 'rb')
                    res = tmpstream.read().decode('utf-8')
                    tmpstream.close()
                except Exception as e:
                    print("something went wrong")
                    print(e)
                    evaluation.predetermined_out = current_predetermined_out
                    return SymbolFailed
                res = String(str(res))
        elif function_channels == Expression('List', String('Streams')):
            from io import StringIO
            from mathics.builtin.files import STREAMS, NSTREAMS
            pystream = StringIO()
            n = next(NSTREAMS)
            STREAMS.append(pystream)
            stream = Expression('OutputStream', String('String'), Integer(n))
            exporter_function = Expression(
                exporter_symbol, stream, expr, *list(chain(stream_options, custom_options)))
            res = exporter_function.evaluate(evaluation)
            if res == Symbol('Null'):
                res = String(str(pystream.getvalue()))
            else:
                res = Symbol("$Failed")
            Expression('Close', stream).evaluate(evaluation)
        else:
            evaluation.message('ExportString', 'emptyfch')
            evaluation.predetermined_out = current_predetermined_out
            return SymbolFailed

        evaluation.predetermined_out = current_predetermined_out
        return res


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

    S> FileFormat["ExampleData/benzene.xyz"]
     = XYZ

    #> FileFormat["ExampleData/colors.json"]
     = JSON

    #> FileFormat["ExampleData/some-typo.extension"]
     : File not found during FileFormat[ExampleData/some-typo.extension].
     = $Failed

    #> FileFormat["ExampleData/Testosterone.svg"]
     = SVG

    #> FileFormat["ExampleData/colors.json"]
     = JSON

    #> FileFormat["ExampleData/InventionNo1.xml"]
     = XML
    """

    messages = {
        'nffil': 'File not found during `1`.',
    }

    detector = None

    def apply(self, filename, evaluation):
        'FileFormat[filename_String]'

        findfile = Expression('FindFile', filename).evaluate(evaluation)
        if findfile == SymbolFailed:
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
        result = []
        for key in mimetype_dict.keys():
            if key in mime:
                result.append(mimetype_dict[key])

        # the following fixes an extremely annoying behaviour on some (not all)
        # installations of Windows, where we end up classifying .csv files als XLS.
        if len(result) == 1 and result[0] == 'XLS' and path.lower().endswith('.csv'):
            return String('CSV')

        if len(result) == 0:
            result = 'Binary'
        elif len(result) == 1:
            result = result[0]
        else:
            return None

        return from_python(result)

import base64

class B64Encode(Builtin):
    """
    <dl>
    <dt> 'System`Convert`B64Dump`B64Encode[$expr$]'
    <dd>Encodes $expr$ in Base64 coding
    </dl>

    >> System`Convert`B64Dump`B64Encode["Hello world"]
     = SGVsbG8gd29ybGQ=
    >> System`Convert`B64Dump`B64Decode[%]
     = Hello world
    >> System`Convert`B64Dump`B64Encode[Integrate[f[x],{x,0,2}]]
     = SW50ZWdyYXRlW2ZbeF0sIHt4LCAwLCAyfV0=
    >> System`Convert`B64Dump`B64Decode[%]
     = Integrate[f[x], {x, 0, 2}]
    >> System`Convert`B64Dump`B64Encode["∫ f  x"]
     = 4oirIGYg752MIHg=
    >> System`Convert`B64Dump`B64Decode[%]
     = ∫ f  x
    """

    context = "System`Convert`B64Dump`"
    name = "B64Encode"

    def apply(self, expr, evaluation):
        'System`Convert`B64Dump`B64Encode[expr_]'
        if isinstance(expr,String):
            stringtocodify = expr.get_string_value()
        else:
            stringtocodify = Expression('ToString',expr).evaluate(evaluation).get_string_value()
        return String(base64.b64encode(bytearray(stringtocodify, 'utf8')).decode('utf8'))


class B64Decode(Builtin):
    """
    <dl>
    <dt> 'System`Convert`B64Dump`B64Decode[$string$]'
    <dd>Decode  $string$ in Base64 coding to an expression.
    </dl>

    >> System`Convert`B64Dump`B64Decode["R!="]
     : String "R!=" is not a valid b64 encoded string.
     = $Failed
    """

    context = "System`Convert`B64Dump`"
    name = "B64Decode"

    messages = {
        'b64invalidstr': 'String "`1`" is not a valid b64 encoded string.',
    }

    def apply(self, expr, evaluation):
        'System`Convert`B64Dump`B64Decode[expr_String]'
        try:
            clearstring = base64.b64decode(bytearray(expr.get_string_value(), 'utf8')).decode('utf8')
            clearstring = String(str(clearstring))
        except Exception as e:
            evaluation.message("System`Convert`B64Dump`B64Decode", "b64invalidstr", expr)
            return Symbol("$Failed")
        return clearstring


