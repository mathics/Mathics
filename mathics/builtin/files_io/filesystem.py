# -*- coding: utf-8 -*-

"""
Filesystem Operations
"""

import os
import pathlib
import shutil
import tempfile
import time

import os.path as osp

from mathics.version import __version__  # noqa used in loading to check consistency.

from mathics.core.expression import (
    Expression,
    String,
    Symbol,
    SymbolFailed,
    SymbolFalse,
    SymbolNull,
    SymbolTrue,
    SymbolList,
    from_python,
    valid_context_name,
)

import mathics.core.streams
from mathics.core.streams import (
    HOME_DIR,
    PATH_VAR,
    ROOT_DIR,
    create_temporary_file,
    path_search,
    urlsave_tmp,
)

from mathics.builtin.base import Builtin, Predefined
from mathics.builtin.files_io.files import (
    DIRECTORY_STACK,
    INITIAL_DIR,  # noqa is used via global
    mathics_open,
)
from mathics.builtin.numeric import Hash
from mathics.builtin.strings import to_regex
from mathics.builtin.base import MessageException
import re

SYS_ROOT_DIR = "/" if os.name == "posix" else "\\"
TMP_DIR = tempfile.gettempdir()


class AbsoluteFileName(Builtin):
    """
    <dl>
    <dt>'AbsoluteFileName["$name$"]'
      <dd>returns the absolute version of the given filename.
    </dl>

    >> AbsoluteFileName["ExampleData/sunflowers.jpg"]
     = ...

    #> AbsoluteFileName["Some/NonExistant/Path.ext"]
     : File not found during AbsoluteFileName[Some/NonExistant/Path.ext].
     = $Failed
    """

    attributes = "Protected"

    messages = {
        "fstr": ("File specification x is not a string of one or more characters."),
        "nffil": "File not found during `1`.",
    }

    def apply(self, name, evaluation):
        "AbsoluteFileName[name_]"

        py_name = name.to_python()

        if not (isinstance(py_name, str) and py_name[0] == py_name[-1] == '"'):
            evaluation.message("AbsoluteFileName", "fstr", name)
            return
        py_name = py_name[1:-1]

        result = path_search(py_name)

        if result is None:
            evaluation.message(
                "AbsoluteFileName", "nffil", Expression("AbsoluteFileName", name)
            )
            return SymbolFailed

        return String(osp.abspath(result))


class BaseDirectory(Predefined):
    """
    <dl>
    <dt>'$UserBaseDirectory'
      <dd>returns the folder where user configurations are stored.
    </dl>

    >> $RootDirectory
     = ...
    """

    name = "$BaseDirectory"

    attributes = "Protected"

    def evaluate(self, evaluation):
        global ROOT_DIR
        return String(ROOT_DIR)


class CopyDirectory(Builtin):
    """
    <dl>
    <dt>'CopyDirectory["$dir1$", "$dir2$"]'
      <dd>copies directory $dir1$ to $dir2$.
    </dl>
    """

    attributes = "Protected"

    messages = {
        "argr": "called with `1` argument; 2 arguments are expected.",
        "fstr": (
            "File specification `1` is not a string of " "one or more characters."
        ),
        "filex": "Cannot overwrite existing file `1`.",
        "nodir": "Directory `1` not found.",
    }

    def apply(self, dirs, evaluation):
        "CopyDirectory[dirs__]"

        seq = dirs.get_sequence()
        if len(seq) != 2:
            evaluation.message("CopyDirectory", "argr", len(seq))
            return
        (dir1, dir2) = (s.to_python() for s in seq)

        if not (isinstance(dir1, str) and dir1[0] == dir1[-1] == '"'):
            evaluation.message("CopyDirectory", "fstr", seq[0])
            return
        dir1 = dir1[1:-1]

        if not (isinstance(dir2, str) and dir2[0] == dir2[-1] == '"'):
            evaluation.message("CopyDirectory", "fstr", seq[1])
            return
        dir2 = dir2[1:-1]

        if not osp.isdir(dir1):
            evaluation.message("CopyDirectory", "nodir", seq[0])
            return SymbolFailed
        if osp.isdir(dir2):
            evaluation.message("CopyDirectory", "filex", seq[1])
            return SymbolFailed

        shutil.copytree(dir1, dir2)

        return String(osp.abspath(dir2))


class CopyFile(Builtin):
    """
    <dl>
    <dt>'CopyFile["$file1$", "$file2$"]'
      <dd>copies $file1$ to $file2$.
    </dl>

    X> CopyFile["ExampleData/sunflowers.jpg", "MathicsSunflowers.jpg"]
     = MathicsSunflowers.jpg
    X> DeleteFile["MathicsSunflowers.jpg"]
    """

    messages = {
        "filex": "Cannot overwrite existing file `1`.",
        "fstr": (
            "File specification `1` is not a string of " "one or more characters."
        ),
        "nffil": "File not found during `1`.",
    }

    attributes = "Protected"

    def apply(self, source, dest, evaluation):
        "CopyFile[source_, dest_]"

        py_source = source.to_python()
        py_dest = dest.to_python()

        # Check filenames
        if not (isinstance(py_source, str) and py_source[0] == py_source[-1] == '"'):
            evaluation.message("CopyFile", "fstr", source)
            return
        if not (isinstance(py_dest, str) and py_dest[0] == py_dest[-1] == '"'):
            evaluation.message("CopyFile", "fstr", dest)
            return

        py_source = py_source[1:-1]
        py_dest = py_dest[1:-1]

        py_source = path_search(py_source)

        if py_source is None:
            evaluation.message("CopyFile", "filex", source)
            return SymbolFailed

        if osp.exists(py_dest):
            evaluation.message("CopyFile", "filex", dest)
            return SymbolFailed

        try:
            shutil.copy(py_source, py_dest)
        except IOError:
            evaluation.message(
                "CopyFile", "nffil", Expression("CopyFile", source, dest)
            )
            return SymbolFailed

        return dest


class CreateDirectory(Builtin):
    """
    <dl>
    <dt>'CreateDirectory["$dir$"]'
      <dd>creates a directory called $dir$.
    <dt>'CreateDirectory[]'
      <dd>creates a temporary directory.
    </dl>

    >> dir = CreateDirectory[]
     = ...
    #> DirectoryQ[dir]
     = True
    #> DeleteDirectory[dir]
    """

    attributes = ("Listable", "Protected")

    options = {
        "CreateIntermediateDirectories": "True",
    }

    messages = {
        "fstr": (
            "File specification `1` is not a string of " "one or more characters."
        ),
        "nffil": "File not found during `1`.",
        "filex": "`1` already exists.",
    }

    def apply(self, dirname, evaluation, options):
        "CreateDirectory[dirname_, OptionsPattern[CreateDirectory]]"

        expr = Expression("CreateDirectory", dirname)
        py_dirname = dirname.to_python()

        if not (isinstance(py_dirname, str) and py_dirname[0] == py_dirname[-1] == '"'):
            evaluation.message("CreateDirectory", "fstr", dirname)
            return

        py_dirname = py_dirname[1:-1]

        if osp.isdir(py_dirname):
            evaluation.message("CreateDirectory", "filex", osp.abspath(py_dirname))
            return

        os.mkdir(py_dirname)

        if not osp.isdir(py_dirname):
            evaluation.message("CreateDirectory", "nffil", expr)
            return

        return String(osp.abspath(py_dirname))

    def apply_empty(self, evaluation, options):
        "CreateDirectory[OptionsPattern[CreateDirectory]]"
        dirname = tempfile.mkdtemp(prefix="m", dir=TMP_DIR)
        return String(dirname)


class CreateFile(Builtin):
    """
    <dl>
    <dt>'CreateFile["filename"]'
        <dd>Creates a file named "filename" temporary file, but do not open it.
    <dt>'CreateFile[]'
        <dd>Creates a temporary file, but do not open it.
    </dl>
    """

    rules = {
        "CreateFile[]": "CreateTemporary[]",
    }
    options = {
        "CreateIntermediateDirectories": "True",
        "OverwriteTarget": "True",
    }

    attributes = ("Listable", "Protected")

    def apply(self, filename, evaluation, **options):
        "CreateFile[filename_String, OptionsPattern[CreateFile]]"
        try:
            # TODO: Implement options
            if not osp.isfile(filename.value):
                f = open(filename.value, "w")
                res = f.name
                f.close()
                return String(res)
            else:
                return filename
        except:
            return SymbolFailed


class CreateTemporary(Builtin):
    """
    <dl>
    <dt>'CreateTemporary[]'
        <dd>Creates a temporary file, but do not open it.
    </dl>
    """

    def apply_0(self, evaluation):
        "CreateTemporary[]"
        try:
            res = create_temporary_file()
        except:
            return SymbolFailed
        return String(res)


class DeleteDirectory(Builtin):
    """
    <dl>
    <dt>'DeleteDirectory["$dir$"]'
      <dd>deletes a directory called $dir$.
    </dl>

    >> dir = CreateDirectory[]
     = ...
    >> DeleteDirectory[dir]
    >> DirectoryQ[dir]
     = False
    #> Quiet[DeleteDirectory[dir]]
     = $Failed
    """

    attributes = "Protected"

    options = {
        "DeleteContents": "False",
    }

    messages = {
        "strs": (
            "String or non-empty list of strings expected at " "position 1 in `1`."
        ),
        "nodir": "Directory `1` not found.",
        "dirne": "Directory `1` not empty.",
        "optx": "Unknown option `1` in `2`",
        "idcts": "DeleteContents expects either True or False.",  # MMA Bug
    }

    def apply(self, dirname, evaluation, options):
        "DeleteDirectory[dirname_, OptionsPattern[DeleteDirectory]]"

        expr = Expression("DeleteDirectory", dirname)
        py_dirname = dirname.to_python()

        delete_contents = options["System`DeleteContents"].to_python()
        if delete_contents not in [True, False]:
            evaluation.message("DeleteDirectory", "idcts")
            return

        if not (isinstance(py_dirname, str) and py_dirname[0] == py_dirname[-1] == '"'):
            evaluation.message("DeleteDirectory", "strs", expr)
            return

        py_dirname = py_dirname[1:-1]

        if not osp.isdir(py_dirname):
            evaluation.message("DeleteDirectory", "nodir", dirname)
            return SymbolFailed

        if delete_contents:
            shutil.rmtree(py_dirname)
        else:
            if os.listdir(py_dirname) != []:
                evaluation.message("DeleteDirectory", "dirne", dirname)
                return SymbolFailed
            os.rmdir(py_dirname)

        return SymbolNull


class DeleteFile(Builtin):
    """
    <dl>
    <dt>'Delete["$file$"]'
      <dd>deletes $file$.
    <dt>'Delete[{"$file1$", "$file2$", ...}]'
      <dd>deletes a list of files.
    </dl>

    >> CopyFile["ExampleData/sunflowers.jpg", "MathicsSunflowers.jpg"];
    >> DeleteFile["MathicsSunflowers.jpg"]

    >> CopyFile["ExampleData/sunflowers.jpg", "MathicsSunflowers1.jpg"];
    >> CopyFile["ExampleData/sunflowers.jpg", "MathicsSunflowers2.jpg"];
    >> DeleteFile[{"MathicsSunflowers1.jpg", "MathicsSunflowers2.jpg"}]
    """

    messages = {
        "filex": "Cannot overwrite existing file `1`.",
        "strs": (
            "String or non-empty list of strings expected at " "position `1` in `2`."
        ),
        "nffil": "File not found during `1`.",
    }

    attributes = "Protected"

    def apply(self, filename, evaluation):
        "DeleteFile[filename_]"

        py_path = filename.to_python()
        if not isinstance(py_path, list):
            py_path = [py_path]

        py_paths = []
        for path in py_path:
            # Check filenames
            if not (isinstance(path, str) and path[0] == path[-1] == '"'):
                evaluation.message(
                    "DeleteFile", "strs", filename, Expression("DeleteFile", filename)
                )
                return

            path = path[1:-1]
            path = path_search(path)

            if path is None:
                evaluation.message(
                    "DeleteFile", "nffil", Expression("DeleteFile", filename)
                )
                return SymbolFailed
            py_paths.append(path)

        for path in py_paths:
            try:
                os.remove(path)
            except OSError:
                return SymbolFailed

        return SymbolNull


class Directory(Builtin):
    """
    <dl>
    <dt>'Directory[]'
      <dd>returns the current working directory.
    </dl>

    >> Directory[]
    = ...
    """

    attributes = "Protected"

    def apply(self, evaluation):
        "Directory[]"
        result = os.getcwd()
        return String(result)


class DirectoryName(Builtin):
    """
    <dl>
    <dt>'DirectoryName["$name$"]'
      <dd>extracts the directory name from a filename.
    </dl>

    >> DirectoryName["a/b/c"]
     = a/b

    >> DirectoryName["a/b/c", 2]
     = a

    #> DirectoryName["a/b/c", 3] // InputForm
     = ""
    #> DirectoryName[""] // InputForm
     = ""

    #> DirectoryName["a/b/c", x]
     : Positive machine-sized integer expected at position 2 in DirectoryName[a/b/c, x].
     = DirectoryName[a/b/c, x]

    #> DirectoryName["a/b/c", -1]
     : Positive machine-sized integer expected at position 2 in DirectoryName[a/b/c, -1].
     = DirectoryName[a/b/c, -1]

    #> DirectoryName[x]
     : String expected at position 1 in DirectoryName[x].
     = DirectoryName[x]
    """

    attributes = "Protected"

    options = {
        "OperatingSystem": "$OperatingSystem",
    }

    messages = {
        "string": "String expected at position 1 in `1`.",
        "intpm": ("Positive machine-sized integer expected at " "position 2 in `1`."),
    }

    def apply_with_n(self, name, n, evaluation, options):
        "DirectoryName[name_, n_, OptionsPattern[DirectoryName]]"

        if n is None:
            expr = Expression("DirectoryName", name)
            py_n = 1
        else:
            expr = Expression("DirectoryName", name, n)
            py_n = n.to_python()

        if not (isinstance(py_n, int) and py_n > 0):
            evaluation.message("DirectoryName", "intpm", expr)
            return

        py_name = name.to_python()
        if not (isinstance(py_name, str) and py_name[0] == py_name[-1] == '"'):
            evaluation.message("DirectoryName", "string", expr)
            return
        py_name = py_name[1:-1]

        result = py_name
        for i in range(py_n):
            (result, tmp) = osp.split(result)

        return String(result)

    def apply(self, name, evaluation, options):
        "DirectoryName[name_, OptionsPattern[DirectoryName]]"
        return self.apply_with_n(name, None, evaluation, options)


class DirectoryStack(Builtin):
    """
    <dl>
    <dt>'DirectoryStack[]'
      <dd>returns the directory stack.
    </dl>

    >> DirectoryStack[]
    = ...
    """

    attributes = "Protected"

    def apply(self, evaluation):
        "DirectoryStack[]"
        global DIRECTORY_STACK
        return from_python(DIRECTORY_STACK)


class DirectoryQ(Builtin):
    """
    <dl>
    <dt>'DirectoryQ["$name$"]'
      <dd>returns 'True' if the directory called $name$ exists and 'False' otherwise.
    </dl>

    >> DirectoryQ["ExampleData/"]
     = True
    >> DirectoryQ["ExampleData/MythicalSubdir/"]
     = False

    #> DirectoryQ["ExampleData"]
     = True

    #> DirectoryQ["ExampleData/MythicalSubdir/NestedDir/"]
     = False
    """

    messages = {
        "fstr": (
            "File specification `1` is not a string of " "one or more characters."
        ),
    }

    attributes = "Protected"

    def apply(self, pathname, evaluation):
        "DirectoryQ[pathname_]"
        path = pathname.to_python()

        if not (isinstance(path, str) and path[0] == path[-1] == '"'):
            evaluation.message("DirectoryQ", "fstr", pathname)
            return
        path = path[1:-1]

        path = path_search(path)

        if path is not None and osp.isdir(path):
            return SymbolTrue
        return SymbolFalse


class ExpandFileName(Builtin):
    """
    <dl>
    <dt>'ExpandFileName["$name$"]'
      <dd>expands $name$ to an absolute filename for your system.
    </dl>

    >> ExpandFileName["ExampleData/sunflowers.jpg"]
     = ...
    """

    attributes = "Protected"

    messages = {
        "string": "String expected at position 1 in `1`.",
    }

    def apply(self, name, evaluation):
        "ExpandFileName[name_]"

        py_name = name.to_python()

        if not (isinstance(py_name, str) and py_name[0] == py_name[-1] == '"'):
            evaluation.message(
                "ExpandFileName", "string", Expression("ExpandFileName", name)
            )
            return
        py_name = py_name[1:-1]

        return String(osp.abspath(py_name))


class File(Builtin):
    attributes = "Protected"


class FileBaseName(Builtin):
    """
    <dl>
    <dt>'FileBaseName["$file$"]'
      <dd>gives the base name for the specified file name.
    </dl>

    >> FileBaseName["file.txt"]
     = file

    >> FileBaseName["file.tar.gz"]
     = file.tar

    #> FileBaseName["file."]
     = file

    #> FileBaseName["file"]
     = file
    """

    attributes = "Protected"

    options = {
        "OperatingSystem": "$OperatingSystem",
    }

    def apply(self, filename, evaluation, options):
        "FileBaseName[filename_String, OptionsPattern[FileBaseName]]"
        path = filename.to_python()[1:-1]

        filename_base, filename_ext = osp.splitext(path)
        return from_python(filename_base)


class FileByteCount(Builtin):
    """
    <dl>
    <dt>'FileByteCount[$file$]'
      <dd>returns the number of bytes in $file$.
    </dl>

    >> FileByteCount["ExampleData/sunflowers.jpg"]
     = 142286
    """

    messages = {
        "fstr": "File specification `1` is not a string of one or more characters.",
    }

    def apply(self, filename, evaluation):
        "FileByteCount[filename_]"
        py_filename = filename.to_python()
        if not (
            isinstance(py_filename, str) and py_filename[0] == py_filename[-1] == '"'
        ):
            evaluation.message("FileByteCount", "fstr", filename)
            return
        py_filename = py_filename[1:-1]

        try:
            with mathics_open(py_filename, "rb") as f:
                count = 0
                tmp = f.read(1)
                while tmp != b"":
                    count += 1
                    tmp = f.read(1)

        except IOError:
            evaluation.message("General", "noopen", filename)
            return
        except MessageException as e:
            e.message(evaluation)
            return

        return from_python(count)


class FileDate(Builtin):
    """
    <dl>
    <dt>'FileDate[$file$, $types$]'
      <dd>returns the time and date at which the file was last modified.
    </dl>

    >> FileDate["ExampleData/sunflowers.jpg"]
     = ...

    >> FileDate["ExampleData/sunflowers.jpg", "Access"]
     = ...

    >> FileDate["ExampleData/sunflowers.jpg", "Creation"]
     = ...

    >> FileDate["ExampleData/sunflowers.jpg", "Change"]
     = ...

    >> FileDate["ExampleData/sunflowers.jpg", "Modification"]
     = ...

    >>  FileDate["ExampleData/sunflowers.jpg", "Rules"]
     = ...

    #>  FileDate["MathicsNonExistantExample"]
     : File not found during FileDate[MathicsNonExistantExample].
     = FileDate[MathicsNonExistantExample]
    #>  FileDate["MathicsNonExistantExample", "Modification"]
     : File not found during FileDate[MathicsNonExistantExample, Modification].
     = FileDate[MathicsNonExistantExample, Modification]

    #> FileDate["ExampleData/sunflowers.jpg", "Fail"]
     : Date type Fail should be "Access", "Modification", "Creation" (Windows only), "Change" (Macintosh and Unix only), or "Rules".
     = FileDate[ExampleData/sunflowers.jpg, Fail]
    """

    messages = {
        "nffil": "File not found during `1`.",
        "datetype": (
            'Date type Fail should be "Access", "Modification", '
            '"Creation" (Windows only), '
            '"Change" (Macintosh and Unix only), or "Rules".'
        ),
    }

    rules = {
        'FileDate[filepath_String, "Rules"]': """{"Access" -> FileDate[filepath, "Access"],
            "Creation" -> FileDate[filepath, "Creation"],
            "Change" -> FileDate[filepath, "Change"],
            "Modification" -> FileDate[filepath, "Modification"]}""",
    }

    attributes = "Protected"

    def apply(self, path, timetype, evaluation):
        "FileDate[path_, timetype_]"
        py_path = path_search(path.to_python()[1:-1])

        if py_path is None:
            if timetype is None:
                evaluation.message("FileDate", "nffil", Expression("FileDate", path))
            else:
                evaluation.message(
                    "FileDate", "nffil", Expression("FileDate", path, timetype)
                )
            return

        if timetype is None:
            time_type = "Modification"
        else:
            time_type = timetype.to_python()[1:-1]

        if time_type == "Access":
            result = osp.getatime(py_path)
        elif time_type == "Creation":
            if os.name == "posix":
                return Expression("Missing", "NotApplicable")
            result = osp.getctime(py_path)
        elif time_type == "Change":
            if os.name != "posix":
                return Expression("Missing", "NotApplicable")
            result = osp.getctime(py_path)
        elif time_type == "Modification":
            result = osp.getmtime(py_path)
        else:
            evaluation.message("FileDate", "datetype")
            return

        # Offset for system epoch
        epochtime = Expression(
            "AbsoluteTime", time.strftime("%Y-%m-%d %H:%M", time.gmtime(0))
        ).to_python(n_evaluation=evaluation)
        result += epochtime

        return Expression("DateList", from_python(result))

    def apply_default(self, path, evaluation):
        "FileDate[path_]"
        return self.apply(path, None, evaluation)


class FileExistsQ(Builtin):
    """
    <dl>
    <dt>'FileExistsQ["$file$"]'
      <dd>returns 'True' if $file$ exists and 'False' otherwise.
    </dl>

    >> FileExistsQ["ExampleData/sunflowers.jpg"]
     = True
    >> FileExistsQ["ExampleData/sunflowers.png"]
     = False
    """

    messages = {
        "fstr": (
            "File specification `1` is not a string of " "one or more characters."
        ),
    }

    attributes = "Protected"

    def apply(self, filename, evaluation):
        "FileExistsQ[filename_]"
        path = filename.to_python()
        if not (isinstance(path, str) and path[0] == path[-1] == '"'):
            evaluation.message("FileExistsQ", "fstr", filename)
            return
        path = path[1:-1]

        path = path_search(path)

        if path is None:
            return SymbolFalse
        return SymbolTrue


class FileExtension(Builtin):
    """
    <dl>
    <dt>'FileExtension["$file$"]'
      <dd>gives the extension for the specified file name.
    </dl>

    >> FileExtension["file.txt"]
     = txt

    >> FileExtension["file.tar.gz"]
     = gz

    #> FileExtension["file."]
     = #<--#
    #> FileExtension["file"]
     = #<--#
    """

    attributes = "Protected"

    options = {
        "OperatingSystem": "$OperatingSystem",
    }

    def apply(self, filename, evaluation, options):
        "FileExtension[filename_String, OptionsPattern[FileExtension]]"
        path = filename.to_python()[1:-1]
        filename_base, filename_ext = osp.splitext(path)
        filename_ext = filename_ext.lstrip(".")
        return from_python(filename_ext)


class FileHash(Builtin):
    """
    <dl>
    <dt>'FileHash[$file$]'
      <dd>returns an integer hash for the given $file$.
    <dt>'FileHash[$file$, $type$]'
      <dd>returns an integer hash of the specified $type$ for the given $file$.</dd>
      <dd>The types supported are "MD5", "Adler32", "CRC32", "SHA", "SHA224", "SHA256", "SHA384", and "SHA512".</dd>
    <dt>'FileHash[$file$, $type$, $format$]'
      <dd>gives a hash code in the specified format.</dd>
    </dl>

    >> FileHash["ExampleData/sunflowers.jpg"]
     = 109937059621979839952736809235486742106

    >> FileHash["ExampleData/sunflowers.jpg", "MD5"]
     = 109937059621979839952736809235486742106

    >> FileHash["ExampleData/sunflowers.jpg", "Adler32"]
     = 1607049478

    >> FileHash["ExampleData/sunflowers.jpg", "SHA256"]
     = 111619807552579450300684600241129773909359865098672286468229443390003894913065

    #> FileHash["ExampleData/sunflowers.jpg", "CRC32"]
     = 933095683
    #> FileHash["ExampleData/sunflowers.jpg", "SHA"]
     = 851696818771101405642332645949480848295550938123
    #> FileHash["ExampleData/sunflowers.jpg", "SHA224"]
     = 8723805623766373862936267623913366865806344065103917676078120867011
    #> FileHash["ExampleData/sunflowers.jpg", "SHA384"]
     = 28288410602533803613059815846847184383722061845493818218404754864571944356226472174056863474016709057507799332611860
    #> FileHash["ExampleData/sunflowers.jpg", "SHA512"]
     = 10111462070211820348006107532340854103555369343736736045463376555356986226454343186097958657445421102793096729074874292511750542388324853755795387877480102

    #> FileHash["ExampleData/sunflowers.jpg", xyzsymbol]
     = FileHash[ExampleData/sunflowers.jpg, xyzsymbol]
    #> FileHash["ExampleData/sunflowers.jpg", "xyzstr"]
     = FileHash[ExampleData/sunflowers.jpg, xyzstr, Integer]
    #> FileHash[xyzsymbol]
     = FileHash[xyzsymbol]
    """

    rules = {
        "FileHash[filename_String]": 'FileHash[filename, "MD5", "Integer"]',
        "FileHash[filename_String, hashtype_String]": 'FileHash[filename, hashtype, "Integer"]',
    }

    attributes = ("Protected", "ReadProtected")

    def apply(self, filename, hashtype, format, evaluation):
        "FileHash[filename_String, hashtype_String, format_String]"
        py_filename = filename.get_string_value()

        try:
            with mathics_open(py_filename, "rb") as f:
                dump = f.read()
        except IOError:
            evaluation.message("General", "noopen", filename)
            return
        except MessageException as e:
            e.message(evaluation)
            return

        return Hash.compute(
            lambda update: update(dump),
            hashtype.get_string_value(),
            format.get_string_value(),
        )


class FileInformation(Builtin):
    """
    <dl>
    <dt>'FileInformation["$file$"]'
      <dd>returns information about $file$.
    </dl>

    This function is totally undocumented in MMA!

    >> FileInformation["ExampleData/sunflowers.jpg"]
     = {File -> ..., FileType -> File, ByteCount -> 142286, Date -> ...}

    #> FileInformation["ExampleData/missing_file.jpg"]
     = {}
    """

    rules = {
        "FileInformation[name_String]": "If[FileExistsQ[name], {File -> ExpandFileName[name], FileType -> FileType[name], ByteCount -> FileByteCount[name], Date -> AbsoluteTime[FileDate[name]]}, {}]",
    }


class FileNameDepth(Builtin):
    """
    <dl>
    <dt>'FileNameDepth["$name$"]'
      <dd>gives the number of path parts in the given filename.
    </dl>

    >> FileNameDepth["a/b/c"]
     = 3

    >> FileNameDepth["a/b/c/"]
     = 3

    #> FileNameDepth[x]
     = FileNameDepth[x]

    #> FileNameDepth[$RootDirectory]
     = 0
    """

    attributes = "Protected"

    options = {
        "OperatingSystem": "$OperatingSystem",
    }

    rules = {
        "FileNameDepth[name_String]": "Length[FileNameSplit[name]]",
    }


class FileNameJoin(Builtin):
    """
    <dl>
      <dt>'FileNameJoin[{"$dir_1$", "$dir_2$", ...}]'
      <dd>joins the $dir_i$ together into one path.

      <dt>'FileNameJoin[..., OperatingSystem->"os"]'
      <dd>yields a file name in the format for the specified operating system. Possible choices are "Windows", "MacOSX", and "Unix".
    </dl>

    >> FileNameJoin[{"dir1", "dir2", "dir3"}]
     = ...

    >> FileNameJoin[{"dir1", "dir2", "dir3"}, OperatingSystem -> "Unix"]
     = dir1/dir2/dir3

    >> FileNameJoin[{"dir1", "dir2", "dir3"}, OperatingSystem -> "Windows"]
     = dir1\\dir2\\dir3
    """

    attributes = "Protected"

    options = {
        "OperatingSystem": "$OperatingSystem",
    }

    messages = {
        "ostype": (
            "The value of option OperatingSystem -> `1` "
            'must be one of "MacOSX", "Windows", or "Unix".'
        ),
    }

    def apply(self, pathlist, evaluation, options):
        "FileNameJoin[pathlist_?ListQ, OptionsPattern[FileNameJoin]]"

        py_pathlist = pathlist.to_python()
        if not all(isinstance(p, str) and p[0] == p[-1] == '"' for p in py_pathlist):
            return
        py_pathlist = [p[1:-1] for p in py_pathlist]

        operating_system = (
            options["System`OperatingSystem"].evaluate(evaluation).get_string_value()
        )

        if operating_system not in ["MacOSX", "Windows", "Unix"]:
            evaluation.message(
                "FileNameSplit", "ostype", options["System`OperatingSystem"]
            )
            if os.name == "posix":
                operating_system = "Unix"
            elif os.name == "nt":
                operating_system = "Windows"
            elif os.name == "os2":
                operating_system = "MacOSX"
            else:
                return

        if operating_system in ("Unix", "MacOSX"):
            import posixpath

            result = posixpath.join(*py_pathlist)
        elif operating_system in ("Windows",):
            import ntpath

            result = ntpath.join(*py_pathlist)
        else:
            result = osp.join(*py_pathlist)

        return from_python(result)


class FileType(Builtin):
    """
    <dl>
    <dt>'FileType["$file$"]'
      <dd>gives the type of a file, a string. This is typically 'File', 'Directory' or 'None'.
    </dl>

    >> FileType["ExampleData/sunflowers.jpg"]
     = File
    >> FileType["ExampleData"]
     = Directory
    >> FileType["ExampleData/nonexistant"]
     = None

    #> FileType[x]
     : File specification x is not a string of one or more characters.
     = FileType[x]
    """

    messages = {
        "fstr": (
            "File specification `1` is not a string of " "one or more characters."
        ),
    }

    attributes = "Protected"

    def apply(self, filename, evaluation):
        "FileType[filename_]"
        if not isinstance(filename, String):
            evaluation.message("FileType", "fstr", filename)
            return
        path = filename.to_python()[1:-1]

        path = path_search(path)

        if path is None:
            return Symbol("None")

        if osp.isfile(path):
            return Symbol("File")
        else:
            return Symbol("Directory")


class FindFile(Builtin):
    """
    <dl>
    <dt>'FindFile[$name$]'
      <dd>searches '$Path' for the given filename.
    </dl>

    >> FindFile["ExampleData/sunflowers.jpg"]
     = ...

    >> FindFile["VectorAnalysis`"]
     = ...

    >> FindFile["VectorAnalysis`VectorAnalysis`"]
     = ...

    #> FindFile["SomeTypoPackage`"]
     = $Failed
    """

    attributes = "Protected"

    messages = {
        "string": "String expected at position 1 in `1`.",
    }

    def apply(self, name, evaluation):
        "FindFile[name_]"

        py_name = name.to_python()

        if not (isinstance(py_name, str) and py_name[0] == py_name[-1] == '"'):
            evaluation.message("FindFile", "string", Expression("FindFile", name))
            return
        py_name = py_name[1:-1]

        result = path_search(py_name)

        if result is None:
            return SymbolFailed

        return String(osp.abspath(result))


class FileNames(Builtin):
    r"""
    <dl>
    <dt>'FileNames[]'
        <dd>Returns a list with the filenames in the current working folder.
    <dt>'FileNames[$form$]'
        <dd>Returns a list with the filenames in the current working folder that matches with $form$.
    <dt>'FileNames[{$form_1$, $form_2$, ...}]'
        <dd>Returns a list with the filenames in the current working folder that matches with one of $form_1$, $form_2$, ....
    <dt>'FileNames[{$form_1$, $form_2$, ...},{$dir_1$, $dir_2$, ...}]'
        <dd>Looks into the directories $dir_1$, $dir_2$, ....
    <dt>'FileNames[{$form_1$, $form_2$, ...},{$dir_1$, $dir_2$, ...}]'
        <dd>Looks into the directories $dir_1$, $dir_2$, ....
    <dt>'FileNames[{$forms$, $dirs$, $n$]'
        <dd>Look for files up to the level $n$.
    </dl>

    >> SetDirectory[$InstallationDirectory <> "/autoload"];
    >> FileNames["*.m", "formats"]//Length
     = ...
    >> FileNames["*.m", "formats", 3]//Length
     = ...
    >> FileNames["*.m", "formats", Infinity]//Length
     = ...
    """
    # >> FileNames[]//Length
    #  = 2
    fmtmaps = {Symbol("System`All"): "*"}
    options = {
        "IgnoreCase": "Automatic",
    }

    messages = {
        "nofmtstr": "`1` is not a format or a list of formats.",
        "nodirstr": "`1` is not a directory name  or a list of directory names.",
        "badn": "`1` is not an integer number.",
    }

    def apply_0(self, evaluation, **options):
        """FileNames[OptionsPattern[FileNames]]"""
        return self.apply_3(
            String("*"), String(os.getcwd()), None, evaluation, **options
        )

    def apply_1(self, forms, evaluation, **options):
        """FileNames[forms_, OptionsPattern[FileNames]]"""
        return self.apply_3(forms, String(os.getcwd()), None, evaluation, **options)

    def apply_2(self, forms, paths, evaluation, **options):
        """FileNames[forms_, paths_, OptionsPattern[FileNames]]"""
        return self.apply_3(forms, paths, None, evaluation, **options)

    def apply_3(self, forms, paths, n, evaluation, **options):
        """FileNames[forms_, paths_, n_, OptionsPattern[FileNames]]"""
        filenames = set()
        # Building a list of forms
        if forms.get_head_name() == "System`List":
            str_forms = []
            for p in forms._leaves:
                if self.fmtmaps.get(p, None):
                    str_forms.append(self.fmtmaps[p])
                else:
                    str_forms.append(p)
        else:
            str_forms = [
                self.fmtmaps[forms] if self.fmtmaps.get(forms, None) else forms
            ]
        # Building a list of directories
        if paths.get_head_name() == "System`String":
            str_paths = [paths.value]
        elif paths.get_head_name() == "System`List":
            str_paths = []
            for p in paths._leaves:
                if p.get_head_name() == "System`String":
                    str_paths.append(p.value)
                else:
                    evaluation.message("FileNames", "nodirstr", paths)
                    return
        else:
            evaluation.message("FileNames", "nodirstr", paths)
            return

        if n is not None:
            if n.get_head_name() == "System`Integer":
                n = n.get_int_value()
            elif n.get_head_name() == "System`DirectedInfinity":
                n = None
            else:
                evaluation.message("FileNames", "badn", n)
                return
        else:
            n = 1

        # list the files
        if options.get("System`IgnoreCase", None) == SymbolTrue:
            patterns = [
                re.compile(
                    "^" + to_regex(p, evaluation, abbreviated_patterns=True),
                    re.IGNORECASE,
                )
                + "$"
                for p in str_forms
            ]
        else:
            patterns = [
                re.compile(
                    "^" + to_regex(p, evaluation, abbreviated_patterns=True) + "$"
                )
                for p in str_forms
            ]

        for path in str_paths:
            if not osp.isdir(path):
                continue
            if n == 1:
                for fn in os.listdir(path):
                    fullname = osp.join(path, fn)
                    for pattern in patterns:
                        if pattern.match(fn):
                            filenames.add(fullname)
                            break
            else:
                pathlen = len(path)
                for root, dirs, files in os.walk(path):
                    # FIXME: This is an ugly and inefficient way
                    # to avoid looking deeper than the level n, but I do not realize
                    # how to do this better without a lot of code...
                    if n is not None and len(root[pathlen:].split(osp.sep)) > n:
                        continue
                    for fn in files + dirs:
                        for pattern in patterns:
                            if pattern.match(fn):
                                filenames.add(osp.join(root, fn))
                                break

        return Expression("List", *[String(s) for s in sorted(filenames)])


class FileNameSplit(Builtin):
    """
    <dl>
      <dt>'FileNameSplit["$filenames$"]'
      <dd>splits a $filename$ into a list of parts.
    </dl>

    >> FileNameSplit["example/path/file.txt"]
     = {example, path, file.txt}

    #> FileNameSplit["example/path", OperatingSystem -> x]
     : The value of option OperatingSystem -> x must be one of "MacOSX", "Windows", or "Unix".
     = {example, path}
    """

    attributes = "Protected"

    options = {
        "OperatingSystem": "$OperatingSystem",
    }

    messages = {
        "ostype": (
            "The value of option OperatingSystem -> `1` "
            'must be one of "MacOSX", "Windows", or "Unix".'
        ),
    }

    def apply(self, filename, evaluation, options):
        "FileNameSplit[filename_String, OptionsPattern[FileNameSplit]]"

        path = filename.to_python()[1:-1]

        operating_system = (
            options["System`OperatingSystem"].evaluate(evaluation).to_python()
        )

        if operating_system not in ['"MacOSX"', '"Windows"', '"Unix"']:
            evaluation.message(
                "FileNameSplit", "ostype", options["System`OperatingSystem"]
            )
            if os.name == "posix":
                operating_system = "Unix"
            elif os.name == "nt":
                operating_system = "Windows"
            elif os.name == "os2":
                operating_system = "MacOSX"
            else:
                return

        # TODO Implement OperatingSystem Option

        result = []
        while path not in ["", SYS_ROOT_DIR]:
            path, ext = osp.split(path)
            if ext != "":
                result.insert(0, ext)

        return from_python(result)


class FileNameTake(Builtin):
    """
    <dl>
      <dt>'FileNameTake["$file$"]'
      <dd>returns the last path element in the file name $name$.

      <dt>'FileNameTake["$file$", $n$]'
      <dd>returns the first $n$ path elements in the file name $name$.

      <dt>'FileNameTake["$file$", $-n$]'
      <dd>returns the last $n$ path elements in the file name $name$.
    </dl>

    """

    # mmatura: please put in a pytest
    # >> FileNameTake["/tmp/file.txt"]
    #  = file.txt
    # >> FileNameTake["tmp/file.txt", 1]
    #  = tmp
    # >> FileNameTake["tmp/file.txt", -1]
    #  = file.txt

    attributes = "Protected"

    options = {
        "OperatingSystem": "$OperatingSystem",
    }

    def apply(self, filename, evaluation, options):
        "FileNameTake[filename_String, OptionsPattern[FileBaseName]]"
        path = pathlib.Path(filename.to_python()[1:-1])
        return from_python(path.name)

    def apply_n(self, filename, n, evaluation, options):
        "FileNameTake[filename_String, n_Integer, OptionsPattern[FileBaseName]]"
        n_int = n.get_int_value()
        parts = pathlib.Path(filename.to_python()[1:-1]).parts
        if n_int >= 0:
            subparts = parts[:n_int]
        else:
            subparts = parts[n_int:]
        return from_python(str(pathlib.PurePath(*subparts)))


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

    >> stream = FindList["ExampleData/EinsteinSzilLetter.txt", "uranium"];
    #> Length[stream]
     = 7

    >> FindList["ExampleData/EinsteinSzilLetter.txt", "uranium", 1]
     = {in manuscript, leads me to expect that the element uranium may be turned into}

    #> FindList["ExampleData/EinsteinSzilLetter.txt", "project"]
     = {}

    #> FindList["ExampleData/EinsteinSzilLetter.txt", "uranium", 0]
     = $Failed
    """

    messages = {
        "strs": "String or non-empty list of strings expected at position `1` in `2`.",
        "intnm": "Non-negative machine-sized integer expected at position `1` in `2`.",
    }

    attributes = "Protected"

    options = {
        "AnchoredSearch": "False",
        "IgnoreCase": "False",
        "RecordSeparators": '{"\r\n", "\n", "\r"}',
        "WordSearch": "False",
        "WordSeparators": '{" ", "\t"}',
    }

    # TODO: Extra options AnchoredSearch, IgnoreCase RecordSeparators,
    # WordSearch, WordSeparators this is probably best done with a regex

    def apply_without_n(self, filename, text, evaluation, options):
        "FindList[filename_, text_, OptionsPattern[FindList]]"
        return self.apply(filename, text, None, evaluation, options)

    def apply(self, filename, text, n, evaluation, options):
        "FindList[filename_, text_, n_, OptionsPattern[FindList]]"
        py_text = text.to_python()
        py_name = filename.to_python()
        if n is None:
            py_n = None
            expr = Expression("FindList", filename, text)
        else:
            py_n = n.to_python()
            expr = Expression("FindList", filename, text, n)

        if not isinstance(py_text, list):
            py_text = [py_text]

        if not isinstance(py_name, list):
            py_name = [py_name]

        if not all(isinstance(t, str) and t[0] == t[-1] == '"' for t in py_name):
            evaluation.message("FindList", "strs", "1", expr)
            return SymbolFailed

        if not all(isinstance(t, str) and t[0] == t[-1] == '"' for t in py_text):
            evaluation.message("FindList", "strs", "2", expr)
            return SymbolFailed

        if not ((isinstance(py_n, int) and py_n >= 0) or py_n is None):
            evaluation.message("FindList", "intnm", "3", expr)
            return SymbolFailed

        if py_n == 0:
            return SymbolFailed

        py_text = [t[1:-1] for t in py_text]
        py_name = [t[1:-1] for t in py_name]

        results = []
        for path in py_name:
            try:
                with mathics_open(path, "r") as f:
                    lines = f.readlines()
            except IOError:
                evaluation.message("General", "noopen", path)
                return
            except MessageException as e:
                e.message(evaluation)
                return

            result = []
            for line in lines:
                for t in py_text:
                    if line.find(t) != -1:
                        result.append(line[:-1])
            results.append(result)

        results = [r for result in results for r in result]

        if isinstance(py_n, int):
            results = results[: min(py_n, len(results))]

        return from_python(results)


class HomeDirectory(Predefined):
    """
    <dl>
    <dt>'$HomeDirectory'
      <dd>returns the users HOME directory.
    </dl>

    >> $HomeDirectory
     = ...
    """

    name = "$HomeDirectory"

    attributes = "Protected"

    def evaluate(self, evaluation):
        global HOME_DIR
        return String(HOME_DIR)


class InitialDirectory(Predefined):
    """
    <dl>
    <dt>'$InitialDirectory'
      <dd>returns the directory from which \\Mathics was started.
    </dl>

    >> $InitialDirectory
     = ...
    """

    name = "$InitialDirectory"

    def evaluate(self, evaluation):
        global INITIAL_DIR
        return String(INITIAL_DIR)


class InstallationDirectory(Predefined):
    """
    <dl>
      <dt>'$InstallationDirectory'
      <dd>returns the top-level directory in which \\Mathics was installed.
    </dl>
    >> $InstallationDirectory
     = ...
    """

    attributes = ("Unprotected",)
    name = "$InstallationDirectory"

    def evaluate(self, evaluation):
        global ROOT_DIR
        return String(ROOT_DIR)


class Needs(Builtin):
    """
    <dl>
    <dt>'Needs["context`"]'
        <dd>loads the specified context if not already in '$Packages'.
    </dl>

    >> Needs["VectorAnalysis`"]
    #> Needs["VectorAnalysis`"]

    #> Needs["SomeFakePackageOrTypo`"]
     : Cannot open SomeFakePackageOrTypo`.
     : Context SomeFakePackageOrTypo` was not created when Needs was evaluated.
     = $Failed

    #> Needs["VectorAnalysis"]
     : Invalid context specified at position 1 in Needs[VectorAnalysis]. A context must consist of valid symbol names separated by and ending with `.
     = Needs[VectorAnalysis]

    ## --- VectorAnalysis ---

    #> Needs["VectorAnalysis`"]

    #> DotProduct[{1,2,3}, {4,5,6}]
     = 32
    #> DotProduct[{-1.4, 0.6, 0.2}, {0.1, 0.6, 1.7}]
     = 0.56

    #> CrossProduct[{1,2,3}, {4,5,6}]
     = {-3, 6, -3}
    #> CrossProduct[{-1.4, 0.6, 0.2}, {0.1, 0.6, 1.7}]
     = {0.9, 2.4, -0.9}

    #> ScalarTripleProduct[{-2,3,1},{0,4,0},{-1,3,3}]
     = -20
    #> ScalarTripleProduct[{-1.4,0.6,0.2}, {0.1,0.6,1.7}, {0.7,-1.5,-0.2}]
     = -2.79

    #> CoordinatesToCartesian[{2, Pi, 3}, Spherical]
     = {0, 0, -2}
    #> CoordinatesFromCartesian[%, Spherical]
     = {2, Pi, 0}
    #> CoordinatesToCartesian[{2, Pi, 3}, Cylindrical]
     = {-2, 0, 3}
    #> CoordinatesFromCartesian[%, Cylindrical]
     = {2, Pi, 3}
    ## Needs Sin/Cos exact value (PR #100) for these tests to pass
    ## #> CoordinatesToCartesian[{2, Pi / 4, Pi / 3}, Spherical]
    ##  = {Sqrt[2] / 2, Sqrt[6] / 2, Sqrt[2]}
    ## #> CoordinatesFromCartesian[%, Spherical]
    ##  = {2, Pi / 4, Pi / 3}
    ## #> CoordinatesToCartesian[{2, Pi / 4, -1}, Cylindrical]
    ##  = {Sqrt[2], Sqrt[2], -1}
    ## #> CoordinatesFromCartesian[%, Cylindrical]
    ##  = {2, Pi / 4, -1}
    #> CoordinatesToCartesian[{0.27, 0.51, 0.92}, Cylindrical]
     = {0.235641, 0.131808, 0.92}
    #> CoordinatesToCartesian[{0.27, 0.51, 0.92}, Spherical]
     = {0.0798519, 0.104867, 0.235641}

    #> Coordinates[]
     = {Xx, Yy, Zz}
    #> Coordinates[Spherical]
     = {Rr, Ttheta, Pphi}
    #> SetCoordinates[Cylindrical]
     = Cylindrical[Rr, Ttheta, Zz]
    #> Coordinates[]
     = {Rr, Ttheta, Zz}
    #> CoordinateSystem
     = Cylindrical
    #> Parameters[]
     = {}
    #> CoordinateRanges[]
    ## = {0 <= Rr < Infinity, -Pi < Ttheta <= Pi, -Infinity < Zz < Infinity}
     = {0 <= Rr && Rr < Infinity, -Pi < Ttheta && Ttheta <= Pi, -Infinity < Zz < Infinity}
    #> CoordinateRanges[Cartesian]
     = {-Infinity < Xx < Infinity, -Infinity < Yy < Infinity, -Infinity < Zz < Infinity}
    #> ScaleFactors[Cartesian]
     = {1, 1, 1}
    #> ScaleFactors[Spherical]
     = {1, Rr, Rr Sin[Ttheta]}
    #> ScaleFactors[Cylindrical]
     = {1, Rr, 1}
    #> ScaleFactors[{2, 1, 3}, Cylindrical]
     = {1, 2, 1}
    #> JacobianDeterminant[Cartesian]
     = 1
    #> JacobianDeterminant[Spherical]
     = Rr ^ 2 Sin[Ttheta]
    #> JacobianDeterminant[Cylindrical]
     = Rr
    #> JacobianDeterminant[{2, 1, 3}, Cylindrical]
     = 2
    #> JacobianMatrix[Cartesian]
     = {{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}
    #> JacobianMatrix[Spherical]
     = {{Cos[Pphi] Sin[Ttheta], Rr Cos[Pphi] Cos[Ttheta], -Rr Sin[Pphi] Sin[Ttheta]}, {Sin[Pphi] Sin[Ttheta], Rr Cos[Ttheta] Sin[Pphi], Rr Cos[Pphi] Sin[Ttheta]}, {Cos[Ttheta], -Rr Sin[Ttheta], 0}}
    #> JacobianMatrix[Cylindrical]
     = {{Cos[Ttheta], -Rr Sin[Ttheta], 0}, {Sin[Ttheta], Rr Cos[Ttheta], 0}, {0, 0, 1}}
    """

    messages = {
        "ctx": (
            "Invalid context specified at position `2` in `1`. "
            "A context must consist of valid symbol names separated by "
            "and ending with `3`."
        ),
        "nocont": "Context `1` was not created when Needs was evaluated.",
    }

    def apply(self, context, evaluation):
        "Needs[context_String]"
        contextstr = context.get_string_value()
        if contextstr == "":
            return SymbolNull
        if contextstr[0] == "`":
            curr_ctxt = evaluation.definitions.get_current_context()
            contextstr = curr_ctxt + contextstr[1:]
            context = String(contextstr)
        if not valid_context_name(contextstr):
            evaluation.message("Needs", "ctx", Expression("Needs", context), 1, "`")
            return
        test_loaded = Expression("MemberQ", Symbol("$Packages"), context)
        test_loaded = test_loaded.evaluate(evaluation)
        if test_loaded.is_true():
            # Already loaded
            return SymbolNull
        result = Expression("Get", context).evaluate(evaluation)

        if result == SymbolFailed:
            evaluation.message("Needs", "nocont", context)
            return SymbolFailed

        return SymbolNull


class OperatingSystem(Predefined):
    """
    <dl>
    <dt>'$OperatingSystem'
      <dd>gives the type of operating system running Mathics.
    </dl>

    >> $OperatingSystem
     = ...
    """

    attributes = ("Locked", "Protected")
    name = "$OperatingSystem"

    def evaluate(self, evaluation):
        if os.name == "posix":
            return String("Unix")
        elif os.name == "nt":
            return String("Windows")
        elif os.name == "os2":
            return String("MacOSX")
        else:
            return String("Unknown")


class ParentDirectory(Builtin):
    """
    <dl>
    <dt>'ParentDirectory[]'
      <dd>returns the parent of the current working directory.
    <dt>'ParentDirectory["$dir$"]'
      <dd>returns the parent $dir$.
    </dl>

    >> ParentDirectory[]
     = ...
    """

    rules = {
        "ParentDirectory[]": "ParentDirectory[Directory[]]",
    }

    messages = {
        "fstr": (
            "File specification `1` is not a string of " "one or more characters."
        ),
    }

    attributes = "Protected"

    def apply(self, path, evaluation):
        "ParentDirectory[path_]"

        if not isinstance(path, String):
            evaluation.message("ParentDirectory", "fstr", path)
            return

        pypath = path.to_python()[1:-1]

        result = osp.abspath(osp.join(pypath, osp.pardir))
        return String(result)


class Path(Predefined):
    """
    <dl>
    <dt>'$Path'
      <dd>returns the list of directories to search when looking for a file.
    </dl>

    >> $Path
     = ...
    """

    attributes = ("Unprotected",)
    name = "$Path"

    def evaluate(self, evaluation):
        return Expression(SymbolList, *[String(p) for p in PATH_VAR])


class PathnameSeparator(Predefined):
    """
    <dl>
    <dt>'$PathnameSeparator'
      <dd>returns a string for the seperator in paths.
    </dl>

    >> $PathnameSeparator
     = ...
    """

    name = "$PathnameSeparator"

    def evaluate(self, evaluation):
        return String(os.sep)


class RenameDirectory(Builtin):
    """
    <dl>
    <dt>'RenameDirectory["$dir1$", "$dir2$"]'
      <dd>renames directory $dir1$ to $dir2$.
    </dl>
    """

    attributes = "Protected"

    messages = {
        "argr": "called with `1` argument; 2 arguments are expected.",
        "fstr": (
            "File specification `1` is not a string of " "one or more characters."
        ),
        "filex": "Cannot overwrite existing file `1`.",
        "nodir": "Directory `1` not found.",
    }

    def apply(self, dirs, evaluation):
        "RenameDirectory[dirs__]"

        seq = dirs.get_sequence()
        if len(seq) != 2:
            evaluation.message("RenameDirectory", "argr", len(seq))
            return
        (dir1, dir2) = (s.to_python() for s in seq)

        if not (isinstance(dir1, str) and dir1[0] == dir1[-1] == '"'):
            evaluation.message("RenameDirectory", "fstr", seq[0])
            return
        dir1 = dir1[1:-1]

        if not (isinstance(dir2, str) and dir2[0] == dir2[-1] == '"'):
            evaluation.message("RenameDirectory", "fstr", seq[1])
            return
        dir2 = dir2[1:-1]

        if not osp.isdir(dir1):
            evaluation.message("RenameDirectory", "nodir", seq[0])
            return SymbolFailed
        if osp.isdir(dir2):
            evaluation.message("RenameDirectory", "filex", seq[1])
            return SymbolFailed

        shutil.move(dir1, dir2)

        return String(osp.abspath(dir2))


class RenameFile(Builtin):
    """
    <dl>
    <dt>'RenameFile["$file1$", "$file2$"]'
      <dd>renames $file1$ to $file2$.
    </dl>

    >> CopyFile["ExampleData/sunflowers.jpg", "MathicsSunflowers.jpg"]
     = MathicsSunflowers.jpg
    >> RenameFile["MathicsSunflowers.jpg", "MathicsSunnyFlowers.jpg"]
     = MathicsSunnyFlowers.jpg
    >> DeleteFile["MathicsSunnyFlowers.jpg"]
    """

    messages = {
        "filex": "Cannot overwrite existing file `1`.",
        "fstr": (
            "File specification `1` is not a string of " "one or more characters."
        ),
        "nffil": "File not found during `1`.",
    }

    attributes = "Protected"

    def apply(self, source, dest, evaluation):
        "RenameFile[source_, dest_]"

        py_source = source.to_python()
        py_dest = dest.to_python()

        # Check filenames
        if not (isinstance(py_source, str) and py_source[0] == py_source[-1] == '"'):
            evaluation.message("RenameFile", "fstr", source)
            return
        if not (isinstance(py_dest, str) and py_dest[0] == py_dest[-1] == '"'):
            evaluation.message("RenameFile", "fstr", dest)
            return

        py_source = py_source[1:-1]
        py_dest = py_dest[1:-1]

        py_source = path_search(py_source)

        if py_source is None:
            evaluation.message("RenameFile", "filex", source)
            return SymbolFailed

        if osp.exists(py_dest):
            evaluation.message("RenameFile", "filex", dest)
            return SymbolFailed

        try:
            shutil.move(py_source, py_dest)
        except IOError:
            evaluation.message("RenameFile", "nffil", dest)
            return SymbolFailed

        return dest


class ResetDirectory(Builtin):
    """
    <dl>
    <dt>'ResetDirectory[]'
      <dd>pops a directory from the directory stack and returns it.
    </dl>

    >> ResetDirectory[]
    = ...
    """

    messages = {
        "dtop": "Directory stack is empty.",
    }

    attributes = "Protected"

    def apply(self, evaluation):
        "ResetDirectory[]"
        try:
            tmp = DIRECTORY_STACK.pop()
        except IndexError:
            tmp = os.getcwd()
            evaluation.message("ResetDirectory", "dtop")
        else:
            os.chdir(tmp)
        return String(tmp)


class RootDirectory(Predefined):
    """
    <dl>
    <dt>'$RootDirectory'
      <dd>returns the system root directory.
    </dl>

    >> $RootDirectory
     = ...
    """

    name = "$RootDirectory"

    attributes = "Protected"

    def evaluate(self, evaluation):
        global SYS_ROOT_DIR
        return String(SYS_ROOT_DIR)


class SetDirectory(Builtin):
    """
    <dl>
    <dt>'SetDirectory[$dir$]'
      <dd>sets the current working directory to $dir$.
    </dl>

    S> SetDirectory[]
    = ...

    #> SetDirectory["MathicsNonExample"]
     : Cannot set current directory to MathicsNonExample.
     = $Failed
    """

    rules = {
        "SetDirectory[]": "SetDirectory[$HomeDirectory]",
    }

    messages = {
        "fstr": (
            "File specification `1` is not a string of " "one or more characters."
        ),
        "cdir": "Cannot set current directory to `1`.",
    }

    attributes = "Protected"

    def apply(self, path, evaluation):
        "SetDirectory[path_]"

        if not isinstance(path, String):
            evaluation.message("SetDirectory", "fstr", path)
            return

        py_path = path.__str__()[1:-1]

        if py_path is None or not osp.isdir(py_path):
            evaluation.message("SetDirectory", "cdir", path)
            return SymbolFailed

        try:
            os.chdir(py_path)
        except:
            return SymbolFailed

        DIRECTORY_STACK.append(os.getcwd())
        return String(os.getcwd())


class SetFileDate(Builtin):
    """
    <dl>
    <dt>'SetFileDate["$file$"]'
      <dd>set the file access and modification dates of $file$ to the current date.
    <dt>'SetFileDate["$file$", $date$]'
      <dd>set the file access and modification dates of $file$ to the specified date list.
    <dt>'SetFileDate["$file$", $date$, "$type$"]'
      <dd>set the file date of $file$ to the specified date list.
      The "$type$" can be one of "$Access$", "$Creation$", "$Modification$", or 'All'.
    </dl>

    Create a temporary file (for example purposes)
    >> tmpfilename = $TemporaryDirectory <> "/tmp0";
    >> Close[OpenWrite[tmpfilename]];

    >> SetFileDate[tmpfilename, {2002, 1, 1, 0, 0, 0.}, "Access"];

    >> FileDate[tmpfilename, "Access"]
     = {2002, 1, 1, 0, 0, 0.}

    #> SetFileDate[tmpfilename, {2002, 1, 1, 0, 0, 0.}];
    #> FileDate[tmpfilename, "Access"]
     = {2002, 1, 1, 0, 0, 0.}

    #> SetFileDate[tmpfilename]
    #> FileDate[tmpfilename, "Access"]
     = {...}

    #> DeleteFile[tmpfilename]

    #> SetFileDate["MathicsNonExample"]
     : File not found during SetFileDate[MathicsNonExample].
     = $Failed
    """

    messages = {
        "fstr": (
            "File specification `1` is not a string of one or " "more characters."
        ),
        "nffil": "File not found during `1`.",
        "fdate": (
            "Date specification should be either the number of seconds "
            "since January 1, 1900 or a {y, m, d, h, m, s} list."
        ),
        "datetype": (
            'Date type a should be "Access", "Modification", '
            '"Creation" (Windows only), or All.'
        ),
        "nocreationunix": (
            "The Creation date of a file cannot be set on " "Macintosh or Unix."
        ),
    }

    attributes = "Protected"

    def apply(self, filename, datelist, attribute, evaluation):
        "SetFileDate[filename_, datelist_, attribute_]"

        py_filename = filename.to_python()

        if datelist is None:
            py_datelist = Expression("DateList").evaluate(evaluation).to_python()
            expr = Expression("SetFileDate", filename)
        else:
            py_datelist = datelist.to_python()

        if attribute is None:
            py_attr = "All"
            if datelist is not None:
                expr = Expression("SetFileDate", filename, datelist)
        else:
            py_attr = attribute.to_python()
            expr = Expression("SetFileDate", filename, datelist, attribute)

        # Check filename
        if not (
            isinstance(py_filename, str) and py_filename[0] == py_filename[-1] == '"'
        ):
            evaluation.message("SetFileDate", "fstr", filename)
            return
        py_filename = path_search(py_filename[1:-1])

        if py_filename is None:
            evaluation.message("SetFileDate", "nffil", expr)
            return SymbolFailed

        # Check datelist
        if not (
            isinstance(py_datelist, list)
            and len(py_datelist) == 6
            and all(isinstance(d, int) for d in py_datelist[:-1])
            and isinstance(py_datelist[-1], float)
        ):
            evaluation.message("SetFileDate", "fdate", expr)

        # Check attribute
        if py_attr not in ['"Access"', '"Creation"', '"Modification"', "All"]:
            evaluation.message("SetFileDate", "datetype")
            return

        epochtime = (
            Expression("AbsoluteTime", time.strftime("%Y-%m-%d %H:%M", time.gmtime(0)))
            .evaluate(evaluation)
            .to_python()
        )

        stattime = Expression("AbsoluteTime", from_python(py_datelist))
        stattime = stattime.to_python(n_evaluation=evaluation)

        stattime -= epochtime

        try:
            os.stat(py_filename)
            if py_attr == '"Access"':
                os.utime(py_filename, (stattime, osp.getatime(py_filename)))
            if py_attr == '"Creation"':
                if os.name == "posix":
                    evaluation.message("SetFileDate", "nocreationunix")
                    return SymbolFailed
                else:
                    # TODO: Note: This is windows only
                    return SymbolFailed
            if py_attr == '"Modification"':
                os.utime(py_filename, (osp.getatime(py_filename), stattime))
            if py_attr == "All":
                os.utime(py_filename, (stattime, stattime))
        except OSError as e:
            # evaluation.message(...)
            return SymbolFailed

        return SymbolNull

    def apply_1arg(self, filename, evaluation):
        "SetFileDate[filename_]"
        return self.apply(filename, None, None, evaluation)

    def apply_2arg(self, filename, datelist, evaluation):
        "SetFileDate[filename_, datelist_]"
        return self.apply(filename, datelist, None, evaluation)


class TemporaryDirectory(Predefined):
    """
    <dl>
    <dt>'$TemporaryDirectory'
      <dd>returns the directory used for temporary files.
    </dl>

    >> $TemporaryDirectory
     = ...
    """

    name = "$TemporaryDirectory"

    def evaluate(self, evaluation):
        return String(TMP_DIR)


class ToFileName(Builtin):
    """
    <dl>
    <dt>'ToFileName[{"$dir_1$", "$dir_2$", ...}]'
      <dd>joins the $dir_i$ together into one path.
    </dl>

    'ToFileName' has been superseded by 'FileNameJoin'.

    >> ToFileName[{"dir1", "dir2"}, "file"]
     = dir1...dir2...file

    >> ToFileName["dir1", "file"]
     = dir1...file

    >> ToFileName[{"dir1", "dir2", "dir3"}]
     = dir1...dir2...dir3
    """

    rules = {
        "ToFileName[dir_String, name_String]": "FileNameJoin[{dir, name}]",
        "ToFileName[dirs_?ListQ, name_String]": "FileNameJoin[Append[dirs, name]]",
        "ToFileName[dirs_?ListQ]": "FileNameJoin[dirs]",
    }


class UserBaseDirectory(Predefined):
    """
    <dl>
    <dt>'$UserBaseDirectory'
      <dd>returns the folder where user configurations are stored.
    </dl>

    >> $RootDirectory
     = ...
    """

    name = "$UserBaseDirectory"

    attributes = "Protected"

    def evaluate(self, evaluation):
        global HOME_DIR
        return String(HOME_DIR + os.sep + ".mathics")


class URLSave(Builtin):
    """
    <dl>
    <dt>'URLSave["url"]'
        <dd>Save "url" in a temporary file.
    <dt>'URLSave["url", $filename$]'
        <dd>Save "url" in $filename$.
    </dl>
    """

    messages = {
        "invfile": "`1` is not a valid Filename",
        "invhttp": "`1` is not a valid URL",
    }

    def apply_1(self, url, evaluation, **options):
        "URLSave[url_String, OptionsPattern[URLSave]]"
        return self.apply_2(url, None, evaluation, **options)

    def apply_2(self, url, filename, evaluation, **options):
        "URLSave[url_String, filename_, OptionsPattern[URLSave]]"
        url = url.value
        if filename is None:
            result = urlsave_tmp(url, None, **options)
        elif filename.get_head_name() == "String":
            filename = filename.value
            result = urlsave_tmp(url, filename, **options)
        else:
            evaluation.message("URLSave", "invfile", filename)
            return SymbolFailed
        if result is None:
            return SymbolFailed
        return String(result)


# To placate import
ROOT_DIR, HOME_DIR
