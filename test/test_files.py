# -*- coding: utf-8 -*-
from .helper import check_evaluation
from mathics.core.parser import parse, MathicsSingleLineFeeder
from mathics.core.definitions import Definitions
from mathics.core.evaluation import Evaluation
import pathlib
import os
import sys


definitions = Definitions(add_builtin=True)
evaluation = Evaluation(definitions=definitions, catch_interrupt=False)


def _evaluate(str_expression):
    expr = parse(definitions, MathicsSingleLineFeeder(str_expression))
    return expr.evaluate(evaluation)

# FIXME: see if we can refine this better such as
# by running some Python code and looking for a failure.
limited_characterset = sys.platform not in {"win32",} and not os.environ.get("CI")
if limited_characterset:
    def test_non_win32_compress():
        for str_expr, str_expected, message in (
            (
             r'Compress["―"]',
                '"eJxTetQwVQkABwMCPA=="',
                ""
             ),
            (r'Uncompress["eJxTetQwVQkABwMCPA=="]',
             r'"―"',
             ""
             ),
        ):
            check_evaluation(str_expr, str_expected, message)

def test_get_and_put():
    temp_directory = _evaluate("$TemporaryDirectory").to_python()
    if len(temp_directory)<3:
        return
    temp_directory = temp_directory[1:-1]
    temp_filename = str(pathlib.Path(temp_directory, "testfile"))
    print(temp_filename)
    result = _evaluate(f"40! >> {temp_filename}").to_python()

    # This needs going over in Windows
    if sys.platform not in {"win32",}:
        assert result is None

        result = _evaluate(f"<< {temp_filename}")
        assert result == _evaluate("40!")

        result = _evaluate(f"DeleteFile[\"{temp_filename}\"]").to_python()
        assert result is None


# TODO: add these Unix-specific test. Be sure not to test
# sys.platform for not Windows and to test for applicability
# ## writing to dir
# S> x >> /var/
#  : Cannot open /var/.
#  = x >> /var/

# ## writing to read only file
# S> x >> /proc/uptime
#  : Cannot open /proc/uptime.
#  = x >> /proc/uptime

# ## writing to full file
# S> x >> /dev/full
#  : No space left on device.

# #> WriteString[OpenWrite["/dev/zero"], "abc"]   (* Null *)
#     ## Return $Failed on special files
#     #> FilePrint["/dev/zero"]
#      = $Failed
#     #> FilePrint["/dev/random"]
#      = $Failed
#     #> FilePrint["/dev/null"]
#      = $Failed
