# -*- coding: utf-8 -*-
from .helper import check_evaluation, session
from mathics.core.parser import parse, MathicsSingleLineFeeder
from mathics.core.definitions import Definitions
from mathics.core.evaluation import Evaluation
import pathlib
import sys


definitions = Definitions(add_builtin=True)
evaluation = Evaluation(definitions=definitions, catch_interrupt=False)




def _evaluate(str_expression):
    expr = parse(definitions, MathicsSingleLineFeeder(str_expression))
    return expr.evaluate(evaluation)


def test_put_and_get_and_InputFileName():
    check_evaluation('Put[HoldForm[$InputFileName], $TemporaryDirectory<>"/getme.m"]', 'Null')
    check_evaluation('Get[$TemporaryDirectory<>"/getme.m"]', '$TemporaryDirectory<>"/getme.m"')




def test_compress():
    for text in ("", "abc", " "):
        str_expr = f'Uncompress[Compress["{text}"]]'
        str_expected = f'"{text}"'
        check_evaluation(
            str_expr, str_expected, to_string_expr=False, to_string_expected=False
        )


def test_unprotected():
    for str_expr, str_expected, message in (
        ("Attributes[$Path]", "{}", ""),
        ("Attributes[$InstallationDirectory]", "{}", ""),
    ):
        check_evaluation(str_expr, str_expected, message)


def test_get_and_put():
    temp_directory = _evaluate("$TemporaryDirectory").to_python()
    if len(temp_directory) < 3:
        return
    temp_directory = temp_directory[1:-1]
    temp_filename = str(pathlib.Path(temp_directory, "testfile"))
    print(temp_filename)
    result = _evaluate(f"40! >> {temp_filename}").to_python()

    # This needs going over in Windows
    if sys.platform not in {
        "win32",
    }:
        assert result is None

        result = _evaluate(f"<< {temp_filename}")
        assert result == _evaluate("40!")

        result = _evaluate(f'DeleteFile["{temp_filename}"]').to_python()
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
