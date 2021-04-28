# -*- coding: utf-8 -*-
from .helper import check_evaluation, session


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
    temp_filename = session.evaluate('$TemporaryDirectory<>"/testfile"').to_python()
    temp_filename_strip = temp_filename[1:-1]
    check_evaluation(f"40! >> {temp_filename_strip}", 'Null')
    check_evaluation(f"<< {temp_filename_strip}", '40!')
    check_evaluation(f"DeleteFile[{temp_filename}]", 'Null')

# I do not know what is it supposed to test with this...
# def test_Inputget_and_put():
#    stream = Expression('Plus', Symbol('x'), Integer(2))

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
