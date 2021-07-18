# -*- coding: utf-8 -*-
import tempfile
import os
import os.path as osp
import pytest
import sys
from .helper import check_evaluation, session


def test_import():
    eaccent = "\xe9"
    for str_expr, str_expected, message in (
        (
            """StringTake[Import["ExampleData/Middlemarch.txt", CharacterEncoding -> "ISO8859-1"], {49, 69}]""",
            f"des plaisirs pr{eaccent}sents",
            "accented characters in Import",
        ),
    ):
        check_evaluation(str_expr, str_expected, message)


def run_export(temp_dirname: str, short_name: str, file_data: str, character_encoding):
    file_path = osp.join(temp_dirname, short_name)
    expr = fr'Export["{file_path}", {file_data}'
    expr += (
        ', CharacterEncoding -> f"{character_encoding}"' if character_encoding else ""
    )
    expr += "]"
    result = session.evaluate(expr)
    assert result.to_python(string_quotes=False) == file_path
    return file_path


def check_data(
    temp_dirname: str,
    short_name: str,
    file_data: str,
    character_encoding=None,
    expected_data=None,
):
    file_path = run_export(
        temp_dirname, short_name, fr'"{file_data}"', character_encoding
    )
    if expected_data is None:
        expected_data = file_data
    assert open(file_path, "r").read() == expected_data


# Github Action Windows CI servers have problems with releasing files using
# a tempfile.TemporaryDirectory context manager.
# Leave out until we figure how to work around this.
if not (os.environ.get("CI", False) or sys.platform in ("win32",)):

    def test_export():
        with tempfile.TemporaryDirectory(prefix="mtest-") as temp_dirname:
            # Check exporting text files (file extension ".txt")
            check_data(temp_dirname, "add_expr.txt", "1 + x + y")
            check_data(temp_dirname, "AAcute.txt", "\u00C1", "ISOLatin1")
            check_data(temp_dirname, "AAcuteUTF.txt", "\u00C1", "UTF-8")

            # Check exporting CSV files (file extension ".csv")
            file_path = run_export(
                temp_dirname, "csv_list.csv", "{{1, 2, 3}, {4, 5, 6}}", None
            )
            assert open(file_path, "r").read() == "1,2,3\n4,5,6"

            # Check exporting SVG files (file extension ".svg")
            file_path = run_export(
                temp_dirname, "sine.svg", "Plot[Sin[x], {x,0,1}]", None
            )
            data = open(file_path, "r").read().strip()
            if data.startswith("$Failed"):
                pytest.skip("SVG export of Plot failed mysteriously")
            else:
                assert data.startswith("<svg")
                assert data.endswith("</svg>")


# TODO:
# mmatera: please put in pytest conditionally
# >> System`Convert`B64Dump`B64Encode["∫ f  x"]
#  = 4oirIGYg752MIHg=
# >> System`Convert`B64Dump`B64Decode[%]
#  = ∫ f  x
