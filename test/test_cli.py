# -*- coding: utf-8 -*-
import subprocess

import os.path as osp
import re
import pytest
import sys


def get_testdir():
    filename = osp.normcase(osp.dirname(osp.abspath(__file__)))
    return osp.realpath(filename)


@pytest.mark.skipif(sys.version_info < (3, 7), reason="requires Python 3.7 or higher")
def test_cli():
    script_file = osp.join(get_testdir(), "data", "script.m")

    # asserts output contains 'Hello' and '2'
    assert re.match(
        r"Hello\s+2",
        subprocess.run(
            ["mathics", "-e", "Print[1+1];", "-script", script_file],
            capture_output=True,
        ).stdout.decode("utf-8"),
    )


if __name__ == "__main__":
    test_cli()
