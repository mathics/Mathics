# -*- coding: utf-8 -*-
from .helper import check_evaluation
import os
import sys

# FIXME: see if we can refine this better such as
# by running some Python code and looking for a failure.
limited_characterset = sys.platform not in {"win32",} and not os.environ.get("CI")
if limited_characterset:
    def test_non_win32_print():
        for str_expr, str_expected, message in (
            (
                'Print["\\[Mu]"]',
                "System`Null",
                "μ"
            ),
        ):
            check_evaluation(str_expr, str_expected, message)
