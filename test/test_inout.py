# -*- coding: utf-8 -*-
from .helper import check_evaluation
import os
import sys

# A better test has to do with handling unicode
if sys.platform not in {"win32",} and not os.environ.get("CI"):
    def test_non_win32_print():
        for str_expr, str_expected, message in (
            (
             'Print["\\[Mu]"]',
             "μ"
            ),
        ):
            check_evaluation(str_expr, str_expected, message)
