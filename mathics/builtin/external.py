# -*- coding: utf8 -*-

"""
Functions for running external commands
"""

from mathics.builtin.base import Builtin
from mathics.core.expression import Number, Integer

import subprocess

class Run(Builtin):
    u"""
    >> Run["echo", "mathics"]
     = mathics
    """

    def apply(self, items, evaluation):
        'Run[items__]'

        items = [str(item.format(evaluation, "InputForm"))
                    for item in items.get_sequence()]

        p = subprocess.Popen(' '.join(items), shell=True)
        p.communicate()

        return Integer(p.returncode)
