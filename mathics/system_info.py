# -*- coding: utf-8 -*-

import os
import sys
import platform
import mathics.builtin.system as msystem
import mathics.builtin.datentime as datentime
import mathics.builtin.files_io.filesystem as filesystem
import mathics.builtin.numeric as numeric

from mathics.core.definitions import Definitions
from mathics.core.evaluation import Evaluation


def mathics_system_info(defs):
    def eval(name, needs_head=True):
        evaled = name().evaluate(evaluation)
        if needs_head:
            return evaled.head.to_python(string_quotes=False)
        else:
            return evaled.to_python(string_quotes=False)

    evaluation = Evaluation(defs, output=None)
    return {
        "$Machine": sys.platform,
        "$MachineName": platform.uname().node,
        "$ProcessID": os.getppid(),
        "$ProcessorType": platform.machine(),
        "$SystemID": sys.platform,
        "$UserName": eval(msystem.UserName),
        "$SystemMemory": eval(msystem.SystemMemory),
        "MemoryAvailable[]": eval(msystem.MemoryAvailable, needs_head=False),
        "$SystemTimeZone": eval(datentime.SystemTimeZone),
        "MachinePrecision": eval(numeric.MachinePrecision_),
        "$BaseDirectory": eval(filesystem.BaseDirectory),
        "$RootDirectory": eval(filesystem.RootDirectory),
        "$HomeDirectory": eval(filesystem.HomeDirectory),
        "$InstallationDirectory": eval(filesystem.InstallationDirectory),
        "$TemporaryDirectory": eval(filesystem.TemporaryDirectory),
    }
