#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import unicode_literals
from __future__ import absolute_import

try:
    import numpy
    from mathics.builtin.numpy_utils import with_numpy as numpy_layer
except ImportError:
    from mathics.builtin.numpy_utils import without_numpy as numpy_layer

import types

for s in dir(numpy_layer):
    f = numpy_layer.__dict__.get(s)
    if isinstance(f, types.FunctionType):
        globals()[s] = getattr(numpy_layer, s)
