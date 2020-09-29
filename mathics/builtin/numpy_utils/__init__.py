#!/usr/bin/env python3
# -*- coding: utf-8 -*-


try:
    import numpy
    from mathics.builtin.numpy_utils import with_numpy as numpy_layer
except ImportError:
    from mathics.builtin.numpy_utils import without_numpy as numpy_layer

# we explicitly list all imported symbols so IDEs as PyCharm can properly
# do their code intelligence.

array = numpy_layer.array

stack = numpy_layer.stack
unstack = numpy_layer.unstack
concat = numpy_layer.concat
stacked = numpy_layer.stacked
vectorize = numpy_layer.vectorize

conditional = numpy_layer.conditional
choose = numpy_layer.choose

clip = numpy_layer.clip
sqrt = numpy_layer.sqrt
floor = numpy_layer.floor
mod = numpy_layer.mod
cos = numpy_layer.cos
sin = numpy_layer.sin
arctan2 = numpy_layer.arctan2

minimum = numpy_layer.minimum
maximum = numpy_layer.maximum
dot_t = numpy_layer.dot_t

is_numpy_available = numpy_layer.is_numpy_available
allclose = numpy_layer.allclose
errstate = numpy_layer.errstate
instantiate_elements = numpy_layer.instantiate_elements

