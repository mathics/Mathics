# -*- coding: utf-8 -*-

"""helper functions for images
"""

# Signals to Mathics doc processing not to include this module in its documentation.
no_doc = True

from mathics.version import __version__  # noqa used in loading to check consistency.

import numpy


def convolve(in1, in2, fixed=True):
    # a very much boiled down version scipy.signal.signaltools.fftconvolve with added padding, see
    # https://github.com/scipy/scipy/blob/master/scipy/signal/signaltools.py; please see the Scipy
    # LICENSE in the accompanying files.

    in1 = numpy.asarray(in1)
    in2 = numpy.asarray(in2)

    padding = numpy.array(in2.shape) // 2
    if fixed:  # add "Fixed" padding?
        in1 = numpy.pad(in1, padding, "edge")

    s1 = numpy.array(in1.shape)
    s2 = numpy.array(in2.shape)
    shape = s1 + s2 - 1

    sp1 = numpy.fft.rfftn(in1, shape)
    sp2 = numpy.fft.rfftn(in2, shape)
    ret = numpy.fft.irfftn(sp1 * sp2, shape)

    excess = (numpy.array(ret.shape) - s1) // 2 + padding
    return ret[tuple(slice(p, -p) for p in excess)]


def matrix_to_numpy(a):
    def matrix():
        for y in a.leaves:
            yield [x.round_to_float() for x in y.leaves]

    return numpy.array(list(matrix()))


def numpy_flip(pixels, axis):
    f = (numpy.flipud, numpy.fliplr)[axis]
    return f(pixels)


def numpy_to_matrix(pixels):
    channels = pixels.shape[2]
    if channels == 1:
        return pixels[:, :, 0].tolist()
    else:
        return pixels.tolist()


def pixels_as_float(pixels):
    dtype = pixels.dtype
    if dtype in (numpy.float32, numpy.float64):
        return pixels
    elif dtype == numpy.uint8:
        return pixels.astype(numpy.float32) / 255.0
    elif dtype == numpy.uint16:
        return pixels.astype(numpy.float32) / 65535.0
    elif dtype == numpy.bool:
        return pixels.astype(numpy.float32)
    else:
        raise NotImplementedError


def pixels_as_ubyte(pixels):
    dtype = pixels.dtype
    if dtype in (numpy.float32, numpy.float64):
        pixels = numpy.maximum(numpy.minimum(pixels, 1.0), 0.0)
        return (pixels * 255.0).astype(numpy.uint8)
    elif dtype == numpy.uint8:
        return pixels
    elif dtype == numpy.uint16:
        return (pixels / 256).astype(numpy.uint8)
    elif dtype == numpy.bool:
        return pixels.astype(numpy.uint8) * 255
    else:
        raise NotImplementedError


def pixels_as_uint(pixels):
    dtype = pixels.dtype
    if dtype in (numpy.float32, numpy.float64):
        pixels = numpy.maximum(numpy.minimum(pixels, 1.0), 0.0)
        return (pixels * 65535.0).astype(numpy.uint16)
    elif dtype == numpy.uint8:
        return pixels.astype(numpy.uint16) * 256
    elif dtype == numpy.uint16:
        return pixels
    elif dtype == numpy.bool:
        return pixels.astype(numpy.uint8) * 65535
    else:
        raise NotImplementedError
