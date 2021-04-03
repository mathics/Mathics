# -*- coding: utf-8 -*-
"""
Image[] and image related functions.

Note that you (currently) need scikit-image installed in order for this module to work.
"""
from mathics.version import __version__  # noqa used in loading to check consistency.
from mathics.builtin.base import Builtin, AtomBuiltin, Test, BoxConstruct, String
from mathics.core.expression import (
    Atom,
    Expression,
    Integer,
    Rational,
    Real,
    MachineReal,
    Symbol,
    SymbolNull,
    SymbolList,
    SymbolRule,
    from_python,
)
from mathics.builtin.colors import (
    convert as convert_color,
    colorspaces as known_colorspaces,
)

import base64
import functools
import itertools
import math
from collections import defaultdict

_image_requires = ("numpy", "PIL")

_skimage_requires = _image_requires + ("skimage", "scipy", "matplotlib", "networkx")

try:
    import warnings

    import PIL
    import PIL.ImageEnhance
    import PIL.ImageOps
    import PIL.ImageFilter
    from PIL.ExifTags import TAGS as ExifTags

    import numpy

    _enabled = True
except ImportError:
    _enabled = False

from io import BytesIO


class _ImageBuiltin(Builtin):
    requires = _image_requires


class _ImageTest(Test):
    requires = _image_requires


class _SkimageBuiltin(_ImageBuiltin):
    requires = _skimage_requires


# helpers


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


def matrix_to_numpy(a):
    def matrix():
        for y in a.leaves:
            yield [x.round_to_float() for x in y.leaves]

    return numpy.array(list(matrix()))


def numpy_to_matrix(pixels):
    channels = pixels.shape[2]
    if channels == 1:
        return pixels[:, :, 0].tolist()
    else:
        return pixels.tolist()


def numpy_flip(pixels, axis):
    f = (numpy.flipud, numpy.fliplr)[axis]
    return f(pixels)


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


# import and export


class _Exif:
    _names = {  # names overriding the ones given by Pillow
        37385: "FlashInfo",
        40960: "FlashpixVersion",
        40962: "PixelXDimension",
        40963: "PixelYDimension",
    }

    @staticmethod
    def extract(im, evaluation):
        if hasattr(im, "_getexif"):
            exif = im._getexif()
            if not exif:
                return

            for k, v in sorted(exif.items(), key=lambda x: x[0]):
                name = ExifTags.get(k)
                if not name:
                    continue

                # EXIF has the following types: Short, Long, Rational, Ascii, Byte
                # (see http://www.exiv2.org/tags.html). we detect the type from the
                # Python type Pillow gives us and do the appropiate MMA handling.

                if isinstance(v, tuple) and len(v) == 2:  # Rational
                    value = Rational(v[0], v[1])
                    if name == "FocalLength":
                        value = value.round(2)
                    else:
                        value = Expression("Simplify", value).evaluate(evaluation)
                elif isinstance(v, bytes):  # Byte
                    value = String(" ".join(["%d" % x for x in v]))
                elif isinstance(v, (int, str)):  # Short, Long, Ascii
                    value = v
                else:
                    continue

                yield Expression(SymbolRule, String(_Exif._names.get(k, name)), value)


class ImageImport(_ImageBuiltin):
    """
    ## Image
    >> Import["ExampleData/Einstein.jpg"]
     = -Image-
    #> Import["ExampleData/sunflowers.jpg"]
     = -Image-
    >> Import["ExampleData/MadTeaParty.gif"]
     = -Image-
    >> Import["ExampleData/moon.tif"]
     = -Image-
    #> Import["ExampleData/lena.tif"]
     = -Image-
    """

    def apply(self, path, evaluation):
        """ImageImport[path_?StringQ]"""
        pillow = PIL.Image.open(path.get_string_value())
        pixels = numpy.asarray(pillow)
        is_rgb = len(pixels.shape) >= 3 and pixels.shape[2] >= 3
        exif = Expression(SymbolList, *list(_Exif.extract(pillow, evaluation)))

        image = Image(pixels, "RGB" if is_rgb else "Grayscale")
        return Expression(
            "List",
            Expression(SymbolRule, String("Image"), image),
            Expression(SymbolRule, String("ColorSpace"), String(image.color_space)),
            Expression(
                SymbolRule, String("ImageSize"), from_python(image.dimensions())
            ),
            Expression(SymbolRule, String("RawExif"), exif),
        )


class ImageExport(_ImageBuiltin):
    messages = {"noimage": "only an Image[] can be exported into an image file"}

    def apply(self, path, expr, opts, evaluation):
        """ImageExport[path_?StringQ, expr_, opts___]"""
        if isinstance(expr, Image):
            expr.pil().save(path.get_string_value())
            return SymbolNull
        else:
            return evaluation.message("ImageExport", "noimage")


# image math


class _ImageArithmetic(_ImageBuiltin):
    messages = {"bddarg": "Expecting a number, image, or graphics instead of `1`."}

    @staticmethod
    def convert_Image(image):
        assert isinstance(image, Image)
        return pixels_as_float(image.pixels)

    @staticmethod
    def convert_args(*args):
        images = []
        for arg in args:
            if isinstance(arg, Image):
                images.append(_ImageArithmetic.convert_Image(arg))
            elif isinstance(arg, (Integer, Rational, Real)):
                images.append(float(arg.to_python()))
            else:
                return None, arg
        return images, None

    @staticmethod
    def _reduce(iterable, ufunc):
        result = None
        for i in iterable:
            if result is None:
                # ufunc is destructive so copy first
                result = numpy.copy(i)
            else:
                # e.g. result *= i
                ufunc(result, i, result)
        return result

    def apply(self, image, args, evaluation):
        "%(name)s[image_Image, args__]"
        images, arg = self.convert_args(image, *args.get_sequence())
        if images is None:
            return evaluation.message(self.get_name(), "bddarg", arg)
        ufunc = getattr(numpy, self.get_name(True)[5:].lower())
        result = self._reduce(images, ufunc).clip(0, 1)
        return Image(result, image.color_space)


class ImageAdd(_ImageArithmetic):
    """
    <dl>
    <dt>'ImageAdd[$image$, $expr_1$, $expr_2$, ...]'
      <dd>adds all $expr_i$ to $image$ where each $expr_i$ must be an image or a real number.
    </dl>

    >> i = Image[{{0, 0.5, 0.2, 0.1, 0.9}, {1.0, 0.1, 0.3, 0.8, 0.6}}];

    >> ImageAdd[i, 0.5]
     = -Image-

    >> ImageAdd[i, i]
     = -Image-

    #> ImageAdd[i, 0.2, i, 0.1]
     = -Image-

    #> ImageAdd[i, x]
     : Expecting a number, image, or graphics instead of x.
     = ImageAdd[-Image-, x]

    >> ein = Import["ExampleData/Einstein.jpg"];
    >> noise = RandomImage[{-0.1, 0.1}, ImageDimensions[ein]];
    >> ImageAdd[noise, ein]
     = -Image-

    >> lena = Import["ExampleData/lena.tif"];
    >> noise = RandomImage[{-0.2, 0.2}, ImageDimensions[lena], ColorSpace -> "RGB"];
    >> ImageAdd[noise, lena]
     = -Image-
    """


class ImageSubtract(_ImageArithmetic):
    """
    <dl>
    <dt>'ImageSubtract[$image$, $expr_1$, $expr_2$, ...]'
      <dd>subtracts all $expr_i$ from $image$ where each $expr_i$ must be an image or a real number.
    </dl>

    >> i = Image[{{0, 0.5, 0.2, 0.1, 0.9}, {1.0, 0.1, 0.3, 0.8, 0.6}}];

    >> ImageSubtract[i, 0.2]
     = -Image-

    >> ImageSubtract[i, i]
     = -Image-

    #> ImageSubtract[i, 0.2, i, 0.1]
     = -Image-

    #> ImageSubtract[i, x]
     : Expecting a number, image, or graphics instead of x.
     = ImageSubtract[-Image-, x]
    """


class ImageMultiply(_ImageArithmetic):
    """
    <dl>
    <dt>'ImageMultiply[$image$, $expr_1$, $expr_2$, ...]'
      <dd>multiplies all $expr_i$ with $image$ where each $expr_i$ must be an image or a real number.
    </dl>

    >> i = Image[{{0, 0.5, 0.2, 0.1, 0.9}, {1.0, 0.1, 0.3, 0.8, 0.6}}];

    >> ImageMultiply[i, 0.2]
     = -Image-

    >> ImageMultiply[i, i]
     = -Image-

    #> ImageMultiply[i, 0.2, i, 0.1]
     = -Image-

    #> ImageMultiply[i, x]
     : Expecting a number, image, or graphics instead of x.
     = ImageMultiply[-Image-, x]

    >> ein = Import["ExampleData/Einstein.jpg"];
    >> noise = RandomImage[{0.7, 1.3}, ImageDimensions[ein]];
    >> ImageMultiply[noise, ein]
     = -Image-
    """


class RandomImage(_ImageBuiltin):
    """
    <dl>
    <dt>'RandomImage[$max$]'
      <dd>creates an image of random pixels with values 0 to $max$.
    <dt>'RandomImage[{$min$, $max$}]'
      <dd>creates an image of random pixels with values $min$ to $max$.
    <dt>'RandomImage[..., $size$]'
      <dd>creates an image of the given $size$.
    </dl>

    >> RandomImage[1, {100, 100}]
     = -Image-

    #> RandomImage[0.5]
     = -Image-
    #> RandomImage[{0.1, 0.9}]
     = -Image-
    #> RandomImage[0.9, {400, 600}]
     = -Image-
    #> RandomImage[{0.1, 0.5}, {400, 600}]
     = -Image-

    #> RandomImage[{0.1, 0.5}, {400, 600}, ColorSpace -> "RGB"]
     = -Image-
    """

    options = {"ColorSpace": "Automatic"}

    rules = {
        "RandomImage[]": "RandomImage[{0, 1}, {150, 150}]",
        "RandomImage[max_?RealNumberQ]": "RandomImage[{0, max}, {150, 150}]",
        "RandomImage[{minval_?RealNumberQ, maxval_?RealNumberQ}]": "RandomImage[{minval, maxval}, {150, 150}]",
        "RandomImage[max_?RealNumberQ, {w_Integer, h_Integer}]": "RandomImage[{0, max}, {w, h}]",
    }

    messages = {
        "bddim": "The specified dimension `1` should be a pair of positive integers.",
        "imgcstype": "`1` is an invalid color space specification.",
    }

    def apply(self, minval, maxval, w, h, evaluation, options):
        "RandomImage[{minval_?RealNumberQ, maxval_?RealNumberQ}, {w_Integer, h_Integer}, OptionsPattern[RandomImage]]"
        color_space = self.get_option(options, "ColorSpace", evaluation)
        if (
            isinstance(color_space, Symbol)
            and color_space.get_name() == "System`Automatic"
        ):
            cs = "Grayscale"
        else:
            cs = color_space.get_string_value()
        size = [w.get_int_value(), h.get_int_value()]
        if size[0] <= 0 or size[1] <= 0:
            return evaluation.message("RandomImage", "bddim", from_python(size))
        minrange, maxrange = minval.round_to_float(), maxval.round_to_float()

        if cs == "Grayscale":
            data = (
                numpy.random.rand(size[1], size[0]) * (maxrange - minrange) + minrange
            )
        elif cs == "RGB":
            data = (
                numpy.random.rand(size[1], size[0], 3) * (maxrange - minrange)
                + minrange
            )
        else:
            return evaluation.message("RandomImage", "imgcstype", color_space)
        return Image(data, cs)


# simple image manipulation


class ImageResize(_ImageBuiltin):
    """
    <dl>
    <dt>'ImageResize[$image$, $width$]'
      <dd>
    <dt>'ImageResize[$image$, {$width$, $height$}]'
      <dd>
    </dl>

    >> ein = Import["ExampleData/Einstein.jpg"];
    >> ImageDimensions[ein]
     = {615, 768}
    >> ImageResize[ein, {400, 600}]
     = -Image-
    #> ImageDimensions[%]
     = {400, 600}

    >> ImageResize[ein, 256]
     = -Image-
    >> ImageDimensions[%]
     = {256, 320}

    The default sampling method is Bicubic
    >> ImageResize[ein, 256, Resampling -> "Bicubic"]
     = -Image-
    #> ImageDimensions[%]
     = {256, 320}
    >> ImageResize[ein, 256, Resampling -> "Nearest"]
     = -Image-
    #> ImageDimensions[%]
     = {256, 320}
    >> ImageResize[ein, 256, Resampling -> "Gaussian"]
     = -Image-
    #> ImageDimensions[%]
     = {256, 320}
    #> ImageResize[ein, {256, 256}, Resampling -> "Gaussian"]
     : Gaussian resampling needs to maintain aspect ratio.
     = ImageResize[-Image-, {256, 256}, Resampling -> Gaussian]
    #> ImageResize[ein, 256, Resampling -> "Invalid"]
     : Invalid resampling method Invalid.
     = ImageResize[-Image-, 256, Resampling -> Invalid]

    #> ImageDimensions[ImageResize[ein, {256}]]
     = {256, 256}

    #> ImageResize[ein, {x}]
     : The size {x} is not a valid image size specification.
     = ImageResize[-Image-, {x}]
    #> ImageResize[ein, x]
     : The size x is not a valid image size specification.
     = ImageResize[-Image-, x]
    """

    options = {"Resampling": "Automatic"}

    messages = {
        "imgrssz": "The size `1` is not a valid image size specification.",
        "imgrsm": "Invalid resampling method `1`.",
        "gaussaspect": "Gaussian resampling needs to maintain aspect ratio.",
        "skimage": "Please install scikit-image to use Resampling -> Gaussian.",
    }

    def _get_image_size_spec(self, old_size, new_size):
        predefined_sizes = {
            "System`Tiny": 75,
            "System`Small": 150,
            "System`Medium": 300,
            "System`Large": 450,
            "System`Automatic": 0,  # placeholder
        }
        result = new_size.round_to_float()
        if result is not None:
            result = int(result)
            if result <= 0:
                return None
            return result

        if isinstance(new_size, Symbol):
            name = new_size.get_name()
            if name == "System`All":
                return old_size
            return predefined_sizes.get(name, None)
        if new_size.has_form("Scaled", 1):
            s = new_size.leaves[0].round_to_float()
            if s is None:
                return None
            return max(1, old_size * s)  # handle negative s values silently
        return None

    def apply_resize_width(self, image, s, evaluation, options):
        "ImageResize[image_Image, s_, OptionsPattern[ImageResize]]"
        old_w = image.pixels.shape[1]
        if s.has_form("List", 1):
            width = s.leaves[0]
        else:
            width = s
        w = self._get_image_size_spec(old_w, width)
        if w is None:
            return evaluation.message("ImageResize", "imgrssz", s)
        if s.has_form("List", 1):
            height = width
        else:
            height = Symbol("Automatic")
        return self.apply_resize_width_height(image, width, height, evaluation, options)

    def apply_resize_width_height(self, image, width, height, evaluation, options):
        "ImageResize[image_Image, {width_, height_}, OptionsPattern[ImageResize]]"
        # resampling method
        resampling = self.get_option(options, "Resampling", evaluation)
        if (
            isinstance(resampling, Symbol)
            and resampling.get_name() == "System`Automatic"
        ):
            resampling_name = "Bicubic"
        else:
            resampling_name = resampling.get_string_value()

        # find new size
        old_w, old_h = image.pixels.shape[1], image.pixels.shape[0]
        w = self._get_image_size_spec(old_w, width)
        h = self._get_image_size_spec(old_h, height)
        if h is None or w is None:
            return evaluation.message(
                "ImageResize", "imgrssz", Expression(SymbolList, width, height)
            )

        # handle Automatic
        old_aspect_ratio = old_w / old_h
        if w == 0 and h == 0:
            # if both width and height are Automatic then use old values
            w, h = old_w, old_h
        elif w == 0:
            w = max(1, h * old_aspect_ratio)
        elif h == 0:
            h = max(1, w / old_aspect_ratio)

        if resampling_name != "Gaussian":
            # Gaussian need to unrounded values to compute scaling ratios.
            # round to closest pixel for other methods.
            h, w = int(round(h)), int(round(w))

        # perform the resize
        if resampling_name == "Nearest":
            return image.filter(
                lambda im: im.resize((w, h), resample=PIL.Image.NEAREST)
            )
        elif resampling_name == "Bicubic":
            return image.filter(
                lambda im: im.resize((w, h), resample=PIL.Image.BICUBIC)
            )
        elif resampling_name != "Gaussian":
            return evaluation.message("ImageResize", "imgrsm", resampling)

        try:
            from skimage import transform

            multichannel = image.pixels.ndim == 3

            sy = h / old_h
            sx = w / old_w
            if sy > sx:
                err = abs((sy * old_w) - (sx * old_w))
                s = sy
            else:
                err = abs((sy * old_h) - (sx * old_h))
                s = sx
            if err > 1.5:
                # TODO overcome this limitation
                return evaluation.message("ImageResize", "gaussaspect")
            elif s > 1:
                pixels = transform.pyramid_expand(
                    image.pixels, upscale=s, multichannel=multichannel
                ).clip(0, 1)
            else:
                pixels = transform.pyramid_reduce(
                    image.pixels, multichannel=multichannel, downscale=(1.0 / s)
                ).clip(0, 1)

            return Image(pixels, image.color_space)
        except ImportError:
            evaluation.message("ImageResize", "skimage")


class ImageReflect(_ImageBuiltin):
    """
    <dl>
    <dt>'ImageReflect[$image$]'
      <dd>Flips $image$ top to bottom.
    <dt>'ImageReflect[$image$, $side$]'
      <dd>Flips $image$ so that $side$ is interchanged with its opposite.
    <dt>'ImageReflect[$image$, $side_1$ -> $side_2$]'
      <dd>Flips $image$ so that $side_1$ is interchanged with $side_2$.
    </dl>

    >> ein = Import["ExampleData/Einstein.jpg"];
    >> ImageReflect[ein]
     = -Image-
    >> ImageReflect[ein, Left]
     = -Image-
    >> ImageReflect[ein, Left -> Top]
     = -Image-

    #> ein == ImageReflect[ein, Left -> Left] == ImageReflect[ein, Right -> Right] == ImageReflect[ein, Top -> Top] == ImageReflect[ein, Bottom -> Bottom]
     = True
    #> ImageReflect[ein, Left -> Right] == ImageReflect[ein, Right -> Left] == ImageReflect[ein, Left] == ImageReflect[ein, Right]
     = True
    #> ImageReflect[ein, Bottom -> Top] == ImageReflect[ein, Top -> Bottom] == ImageReflect[ein, Top] == ImageReflect[ein, Bottom]
     = True
    #> ImageReflect[ein, Left -> Top] == ImageReflect[ein, Right -> Bottom]     (* Transpose *)
     = True
    #> ImageReflect[ein, Left -> Bottom] == ImageReflect[ein, Right -> Top]     (* Anti-Transpose *)
     = True

    #> ImageReflect[ein, x -> Top]
     : x -> Top is not a valid 2D reflection specification.
     = ImageReflect[-Image-, x -> Top]
    """

    rules = {
        "ImageReflect[image_Image]": "ImageReflect[image, Top -> Bottom]",
        "ImageReflect[image_Image, Top|Bottom]": "ImageReflect[image, Top -> Bottom]",
        "ImageReflect[image_Image, Left|Right]": "ImageReflect[image, Left -> Right]",
    }

    messages = {"bdrfl2": "`1` is not a valid 2D reflection specification."}

    def apply(self, image, orig, dest, evaluation):
        "ImageReflect[image_Image, Rule[orig_, dest_]]"
        if isinstance(orig, Symbol) and isinstance(dest, Symbol):
            specs = [orig.get_name(), dest.get_name()]
            specs.sort()  # `Top -> Bottom` is the same as `Bottom -> Top`

        anti_transpose = lambda i: numpy.flipud(numpy.transpose(numpy.flipud(i)))
        no_op = lambda i: i

        method = {
            ("System`Bottom", "System`Top"): numpy.flipud,
            ("System`Left", "System`Right"): numpy.fliplr,
            ("System`Left", "System`Top"): numpy.transpose,
            ("System`Right", "System`Top"): anti_transpose,
            ("System`Bottom", "System`Left"): anti_transpose,
            ("System`Bottom", "System`Right"): numpy.transpose,
            ("System`Bottom", "System`Bottom"): no_op,
            ("System`Top", "System`Top"): no_op,
            ("System`Left", "System`Left"): no_op,
            ("System`Right", "System`Right"): no_op,
        }.get(tuple(specs), None)

        if method is None:
            return evaluation.message(
                "ImageReflect", "bdrfl2", Expression(SymbolRule, orig, dest)
            )

        return Image(method(image.pixels), image.color_space)


class ImageRotate(_ImageBuiltin):
    """
    <dl>
    <dt>'ImageRotate[$image$]'
      <dd>Rotates $image$ 90 degrees counterclockwise.
    <dt>'ImageRotate[$image$, $theta$]'
      <dd>Rotates $image$ by a given angle $theta$
    </dl>

    >> ein = Import["ExampleData/Einstein.jpg"];

    >> ImageRotate[ein]
     = -Image-

    >> ImageRotate[ein, 45 Degree]
     = -Image-

    >> ImageRotate[ein, Pi / 2]
     = -Image-

    #> ImageRotate[ein, ein]
     : Angle -Image- should be a real number, one of Top, Bottom, Left, Right, or a rule from one to another.
     = ImageRotate[-Image-, -Image-]
    """

    rules = {"ImageRotate[i_Image]": "ImageRotate[i, 90 Degree]"}

    messages = {
        "imgang": "Angle `1` should be a real number, one of Top, Bottom, Left, Right, or a rule from one to another."
    }

    def apply(self, image, angle, evaluation):
        "ImageRotate[image_Image, angle_]"
        py_angle = angle.round_to_float(evaluation)
        if py_angle is None:
            return evaluation.message("ImageRotate", "imgang", angle)

        def rotate(im):
            return im.rotate(
                180 * py_angle / math.pi, resample=PIL.Image.BICUBIC, expand=True
            )

        return image.filter(rotate)


class ImagePartition(_ImageBuiltin):
    """
    <dl>
    <dt>'ImagePartition[$image$, $s$]'
      <dd>Partitions an image into an array of $s$ x $s$ pixel subimages.
    <dt>'ImagePartition[$image$, {$w$, $h$}]'
      <dd>Partitions an image into an array of $w$ x $h$ pixel subimages.
    </dl>

    >> lena = Import["ExampleData/lena.tif"];
    >> ImageDimensions[lena]
     = {512, 512}
    >> ImagePartition[lena, 256]
     = {{-Image-, -Image-}, {-Image-, -Image-}}

    >> ImagePartition[lena, {512, 128}]
     = {{-Image-}, {-Image-}, {-Image-}, {-Image-}}

    #> ImagePartition[lena, 257]
     = {{-Image-}}
    #> ImagePartition[lena, 512]
     = {{-Image-}}
    #> ImagePartition[lena, 513]
     = {}
    #> ImagePartition[lena, {256, 300}]
     = {{-Image-, -Image-}}

    #> ImagePartition[lena, {0, 300}]
     : {0, 300} is not a valid size specification for image partitions.
     = ImagePartition[-Image-, {0, 300}]
    """

    rules = {"ImagePartition[i_Image, s_Integer]": "ImagePartition[i, {s, s}]"}

    messages = {"arg2": "`1` is not a valid size specification for image partitions."}

    def apply(self, image, w, h, evaluation):
        "ImagePartition[image_Image, {w_Integer, h_Integer}]"
        w = w.get_int_value()
        h = h.get_int_value()
        if w <= 0 or h <= 0:
            return evaluation.message("ImagePartition", "arg2", from_python([w, h]))
        pixels = image.pixels
        shape = pixels.shape

        # drop blocks less than w x h
        parts = []
        for yi in range(shape[0] // h):
            row = []
            for xi in range(shape[1] // w):
                p = pixels[yi * h : (yi + 1) * h, xi * w : (xi + 1) * w]
                row.append(Image(p, image.color_space))
            if row:
                parts.append(row)
        return from_python(parts)


# simple image filters


class ImageAdjust(_ImageBuiltin):
    """
    <dl>
    <dt>'ImageAdjust[$image$]'
      <dd>adjusts the levels in $image$.
    <dt>'ImageAdjust[$image$, $c$]'
      <dd>adjusts the contrast in $image$ by $c$.
    <dt>'ImageAdjust[$image$, {$c$, $b$}]'
      <dd>adjusts the contrast $c$, and brightness $b$ in $image$.
    <dt>'ImageAdjust[$image$, {$c$, $b$, $g$}]'
      <dd>adjusts the contrast $c$, brightness $b$, and gamma $g$ in $image$.
    </dl>

    >> lena = Import["ExampleData/lena.tif"];
    >> ImageAdjust[lena]
     = -Image-
    """

    rules = {
        "ImageAdjust[image_Image, c_?RealNumberQ]": "ImageAdjust[image, {c, 0, 1}]",
        "ImageAdjust[image_Image, {c_?RealNumberQ, b_?RealNumberQ}]": "ImageAdjust[image, {c, b, 1}]",
    }

    def apply_auto(self, image, evaluation):
        "ImageAdjust[image_Image]"
        pixels = pixels_as_float(image.pixels)

        # channel limits
        axis = (0, 1)
        cmaxs, cmins = pixels.max(axis=axis), pixels.min(axis=axis)

        # normalise channels
        scales = cmaxs - cmins
        if not scales.shape:
            scales = numpy.array([scales])
        scales[scales == 0.0] = 1
        pixels -= cmins
        pixels /= scales
        return Image(pixels, image.color_space)

    def apply_contrast_brightness_gamma(self, image, c, b, g, evaluation):
        "ImageAdjust[image_Image, {c_?RealNumberQ, b_?RealNumberQ, g_?RealNumberQ}]"

        im = image.pil()

        # gamma
        g = g.round_to_float()
        if g != 1:
            im = PIL.ImageEnhance.Color(im).enhance(g)

        # brightness
        b = b.round_to_float()
        if b != 0:
            im = PIL.ImageEnhance.Brightness(im).enhance(b + 1)

        # contrast
        c = c.round_to_float()
        if c != 0:
            im = PIL.ImageEnhance.Contrast(im).enhance(c + 1)

        return Image(numpy.array(im), image.color_space)


class Blur(_ImageBuiltin):
    """
    <dl>
    <dt>'Blur[$image$]'
      <dd>gives a blurred version of $image$.
    <dt>'Blur[$image$, $r$]'
      <dd>blurs $image$ with a kernel of size $r$.
    </dl>

    >> lena = Import["ExampleData/lena.tif"];
    >> Blur[lena]
     = -Image-
    >> Blur[lena, 5]
     = -Image-
    """

    rules = {
        "Blur[image_Image]": "Blur[image, 2]",
        "Blur[image_Image, r_?RealNumberQ]": "ImageConvolve[image, BoxMatrix[r] / Total[Flatten[BoxMatrix[r]]]]",
    }


class Sharpen(_ImageBuiltin):
    """
    <dl>
    <dt>'Sharpen[$image$]'
      <dd>gives a sharpened version of $image$.
    <dt>'Sharpen[$image$, $r$]'
      <dd>sharpens $image$ with a kernel of size $r$.
    </dl>

    >> lena = Import["ExampleData/lena.tif"];
    >> Sharpen[lena]
     = -Image-
    >> Sharpen[lena, 5]
     = -Image-
    """

    rules = {"Sharpen[i_Image]": "Sharpen[i, 2]"}

    def apply(self, image, r, evaluation):
        "Sharpen[image_Image, r_?RealNumberQ]"
        f = PIL.ImageFilter.UnsharpMask(r.round_to_float())
        return image.filter(lambda im: im.filter(f))


class GaussianFilter(_ImageBuiltin):
    """
    <dl>
    <dt>'GaussianFilter[$image$, $r$]'
      <dd>blurs $image$ using a Gaussian blur filter of radius $r$.
    </dl>

    >> lena = Import["ExampleData/lena.tif"];
    >> GaussianFilter[lena, 2.5]
     = -Image-
    """

    messages = {"only3": "GaussianFilter only supports up to three channels."}

    def apply_radius(self, image, radius, evaluation):
        "GaussianFilter[image_Image, radius_?RealNumberQ]"
        if len(image.pixels.shape) > 2 and image.pixels.shape[2] > 3:
            return evaluation.message("GaussianFilter", "only3")
        else:
            f = PIL.ImageFilter.GaussianBlur(radius.round_to_float())
            return image.filter(lambda im: im.filter(f))


# morphological image filters


class PillowImageFilter(_ImageBuiltin):
    def compute(self, image, f):
        return image.filter(lambda im: im.filter(f))


class MinFilter(PillowImageFilter):
    """
    <dl>
    <dt>'MinFilter[$image$, $r$]'
      <dd>gives $image$ with a minimum filter of radius $r$ applied on it. This always
      picks the smallest value in the filter's area.
    </dl>

    >> lena = Import["ExampleData/lena.tif"];
    >> MinFilter[lena, 5]
     = -Image-
    """

    def apply(self, image, r, evaluation):
        "MinFilter[image_Image, r_Integer]"
        return self.compute(image, PIL.ImageFilter.MinFilter(1 + 2 * r.get_int_value()))


class MaxFilter(PillowImageFilter):
    """
    <dl>
    <dt>'MaxFilter[$image$, $r$]'
      <dd>gives $image$ with a maximum filter of radius $r$ applied on it. This always
      picks the largest value in the filter's area.
    </dl>

    >> lena = Import["ExampleData/lena.tif"];
    >> MaxFilter[lena, 5]
     = -Image-
    """

    def apply(self, image, r, evaluation):
        "MaxFilter[image_Image, r_Integer]"
        return self.compute(image, PIL.ImageFilter.MaxFilter(1 + 2 * r.get_int_value()))


class MedianFilter(PillowImageFilter):
    """
    <dl>
    <dt>'MedianFilter[$image$, $r$]'
      <dd>gives $image$ with a median filter of radius $r$ applied on it. This always
      picks the median value in the filter's area.
    </dl>

    >> lena = Import["ExampleData/lena.tif"];
    >> MedianFilter[lena, 5]
     = -Image-
    """

    def apply(self, image, r, evaluation):
        "MedianFilter[image_Image, r_Integer]"
        return self.compute(
            image, PIL.ImageFilter.MedianFilter(1 + 2 * r.get_int_value())
        )


class EdgeDetect(_SkimageBuiltin):
    """
    <dl>
    <dt>'EdgeDetect[$image$]'
      <dd>returns an image showing the edges in $image$.
    </dl>

    >> lena = Import["ExampleData/lena.tif"];
    >> EdgeDetect[lena]
     = -Image-
    >> EdgeDetect[lena, 5]
     = -Image-
    >> EdgeDetect[lena, 4, 0.5]
     = -Image-
    """

    rules = {
        "EdgeDetect[i_Image]": "EdgeDetect[i, 2, 0.2]",
        "EdgeDetect[i_Image, r_?RealNumberQ]": "EdgeDetect[i, r, 0.2]",
    }

    def apply(self, image, r, t, evaluation):
        "EdgeDetect[image_Image, r_?RealNumberQ, t_?RealNumberQ]"
        import skimage.feature

        pixels = image.grayscale().pixels
        return Image(
            skimage.feature.canny(
                pixels.reshape(pixels.shape[:2]),
                sigma=r.round_to_float() / 2,
                low_threshold=0.5 * t.round_to_float(),
                high_threshold=t.round_to_float(),
            ),
            "Grayscale",
        )


def _matrix(rows):
    return Expression(SymbolList, *[Expression(SymbolList, *r) for r in rows])


class BoxMatrix(_ImageBuiltin):
    """
    <dl>
    <dt>'BoxMatrix[$s]'
      <dd>Gives a box shaped kernel of size 2 $s$ + 1.
    </dl>

    >> BoxMatrix[3]
     = {{1, 1, 1, 1, 1, 1, 1}, {1, 1, 1, 1, 1, 1, 1}, {1, 1, 1, 1, 1, 1, 1}, {1, 1, 1, 1, 1, 1, 1}, {1, 1, 1, 1, 1, 1, 1}, {1, 1, 1, 1, 1, 1, 1}, {1, 1, 1, 1, 1, 1, 1}}
    """

    def apply(self, r, evaluation):
        "BoxMatrix[r_?RealNumberQ]"
        py_r = abs(r.round_to_float())
        s = int(math.floor(1 + 2 * py_r))
        return _matrix([[Integer(1)] * s] * s)


class DiskMatrix(_ImageBuiltin):
    """
    <dl>
    <dt>'DiskMatrix[$s]'
      <dd>Gives a disk shaped kernel of size 2 $s$ + 1.
    </dl>

    >> DiskMatrix[3]
     = {{0, 0, 1, 1, 1, 0, 0}, {0, 1, 1, 1, 1, 1, 0}, {1, 1, 1, 1, 1, 1, 1}, {1, 1, 1, 1, 1, 1, 1}, {1, 1, 1, 1, 1, 1, 1}, {0, 1, 1, 1, 1, 1, 0}, {0, 0, 1, 1, 1, 0, 0}}
    """

    def apply(self, r, evaluation):
        "DiskMatrix[r_?RealNumberQ]"
        py_r = abs(r.round_to_float())
        s = int(math.floor(0.5 + py_r))

        m = (Integer(0), Integer(1))
        r_sqr = (py_r + 0.5) * (py_r + 0.5)

        def rows():
            for y in range(-s, s + 1):
                yield [m[int((x) * (x) + (y) * (y) <= r_sqr)] for x in range(-s, s + 1)]

        return _matrix(rows())


class DiamondMatrix(_ImageBuiltin):
    """
    <dl>
    <dt>'DiamondMatrix[$s]'
      <dd>Gives a diamond shaped kernel of size 2 $s$ + 1.
    </dl>

    >> DiamondMatrix[3]
     = {{0, 0, 0, 1, 0, 0, 0}, {0, 0, 1, 1, 1, 0, 0}, {0, 1, 1, 1, 1, 1, 0}, {1, 1, 1, 1, 1, 1, 1}, {0, 1, 1, 1, 1, 1, 0}, {0, 0, 1, 1, 1, 0, 0}, {0, 0, 0, 1, 0, 0, 0}}
    """

    def apply(self, r, evaluation):
        "DiamondMatrix[r_?RealNumberQ]"
        py_r = abs(r.round_to_float())
        t = int(math.floor(0.5 + py_r))

        zero = Integer(0)
        one = Integer(1)

        def rows():
            for d in range(0, t):
                p = [zero] * (t - d)
                yield p + ([one] * (1 + d * 2)) + p

            yield [one] * (2 * t + 1)

            for d in reversed(range(0, t)):
                p = [zero] * (t - d)
                yield p + ([one] * (1 + d * 2)) + p

        return _matrix(rows())


class ImageConvolve(_ImageBuiltin):
    """
    <dl>
    <dt>'ImageConvolve[$image$, $kernel$]'
      <dd>Computes the convolution of $image$ using $kernel$.
    </dl>

    >> img = Import["ExampleData/lena.tif"];
    >> ImageConvolve[img, DiamondMatrix[5] / 61]
     = -Image-
    >> ImageConvolve[img, DiskMatrix[5] / 97]
     = -Image-
    >> ImageConvolve[img, BoxMatrix[5] / 121]
     = -Image-
    """

    def apply(self, image, kernel, evaluation):
        "%(name)s[image_Image, kernel_?MatrixQ]"
        numpy_kernel = matrix_to_numpy(kernel)
        pixels = pixels_as_float(image.pixels)
        shape = pixels.shape[:2]
        channels = []
        for c in (pixels[:, :, i] for i in range(pixels.shape[2])):
            channels.append(convolve(c.reshape(shape), numpy_kernel, fixed=True))
        return Image(numpy.dstack(channels), image.color_space)


class _MorphologyFilter(_SkimageBuiltin):

    messages = {
        "grayscale": "Your image has been converted to grayscale as color images are not supported yet."
    }

    rules = {"%(name)s[i_Image, r_?RealNumberQ]": "%(name)s[i, BoxMatrix[r]]"}

    def apply(self, image, k, evaluation):
        "%(name)s[image_Image, k_?MatrixQ]"
        if image.color_space != "Grayscale":
            image = image.grayscale()
            evaluation.message(self.get_name(), "grayscale")
        import skimage.morphology

        f = getattr(skimage.morphology, self.get_name(True).lower())
        shape = image.pixels.shape[:2]
        img = f(image.pixels.reshape(shape), matrix_to_numpy(k))
        return Image(img, "Grayscale")


class Dilation(_MorphologyFilter):
    """
    <dl>
    <dt>'Dilation[$image$, $ker$]'
      <dd>Gives the morphological dilation of $image$ with respect to structuring element $ker$.
    </dl>

    >> ein = Import["ExampleData/Einstein.jpg"];
    >> Dilation[ein, 2.5]
     = -Image-
    """


class Erosion(_MorphologyFilter):
    """
    <dl>
    <dt>'Erosion[$image$, $ker$]'
      <dd>Gives the morphological erosion of $image$ with respect to structuring element $ker$.
    </dl>

    >> ein = Import["ExampleData/Einstein.jpg"];
    >> Erosion[ein, 2.5]
     = -Image-
    """


class Opening(_MorphologyFilter):
    """
    <dl>
    <dt>'Opening[$image$, $ker$]'
      <dd>Gives the morphological opening of $image$ with respect to structuring element $ker$.
    </dl>

    >> ein = Import["ExampleData/Einstein.jpg"];
    >> Opening[ein, 2.5]
     = -Image-
    """


class Closing(_MorphologyFilter):
    """
    <dl>
    <dt>'Closing[$image$, $ker$]'
      <dd>Gives the morphological closing of $image$ with respect to structuring element $ker$.
    </dl>

    >> ein = Import["ExampleData/Einstein.jpg"];
    >> Closing[ein, 2.5]
     = -Image-
    """


class MorphologicalComponents(_SkimageBuiltin):

    rules = {"MorphologicalComponents[i_Image]": "MorphologicalComponents[i, 0]"}

    def apply(self, image, t, evaluation):
        "MorphologicalComponents[image_Image, t_?RealNumberQ]"
        pixels = pixels_as_ubyte(
            pixels_as_float(image.grayscale().pixels) > t.round_to_float()
        )
        import skimage.measure

        return from_python(
            skimage.measure.label(pixels, background=0, connectivity=2).tolist()
        )


# color space


class ImageColorSpace(_ImageBuiltin):
    """
    <dl>
    <dt>'ImageColorSpace[$image$]'
        <dd>gives $image$'s color space, e.g. "RGB" or "CMYK".
    </dl>

    >> img = Import["ExampleData/lena.tif"];
    >> ImageColorSpace[img]
     = RGB
    """

    def apply(self, image, evaluation):
        "ImageColorSpace[image_Image]"
        return String(image.color_space)


class ColorConvert(Builtin):
    """
    <dl>
    <dt>'ColorConvert[$c$, $colspace$]'
        <dd>returns the representation of $c$ in the color space $colspace$. $c$
        may be a color or an image.
    </dl>

    Valid values for $colspace$ are:

    CMYK: convert to CMYKColor
    Grayscale: convert to GrayLevel
    HSB: convert to Hue
    LAB: concert to LABColor
    LCH: convert to LCHColor
    LUV: convert to LUVColor
    RGB: convert to RGBColor
    XYZ: convert to XYZColor
    """

    messages = {
        "ccvinput": "`` should be a color.",
        "imgcstype": "`` is not a valid color space.",
    }

    def apply(self, input, colorspace, evaluation):
        "ColorConvert[input_, colorspace_String]"

        if isinstance(input, Image):
            return input.color_convert(colorspace.get_string_value())
        else:
            from mathics.builtin.graphics import (
                expression_to_color,
                color_to_expression,
            )

            py_color = expression_to_color(input)
            if py_color is None:
                evaluation.message("ColorConvert", "ccvinput", input)
                return

            py_colorspace = colorspace.get_string_value()
            converted_components = convert_color(
                py_color.components, py_color.color_space, py_colorspace
            )

            if converted_components is None:
                evaluation.message("ColorConvert", "imgcstype", colorspace)
                return

            return color_to_expression(converted_components, py_colorspace)


class ColorQuantize(_ImageBuiltin):
    """
    <dl>
    <dt>'ColorQuantize[$image$, $n$]'
      <dd>gives a version of $image$ using only $n$ colors.
    </dl>

    >> img = Import["ExampleData/lena.tif"];
    >> ColorQuantize[img, 6]
     = -Image-

    #> ColorQuantize[img, 0]
     : Positive integer expected at position 2 in ColorQuantize[-Image-, 0].
     = ColorQuantize[-Image-, 0]
    #> ColorQuantize[img, -1]
     : Positive integer expected at position 2 in ColorQuantize[-Image-, -1].
     = ColorQuantize[-Image-, -1]
    """

    messages = {"intp": "Positive integer expected at position `2` in `1`."}

    def apply(self, image, n, evaluation):
        "ColorQuantize[image_Image, n_Integer]"
        if n.get_int_value() <= 0:
            return evaluation.message(
                "ColorQuantize", "intp", Expression("ColorQuantize", image, n), 2
            )
        converted = image.color_convert("RGB")
        if converted is None:
            return
        pixels = pixels_as_ubyte(converted.pixels)
        im = PIL.Image.fromarray(pixels).quantize(n.get_int_value())
        im = im.convert("RGB")
        return Image(numpy.array(im), "RGB")


class Threshold(_SkimageBuiltin):
    """
    <dl>
    <dt>'Threshold[$image$]'
      <dd>gives a value suitable for binarizing $image$.
    </dl>

    The option "Method" may be "Cluster" (use Otsu's threshold), "Median", or "Mean".

    >> img = Import["ExampleData/lena.tif"];
    >> Threshold[img]
     = 0.456739
    >> Binarize[img, %]
     = -Image-
    >> Threshold[img, Method -> "Mean"]
     = 0.486458
    >> Threshold[img, Method -> "Median"]
     = 0.504726
    """

    options = {"Method": '"Cluster"'}

    messages = {
        "illegalmethod": "Method `` is not supported.",
        "skimage": "Please install scikit-image to use Method -> Cluster.",
    }

    def apply(self, image, evaluation, options):
        "Threshold[image_Image, OptionsPattern[Threshold]]"
        pixels = image.grayscale().pixels

        method = self.get_option(options, "Method", evaluation)
        method_name = (
            method.get_string_value()
            if isinstance(method, String)
            else method.to_python()
        )
        if method_name == "Cluster":
            import skimage.filters

            threshold = skimage.filters.threshold_otsu(pixels)
        elif method_name == "Median":
            threshold = numpy.median(pixels)
        elif method_name == "Mean":
            threshold = numpy.mean(pixels)
        else:
            return evaluation.message("Threshold", "illegalmethod", method)

        return MachineReal(float(threshold))


class Binarize(_SkimageBuiltin):
    """
    <dl>
    <dt>'Binarize[$image$]'
      <dd>gives a binarized version of $image$, in which each pixel is either 0 or 1.
    <dt>'Binarize[$image$, $t$]'
      <dd>map values $x$ > $t$ to 1, and values $x$ <= $t$ to 0.
    <dt>'Binarize[$image$, {$t1$, $t2$}]'
      <dd>map $t1$ < $x$ < $t2$ to 1, and all other values to 0.
    </dl>

    >> img = Import["ExampleData/lena.tif"];
    >> Binarize[img]
     = -Image-
    >> Binarize[img, 0.7]
     = -Image-
    >> Binarize[img, {0.2, 0.6}]
     = -Image-
    """

    def apply(self, image, evaluation):
        "Binarize[image_Image]"
        image = image.grayscale()
        thresh = Expression("Threshold", image).evaluate(evaluation).round_to_float()
        if thresh is not None:
            return Image(image.pixels > thresh, "Grayscale")

    def apply_t(self, image, t, evaluation):
        "Binarize[image_Image, t_?RealNumberQ]"
        pixels = image.grayscale().pixels
        return Image(pixels > t.round_to_float(), "Grayscale")

    def apply_t1_t2(self, image, t1, t2, evaluation):
        "Binarize[image_Image, {t1_?RealNumberQ, t2_?RealNumberQ}]"
        pixels = image.grayscale().pixels
        mask1 = pixels > t1.round_to_float()
        mask2 = pixels < t2.round_to_float()
        return Image(mask1 * mask2, "Grayscale")


class ColorNegate(_ImageBuiltin):
    """
    <dl>
    <dt>'ColorNegate[$image$]'
      <dd>Gives a version of $image$ with all colors negated.
    </dl>
    """

    def apply(self, image, evaluation):
        "ColorNegate[image_Image]"
        return image.filter(lambda im: PIL.ImageOps.invert(im))


class ColorSeparate(_ImageBuiltin):
    """
    <dl>
    <dt>'ColorSeparate[$image$]'
      <dd>Gives each channel of $image$ as a separate grayscale image.
    </dl>
    """

    def apply(self, image, evaluation):
        "ColorSeparate[image_Image]"
        images = []
        pixels = image.pixels
        if len(pixels.shape) < 3:
            images.append(pixels)
        else:
            for i in range(pixels.shape[2]):
                images.append(Image(pixels[:, :, i], "Grayscale"))
        return Expression(SymbolList, *images)


class ColorCombine(_ImageBuiltin):
    """
    <dl>
    <dt>'ColorCombine[$channels$, $colorspace$]'
      <dd>Gives an image with $colorspace$ and the respective components described by the given channels.
    </dl>

    >> ColorCombine[{{{1, 0}, {0, 0.75}}, {{0, 1}, {0, 0.25}}, {{0, 0}, {1, 0.5}}}, "RGB"]
     = -Image-
    """

    def apply(self, channels, colorspace, evaluation):
        "ColorCombine[channels_List, colorspace_String]"

        py_colorspace = colorspace.get_string_value()
        if py_colorspace not in known_colorspaces:
            return

        numpy_channels = []
        for channel in channels.leaves:
            if not Expression("MatrixQ", channel).evaluate(evaluation).is_true():
                return
            numpy_channels.append(matrix_to_numpy(channel))

        if not numpy_channels:
            return

        if not all(x.shape == numpy_channels[0].shape for x in numpy_channels[1:]):
            return

        return Image(numpy.dstack(numpy_channels), py_colorspace)


def _linearize(a):
    # this uses a vectorized binary search to compute
    # strictly sequential indices for all values in a.

    orig_shape = a.shape
    a = a.reshape((functools.reduce(lambda x, y: x * y, a.shape),))  # 1 dimension

    u = numpy.unique(a)
    n = len(u)

    lower = numpy.ndarray(a.shape, dtype=numpy.int)
    lower.fill(0)
    upper = numpy.ndarray(a.shape, dtype=numpy.int)
    upper.fill(n - 1)

    h = numpy.sort(u)
    q = n  # worst case partition size

    while q > 2:
        m = numpy.right_shift(lower + upper, 1)
        f = a <= h[m]
        # (lower, m) vs (m + 1, upper)
        lower = numpy.where(f, lower, m + 1)
        upper = numpy.where(f, m, upper)
        q = (q + 1) // 2

    return numpy.where(a == h[lower], lower, upper).reshape(orig_shape), n


class Colorize(_ImageBuiltin):
    """
    <dl>
    <dt>'Colorize[$values$]'
      <dd>returns an image where each number in the rectangular matrix $values$ is a pixel and each
      occurence of the same number is displayed in the same unique color, which is different from the
      colors of all non-identical numbers.
    <dt>'Colorize[$image$]'
      <dd>gives a colorized version of $image$.
    </dl>

    >> Colorize[{{1.3, 2.1, 1.5}, {1.3, 1.3, 2.1}, {1.3, 2.1, 1.5}}]
     = -Image-

    >> Colorize[{{1, 2}, {2, 2}, {2, 3}}, ColorFunction -> (Blend[{White, Blue}, #]&)]
     = -Image-
    """

    options = {"ColorFunction": "Automatic"}

    messages = {
        "cfun": "`1` is neither a gradient ColorData nor a pure function suitable as ColorFunction."
    }

    def apply(self, values, evaluation, options):
        "Colorize[values_, OptionsPattern[%(name)s]]"

        if isinstance(values, Image):
            pixels = values.grayscale().pixels
            matrix = pixels_as_ubyte(pixels.reshape(pixels.shape[:2]))
        else:
            if not Expression("MatrixQ", values).evaluate(evaluation).is_true():
                return
            matrix = matrix_to_numpy(values)

        a, n = _linearize(matrix)
        # the maximum value for n is the number of pixels in a, which is acceptable and never too large.

        color_function = self.get_option(options, "ColorFunction", evaluation)
        if (
            isinstance(color_function, Symbol)
            and color_function.get_name() == "System`Automatic"
        ):
            color_function = String("LakeColors")

        from mathics.builtin.plot import gradient_palette

        cmap = gradient_palette(color_function, n, evaluation)
        if not cmap:
            evaluation.message("Colorize", "cfun", color_function)
            return

        s = (a.shape[0], a.shape[1], 1)
        p = numpy.transpose(numpy.array([cmap[i] for i in range(n)])[:, 0:3])
        return Image(
            numpy.concatenate([p[i][a].reshape(s) for i in range(3)], axis=2),
            color_space="RGB",
        )


class DominantColors(_ImageBuiltin):
    """
    <dl>
    <dt>'DominantColors[$image$]'
      <dd>gives a list of colors which are dominant in the given image.
    <dt>'DominantColors[$image$, $n$]'
      <dd>returns at most $n$ colors.
    <dt>'DominantColors[$image$, $n$, $prop$]'
      <dd>returns the given property $prop$, which may be "Color" (return RGB colors), "LABColor" (return
      LAB colors), "Count" (return the number of pixels a dominant color covers), "Coverage" (return the
      fraction of the image a dominant color covers), or "CoverageImage" (return a black and white image
      indicating with white the parts that are covered by a dominant color).
    </dl>

    The option "ColorCoverage" specifies the minimum amount of coverage needed to include a dominant color
    in the result.

    The option "MinColorDistance" specifies the distance (in LAB color space) up to which colors are merged
    and thus regarded as belonging to the same dominant color.

    >> img = Import["ExampleData/lena.tif"]
     = -Image-

    >> DominantColors[img]
     = {RGBColor[0.827451, 0.537255, 0.486275], RGBColor[0.87451, 0.439216, 0.45098], RGBColor[0.341176, 0.0705882, 0.254902], RGBColor[0.690196, 0.266667, 0.309804], RGBColor[0.533333, 0.192157, 0.298039], RGBColor[0.878431, 0.760784, 0.721569]}

    >> DominantColors[img, 3]
     = {RGBColor[0.827451, 0.537255, 0.486275], RGBColor[0.87451, 0.439216, 0.45098], RGBColor[0.341176, 0.0705882, 0.254902]}

    >> DominantColors[img, 3, "Coverage"]
     = {28579 / 131072, 751 / 4096, 23841 / 131072}

    >> DominantColors[img, 3, "CoverageImage"]
     = {-Image-, -Image-, -Image-}

    >> DominantColors[img, 3, "Count"]
     = {57158, 48064, 47682}

    >> DominantColors[img, 2, "LABColor"]
     = {LABColor[0.646831, 0.279785, 0.193184], LABColor[0.608465, 0.443559, 0.195911]}

    >> DominantColors[img, MinColorDistance -> 0.5]
     = {RGBColor[0.87451, 0.439216, 0.45098], RGBColor[0.341176, 0.0705882, 0.254902]}

    >> DominantColors[img, ColorCoverage -> 0.15]
     = {RGBColor[0.827451, 0.537255, 0.486275], RGBColor[0.87451, 0.439216, 0.45098], RGBColor[0.341176, 0.0705882, 0.254902]}
    """

    rules = {
        "DominantColors[image_Image, n_Integer, options___]": 'DominantColors[image, n, "Color", options]',
        "DominantColors[image_Image, options___]": 'DominantColors[image, 256, "Color", options]',
    }

    options = {"ColorCoverage": "Automatic", "MinColorDistance": "Automatic"}

    def apply(self, image, n, prop, evaluation, options):
        "DominantColors[image_Image, n_Integer, prop_String, OptionsPattern[%(name)s]]"

        py_prop = prop.get_string_value()
        if py_prop not in ("Color", "LABColor", "Count", "Coverage", "CoverageImage"):
            return

        color_coverage = self.get_option(options, "ColorCoverage", evaluation)
        min_color_distance = self.get_option(options, "MinColorDistance", evaluation)

        if (
            isinstance(min_color_distance, Symbol)
            and min_color_distance.get_name() == "System`Automatic"
        ):
            py_min_color_distance = 0.15
        else:
            py_min_color_distance = min_color_distance.round_to_float()
            if py_min_color_distance is None:
                return

        if (
            isinstance(color_coverage, Symbol)
            and color_coverage.get_name() == "System`Automatic"
        ):
            py_min_color_coverage = 0.05
            py_max_color_coverage = 1.0
        elif color_coverage.has_form("List", 2):
            py_min_color_coverage = color_coverage.leaves[0].round_to_float()
            py_max_color_coverage = color_coverage.leaves[1].round_to_float()
        else:
            py_min_color_coverage = color_coverage.round_to_float()
            py_max_color_coverage = 1.0

        if py_min_color_coverage is None or py_max_color_coverage is None:
            return

        at_most = n.get_int_value()

        if at_most > 256:
            return

        # reduce complexity by reducing to 256 colors. this is not uncommon; see Kiranyaz et al.,
        # "Perceptual Dominant Color Extraction by Multidimensional Particle Swarm Optimization":
        # "to reduce the computational complexity [...] a preprocessing step, which creates a
        # limited color palette in RGB color domain, is first performed."

        im = (
            image.color_convert("RGB")
            .pil()
            .convert("P", palette=PIL.Image.ADAPTIVE, colors=256)
        )
        pixels = numpy.array(list(im.getdata()))

        flat = numpy.array(list(im.getpalette())) / 255.0  # float values now
        rgb_palette = [flat[i : i + 3] for i in range(0, len(flat), 3)]  # group by 3
        lab_palette = [
            numpy.array(x) for x in convert_color(rgb_palette, "RGB", "LAB", False)
        ]

        bins = numpy.bincount(pixels, minlength=len(rgb_palette))
        num_pixels = im.size[0] * im.size[1]

        from mathics.algorithm.clusters import (
            agglomerate,
            PrecomputedDistances,
            FixedDistanceCriterion,
        )

        norm = numpy.linalg.norm

        def df(i, j):
            return norm(lab_palette[i] - lab_palette[j])

        lab_distances = [df(i, j) for i in range(len(lab_palette)) for j in range(i)]

        if py_prop == "LABColor":
            out_palette = lab_palette
            out_palette_head = "LABColor"
        else:
            out_palette = rgb_palette
            out_palette_head = "RGBColor"

        dominant = agglomerate(
            (out_palette, bins),
            (FixedDistanceCriterion, {"merge_limit": py_min_color_distance}),
            PrecomputedDistances(lab_distances),
            mode="dominant",
        )

        def result():
            min_count = max(0, int(num_pixels * py_min_color_coverage))
            max_count = min(num_pixels, int(num_pixels * py_max_color_coverage))

            for prototype, count, members in dominant:
                if max_count >= count > min_count:
                    if py_prop == "Count":
                        yield Integer(count)
                    elif py_prop == "Coverage":
                        yield Rational(int(count), num_pixels)
                    elif py_prop == "CoverageImage":
                        mask = numpy.ndarray(shape=pixels.shape, dtype=numpy.bool)
                        mask.fill(0)
                        for i in members:
                            mask = mask | (pixels == i)
                        yield Image(mask.reshape(tuple(reversed(im.size))), "Grayscale")
                    else:
                        yield Expression(out_palette_head, *prototype)

        return Expression(SymbolList, *itertools.islice(result(), 0, at_most))


# pixel access


class ImageData(_ImageBuiltin):
    """
    <dl>
    <dt>'ImageData[$image$]'
      <dd>gives a list of all color values of $image$ as a matrix.
    <dt>'ImageData[$image$, $stype$]'
      <dd>gives a list of color values in type $stype$.
    </dl>

    >> img = Image[{{0.2, 0.4}, {0.9, 0.6}, {0.5, 0.8}}];
    >> ImageData[img]
     = {{0.2, 0.4}, {0.9, 0.6}, {0.5, 0.8}}

    >> ImageData[img, "Byte"]
     = {{51, 102}, {229, 153}, {127, 204}}

    >> ImageData[Image[{{0, 1}, {1, 0}, {1, 1}}], "Bit"]
     = {{0, 1}, {1, 0}, {1, 1}}

    #> ImageData[img, "Bytf"]
     : Unsupported pixel format "Bytf".
     = ImageData[-Image-, Bytf]
    """

    rules = {"ImageData[image_Image]": 'ImageData[image, "Real"]'}

    messages = {"pixelfmt": 'Unsupported pixel format "``".'}

    def apply(self, image, stype, evaluation):
        "ImageData[image_Image, stype_String]"
        pixels = image.pixels
        stype = stype.get_string_value()
        if stype == "Real":
            pixels = pixels_as_float(pixels)
        elif stype == "Byte":
            pixels = pixels_as_ubyte(pixels)
        elif stype == "Bit16":
            pixels = pixels_as_uint(pixels)
        elif stype == "Bit":
            pixels = pixels.astype(numpy.int)
        else:
            return evaluation.message("ImageData", "pixelfmt", stype)
        return from_python(numpy_to_matrix(pixels))


class ImageTake(_ImageBuiltin):
    """
    <dl>
    <dt>'ImageTake[$image$, $n$]'
      <dd>gives the first $n$ rows of $image$.
    <dt>'ImageTake[$image$, -$n$]'
      <dd>gives the last $n$ rows of $image$.
    <dt>'ImageTake[$image$, {$r1$, $r2$}]'
      <dd>gives rows $r1$, ..., $r2$ of $image$.
    <dt>'ImageTake[$image$, {$r1$, $r2$}, {$c1$, $c2$}]'
      <dd>gives a cropped version of $image$.
    </dl>
    """

    def apply(self, image, n, evaluation):
        "ImageTake[image_Image, n_Integer]"
        py_n = n.get_int_value()
        if py_n >= 0:
            pixels = image.pixels[:py_n]
        elif py_n < 0:
            pixels = image.pixels[py_n:]
        return Image(pixels, image.color_space)

    def _slice(self, image, i1, i2, axis):
        n = image.pixels.shape[axis]
        py_i1 = min(max(i1.get_int_value() - 1, 0), n - 1)
        py_i2 = min(max(i2.get_int_value() - 1, 0), n - 1)

        def flip(pixels):
            if py_i1 > py_i2:
                return numpy_flip(pixels, axis)
            else:
                return pixels

        return slice(min(py_i1, py_i2), 1 + max(py_i1, py_i2)), flip

    def apply_rows(self, image, r1, r2, evaluation):
        "ImageTake[image_Image, {r1_Integer, r2_Integer}]"
        s, f = self._slice(image, r1, r2, 0)
        return Image(f(image.pixels[s]), image.color_space)

    def apply_rows_cols(self, image, r1, r2, c1, c2, evaluation):
        "ImageTake[image_Image, {r1_Integer, r2_Integer}, {c1_Integer, c2_Integer}]"
        sr, fr = self._slice(image, r1, r2, 0)
        sc, fc = self._slice(image, c1, c2, 1)
        return Image(fc(fr(image.pixels[sr, sc])), image.color_space)


class PixelValue(_ImageBuiltin):
    """
    <dl>
    <dt>'PixelValue[$image$, {$x$, $y$}]'
      <dd>gives the value of the pixel at position {$x$, $y$} in $image$.
    </dl>

    >> lena = Import["ExampleData/lena.tif"];
    >> PixelValue[lena, {1, 1}]
     = {0.321569, 0.0862745, 0.223529}
    #> {82 / 255, 22 / 255, 57 / 255} // N  (* pixel byte values from bottom left corner *)
     = {0.321569, 0.0862745, 0.223529}

    #> PixelValue[lena, {0, 1}];
     : Padding not implemented for PixelValue.
    #> PixelValue[lena, {512, 1}]
     = {0.72549, 0.290196, 0.317647}
    #> PixelValue[lena, {513, 1}];
     : Padding not implemented for PixelValue.
    #> PixelValue[lena, {1, 0}];
     : Padding not implemented for PixelValue.
    #> PixelValue[lena, {1, 512}]
     = {0.886275, 0.537255, 0.490196}
    #> PixelValue[lena, {1, 513}];
     : Padding not implemented for PixelValue.
    """

    messages = {"nopad": "Padding not implemented for PixelValue."}

    def apply(self, image, x, y, evaluation):
        "PixelValue[image_Image, {x_?RealNumberQ, y_?RealNumberQ}]"
        x = int(x.round_to_float())
        y = int(y.round_to_float())
        height = image.pixels.shape[0]
        width = image.pixels.shape[1]
        if not (1 <= x <= width and 1 <= y <= height):
            return evaluation.message("PixelValue", "nopad")
        pixel = pixels_as_float(image.pixels)[height - y, x - 1]
        if isinstance(pixel, (numpy.ndarray, numpy.generic, list)):
            return Expression(SymbolList, *[MachineReal(float(x)) for x in list(pixel)])
        else:
            return MachineReal(float(pixel))


class PixelValuePositions(_ImageBuiltin):
    """
    <dl>
    <dt>'PixelValuePositions[$image$, $val$]'
      <dd>gives the positions of all pixels in $image$ that have value $val$.
    </dl>

    >> PixelValuePositions[Image[{{0, 1}, {1, 0}, {1, 1}}], 1]
     = {{1, 1}, {1, 2}, {2, 1}, {2, 3}}

    >> PixelValuePositions[Image[{{0.2, 0.4}, {0.9, 0.6}, {0.3, 0.8}}], 0.5, 0.15]
     = {{2, 2}, {2, 3}}

    >> img = Import["ExampleData/lena.tif"];
    >> PixelValuePositions[img, 3 / 255, 0.5 / 255]
     = {{180, 192, 2}, {181, 192, 2}, {181, 193, 2}, {188, 204, 2}, {265, 314, 2}, {364, 77, 2}, {365, 72, 2}, {365, 73, 2}, {365, 77, 2}, {366, 70, 2}, {367, 65, 2}}
    >> PixelValue[img, {180, 192}]
     = {0.25098, 0.0117647, 0.215686}
    """

    rules = {
        "PixelValuePositions[image_Image, val_?RealNumberQ]": "PixelValuePositions[image, val, 0]"
    }

    def apply(self, image, val, d, evaluation):
        "PixelValuePositions[image_Image, val_?RealNumberQ, d_?RealNumberQ]"
        val = val.round_to_float()
        d = d.round_to_float()

        positions = numpy.argwhere(
            numpy.isclose(pixels_as_float(image.pixels), val, atol=d, rtol=0)
        )

        # python indexes from 0 at top left -> indices from 1 starting at bottom left
        # if single channel then ommit channel indices
        height = image.pixels.shape[0]
        if image.pixels.shape[2] == 1:
            result = sorted((j + 1, height - i) for i, j, k in positions.tolist())
        else:
            result = sorted(
                (j + 1, height - i, k + 1) for i, j, k in positions.tolist()
            )
        return Expression(SymbolList, *(Expression(SymbolList, *arg) for arg in result))


# image attribute queries


class ImageDimensions(_ImageBuiltin):
    """
    <dl>
    <dt>'ImageDimensions[$image$]'
      <dd>Returns the dimensions of $image$ in pixels.
    </dl>

    >> lena = Import["ExampleData/lena.tif"];
    >> ImageDimensions[lena]
     = {512, 512}

    >> ImageDimensions[RandomImage[1, {50, 70}]]
     = {50, 70}

    #> Image[{{0, 1}, {1, 0}, {1, 1}}] // ImageDimensions
     = {2, 3}
    #> Image[{{0.2, 0.4}, {0.9, 0.6}, {0.3, 0.8}}] // ImageDimensions
     = {2, 3}
    """

    def apply(self, image, evaluation):
        "ImageDimensions[image_Image]"
        return Expression(SymbolList, *image.dimensions())


class ImageAspectRatio(_ImageBuiltin):
    """
    <dl>
    <dt>'ImageAspectRatio[$image$]'
      <dd>gives the aspect ratio of $image$.
    </dl>

    >> img = Import["ExampleData/lena.tif"];
    >> ImageAspectRatio[img]
     = 1

    >> ImageAspectRatio[Image[{{0, 1}, {1, 0}, {1, 1}}]]
     = 3 / 2
    """

    def apply(self, image, evaluation):
        "ImageAspectRatio[image_Image]"
        dim = image.dimensions()
        return Expression("Divide", Integer(dim[1]), Integer(dim[0]))


class ImageChannels(_ImageBuiltin):
    """
    <dl>
    <dt>'ImageChannels[$image$]'
      <dd>gives the number of channels in $image$.
    </dl>

    >> ImageChannels[Image[{{0, 1}, {1, 0}}]]
     = 1

    >> img = Import["ExampleData/lena.tif"];
    >> ImageChannels[img]
     = 3
    """

    def apply(self, image, evaluation):
        "ImageChannels[image_Image]"
        return Integer(image.channels())


class ImageType(_ImageBuiltin):
    """
    <dl>
    <dt>'ImageType[$image$]'
      <dd>gives the interval storage type of $image$, e.g. "Real", "Bit32", or "Bit".
    </dl>

    >> img = Import["ExampleData/lena.tif"];
    >> ImageType[img]
     = Byte

    >> ImageType[Image[{{0, 1}, {1, 0}}]]
     = Real

    >> ImageType[Binarize[img]]
     = Bit

    """

    def apply(self, image, evaluation):
        "ImageType[image_Image]"
        return String(image.storage_type())


class BinaryImageQ(_ImageTest):
    """
    <dl>
    <dt>'BinaryImageQ[$image]'
      <dd>returns True if the pixels of $image are binary bit values, and False otherwise.
    </dl>

    >> img = Import["ExampleData/lena.tif"];
    S> BinaryImageQ[img]
     = False

    S> BinaryImageQ[Binarize[img]]
     = True
    """

    def test(self, expr):
        return isinstance(expr, Image) and expr.storage_type() == "Bit"


# Image core classes


def _image_pixels(matrix):
    try:
        pixels = numpy.array(matrix, dtype="float64")
    except ValueError:  # irregular array, e.g. {{0, 1}, {0, 1, 1}}
        return None
    shape = pixels.shape
    if len(shape) == 2 or (len(shape) == 3 and shape[2] in (1, 3, 4)):
        return pixels
    else:
        return None


class ImageQ(_ImageTest):
    """
    <dl>
    <dt>'ImageQ[Image[$pixels]]'
      <dd>returns True if $pixels has dimensions from which an Image can be constructed, and False otherwise.
    </dl>

    >> ImageQ[Image[{{0, 1}, {1, 0}}]]
     = True

    >> ImageQ[Image[{{{0, 0, 0}, {0, 1, 0}}, {{0, 1, 0}, {0, 1, 1}}}]]
     = True

    >> ImageQ[Image[{{{0, 0, 0}, {0, 1}}, {{0, 1, 0}, {0, 1, 1}}}]]
     = False

    >> ImageQ[Image[{1, 0, 1}]]
     = False

    >> ImageQ["abc"]
     = False
    """

    def test(self, expr):
        return isinstance(expr, Image)


class ImageBox(BoxConstruct):
    def boxes_to_text(self, leaves=None, **options):
        return "-Image-"

    def boxes_to_mathml(self, leaves=None, **options):
        if leaves is None:
            leaves = self._leaves
        # see https://tools.ietf.org/html/rfc2397
        return '<mglyph src="%s" width="%dpx" height="%dpx" />' % (
            leaves[0].get_string_value(),
            leaves[1].get_int_value(),
            leaves[2].get_int_value(),
        )

    def boxes_to_tex(self, leaves=None, **options):
        return "-Image-"


class Image(Atom):
    def __init__(self, pixels, color_space, metadata={}, **kwargs):
        super(Image, self).__init__(**kwargs)
        if len(pixels.shape) == 2:
            pixels = pixels.reshape(list(pixels.shape) + [1])
        self.pixels = pixels
        self.color_space = color_space
        self.metadata = metadata

    def filter(self, f):  # apply PIL filters component-wise
        pixels = self.pixels
        n = pixels.shape[2]
        channels = [
            f(PIL.Image.fromarray(c, "L")) for c in (pixels[:, :, i] for i in range(n))
        ]
        return Image(numpy.dstack(channels), self.color_space)

    def pil(self):
        # see http://pillow.readthedocs.io/en/3.1.x/handbook/concepts.html#concept-modes
        n = self.channels()

        if n == 1:
            dtype = self.pixels.dtype

            if dtype in (numpy.float32, numpy.float64):
                pixels = self.pixels.astype(numpy.float32)
                mode = "F"
            elif dtype == numpy.uint32:
                pixels = self.pixels
                mode = "I"
            else:
                pixels = pixels_as_ubyte(self.pixels)
                mode = "L"

            pixels = pixels.reshape(pixels.shape[:2])
        elif n == 3:
            if self.color_space == "LAB":
                mode = "LAB"
                pixels = self.pixels
            elif self.color_space == "HSB":
                mode = "HSV"
                pixels = self.pixels
            elif self.color_space == "RGB":
                mode = "RGB"
                pixels = self.pixels
            else:
                mode = "RGB"
                pixels = self.color_convert("RGB").pixels

            pixels = pixels_as_ubyte(pixels)
        elif n == 4:
            if self.color_space == "CMYK":
                mode = "CMYK"
                pixels = self.pixels
            elif self.color_space == "RGB":
                mode = "RGBA"
                pixels = self.pixels
            else:
                mode = "RGBA"
                pixels = self.color_convert("RGB").pixels

            pixels = pixels_as_ubyte(pixels)
        else:
            raise NotImplementedError

        return PIL.Image.fromarray(pixels, mode)

    def color_convert(self, to_color_space, preserve_alpha=True):
        if to_color_space == self.color_space and preserve_alpha:
            return self
        else:
            pixels = pixels_as_float(self.pixels)
            converted = convert_color(
                pixels, self.color_space, to_color_space, preserve_alpha
            )
            if converted is None:
                return None
            return Image(converted, to_color_space)

    def grayscale(self):
        return self.color_convert("Grayscale")

    def atom_to_boxes(self, f, evaluation):
        pixels = pixels_as_ubyte(self.color_convert("RGB", True).pixels)
        shape = pixels.shape

        width = shape[1]
        height = shape[0]
        scaled_width = width
        scaled_height = height

        if len(shape) >= 3 and shape[2] == 4:
            pixels_format = "RGBA"
        else:
            pixels_format = "RGB"

        pillow = PIL.Image.fromarray(pixels, pixels_format)

        # if the image is very small, scale it up using nearest neighbour.
        min_size = 128
        if width < min_size and height < min_size:
            scale = min_size / max(width, height)
            scaled_width = int(scale * width)
            scaled_height = int(scale * height)
            pillow = pillow.resize(
                (scaled_height, scaled_width), resample=PIL.Image.NEAREST
            )

        with warnings.catch_warnings():
            warnings.simplefilter("ignore")

            stream = BytesIO()
            pillow.save(stream, format="png")
            stream.seek(0)
            contents = stream.read()
            stream.close()

        encoded = base64.b64encode(contents)
        encoded = b"data:image/png;base64," + encoded

        return ImageBox(String(encoded), Integer(scaled_width), Integer(scaled_height))

    def __str__(self):
        return "-Image-"

    def do_copy(self):
        return Image(self.pixels, self.color_space, self.metadata)

    def default_format(self, evaluation, form):
        return "-Image-"

    def get_sort_key(self, pattern_sort=False):
        if pattern_sort:
            return super(Image, self).get_sort_key(True)
        else:
            return hash(self)

    def same(self, other):
        if not isinstance(other, Image):
            return False
        if self.color_space != other.color_space or self.metadata != other.metadata:
            return False
        return numpy.array_equal(self.pixels, other.pixels)

    def to_python(self, *args, **kwargs):
        return self.pixels

    def __hash__(self):
        return hash(
            (
                "Image",
                self.pixels.tobytes(),
                self.color_space,
                frozenset(self.metadata.items()),
            )
        )

    def dimensions(self):
        shape = self.pixels.shape
        return shape[1], shape[0]

    def channels(self):
        return self.pixels.shape[2]

    def storage_type(self):
        dtype = self.pixels.dtype
        if dtype in (numpy.float32, numpy.float64):
            return "Real"
        elif dtype == numpy.uint32:
            return "Bit32"
        elif dtype == numpy.uint16:
            return "Bit16"
        elif dtype == numpy.uint8:
            return "Byte"
        elif dtype == numpy.bool:
            return "Bit"
        else:
            return str(dtype)

    def options(self):
        return Expression(
            "List",
            Expression(SymbolRule, String("ColorSpace"), String(self.color_space)),
            Expression(SymbolRule, String("MetaInformation"), self.metadata),
        )


class ImageAtom(AtomBuiltin):
    """
    #> Image[{{{1,1,0},{0,1,1}}, {{1,0,1},{1,1,0}}}]
     = -Image-

    #> Image[{{{0,0,0,0.25},{0,0,0,0.5}}, {{0,0,0,0.5},{0,0,0,0.75}}}]
     = -Image-
    """

    requires = _image_requires

    def apply_create(self, array, evaluation):
        "Image[array_]"
        pixels = _image_pixels(array.to_python())
        if pixels is not None:
            shape = pixels.shape
            is_rgb = len(shape) == 3 and shape[2] in (3, 4)
            return Image(pixels.clip(0, 1), "RGB" if is_rgb else "Grayscale")
        else:
            return Expression("Image", array)


# complex operations


class TextRecognize(Builtin):
    """
    <dl>
    <dt>'TextRecognize[{$image$}]'
      <dd>Recognizes text in $image$ and returns it as string.
    </dl>
    """

    requires = _image_requires + ("pyocr",)

    messages = {
        "tool": "No text recognition tools were found in the paths available to the Mathics kernel.",
        "langinv": "No language data for `1` is available.",
        "lang": "Language `1` is not supported in your installation of `2`. Please install it.",
    }

    options = {"Language": '"English"'}

    def apply(self, image, evaluation, options):
        "TextRecognize[image_Image, OptionsPattern[%(name)s]]"
        import pyocr
        from mathics.builtin.codetables import iso639_3

        language = self.get_option(options, "Language", evaluation)
        if not isinstance(language, String):
            return
        py_language = language.get_string_value()
        py_language_code = iso639_3.get(py_language)

        if py_language_code is None:
            evaluation.message("TextRecognize", "langcode", py_language)
            return

        tools = pyocr.get_available_tools()
        if not tools:
            evaluation.message("TextRecognize", "tool")
            return
        best_tool = tools[0]

        langs = best_tool.get_available_languages()
        if py_language_code not in langs:
            # if we use Tesseract, then this means copying the necessary language files from
            # https://github.com/tesseract-ocr/tessdatainstalling to tessdata, which is
            # usually located at /usr/share/tessdata or similar, but there's no API to query
            # the exact location, so we cannot, for now, give a better message.

            evaluation.message(
                "TextRecognize", "lang", py_language, best_tool.get_name()
            )
            return

        import pyocr.builders

        text = best_tool.image_to_string(
            image.pil(), lang=py_language_code, builder=pyocr.builders.TextBuilder()
        )

        if isinstance(text, (list, tuple)):
            text = "\n".join(text)

        return String(text)


class WordCloud(Builtin):
    """
    <dl>
    <dt>'WordCloud[{$word1$, $word2$, ...}]'
      <dd>Gives a word cloud with the given list of words.
    <dt>'WordCloud[{$weight1$ -> $word1$, $weight2$ -> $word2$, ...}]'
      <dd>Gives a word cloud with the words weighted using the given weights.
    <dt>'WordCloud[{$weight1$, $weight2$, ...} -> {$word1$, $word2$, ...}]'
      <dd>Also gives a word cloud with the words weighted using the given weights.
    <dt>'WordCloud[{{$word1$, $weight1$}, {$word2$, $weight2$}, ...}]'
      <dd>Gives a word cloud with the words weighted using the given weights.
    </dl>

    >> WordCloud[StringSplit[Import["ExampleData/EinsteinSzilLetter.txt"]]]
     = -Image-

    >> WordCloud[Range[50] -> ToString /@ Range[50]]
     = -Image-
    """

    requires = _image_requires + ("wordcloud",)

    options = {"IgnoreCase": "True", "ImageSize": "Automatic", "MaxItems": "Automatic"}

    # this is the palettable.colorbrewer.qualitative.Dark2_8 palette
    default_colors = (
        (27, 158, 119),
        (217, 95, 2),
        (117, 112, 179),
        (231, 41, 138),
        (102, 166, 30),
        (230, 171, 2),
        (166, 118, 29),
        (102, 102, 102),
    )

    def apply_words_weights(self, weights, words, evaluation, options):
        "WordCloud[weights_List -> words_List, OptionsPattern[%(name)s]]"
        if len(weights.leaves) != len(words.leaves):
            return

        def weights_and_words():
            for weight, word in zip(weights.leaves, words.leaves):
                yield weight.round_to_float(), word.get_string_value()

        return self._word_cloud(weights_and_words(), evaluation, options)

    def apply_words(self, words, evaluation, options):
        "WordCloud[words_List, OptionsPattern[%(name)s]]"

        if not words:
            return
        elif isinstance(words.leaves[0], String):

            def weights_and_words():
                for word in words.leaves:
                    yield 1, word.get_string_value()

        else:

            def weights_and_words():
                for word in words.leaves:
                    if len(word.leaves) != 2:
                        raise ValueError

                    head_name = word.get_head_name()
                    if head_name == "System`Rule":
                        weight, s = word.leaves
                    elif head_name == "System`List":
                        s, weight = word.leaves
                    else:
                        raise ValueError

                    yield weight.round_to_float(), s.get_string_value()

        try:
            return self._word_cloud(weights_and_words(), evaluation, options)
        except ValueError:
            return

    def _word_cloud(self, words, evaluation, options):
        ignore_case = self.get_option(options, "IgnoreCase", evaluation) == Symbol(
            "True"
        )

        freq = defaultdict(int)
        for py_weight, py_word in words:
            if py_word is None or py_weight is None:
                return
            key = py_word.lower() if ignore_case else py_word
            freq[key] += py_weight

        max_items = self.get_option(options, "MaxItems", evaluation)
        if isinstance(max_items, Integer):
            py_max_items = max_items.get_int_value()
        else:
            py_max_items = 200

        image_size = self.get_option(options, "ImageSize", evaluation)
        if image_size == Symbol("Automatic"):
            py_image_size = (800, 600)
        elif (
            image_size.get_head_name() == "System`List" and len(image_size.leaves) == 2
        ):
            py_image_size = []
            for leaf in image_size.leaves:
                if not isinstance(leaf, Integer):
                    return
                py_image_size.append(leaf.get_int_value())
        elif isinstance(image_size, Integer):
            size = image_size.get_int_value()
            py_image_size = (size, size)
        else:
            return

        # inspired by http://minimaxir.com/2016/05/wordclouds/
        import random
        import os

        def color_func(
            word, font_size, position, orientation, random_state=None, **kwargs
        ):
            return self.default_colors[random.randint(0, 7)]

        font_base_path = os.path.dirname(os.path.abspath(__file__)) + "/../fonts/"

        font_path = os.path.realpath(font_base_path + "AmaticSC-Bold.ttf")
        if not os.path.exists(font_path):
            font_path = None

        from wordcloud import WordCloud

        wc = WordCloud(
            width=py_image_size[0],
            height=py_image_size[1],
            font_path=font_path,
            max_font_size=300,
            mode="RGB",
            background_color="white",
            max_words=py_max_items,
            color_func=color_func,
            random_state=42,
            stopwords=set(),
        )
        wc.generate_from_frequencies(freq)

        image = wc.to_image()
        return Image(numpy.array(image), "RGB")
