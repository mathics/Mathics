'''
Image[] and image related functions.

Note that you (currently) need scikit-image installed in order for this module to work.
'''

from __future__ import division

from mathics.builtin.base import (
    Builtin, AtomBuiltin, Test, BoxConstruct, String)
from mathics.core.expression import (
    Atom, Expression, Integer, Rational, Real, Symbol, from_python)

import six
import base64
import functools
import math

_image_requires = (
    'skimage',
    'warnings',
    'numpy',
    'scipy',
    'PIL',
    'matplotlib',
)

try:
    import skimage
    import skimage.io
    import skimage.transform
    import skimage.filters
    import skimage.exposure
    import skimage.feature
    import skimage.filters.rank
    import skimage.morphology
    import skimage.measure

    import warnings

    import PIL
    import PIL.ImageEnhance
    import PIL.ImageOps
    import PIL.ImageFilter

    import numpy

    import matplotlib.cm

    _enabled = True
except ImportError:
    _enabled = False

from io import BytesIO


if _enabled:
    _color_space_conversions = {
        'RGB2Grayscale': skimage.color.rgb2gray,
        'Grayscale2RGB': skimage.color.gray2rgb,

        'HSV2RGB': skimage.color.hsv2rgb,
        'RGB2HSV': skimage.color.rgb2hsv,

        'LAB2LCH': skimage.color.lab2lch,
        'LCH2LAB': skimage.color.lch2lab,

        'LAB2RGB': skimage.color.lab2rgb,
        'LAB2XYZ': skimage.color.lab2xyz,

        'LUV2RGB': skimage.color.luv2rgb,
        'LUV2XYZ': skimage.color.luv2xyz,

        'RGB2LAB': skimage.color.rgb2lab,
        'RGB2LUV': skimage.color.rgb2luv,
        'RGB2XYZ': skimage.color.rgb2xyz,

        'XYZ2LAB': skimage.color.xyz2lab,
        'XYZ2LUV': skimage.color.xyz2luv,
        'XYZ2RGB': skimage.color.xyz2rgb,
    }


class _ImageBuiltin(Builtin):
    requires = _image_requires


class _ImageTest(Test):
    requires = _image_requires

# import and export


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
        '''ImageImport[path_?StringQ]'''
        pixels = skimage.io.imread(path.get_string_value())
        is_rgb = len(pixels.shape) >= 3 and pixels.shape[2] >= 3
        atom = Image(pixels, 'RGB' if is_rgb else 'Grayscale')
        return Expression('List', Expression('Rule', String('Image'), atom))


class ImageExport(_ImageBuiltin):
    messages = {
        'noimage': 'only an Image[] can be exported into an image file'
    }

    def apply(self, path, expr, opts, evaluation):
        '''ImageExport[path_?StringQ, expr_, opts___]'''
        if isinstance(expr, Image):
            skimage.io.imsave(path.get_string_value(), expr.pixels)
            return Symbol('Null')
        else:
            return evaluation.message('ImageExport', 'noimage')


# image math


class _ImageArithmetic(_ImageBuiltin):
    messages = {
        'bddarg': 'Expecting a number, image, or graphics instead of `1`.',
    }

    @staticmethod
    def convert_Image(image):
        assert isinstance(image, Image)
        return skimage.img_as_float(image.pixels)

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
        '%(name)s[image_Image, args__]'
        images, arg = self.convert_args(image, *args.get_sequence())
        if images is None:
            return evaluation.message(self.get_name(), 'bddarg', arg)
        ufunc = getattr(numpy, self.get_name(True)[5:].lower())
        result = self._reduce(images, ufunc).clip(0, 1)
        return Image(result, image.color_space)


class ImageAdd(_ImageArithmetic):
    '''
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
    '''


class ImageSubtract(_ImageArithmetic):
    '''
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
    '''


class ImageMultiply(_ImageArithmetic):
    '''
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
    '''


class RandomImage(_ImageBuiltin):
    '''
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
    '''

    options = {
        'ColorSpace': 'Automatic',
    }

    rules = {
        'RandomImage[]': 'RandomImage[{0, 1}, {150, 150}]',
        'RandomImage[max_?RealNumberQ]': 'RandomImage[{0, max}, {150, 150}]',
        'RandomImage[{minval_?RealNumberQ, maxval_?RealNumberQ}]': 'RandomImage[{minval, maxval}, {150, 150}]',
        'RandomImage[max_?RealNumberQ, {w_Integer, h_Integer}]': 'RandomImage[{0, max}, {w, h}]',
    }

    messages = {
        'bddim': 'The specified dimension `1` should be a pair of positive integers.',
        'imgcstype': '`1` is an invalid color space specification.',
    }

    def apply(self, minval, maxval, w, h, evaluation, options):
        'RandomImage[{minval_?RealNumberQ, maxval_?RealNumberQ}, {w_Integer, h_Integer}, OptionsPattern[RandomImage]]'
        color_space = self.get_option(options, 'ColorSpace', evaluation)
        if isinstance(color_space, Symbol) and color_space.get_name() == 'System`Automatic':
            cs = 'Grayscale'
        else:
            cs = color_space.get_string_value()
        size = [w.get_int_value(), h.get_int_value()]
        if size[0] <= 0 or size[1] <= 0:
            return evaluation.message('RandomImage', 'bddim', from_python(size))
        minrange, maxrange = minval.round_to_float(), maxval.round_to_float()

        if cs == 'Grayscale':
            data = numpy.random.rand(size[1], size[0]) * (maxrange - minrange) + minrange
        elif cs == 'RGB':
            data = numpy.random.rand(size[1], size[0], 3) * (maxrange - minrange) + minrange
        else:
            return evaluation.message('RandomImage', 'imgcstype', color_space)
        return Image(data, cs)


# simple image manipulation


class ImageResize(_ImageBuiltin):
    '''
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
    '''

    options = {
        'Resampling': 'Automatic',
    }

    messages = {
        'imgrssz': 'The size `1` is not a valid image size specification.',
        'imgrsm': 'Invalid resampling method `1`.',
        'gaussaspect': 'Gaussian resampling needs to maintain aspect ratio.',
    }

    def _get_image_size_spec(self, old_size, new_size):
        predefined_sizes = {
            'System`Tiny': 75,
            'System`Small': 150,
            'System`Medium': 300,
            'System`Large': 450,
            'System`Automatic': 0,      # placeholder
        }
        result = new_size.round_to_float()
        if result is not None:
            result = int(result)
            if result <= 0:
                return None
            return result

        if isinstance(new_size, Symbol):
            name = new_size.get_name()
            if name == 'System`All':
                return old_size
            return predefined_sizes.get(name, None)
        if new_size.has_form('Scaled', 1):
            s = new_size.leaves[0].round_to_float()
            if s is None:
                return None
            return max(1, old_size * s)     # handle negative s values silently
        return None

    def apply_resize_width(self, image, s, evaluation, options):
        'ImageResize[image_Image, s_, OptionsPattern[ImageResize]]'
        old_w = image.pixels.shape[1]
        if s.has_form('List', 1):
            width = s.leaves[0]
        else:
            width = s
        w = self._get_image_size_spec(old_w, width)
        if w is None:
            return evaluation.message('ImageResize', 'imgrssz', s)
        if s.has_form('List', 1):
            height = width
        else:
            height = Symbol('Automatic')
        return self.apply_resize_width_height(image, width, height, evaluation, options)

    def apply_resize_width_height(self, image, width, height, evaluation, options):
        'ImageResize[image_Image, {width_, height_}, OptionsPattern[ImageResize]]'
        # resampling method
        resampling = self.get_option(options, 'Resampling', evaluation)
        if isinstance(resampling, Symbol) and resampling.get_name() == 'System`Automatic':
            resampling_name = 'Bicubic'
        else:
            resampling_name = resampling.get_string_value()

        # find new size
        old_w, old_h = image.pixels.shape[1], image.pixels.shape[0]
        w = self._get_image_size_spec(old_w, width)
        h = self._get_image_size_spec(old_h, height)
        if h is None or w is None:
            return evaluation.message('ImageResize', 'imgrssz', Expression('List', width, height))

        # handle Automatic
        old_aspect_ratio = old_w / old_h
        if w == 0 and h == 0:
            # if both width and height are Automatic then use old values
            w, h = old_w, old_h
        elif w == 0:
            w = max(1, h * old_aspect_ratio)
        elif h == 0:
            h = max(1, w / old_aspect_ratio)

        if resampling_name != 'Gaussian':
            # Gaussian need to unrounded values to compute scaling ratios.
            # round to closest pixel for other methods.
            h, w = int(round(h)), int(round(w))

        # perform the resize
        if resampling_name == 'Nearest':
            pixels = skimage.transform.resize(image.pixels, (h, w), order=0)
        elif resampling_name == 'Bicubic':
            pixels = skimage.transform.resize(image.pixels, (h, w), order=3)
        elif resampling_name == 'Gaussian':
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
                return evaluation.message('ImageResize', 'gaussaspect')
            elif s > 1:
                pixels = skimage.transform.pyramid_expand(image.pixels, upscale=s).clip(0, 1)
            else:
                pixels = skimage.transform.pyramid_reduce(image.pixels, downscale=1 / s).clip(0, 1)
        else:
            return evaluation.message('ImageResize', 'imgrsm', resampling)

        return Image(pixels, image.color_space)


class ImageReflect(_ImageBuiltin):
    '''
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
    #> ImageReflect[ein, Left -> Right] == ImageReflect[Right -> Left] == ImageReflect[ein, Left] == ImageReflect[ein, Right]
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
    '''

    rules = {
        'ImageReflect[image_Image]': 'ImageReflect[image, Top -> Bottom]',
        'ImageReflect[image_Image, Top|Bottom]': 'ImageReflect[image, Top -> Bottom]',
        'ImageReflect[image_Image, Left|Right]': 'ImageReflect[image, Left -> Right]',
    }

    messages = {
        'bdrfl2': '`1` is not a valid 2D reflection specification.',
    }

    def apply(self, image, orig, dest, evaluation):
        'ImageReflect[image_Image, Rule[orig_, dest_]]'
        if isinstance(orig, Symbol) and isinstance(dest, Symbol):
            specs = [orig.get_name(), dest.get_name()]
            specs.sort()    # `Top -> Bottom` is the same as `Bottom -> Top`

        anti_transpose = lambda i: numpy.flipud(numpy.transpose(numpy.flipud(i)))
        no_op = lambda i: i

        method = {
            ('System`Bottom', 'System`Top'): numpy.flipud,
            ('System`Left', 'System`Right'): numpy.fliplr,
            ('System`Left', 'System`Top'): numpy.transpose,
            ('System`Right', 'System`Top'): anti_transpose,
            ('System`Bottom', 'System`Left'): anti_transpose,
            ('System`Bottom', 'System`Right'): numpy.transpose,
            ('System`Bottom', 'System`Bottom'): no_op,
            ('System`Top', 'System`Top'): no_op,
            ('System`Left', 'System`Left'): no_op,
            ('System`Right', 'System`Right'): no_op,
        }.get(tuple(specs), None)

        if method is None:
            return evaluation.message('ImageReflect', 'bdrfl2', Expression('Rule', orig, dest))

        return Image(method(image.pixels), image.color_space)


class ImageRotate(_ImageBuiltin):
    '''
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
    '''

    rules = {
        'ImageRotate[i_Image]': 'ImageRotate[i, 90 Degree]'
    }

    messages = {
        'imgang': 'Angle `1` should be a real number, one of Top, Bottom, Left, Right, or a rule from one to another.',
    }

    def apply(self, image, angle, evaluation):
        'ImageRotate[image_Image, angle_]'
        py_angle = angle.to_python(n_evaluation=evaluation)
        if not isinstance(py_angle, six.integer_types + (float,)):
            return evaluation.message('ImageRotate', 'imgang', angle)
        return Image(skimage.transform.rotate(image.pixels, 180 * py_angle / math.pi, resize=True), image.color_space)


class ImagePartition(_ImageBuiltin):
    '''
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
    '''

    rules = {
        'ImagePartition[i_Image, s_Integer]': 'ImagePartition[i, {s, s}]'
    }

    messages = {
        'arg2': '`1` is not a valid size specification for image partitions.',
    }

    def apply(self, image, w, h, evaluation):
        'ImagePartition[image_Image, {w_Integer, h_Integer}]'
        w = w.get_int_value()
        h = h.get_int_value()
        if w <= 0 or h <= 0:
            return evaluation.message('ImagePartition', 'arg2', from_python([w, h]))
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
    '''
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

    #> img = Image[{{0.1, 0.5}, {0.5, 0.9}}];
    #> ImageData[ImageAdjust[img]]
     = {{0., 0.5}, {0.5, 1.}}
    '''

    rules = {
        'ImageAdjust[image_Image, c_?RealNumberQ]': 'ImageAdjust[image, {c, 0, 1}]',
        'ImageAdjust[image_Image, {c_?RealNumberQ, b_?RealNumberQ}]': 'ImageAdjust[image, {c, b, 1}]',
    }

    def apply_auto(self, image, evaluation):
        'ImageAdjust[image_Image]'
        pixels = skimage.img_as_float(image.pixels)

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
        'ImageAdjust[image_Image, {c_?RealNumberQ, b_?RealNumberQ, g_?RealNumberQ}]'
        im = image.pil()

        # gamma
        g = g.to_python()
        if g != 1:
            im = PIL.ImageEnhance.Color(im).enhance(g)

        # brightness
        b = b.to_python()
        if b != 0:
            im = PIL.ImageEnhance.Brightness(im).enhance(b + 1)

        # contrast
        c = c.to_python()
        if c != 0:
            im = PIL.ImageEnhance.Contrast(im).enhance(c + 1)

        return Image(numpy.array(im), image.color_space)


class Blur(_ImageBuiltin):
    rules = {
        'Blur[i_Image]': 'Blur[i, 2]'
    }

    def apply(self, image, r, evaluation):
        'Blur[image_Image, r_?RealNumberQ]'
        return Image(numpy.array(image.pil().filter(
            PIL.ImageFilter.GaussianBlur(r.to_python()))), image.color_space)


class Sharpen(_ImageBuiltin):
    rules = {
        'Sharpen[i_Image]': 'Sharpen[i, 2]'
    }

    def apply(self, image, r, evaluation):
        'Sharpen[image_Image, r_?RealNumberQ]'
        return Image(numpy.array(image.pil().filter(
            PIL.ImageFilter.UnsharpMask(r.to_python()))), image.color_space)


class GaussianFilter(_ImageBuiltin):
    messages = {
        'only3': 'GaussianFilter only supports up to three channels.'
    }

    def apply_radius(self, image, radius, evaluation):
        'GaussianFilter[image_Image, radius_?RealNumberQ]'
        if len(image.pixels.shape) > 2 and image.pixels.shape[2] > 3:
            return evaluation.message('GaussianFilter', 'only3')
        else:
            return Image(skimage.filters.gaussian(
                skimage.img_as_float(image.pixels),
                sigma=radius.to_python() / 2, multichannel=True), image.color_space)


# morphological image filters


class PillowImageFilter(_ImageBuiltin):
    def compute(self, image, f):
        return Image(numpy.array(image.pil().filter(f)), image.color_space)


class MinFilter(PillowImageFilter):
    def apply(self, image, r, evaluation):
        'MinFilter[image_Image, r_Integer]'
        return self.compute(image, PIL.ImageFilter.MinFilter(1 + 2 * r.to_python()))


class MaxFilter(PillowImageFilter):
    def apply(self, image, r, evaluation):
        'MaxFilter[image_Image, r_Integer]'
        return self.compute(image, PIL.ImageFilter.MaxFilter(1 + 2 * r.to_python()))


class MedianFilter(PillowImageFilter):
    def apply(self, image, r, evaluation):
        'MedianFilter[image_Image, r_Integer]'
        return self.compute(image, PIL.ImageFilter.MedianFilter(1 + 2 * r.to_python()))


class EdgeDetect(_ImageBuiltin):
    rules = {
        'EdgeDetect[i_Image]': 'EdgeDetect[i, 2, 0.2]',
        'EdgeDetect[i_Image, r_?RealNumberQ]': 'EdgeDetect[i, r, 0.2]'
    }

    def apply(self, image, r, t, evaluation):
        'EdgeDetect[image_Image, r_?RealNumberQ, t_?RealNumberQ]'
        return Image(skimage.feature.canny(
            image.grayscale().pixels, sigma=r.to_python() / 2,
            low_threshold=0.5 * t.to_python(), high_threshold=t.to_python()),
            'Grayscale')


class BoxMatrix(_ImageBuiltin):
    def apply(self, r, evaluation):
        'BoxMatrix[r_?RealNumberQ]'
        s = 1 + 2 * r.to_python()
        return from_python(skimage.morphology.rectangle(s, s).tolist())


class DiskMatrix(_ImageBuiltin):
    def apply(self, r, evaluation):
        'DiskMatrix[r_?RealNumberQ]'
        return from_python(skimage.morphology.disk(r).tolist())


class DiamondMatrix(_ImageBuiltin):
    def apply(self, r, evaluation):
        'DiamondMatrix[r_?RealNumberQ]'
        return from_python(skimage.morphology.diamond(r).tolist())


class _MorphologyFilter(_ImageBuiltin):
    messages = {
        'grayscale': 'Your image has been converted to grayscale as color images are not supported yet.'
    }

    rules = {
        '%(name)s[i_Image, r_?RealNumberQ]': '%(name)s[i, BoxMatrix[r]]'
    }

    def apply(self, image, k, evaluation):
        '%(name)s[image_Image, k_?MatrixQ]'
        if image.color_space != 'Grayscale':
            image = image.color_convert('Grayscale')
            evaluation.message(self.name, 'grayscale')
        f = getattr(skimage.morphology, self.get_name(True).lower())
        img = f(image.pixels, numpy.array(k.to_python()))
        return Image(img, 'Grayscale')


class Dilation(_MorphologyFilter):
    '''
    <dl>
    <dt>'Dilation[$image$, $ker$]'
      <dd>Gives the morphological dilation of $image$ with respect to structuring element $ker$.
    </dl>

    >> ein = Import["ExampleData/Einstein.jpg"];
    >> Dilation[ein, 2.5]
     = -Image-
    '''


class Erosion(_MorphologyFilter):
    '''
    <dl>
    <dt>'Erosion[$image$, $ker$]'
      <dd>Gives the morphological erosion of $image$ with respect to structuring element $ker$.
    </dl>

    >> ein = Import["ExampleData/Einstein.jpg"];
    >> Erosion[ein, 2.5]
     = -Image-
    '''


class Opening(_MorphologyFilter):
    '''
    <dl>
    <dt>'Opening[$image$, $ker$]'
      <dd>Gives the morphological opening of $image$ with respect to structuring element $ker$.
    </dl>

    >> ein = Import["ExampleData/Einstein.jpg"];
    >> Opening[ein, 2.5]
     = -Image-
    '''


class Closing(_MorphologyFilter):
    '''
    <dl>
    <dt>'Closing[$image$, $ker$]'
      <dd>Gives the morphological closing of $image$ with respect to structuring element $ker$.
    </dl>

    >> ein = Import["ExampleData/Einstein.jpg"];
    >> Closing[ein, 2.5]
     = -Image-
    '''


class MorphologicalComponents(_ImageBuiltin):
    rules = {
        'MorphologicalComponents[i_Image]': 'MorphologicalComponents[i, 0]'
    }

    def apply(self, image, t, evaluation):
        'MorphologicalComponents[image_Image, t_?RealNumberQ]'
        pixels = skimage.img_as_ubyte(skimage.img_as_float(image.grayscale().pixels) > t.to_python())
        return from_python(skimage.measure.label(pixels, background=0, connectivity=2).tolist())


# color space


class ImageColorSpace(_ImageBuiltin):
    def apply(self, image, evaluation):
        'ImageColorSpace[image_Image]'
        return String(image.color_space)


class ColorConvert(_ImageBuiltin):
    def apply(self, image, colorspace, evaluation):
        'ColorConvert[image_Image, colorspace_String]'
        return image.color_convert(colorspace.get_string_value())


class ColorQuantize(_ImageBuiltin):
    def apply(self, image, n, evaluation):
        'ColorQuantize[image_Image, n_Integer]'
        pixels = skimage.img_as_ubyte(image.color_convert('RGB').pixels)
        im = PIL.Image.fromarray(pixels).quantize(n.to_python())
        im = im.convert('RGB')
        return Image(numpy.array(im), 'RGB')


class Threshold(_ImageBuiltin):
    options = {
        'Method': '"Cluster"'
    }

    messages = {
        'illegalmethod': 'Method `` is not supported.'
    }

    def apply(self, image, evaluation, options):
        'Threshold[image_Image, OptionsPattern[Threshold]]'
        pixels = image.grayscale().pixels

        method = self.get_option(options, 'Method', evaluation)
        method_name = method.get_string_value() if isinstance(method, String) else method.to_python()
        if method_name == 'Cluster':
            threshold = skimage.filters.threshold_otsu(pixels)
        elif method_name == 'Median':
            threshold = numpy.median(pixels)
        elif method_name == 'Mean':
            threshold = numpy.mean(pixels)
        else:
            return evaluation.message('Threshold', 'illegalmethod', method)

        return Real(threshold)


class Binarize(_ImageBuiltin):
    def apply(self, image, evaluation):
        'Binarize[image_Image]'
        image = image.grayscale()
        threshold = Expression('Threshold', image).evaluate(evaluation).to_python()
        return Image(image.pixels > threshold, 'Grayscale')

    def apply_t(self, image, t, evaluation):
        'Binarize[image_Image, t_?RealNumberQ]'
        pixels = image.grayscale().pixels
        return Image(pixels > t.to_python(), 'Grayscale')

    def apply_t1_t2(self, image, t1, t2, evaluation):
        'Binarize[image_Image, {t1_?RealNumberQ, t2_?RealNumberQ}]'
        pixels = image.grayscale().pixels
        mask1 = pixels > t1.to_python()
        mask2 = pixels < t2.to_python()
        return Image(mask1 * mask2, 'Grayscale')


class ColorNegate(_ImageBuiltin):
    def apply(self, image, evaluation):
        'ColorNegate[image_Image]'
        pixels = image.pixels
        anchor = numpy.ndarray(pixels.shape, dtype=pixels.dtype)
        anchor.fill(skimage.dtype_limits(pixels)[1])
        return Image(anchor - pixels, image.color_space)


class ColorSeparate(_ImageBuiltin):
    def apply(self, image, evaluation):
        'ColorSeparate[image_Image]'
        images = []
        pixels = image.pixels
        if len(pixels.shape) < 3:
            images.append(pixels)
        else:
            for i in range(pixels.shape[2]):
                images.append(Image(pixels[:, :, i], 'Grayscale'))
        return Expression('List', *images)


def _linearize(a):
    # this uses a vectorized binary search to compute
    # strictly sequential indices for all values in a.

    orig_shape = a.shape
    a = a.reshape((functools.reduce(lambda x, y: x*y, a.shape), ))  # 1 dimension

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
    def apply(self, a, evaluation):
        'Colorize[a_?MatrixQ]'

        a, n = _linearize(numpy.array(a.to_python()))
        # the maximum value for n is the number of pixels in a, which is acceptable and never too large.

        cmap = matplotlib.cm.get_cmap('hot', n)
        p = numpy.transpose(numpy.array([cmap(i) for i in range(n)])[:, 0:3])
        s = (a.shape[0], a.shape[1], 1)
        return Image(numpy.concatenate([p[i][a].reshape(s) for i in range(3)], axis=2), color_space='RGB')


# pixel access


class ImageData(_ImageBuiltin):
    rules = {
        'ImageData[image_Image]': 'ImageData[image, "Real"]'
    }

    messages = {
        'pixelfmt': 'unsupported pixel format "``"'
    }

    def apply(self, image, stype, evaluation):
        'ImageData[image_Image, stype_String]'
        pixels = image.pixels
        stype = stype.get_string_value()
        if stype == 'Real':
            pixels = skimage.img_as_float(pixels)
        elif stype == 'Byte':
            pixels = skimage.img_as_ubyte(pixels)
        elif stype == 'Bit16':
            pixels = skimage.img_as_uint(pixels)
        elif stype == 'Bit':
            pixels = pixels.as_dtype(numpy.bool)
        else:
            return evaluation.message('ImageData', 'pixelfmt', stype)
        return from_python(pixels.tolist())


class ImageTake(_ImageBuiltin):
    def apply(self, image, n, evaluation):
        'ImageTake[image_Image, n_Integer]'
        return Image(image.pixels[:int(n.to_python())], image.color_space)


class PixelValue(_ImageBuiltin):
    def apply(self, image, x, y, evaluation):
        'PixelValue[image_Image, {x_?RealNumberQ, y_?RealNumberQ}]'
        return Real(image.pixels[int(y.to_python() - 1), int(x.to_python() - 1)])


class PixelValuePositions(_ImageBuiltin):
    def apply(self, image, val, evaluation):
        'PixelValuePositions[image_Image, val_?RealNumberQ]'
        rows, cols = numpy.where(skimage.img_as_float(image.pixels) == float(val.to_python()))
        p = numpy.dstack((cols, rows)) + numpy.array([1, 1])
        return from_python(p.tolist())


# image attribute queries


class ImageDimensions(_ImageBuiltin):
    def apply(self, image, evaluation):
        'ImageDimensions[image_Image]'
        return Expression('List', *image.dimensions())


class ImageAspectRatio(_ImageBuiltin):
    def apply(self, image, evaluation):
        'ImageAspectRatio[image_Image]'
        dim = image.dimensions()
        return Real(dim[1] / float(dim[0]))


class ImageChannels(_ImageBuiltin):
    def apply(self, image, evaluation):
        'ImageChannels[image_Image]'
        return Integer(image.channels())


class ImageType(_ImageBuiltin):
    def apply(self, image, evaluation):
        'ImageType[image_Image]'
        return String(image.storage_type())


class BinaryImageQ(_ImageTest):
    '''
    <dl>
    <dt>'BinaryImageQ[$image]'
      <dd>returns True if the pixels of $image are binary bit values, and False otherwise.
    </dl>
    '''

    def test(self, expr):
        return isinstance(expr, Image) and expr.storage_type() == 'Bit'


# Image core classes


def _image_pixels(matrix):
    try:
        pixels = numpy.array(matrix, dtype='float64')
    except ValueError:  # irregular array, e.g. {{0, 1}, {0, 1, 1}}
        return None
    shape = pixels.shape
    if len(shape) == 2 or (len(shape) == 3 and shape[2] in (1, 3)):
        return pixels
    else:
        return None


class ImageQ(_ImageTest):
    '''
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
    '''

    def test(self, expr):
        return isinstance(expr, Image)


class ImageBox(BoxConstruct):
    def boxes_to_text(self, leaves, **options):
        return '-Image-'

    def boxes_to_xml(self, leaves, **options):
        # see https://tools.ietf.org/html/rfc2397
        return '<mglyph src="%s" width="%dpx" height="%dpx" />' % (
            leaves[0].get_string_value(), leaves[1].get_int_value(), leaves[2].get_int_value())

    def boxes_to_tex(self, leaves, **options):
        return '-Image-'


class Image(Atom):
    def __init__(self, pixels, color_space, **kwargs):
        super(Image, self).__init__(**kwargs)
        self.pixels = pixels
        self.color_space = color_space

    def pil(self):
        return PIL.Image.fromarray(self.pixels)

    def color_convert(self, to_color_space):
        if to_color_space == self.color_space:
            return self
        else:
            conversion = '%s2%s' % (self.color_space, to_color_space)
            if conversion in _color_space_conversions:
                return Image(_color_space_conversions[conversion](self.pixels), to_color_space)
            else:
                raise ValueError('cannot convert from color space %s to %s' % (self.color_space, to_color_space))

    def grayscale(self):
        return self.color_convert('Grayscale')

    def atom_to_boxes(self, f, evaluation):
        try:
            if self.color_space == 'Grayscale':
                pixels = self.pixels
            else:
                pixels = self.color_convert('RGB').pixels

            if pixels.dtype == numpy.bool:
                pixels = skimage.img_as_ubyte(pixels)

            shape = pixels.shape

            width = shape[1]
            height = shape[0]
            scaled_width = width
            scaled_height = height

            # if the image is very small, scale it up using nearest neighbour.
            min_size = 128
            if width < min_size and height < min_size:
                scale = min_size / max(width, height)
                scaled_width = int(scale * width)
                scaled_height = int(scale * height)
                pixels = skimage.transform.resize(pixels, (scaled_height, scaled_width), order=0)

            with warnings.catch_warnings():
                warnings.simplefilter("ignore")

                stream = BytesIO()
                skimage.io.imsave(stream, pixels, 'pil', format_str='png')
                stream.seek(0)
                contents = stream.read()
                stream.close()

            encoded = base64.b64encode(contents)
            if not six.PY2:
                encoded = encoded.decode('utf8')
            encoded = 'data:image/png;base64,' + encoded

            return Expression('ImageBox', String(encoded), Integer(scaled_width), Integer(scaled_height))
        except:
            return Symbol("$Failed")

    def __str__(self):
        return '-Image-'

    def do_copy(self):
        return Image(self.pixels)

    def default_format(self, evaluation, form):
        return '-Image-'

    def get_sort_key(self, pattern_sort=False):
        if pattern_sort:
            return super(Image, self).get_sort_key(True)
        else:
            return hash(self)

    def same(self, other):
        return isinstance(other, Image) and numpy.array_equal(self.pixels, other.pixels)

    def to_python(self, *args, **kwargs):
        return self.pixels

    def __hash__(self):
        return hash(("Image", self.pixels.tobytes()))

    def dimensions(self):
        shape = self.pixels.shape
        return (shape[1], shape[0])

    def channels(self):
        shape = self.pixels.shape
        if len(shape) < 3:
            return 1
        else:
            return shape[2]

    def storage_type(self):
        dtype = self.pixels.dtype
        if dtype in (numpy.float32, numpy.float64):
            return 'Real'
        elif dtype == numpy.uint32:
            return 'Bit32'
        elif dtype == numpy.uint16:
            return 'Bit16'
        elif dtype == numpy.uint8:
            return 'Byte'
        elif dtype == numpy.bool:
            return 'Bit'
        else:
            return str(dtype)


class ImageAtom(AtomBuiltin):
    requires = _image_requires

    def apply_create(self, array, evaluation):
        'Image[array_]'
        pixels = _image_pixels(array.to_python())
        if pixels is not None:
            shape = pixels.shape
            is_rgb = (len(shape) == 3 and shape[2] == 3)
            return Image(pixels.clip(0, 1), 'RGB' if is_rgb else 'Grayscale')
        else:
            return Expression('Image', array)


# word clouds

class WordCloud(Builtin):
    '''
    <dl>
    <dt>'WordCloud[{$word1$, $word2$, ...}]'
      <dd>Gives a word cloud with the given list of words.
    </dl>

    >> WordCloud[StringSplit[Import["ExampleData/EinsteinSzilLetter.txt"]]]
     = -Image-
    '''

    requires = _image_requires + (
        'wordcloud',
    )

    options = {
        'IgnoreCase': 'True',
        'ImageSize': 'Automatic',
        'MaxItems': 'Automatic',
    }

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

    def apply_words(self, words, evaluation, options):
        'WordCloud[words_List, OptionsPattern[%(name)s]]'
        ignore_case = self.get_option(options, 'IgnoreCase', evaluation) == Symbol('True')

        freq = dict()
        for word in words.leaves:
            if not isinstance(word, String):
                return
            py_word = word.get_string_value()
            if ignore_case:
                key = py_word.lower()
            else:
                key = py_word
            record = freq.get(key, None)
            if record is None:
                freq[key] = [py_word, 1]
            else:
                record[1] += 1

        max_items = self.get_option(options, 'MaxItems', evaluation)
        if isinstance(max_items, Integer):
            py_max_items = max_items.get_int_value()
        else:
            py_max_items = 200

        image_size = self.get_option(options, 'ImageSize', evaluation)
        if image_size == Symbol('Automatic'):
            py_image_size = (800, 600)
        elif image_size.get_head_name() == 'System`List' and len(image_size.leaves) == 2:
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

        def color_func(word, font_size, position, orientation, random_state=None, **kwargs):
            return self.default_colors[random.randint(0, 7)]

        font_base_path = os.path.dirname(os.path.abspath(__file__)) + '/../fonts/'

        font_path = os.path.realpath(font_base_path + 'AmaticSC-Bold.ttf')
        if not os.path.exists(font_path):
            font_path = None

        from wordcloud import WordCloud
        wc = WordCloud(
            width=py_image_size[0], height=py_image_size[1],
            font_path=font_path, max_font_size=300, mode='RGB',
            background_color='white', max_words=py_max_items,
            color_func=color_func, random_state=42, stopwords=set())
        wc.generate_from_frequencies(freq.values())

        image = wc.to_image()
        return Image(numpy.array(image), 'RGB')
