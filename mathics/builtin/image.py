'''
A place for Image[] and related functions.
'''

from mathics.builtin.base import (
    Builtin, Test, BoxConstruct, String)
from mathics.core.expression import (
    Atom, Expression, Integer, Real, Symbol, from_python)

import six
import numpy
import base64

try:
    import skimage
    import skimage.io
    import skimage.transform
    import skimage.filters
    import skimage.exposure
    import skimage.feature
    import skimage.filters.rank

    from skimage.morphology import disk

    import PIL
    import PIL.ImageEnhance
    import PIL.ImageOps
    import PIL.ImageFilter

    _enabled = True
except ImportError:
    _enabled = False

if six.PY2:
    from io import StringIO
else:
    import io

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


class ImageImport(Builtin):
    messages = {
        'noskimage': 'image import needs scikit-image in order to work.'
    }

    def apply(self, path, evaluation):
        '''ImageImport[path_?StringQ]'''
        if not _enabled:
            return evaluation.message('ImageImport', 'noskimage')
        else:
            pixels = skimage.io.imread(path.get_string_value())
            is_rgb = len(pixels.shape) >= 3 and pixels.shape[2] >= 3
            atom = Image(pixels, 'RGB' if is_rgb else 'Grayscale')
            return Expression('List', Expression('Rule', String('Image'), atom))


class ImageExport(Builtin):
    messages = {
        'noskimage': 'image export needs scikit-image in order to work.',
        'noimage': 'only an Image[] can be exported into an image file'
    }

    def apply(self, path, expr, opts, evaluation):
        '''ImageExport[path_?StringQ, expr_, opts___]'''
        if not _enabled:
            return evaluation.message('ImageExport', 'noskimage')
        elif isinstance(expr, Image):
            skimage.io.imsave(path.get_string_value(), expr.pixels)
            return Symbol('Null')
        else:
            return evaluation.message('ImageExport', 'noimage')


class ImageBox(BoxConstruct):
    def boxes_to_text(self, leaves, **options):
        return '-Image-'

    def boxes_to_xml(self, leaves, **options):
        # see https://tools.ietf.org/html/rfc2397
        img = '<img src="data:image/png;base64,%s" />' % (leaves[0].get_string_value())
        return '</math><mtable>%s</mtable><math>' % img

    def boxes_to_tex(self, leaves, **options):
        return '-Image-'


class ImageResize(Builtin):
    def apply_resize_width(self, image, width, evaluation):
        'ImageResize[image_Image, width_Integer]'
        shape = image.pixels.shape
        height = int((float(shape[0]) / float(shape[1])) * width.value)
        return self.apply_resize_width_height(image, width, Integer(height), evaluation)

    def apply_resize_width_height(self, image, width, height, evaluation):
        'ImageResize[image_Image, {width_Integer, height_Integer}]'
        return Image(skimage.transform.resize(
            image.pixels, (int(height.value), int(width.value))), image.color_space)


class ImageReflect(Builtin):
    def apply(self, image, evaluation):
        'ImageReflect[image_Image]'
        return Image(numpy.flipud(image.pixels), image.color_space)


class ImageRotate(Builtin):
    rules = {
        'ImageRotate[i_Image]': 'ImageRotate[i, 90]'
    }

    def apply(self, image, angle, evaluation):
        'ImageRotate[image_Image, angle_?RealNumberQ]'
        return Image(skimage.transform.rotate(image.pixels, angle.value, resize=True), image.color_space)


class ImageAdjust(Builtin):
    def apply_auto(self, image, evaluation):
        'ImageAdjust[image_Image]'
        pixels = skimage.img_as_ubyte(image.pixels)
        return Image(numpy.array(PIL.ImageOps.equalize(PIL.Image.fromarray(pixels))), image.color_space)

    def apply_contrast(self, image, c, evaluation):
        'ImageAdjust[image_Image, c_?RealNumberQ]'
        enhancer_c = PIL.ImageEnhance.Contrast(image.as_pil())
        return Image(numpy.array(enhancer_c.enhance(c.value)), image.color_space)

    def apply_contrast_brightness(self, image, c, b, evaluation):
        'ImageAdjust[image_Image, {c_?RealNumberQ, b_?RealNumberQ}]'
        im = image.as_pil()
        enhancer_b = PIL.ImageEnhance.Brightness(im)
        im = enhancer_b.enhance(b.value)  # brightness first!
        enhancer_c = PIL.ImageEnhance.Contrast(im)
        return Image(numpy.array(enhancer_c.enhance(c.value)), image.color_space)


class Blur(Builtin):
    rules = {
        'Blur[i_Image]': 'Blur[i, 2]'
    }

    def apply(self, image, r, evaluation):
        'Blur[image_Image, r_?RealNumberQ]'
        return Image(numpy.array(PIL.Image.fromarray(image.pixels).filter(
            PIL.ImageFilter.GaussianBlur(r.value))), image.color_space)


class Sharpen(Builtin):
    rules = {
        'Sharpen[i_Image]': 'Sharpen[i, 2]'
    }

    def apply(self, image, r, evaluation):
        'Sharpen[image_Image, r_?RealNumberQ]'
        return Image(numpy.array(PIL.Image.fromarray(image.pixels).filter(
            PIL.ImageFilter.UnsharpMask(r.value))), image.color_space)


class GaussianFilter(Builtin):
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
                sigma=radius.value / 2, multichannel=True), image.color_space)


class BoxMatrix(Builtin):
    def apply(self, r, evaluation):
        'BoxMatrix[r_?RealNumberQ]'
        s = 1 + 2 * r.value
        return from_python(skimage.morphology.rectangle(s, s).tolist())


class DiskMatrix(Builtin):
    def apply(self, r, evaluation):
        'DiskMatrix[r_?RealNumberQ]'
        return from_python(skimage.morphology.disk(r).tolist())


class DiamondMatrix(Builtin):
    def apply(self, r, evaluation):
        'DiamondMatrix[r_?RealNumberQ]'
        return from_python(skimage.morphology.diamond(r).tolist())


class MorphologyFilter(Builtin):
    messages = {
        'grayscale': 'Your image has been converted to grayscale as color images are not supported yet.'
    }

    def compute(self, image, f, k, evaluation):
        if image.color_space != 'Grayscale':
            image = image.color_convert('Grayscale')
            evaluation.message('MorphologyFilter', 'grayscale')
        return Image(f(image.pixels, numpy.array(k.to_python())), 'Grayscale')


class Dilation(MorphologyFilter):
    rules = {
        'Dilation[i_Image, r_?RealNumberQ]': 'Dilation[i, BoxMatrix[r]]'
    }

    def apply(self, image, k, evaluation):
        'Dilation[image_Image, k_?MatrixQ]'
        return self.compute(image, skimage.morphology.dilation, k, evaluation)


class Erosion(MorphologyFilter):
    rules = {
        'Erosion[i_Image, r_?RealNumberQ]': 'Erosion[i, BoxMatrix[r]]'
    }

    def apply(self, image, k, evaluation):
        'Erosion[image_Image, k_?MatrixQ]'
        return self.compute(image, skimage.morphology.erosion, k, evaluation)


class Opening(MorphologyFilter):
    rules = {
        'Opening[i_Image, r_?RealNumberQ]': 'Opening[i, BoxMatrix[r]]'
    }

    def apply(self, image, k, evaluation):
        'Opening[image_Image, k_?MatrixQ]'
        return self.compute(image, skimage.morphology.opening, k, evaluation)


class Closing(MorphologyFilter):
    rules = {
        'Closing[i_Image, r_?RealNumberQ]': 'Closing[i, BoxMatrix[r]]'
    }

    def apply(self, image, k, evaluation):
        'Closing[image_Image, k_?MatrixQ]'
        return self.compute(image, skimage.morphology.closing, k, evaluation)


class PixelValue(Builtin):
    def apply(self, image, x, y, evaluation):
        'PixelValue[image_Image, {x_?RealNumberQ, y_?RealNumberQ}]'
        return Real(image.pixels[int(y.value), int(x.value)])


class ImageAdd(Builtin):
    def apply(self, image, x, evaluation):
        'ImageAdd[image_Image, x_?RealNumberQ]'
        return Image((skimage.img_as_float(image.pixels) + float(x.value)).clip(0, 1), image.color_space)


class ImageSubtract(Builtin):
    def apply(self, image, x, evaluation):
        'ImageSubtract[image_Image, x_?RealNumberQ]'
        return Image((skimage.img_as_float(image.pixels) - float(x.value)).clip(0, 1), image.color_space)


class ImageMultiply(Builtin):
    def apply(self, image, x, evaluation):
        'ImageMultiply[image_Image, x_?RealNumberQ]'
        return Image((skimage.img_as_float(image.pixels) * float(x.value)).clip(0, 1), image.color_space)


class ColorSeparate(Builtin):
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


class Binarize(Builtin):
    def apply(self, image, evaluation):
        'Binarize[image_Image]'
        pixels = image.grayscale().pixels
        threshold = skimage.filters.threshold_otsu(pixels)
        return Image(pixels > threshold, 'Grayscale')

    def apply_t(self, image, t, evaluation):
        'Binarize[image_Image, t_?RealNumberQ]'
        pixels = image.grayscale().pixels
        return Image(pixels > t.value, 'Grayscale')

    def apply_t1_t2(self, image, t1, t2, evaluation):
        'Binarize[image_Image, {t1_?RealNumberQ, t2_?RealNumberQ}]'
        pixels = image.grayscale().pixels
        mask1 = pixels > t1.value
        mask2 = pixels < t2.value
        return Image(mask1 * mask2, 'Grayscale')


class ColorNegate(Builtin):
    def apply(self, image, evaluation):
        'ColorNegate[image_Image]'
        pixels = image.pixels
        anchor = numpy.ndarray(pixels.shape, dtype=pixels.dtype)
        anchor.fill(skimage.dtype_limits(pixels)[1])
        return Image(anchor - pixels, image.color_space)


class ImageDimensions(Builtin):
    def apply(self, image, evaluation):
        'ImageDimensions[image_Image]'
        return Expression('List', *image.dimensions())


class ImageAspectRatio(Builtin):
    def apply(self, image, evaluation):
        'ImageAspectRatio[image_Image]'
        dim = image.dimensions()
        return Real(dim[1] / float(dim[0]))


class ImageChannels(Builtin):
    def apply(self, image, evaluation):
        'ImageChannels[image_Image]'
        return Integer(image.channels())


class ImageType(Builtin):
    def apply(self, image, evaluation):
        'ImageType[image_Image]'
        return String(image.storage_type())


class BinaryImageQ(Test):
    def apply(self, image, evaluation):
        'BinaryImageQ[image_Image]'
        return Symbol('True') if image.storage_type() == 'Bit' else Symbol('False')


class ImageColorSpace(Builtin):
    def apply(self, image, evaluation):
        'ImageColorSpace[image_Image]'
        return String(image.color_space)


class ColorConvert(Builtin):
    def apply(self, image, colorspace, evaluation):
        'ColorConvert[image_Image, colorspace_String]'
        return image.color_convert(colorspace.get_string_value())


class ImageData(Builtin):
    def apply(self, image, evaluation):
        'ImageData[image_Image]'
        return from_python(skimage.img_as_float(image.pixels).tolist())


class ImageTake(Builtin):
    def apply(self, image, n, evaluation):
        'ImageTake[image_Image, n_Integer]'
        return Image(image.pixels[:int(n.value)], image.color_space)


class ImagePartition(Builtin):
    rules = {
        'ImagePartition[i_Image, s_Integer]': 'ImagePartition[i, {s, s}]'
    }

    def apply(self, image, w, h, evaluation):
        'ImagePartition[image_Image, {w_Integer, h_Integer}]'
        w = w.value
        h = h.value
        pixels = image.pixels
        shape = pixels.shape
        parts = [Image(pixels[y:y + w, x:x + w], image.color_space)
                 for x in range(0, shape[1], w) for y in range(0, shape[0], h)]
        return Expression('List', *parts)

class ColorQuantize(Builtin):
    def apply(self, image, n, evaluation):
        'ColorQuantize[image_Image, n_Integer]'
        pixels = skimage.img_as_ubyte(image.color_convert('RGB').pixels)
        im = PIL.Image.fromarray(pixels).quantize(n.value)
        im = im.convert('RGB')
        return Image(numpy.array(im), 'RGB')


class EdgeDetect(Builtin):
    rules = {
        'EdgeDetect[i_Image]': 'EdgeDetect[i, 2, 0.2]',
        'EdgeDetect[i_Image, r_?RealNumberQ]': 'EdgeDetect[i, r, 0.2]'
    }

    def apply(self, image, r, t, evaluation):
        'EdgeDetect[image_Image, r_?RealNumberQ, t_?RealNumberQ]'
        return Image(skimage.feature.canny(image.grayscale().pixels, sigma=r.value / 2,
                                               low_threshold=0.5 * t.value, high_threshold=t.value), 'Grayscale')


class ImageCreate(Builtin):
    messages = {
        'noskimage': 'image creation needs scikit-image in order to work.'
    }

    def apply(self, array, evaluation):
        '''ImageCreate[array_?MatrixQ]'''
        if not _enabled:
            return evaluation.message('ImageCreate', 'noskimage')
        else:
            pixels = numpy.array(array.to_python(), dtype='float64')
            shape = pixels.shape
            is_rgb = (len(shape) == 3 and shape[2] == 3)
            if len(shape) == 2 or (len(shape) == 3 and shape[2] in (1, 3)):
                return Image(pixels.clip(0, 1), 'RGB' if is_rgb else 'Grayscale')
            else:
                return Expression('Image', array)


class Image(Atom):
    def __init__(self, pixels, color_space, **kwargs):
        super(Image, self).__init__(**kwargs)
        self.pixels = pixels
        self.color_space = color_space

    def as_pil(self):
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

    def make_boxes(self, form):
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

            # if the image is very small, scale it up using nearest neighbour.
            min_size = 128
            if width < min_size and height < min_size:
                scale = min_size / max(width, height)
                pixels = skimage.transform.resize(pixels, (int(scale * height), int(scale * width)), order=0)

            if six.PY2:
                pass
            else:
                stream = io.BytesIO()
                skimage.io.imsave(stream, pixels, 'pil', format_str='png')
                stream.seek(0)
                contents = stream.read()
                stream.close()

            return Expression('ImageBox', String(base64.b64encode(contents).decode('utf8')),
                              Integer(width), Integer(height))
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

    def to_sympy(self, **kwargs):
        return '-Image-'

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
