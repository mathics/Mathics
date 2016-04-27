from mathics.builtin.base import (
    Builtin, InstancableBuiltin, BoxConstruct, BoxConstructError, String)
from mathics.core.expression import (
    Atom, Expression, Integer, Real, NumberError, Symbol, strip_context,
    system_symbols, system_symbols_dict)

import sys
import numpy
import base64

import skimage
import skimage.io
import skimage.transform
import skimage.filters
import skimage.exposure
import skimage.feature
import skimage.filters.rank
from skimage.morphology import disk

import PIL
from PIL import ImageEnhance

try:
    import io  # python3
except ImportError:
    from io import StringIO

class ImportImage(Builtin):
    def apply_load(self, path, evaluation):
        '''ImportImage[path_?StringQ]'''
        from mathics.core.parser import parse_builtin_rule
        pixels = skimage.io.imread(path.get_string_value())
        atom = ImageAtom(skimage.img_as_float(pixels))
        return Expression('List', Expression('Rule', String('Image'), atom))


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
        'ImageResize[image_Image, width_?RealNumberQ]'
        shape = image.pixels.shape
        height = int((float(shape[0]) / float(shape[1])) * width.value)
        return self.apply_resize_width_height(image, width, Integer(height), evaluation)

    def apply_resize_width_height(self, image, width, height, evaluation):
        'ImageResize[image_Image, {width_?RealNumberQ, height_?RealNumberQ}]'
        return ImageAtom(skimage.transform.resize(image.pixels, (int(height.value), int(width.value))))


class ImageRotate(Builtin):
    def apply_rotate_90(self, image, evaluation):
        'ImageRotate[image_Image]'
        return self.apply_rotate(image, Real(90), evaluation)

    def apply_rotate(self, image, angle, evaluation):
        'ImageRotate[image_Image, angle_?RealNumberQ]'
        return ImageAtom(skimage.transform.rotate(image.pixels, angle.value, resize=True))


class ImageAdjust(Builtin):
    def apply_auto(self, image, evaluation):
        'ImageAdjust[image_Image]'
        try:
            return ImageAtom(skimage.filters.rank.autolevel(
                skimage.img_as_float(image.pixels), disk(5, dtype='float64')))
        except:
            import sys
            return String(repr(sys.exc_info()))

    def apply_contrast(self, image, c, evaluation):
        'ImageAdjust[image_Image, c_?RealNumberQ]'
        enhancer_c = ImageEnhance.Contrast(image.as_pil())
        return ImageAtom(numpy.array(enhancer_c.enhance(c.value)))

    def apply_contrast_brightness(self, image, c, b, evaluation):
        'ImageAdjust[image_Image, {c_?RealNumberQ, b_?RealNumberQ}]'
        im = image.as_pil()
        enhancer_b = ImageEnhance.Brightness(im)
        im = enhancer_b.enhance(b.value)  # brightness first!
        enhancer_c = ImageEnhance.Contrast(im)
        return ImageAtom(numpy.array(enhancer_c.enhance(c.value)))


class GaussianFilter(Builtin):
    def apply_radius(self, image, radius, evaluation):
        'GaussianFilter[image_Image, radius_?RealNumberQ]'
        if len(image.pixels.shape) > 2 and image.pixels.shape[2] > 3:
            pass  # FIXME
        return ImageAtom(skimage.filters.gaussian(
            skimage.img_as_float(image.pixels), sigma=radius.value / 2, multichannel=True))

    # def apply_radius_sigma(self, image, radius, sigma, evaluation):
    #    'GaussianFilter[image_Image, {radius_?RealNumberQ, sigma_?RealNumberQ}]'


class PixelValue(Builtin):
    def apply(self, image, x, y, evaluation):
        'PixelValue[image_Image, {x_?RealNumberQ, y_?RealNumberQ}]'
        return Real(image.pixels[int(y.value), int(x.value)])


class ImageAdd(Builtin):
    def apply(self, image, x, evaluation):
        'ImageAdd[image_Image, x_?RealNumberQ]'
        # v = x.value * im.shape[2]
        return ImageAtom(image.pixels + v)


class ColorSeparate(Builtin):
    def apply(self, image, evaluation):
        'ColorSeparate[image_Image]'
        images = []
        pixels = image.pixels
        for i in range(im.shape[2]):
            images.append(ImageAtom(pixels[:, :, i]))
        return Expression('List', *images)


class Binarize(Builtin):
    def apply(self, image, evaluation):
        'Binarize[image_Image]'
        pixels = image.grey()
        threshold = skimage.filters.threshold_otsu(pixels)
        return ImageAtom(skimage.img_as_float(pixels > threshold))

    def apply_t(self, image, t, evaluation):
        'Binarize[image_Image, t_?RealNumberQ]'
        pixels = image.grey()
        return ImageAtom(skimage.img_as_float(pixels > t.value))

    def apply_t1_t2(self, image, t1, t2, evaluation):
        'Binarize[image_Image, {t1_?RealNumberQ, t2_?RealNumberQ}]'
        pixels = image.grey()
        mask1 = pixels > t1.value
        mask2 = pixels < t2.value
        return ImageAtom(skimage.img_as_float(mask1 * mask2))


class EdgeDetect(Builtin):
    def apply(self, image, evaluation):
        'EdgeDetect[image_Image]'
        return self._compute(image)

    def apply_r(self, image, r, evaluation):
        'EdgeDetect[image_Image, r_?RealNumberQ]'
        return self._compute(image, r.value)

    def apply_r_t(self, image, r, t, evaluation):
        'EdgeDetect[image_Image, r_?RealNumberQ, t_?RealNumberQ]'
        return self._compute(image, r.value, t.value)

    def _compute(self, image, radius=2, threshold=0.2):
        return ImageAtom(skimage.img_as_float(skimage.feature.canny(image.grey(), sigma=radius / 2,
                                           low_threshold=0.5 * threshold, high_threshold=threshold)))


class Image(Builtin):
    def apply_create(self, array, evaluation):
        '''Image[array_?MatrixQ]'''
        pixels = numpy.array(array.to_python(), dtype='float64')
        shape = pixels.shape
        if len(shape) == 2 or (len(shape) == 3 and shape[2] in (1, 3)):
            return ImageAtom(skimage.img_as_float(pixels.clip(0, 1)))
        else:
            return Expression('Image', array)

class ImageAtom(Atom):
    def __init__(self, pixels, **kwargs):
        super(ImageAtom, self).__init__(**kwargs)
        self.pixels = pixels

    def as_pil(self):
        return PIL.Image.fromarray(self.pixels)

    def grey(self):
        pixels = self.pixels
        if len(pixels.shape) >= 3 and pixels.shape[2] > 1:
            return skimage.color.rgb2gray(pixels)
        else:
            return pixels

    def make_boxes(self, form):
        try:
            pixels = self.pixels
            shape = pixels.shape

            width = shape[1]
            height = shape[0]

            # if the image is very small, scale it up using nearest neighbour.
            min_size = 128
            if width < min_size and height < min_size:
                scale = min_size / max(width, height)
                pixels = skimage.transform.resize(pixels, (int(scale * height), int(scale * width)), order=0)

            # python3 version
            stream = io.BytesIO()
            skimage.io.imsave(stream, pixels, 'pil', format_str='png')
            stream.seek(0)
            contents = stream.read()
            stream.close()

            return Expression('ImageBox', String(base64.b64encode(contents).decode('utf8')),
                              Integer(width), Integer(height))
        except:
            return String("error while streaming image: " + repr(sys.exc_info()))

    def __str__(self):
        return '-Image-'

    def do_copy(self):
        return ImageAtom(self.pixels)

    def default_format(self, evaluation, form):
        return '-Image-'

    def get_sort_key(self, pattern_sort=False):
        if pattern_sort:
            return super(ImageAtom, self).get_sort_key(True)
        else:
            return hash(self)

    def same(self, other):
        return isinstance(other, ImageAtom) and numpy.array_equal(self.pixels, other.pixels)

    def to_sympy(self, **kwargs):
        return '-Image-'

    def to_python(self, *args, **kwargs):
        return self.pixels

    def __hash__(self):
        return hash(("Image", self.pixels.tobytes()))
