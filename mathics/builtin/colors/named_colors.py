# -*- coding: utf-8 -*-
"""Named Colors

Mathics has definitions for the most common color names which can be used in a graphics or style specification.
"""

from mathics.builtin.base import Builtin

from mathics.core.expression import strip_context

from mathics.version import __version__  # noqa used in loading to check consistency.


class _ColorObject(Builtin):
    text_name = None

    def __init__(self, *args, **kwargs):
        super(_ColorObject, self).__init__(*args, **kwargs)
        if self.text_name is None:
            text_name = strip_context(self.get_name()).lower()
        else:
            text_name = self.text_name
        doc = """
            <dl>
            <dt>'%(name)s'
            <dd>represents the color %(text_name)s in graphics.
            </dl>

            >> Graphics[{EdgeForm[Black], %(name)s, Disk[]}, ImageSize->Small]
             = -Graphics-

            >> %(name)s // ToBoxes
             = StyleBox[GraphicsBox[...], ...]
        """ % {
            "name": strip_context(self.get_name()),
            "text_name": text_name,
        }
        if self.__doc__ is None:
            self.__doc__ = doc
        else:
            self.__doc__ = doc + self.__doc__


class Black(_ColorObject):
    """
    >> Black
     = RGBColor[0, 0, 0]
    """

    rules = {"Black": "RGBColor[0, 0, 0]"}


class Blue(_ColorObject):
    """
    >> Blue
     = RGBColor[0, 0, 1]
    """

    rules = {"Blue": "RGBColor[0, 0, 1]"}


class Brown(_ColorObject):
    """
    >> Brown
     = RGBColor[0.6, 0.4, 0.2]
    """

    rules = {"Brown": "RGBColor[0.6, 0.4, 0.2]"}


class Cyan(_ColorObject):
    """
    >> Cyan
     = RGBColor[0, 1, 1]
    """

    rules = {"Cyan": "RGBColor[0, 1, 1]"}


class Gray(_ColorObject):
    """
    >> Gray
     = GrayLevel[0.5]
    """

    rules = {"Gray": "GrayLevel[0.5]"}


class Green(_ColorObject):
    """
    >> Green
     = RGBColor[0, 1, 0]
    """

    rules = {"Green": "RGBColor[0, 1, 0]"}


class Magenta(_ColorObject):
    """
    >> Magenta
     = RGBColor[1, 0, 1]
    """

    rules = {"Magenta": "RGBColor[1, 0, 1]"}


class LightBlue(_ColorObject):
    """
    >> Graphics[{LightBlue, EdgeForm[Black], Disk[]}]
     = -Graphics-

    >> Plot[Sin[x], {x, 0, 2 Pi}, Background -> LightBlue]
     = -Graphics-
    """

    text_name = "light blue"
    rules = {"LightBlue": "RGBColor[0.87, 0.94, 1]"}


class LightBrown(_ColorObject):
    text_name = "light brown"

    rules = {"LightBrown": "Lighter[Brown, 0.85]"}


class LightCyan(_ColorObject):
    text_name = "light cyan"
    rules = {"LightCyan": "Lighter[Cyan, 0.9]"}


class LightGray(_ColorObject):
    text_name = "light gray"
    rules = {"LightGray": "Lighter[Gray]"}


class LightGreen(_ColorObject):
    text_name = "light green"
    rules = {"LightGreen": "Lighter[Green, 0.88]"}


class LightMagenta(_ColorObject):
    text_name = "light magenta"
    rules = {"LightMagenta": "Lighter[Magenta]"}


class LightOrange(_ColorObject):
    text_name = "light orange"
    rules = {"LightOrange": "RGBColor[1, 0.9, 0.8]"}


class LightPink(_ColorObject):
    text_name = "light pink"

    rules = {"LightPink": "Lighter[Pink, 0.85]"}


class LightPurple(_ColorObject):
    text_name = "light purple"
    rules = {"LightPurple": "Lighter[Purple, 0.88]"}


class LightRed(_ColorObject):
    text_name = "light red"
    rules = {"LightRed": "Lighter[Red, 0.85]"}


class LightYellow(_ColorObject):
    text_name = "light yellow"
    rules = {"LightYellow": "Lighter[Yellow]"}


class Pink(_ColorObject):
    rules = {"Pink": "RGBColor[1.0, 0.5, 0.5]"}


class Purple(_ColorObject):
    rules = {"Purple": "RGBColor[0.5, 0, 0.5]"}


class Orange(_ColorObject):
    rules = {"Orange": "RGBColor[1, 0.5, 0]"}


class Red(_ColorObject):
    """
    >> Red
     = RGBColor[1, 0, 0]
    """

    rules = {"Red": "RGBColor[1, 0, 0]"}


class Yellow(_ColorObject):
    """
    >> Yellow
     = RGBColor[1, 1, 0]
    """

    rules = {"Yellow": "RGBColor[1, 1, 0]"}


class White(_ColorObject):
    """
    >> White
     = GrayLevel[1]
    """

    rules = {"White": "GrayLevel[1]"}
