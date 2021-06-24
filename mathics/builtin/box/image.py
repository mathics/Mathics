# -*- coding: utf-8 -*-

from mathics.builtin.base import BoxConstruct


class ImageBox(BoxConstruct):
    """Routines which get called when Boxing (adding formatting and bounding-box information)
    an Image object.
    """

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
