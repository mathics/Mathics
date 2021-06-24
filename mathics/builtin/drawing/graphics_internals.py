# -*- coding: utf-8 -*-
# Internal graphics routines.
# No external builtins appear here.
# Also no docstring which may confuse the doc system

from mathics.version import __version__  # noqa used in loading to check consistency.

from mathics.builtin.base import (
    InstanceableBuiltin,
    BoxConstructError,
)

from mathics.core.expression import system_symbols_dict


class _GraphicsElement(InstanceableBuiltin):
    def init(self, graphics, item=None, style=None, opacity=1.0):
        if item is not None and not item.has_form(self.get_name(), None):
            raise BoxConstructError
        self.graphics = graphics
        self.style = style
        self.opacity = opacity
        self.is_completely_visible = False  # True for axis elements

    @staticmethod
    def create_as_style(klass, graphics, item):
        return klass(graphics, item)


def get_class(name):
    c = GLOBALS.get(name)
    if c is None:
        return GLOBALS3D.get(name)
    else:
        return c

    # globals() does not work with Cython, otherwise one could use something
    # like return globals().get(name)


# FIXME: GLOBALS and GLOBALS3D are a horrible names.
# These ares updated in mathics.builtin.graphics in and mathics.builtin.box.graphics3d
GLOBALS = system_symbols_dict({})
GLOBALS3D = system_symbols_dict({})
