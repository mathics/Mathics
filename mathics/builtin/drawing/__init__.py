"""
Graphics, Drawing, and Images

Functions like 'Plot' and 'ListPlot' can be used to draw graphs of functions and data.

Graphics is implemented as a collection of <i>graphics primitives</i>. Primatives are objects like 'Point', 'Line', and 'Polygon' and become elements of a <i>graphics object</i>.

A graphics object can have directives as well such as 'RGBColor', and 'Thickness'.

There are several kinds of graphics objects; each kind has a head which identifies its type.

>> ListPlot[ Table[Prime[n], {n, 20} ]]
 = -Graphics-
>> Head[%]
 = Graphics
>> Graphics3D[Sphere[]]
 = -Graphics3D-
>> Head[%]
 = Graphics3D
>>


"""

from mathics.version import __version__  # noqa used in loading to check consistency.
