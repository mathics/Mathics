For the Future
==============

Additional Format Types
-----------------------

There are currently 4 kinds of format types:

- ``xml`` (which is really MathML)
- ``text``: ASCII text
- ``tex``: Knuth's TeX typesetting system
- ``boxes``: combinations of the above

Proposed is to add two more:

- ``rst``: restructured text
- ``graphics``: a higher-level graphics-package independent format

The problem with using TeX for formatting is that in of itself it is
more of a low-level formatter. LaTeX was the corresponding
higher-level formatter, but this too is a bit more cumbersome than
current documentation practice. Since the code is Python-based,
ReStructured text now makes more sense since there are good libraries
for that and this integrates with the documentation better described
above.

Right now for non-text front-ends the XML format is used. Within that,
is an embedded image of some sort like SVG, or PNG. The problem with
this is that decisions have already been baked in with respect to a
number of drawing parameters and those are impossible to undo since
metadata and user-supplied options have been lost. Better graphing and
drawing packages exist which are in a better position to make layout
and drawing parameter if given a chance.

Actually, it is often the case it it is not that there are new drawing
packages so much as there are *newer* graphic packages and the
Mathics core hasn't been updated to make use of those improvements

In sum, decisions about plotting and drawing need to get moved closer
to the front end which knows better about which drawing packages are
available and what it capabilities are.

PyModules
---------

To reduce core depencencies on python and system libraries, allow for
a more distributed growth and speed up startup time, Mathics
introdcued a mechanism which is basically doing a Python ``import``
which is similar to how its internal built-in and predefined packages
and symbols work. From Mathics the function is called ``LoadModule[]``.

Unfortunately, this wasn't scalable and more work is needed. As a
result some modules, notably the module for Natural Language
Processesing (NLP) was removed.

Graph Module
++++++++++++

A module for working and plotting with graphs was also started and
made one of the few custom PyMathics module. It started to use the
fine `networkx <https://networkx.github.io/>`_ package for showing
graphs.

However it was using a rather old version *networkx* and there is the
problem of handling graphics in Mathics mentioned above.

For these reasons rather than provide a somewhat incomplete package,
we optioed for delaying introduction of this until a better job can be
done.

The hope is that with this package in place we will be able to support
the full `Combinatorica
<http://homepage.divms.uiowa.edu/~sriram/Combinatorica/>`_ package
doing computational discrete mathematics.


Documentation
-------------

After release 1.1.0, a rethinking of the documentation systems may be
done to reflect 2020's Python-centric tools, thinking and practice.

This may include integration into RsT/Sphinx/Readthedocs.
Sphinx has a mechanism for embedding testable code into its docs.

Testing
-------

Related to documentation, testing may be modularized better and
expanded. Right now most of the tests are hooked into documentation,
and while this is cool, not all tests are interesting to have in
documentation. In particular, obscure bugs fall into this category.

Command-line interface
----------------------

In 1.1.0, a new CLI frontend, ``mathicsscript`` was been started.

It supports:

* Save history over sessions
* Understands groupings of lines which form one logical Mathics statement
* Supports Pygments-based syntax coloring of output, and Pygments styles
* Can automatically detect whether a terminal has a dark or light background

With changes to the format types mentioned above and by using
``sympy``'s ASCII rendering routines, ``mathicsscript`` will support
opening a matplotlib window to show graphics, and will display output
in ASCII better.

Jupyter Interface
-----------------

this may happen around January 2021.

In the future, Django may be split off to a separate package, same as
the CLI and existing Jupyter interfaces.

It is possbile if IPython via Jupyter works well, the CLI interface
won't be needed. However I (rocky) suspect not.
