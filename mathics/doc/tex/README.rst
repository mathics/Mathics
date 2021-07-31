Overview
--------

Here we have document data and scripts for generating the Mathics Reference manual in PDF form.

The document is a LaTeX file run through XeTeX with Asymptote graphics.

You'll need Asymptote 2.71-37 or greater and 9.54.0 or greater to
process the graphics. Ealier version may have bugs in them which
prevent some images from getting processed. Notable here are the
"Filling" ande "Bottom" examples which need opacity to work on various kinds of plots.

inkscape is needed for the Mathics logos if you need to generate these from source.

Workflow
--------

The overall top-level LaTeX document is ``mathic.tex``. The pulls in
``documentation.tex`` which is automatically generated from the Python
program ``doc2latex.py`` and that in turn gets its data from
``doc_tex_data.pcl`` which in turn gets its data from ``../documentation/*.mdoc``.

Here is a flow of the data::

    doc/documentation/*.mdoc --+
                               |
    bultins/*.py  -------------+--> doc/tex/doc_tex_data.pcl ---> documentation.tex -+
                   docpipeline.py                          doc2latex.py              |
                                                                                     |
    doc/images/*.svg -------------> doc/tex/log*.pdf --------------------------------+------------------------------> mathics.pdf
                     images.sh                                                       |  latexmk,xetex,asyptote,gv
                                                                                     |
    doc/tex/mathics.tex -------------------------------------------------------------+

A GNU Makefile in this directory has been created to manage the complicated workflow above.

Troubleshooting
---------------

GNU makefile targets can be used to make pieces of the document pipeline. Run ``remake --tasks`` to see
a list of Makefile targets.

If the problem is with graphics, if you don't have the figure name but
just its number, you may be able to figure out its number by going
into ``documetation.tex`` and finding the appropriate section in the
LaTeX document and then getting the number that is listed there.

To view one of the asymptote figures, e.g. ``mathics-83.asy``::

    $ asy
    asy
    Welcome to Asymptote version 2.71-37 (to view the manual, type help)
    > include "mathics-83.asy"
    include "mathics-83.asy"
