For the Future
==============

*One can always dream...*

Graphics3D
----------

With 4.0.0, we have started defining a Graphics3D protocol.  It is
currently expressed in JSON. There is an independent `threejs-based
module
<https://www.npmjs.com/package/@mathicsorg/mathics-threejs-backend>`_
to implement this. Tiago Cavalcante Trindade is responsible for this
code and for modernizing our JavaScript, and it use in threejs.

We expect a lot more to come. For example UniformPolyhedra is too new
to have been able to make this release.

We also need to define a protocol and implementation for 2D Graphics.


Boxing, Formatting, Forms
-------------------------

While we have started to segregate boxing (bounding-box layout) and
formatting (translation to a conventional rendering format or
language), a lot more work needs to be done.

Also, a lot more Forms should be defined. And those that exist, like
TeXForm, and StandardForm, could use improvement.

This area is still a big mess.

Jupyter and other Front Ends
----------------------------

Although we had planned to move forward on this previously, it now
appears that we should nail down some of the above better, before
undertaking. Jupyter uses a wire protocol, and we still have
work to do in defining the interfaces mentioned above.

That said, this is still on the horizon.

Interest has also been expressed in WebGL, and Flask front ends. But
these too will require use to have better protocols defined and in
place.


Documentation
-------------

Sometime release 4.0.0, all of the code related to producing
documentation in LaTeX and in Mathics Django, and running doctests
will be split off and put into its own git repository.

I've spent a lot of time banging on this to try to get to to be be
less fragile, more modular, more intelligible, but it still needs a
*lot* more work and still is very fragile.

Also there is much to do on the editor side of things in terms of
reorganizing sections (which also implies reorganizing the builtin
module structure, since those are tightly bound together).

We still need to convert this into Sphinx-based, with its doctest.  We
also need to be able to extract information in sphinx/RsT format
rather than its home-brew markup language which is sort of XML like.

Performance
-----------

This is one area where we know a lot about what *kinds* of things need
to be done, but have barely scratched the surface here.

The current implementation is pretty bare bones.

We have problems with recursion, memory consumption, loading time, and
overall speed in computation.

Support for External Packages
-----------------------------

I would have liked to have seen this going earlier. However right now
Mathics is still at too primitive a level for any serious package to
be run on it. This will change at some point though.

Support for Mathematica Language Levels
---------------------------------------

This is something that I think would be extremely useful and is
straightforward to do someone has used Mathematica over the years
knows it well. I think most of this could be supported in Mathics code
itself and loaded as packages. Any takers?
