This directory contains modules that are not intially loaded into mathics but can be.

Packages here often have additional OS dependencies.

The we have here modules are:

* `pymathics.natlang`: tools to work with expressions in natural language, using the libraries `nltk` and `spacy`.


In the future we will add `pymathics.graphs` to work with graphs, using the OS dependency `graphviz` and `pyplot`.


To load a module, say `pymathics.natlang` inside Mathics run:

::
   LoadModule["pymathics.natlang"]
