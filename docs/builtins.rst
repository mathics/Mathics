.. pymathics-hello documentation master file, created by
   sphinx-quickstart on Sun Nov 29 14:06:02 2020.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. toctree::
   :maxdepth: 2
   :caption: Contents:

Adding builtin symbols
======================

Adding new built-in symbols to Mathics is very easy. Either place a new module
in the builtin directory and add it to the list of modules in
``builtin/__init__.py`` or use an existing module. Create a new class derived
from ``Builtin``.

To get an idea of how a built-in class can look like, consider the following
implementation of ``Hello``:

.. code-block:: python

  from mathics.builtin.base import Builtin

  class Hello(Builtin):
    """
    <dl>
      <dt>Hello[$person$]
      <dd>An example function in a Python-importable Mathics module.
    </dl>
    >> Hello["World"]
     = Hello, World!
    """
    def apply(self, person, evaluation):
      "Hello[person_]"
      return String(f"Hello, {person.get_string_value()}!")

The class starts with a Python docstring that specifies the documentation and
tests for the symbol.  Please refer to `Documentation markup`_ for more
details on how to write the documentation.

Python functions starting with "apply" are converted to built-in rules. Their
docstring is compiled to the corresponding Mathics pattern. Pattern variables
used in the pattern are passed to the Python function by their same name, plus
an additional evaluation object. This object is needed to evaluate further
expressions, print messages in the Python code, etc. Unsurprisingly, the return
value of the Python function is the expression that is replaced for the matched
pattern. If the function does not return any value, the Mathics expression is
left unchanged. Note that you have to return ``Symbol["Null"]`` explicitly if
you want that.

Working with multiple patterns
++++++++++++++++++++++++++++++

It's important to note that a builtin can be defined over *more than one
pattern*, such as in the following case:

.. code-block:: python

  from mathics.builtin.base import Builtin, String

  class Hello(Builtin):
    """
    <dl>
      <dt>Hello[$person$]
      <dd>An example function in a Python-importable Mathics module.
    </dl>
    >> Hello["World"]
     = Hello, World!
    """

    def apply(self, person, language, evaluation):
      "Hello[person_, language_]"

      if language.has_form("French"):
        return String(f"Bonjour, {person.get_string_value()}!")
      else:
        return String(f"Hello, {person.get_string_value()}!")

    def apply_english(self, person, evaluation):
      "Hello[person_]"
      return self.apply(person, Expression("English"), evaluation)

In this case, ``Hello["Peter", French]`` will resolve to ``"Bonjour,
Peter!"``, while ``Pymathics`Hello["Peter", English]`` and
``Pymathics`Hello["Peter"]`` will both resolve to ``"Hello, Peter!"``.

We may also want to use different definitions according to the types of the
arguments passed to a builtin. We can do so by specifying type-constraints in
the definitions's pattern, such as in the following example:

.. code-block:: python

  from mathics.builtin.base import Builtin

  class Hello(Builtin):
    """
    <dl>
      <dt>Hello[$person$]
      <dd>An example function in a Python-importable Mathics module.
    </dl>
    >> Hello["World"]
     = Hello, World!
    """

    def apply(self, person, evaluation):
      "Hello[person_String]"
      return String(f"Hello, {person.get_string_value()}!")

When evaluating a call to a builtin, Mathics will pattern-match on the inputs
and search for the most appropriate definition. If the inputs don't match any
of the patterns of the existing definitions then the entire expression is
returned unchanged. For example, if we implemented ``Hello`` as above then
``Hello[45]`` would resolve to ``Hello[45]``, because ``45`` doesn't match
``person_String`` (``45`` is not a string).

Rules
+++++

Let's recall the example given in `Working with multiple patterns`_. Notice
that ``English`` is essentially the default value of the ``language``. We
don't actually need to use multiple definitions to encode parameters with
default values. By adding an entry to the builtin's ``rules`` class-field, we
can tell Mathics that an expression should resolve the value of a different
expression, such as in the following example:

.. code-block:: python

  from mathics.builtin.base import Builtin, String

  class Hello(Builtin):
    """
    <dl>
      <dt>Hello[$person$]
      <dd>An example function in a Python-importable Mathics module.
    </dl>
    >> Hello["World"]
     = Hello, World!
    """

    rules = {
      "Hello[person_]": "Hello[person, English]",
    }

    def apply(self, person, language, evaluation):
      "Hello[person_, language_]"

      if language.has_form("French"):
        return String(f"Bonjour, {person.get_string_value()}!")
      else:
        return String(f"Hello, {person.get_string_value()}!")

In this example, the first entry in ``rules`` tells Mathics that
``Hello[person]`` is just sintactic suger for ``Hello[person, English]``.

Special attributes
++++++++++++++++++

One can specify general attributes of a builtin in it's ``attributes``
class field. For example, let's say you expect ``Hello[{"Peter",
"Roger"}]`` to evaluate to the same as ``{Hello["Peter"],
Hello["Roger"]}``. We can to that by overwriting ``Hello``'s
``attributes`` class-field.

.. code-block:: python

  from mathics.builtin.base import Builtin, String

  class Hello(Builtin):
    """
    <dl>
      <dt>Hello[$person$]
      <dd>An example function in a Python-importable Mathics module.
    </dl>
    >> Hello["World"]
     = Hello, World!
    """

    attributes = ('Listable',)

    def apply(self, person, evaluation):
      "Hello[person_]"

      return String(f"Hello, {person.get_string_value()}!")

.. TODO: Document what which attribute does. Place a table in here

Emitting warnings
+++++++++++++++++

Sometimes things go wrong. When things go wrong, we should report an error to
our users. But how can one emit a warning from inside an evaluator?

Warnings in Mathics can be specified via the ``messages`` class field. The
``messages`` class field is a dictionary whose keys are the names of possible
warning messages and whose values are template warning messages. For example,
we may want to display a warning when our users pass something other than a
string to ``Hello``:

.. code-block:: python

  from mathics.builtin.base import Builtin, String

  class Hello(Builtin):
    """
    <dl>
      <dt>Hello[$person$]
      <dd>An example function in a Python-importable Mathics module.
    </dl>
    >> Hello["World"]
     = Hello, World!
    """

    messages = {
      'nstr': '`1` is not a string',
    }

    def apply(self, person, evaluation):
      "Hello[person_]"

      if not person.has_form('String'):
        return evaluation.message('Hello', 'nstr', person)

      return String(f"Hello, {person.get_string_value()}!")

In this case, calling ``Hello[45]`` will emit the warning ``nstr: 45
is not a string``.

.. TODO: Document Operator and SympyFunction
.. TODO: Document interupts

Documentation markup
====================

There is a lot of special markup syntax you can use in the
documentation. It is kind of a mixture of XML, LaTeX, Python doctest,
and custom markup.

The following commands can be used to specify test cases.

+-----------+----------------------------------------------------------------+
| Syntax    | Explanation                                                    |
+===========+================================================================+
| ``>> $que | a test query.                                                  |
| ry$``     |                                                                |
+-----------+----------------------------------------------------------------+
| ``: $mess | a message in the result of the test query.                     |
| age$``    |                                                                |
+-----------+----------------------------------------------------------------+
| ``\| $pri | a printed line in the result of the test query.                |
| nt$``     |                                                                |
+-----------+----------------------------------------------------------------+
| ``= $resu | the actual result of the test query.                           |
| lt$``     |                                                                |
+-----------+----------------------------------------------------------------+
| ``. $newl | a newline in the test result.                                  |
| ine$``    |                                                                |
+-----------+----------------------------------------------------------------+
| ``$$ident | a variable identifier in Mathics code or in text.              |
| ifier$$`` |                                                                |
+-----------+----------------------------------------------------------------+
| ``#> $que | a test query that is not shown in the documentation.           |
| ry$``     |                                                                |
+-----------+----------------------------------------------------------------+
| ``X>``    | a test query that is shown in the documentation but is not     |
|           | ran.                                                           |
+-----------+----------------------------------------------------------------+
| ``S>``    | a test query that is shown in the documentation but is only    |
|           | ran if the ``SANDBOX`` environment variable is not set.        |
+-----------+----------------------------------------------------------------+
| ``-Graphi | graphics in the test result.                                   |
| cs-``     |                                                                |
+-----------+----------------------------------------------------------------+
| ``...``   | a part of the test result which is not checked in the test,    |
|           | e.g., for randomized or system-dependent output.               |
+-----------+----------------------------------------------------------------+

The following commands can be used to markup documentation text.

+----------------------------------+-----------------------------------------+
| Syntax                           | Explanation                             |
+==================================+=========================================+
| ``## $comment$``                 | a comment line that is not shown in the |
|                                  | documentation.                          |
+----------------------------------+-----------------------------------------+
| ``<dl>$list$</dl>``              | a definition list with ``<dt>`` and     |
|                                  | ``<dd>`` entries.                       |
+----------------------------------+-----------------------------------------+
| ``<dt>$title$``                  | the title of a description item.        |
+----------------------------------+-----------------------------------------+
| ``<dd>$description$``            | the description of a description item.  |
+----------------------------------+-----------------------------------------+
| ``<ul>$list$</ul>``              | an unordered list with ``<li>``         |
|                                  | entries.                                |
+----------------------------------+-----------------------------------------+
| ``<ol>$list$</ol>``              | an ordered list with ``<li>`` entries.  |
+----------------------------------+-----------------------------------------+
| ``<li>$item$``                   | an item of an unordered or ordered      |
|                                  | list.                                   |
+----------------------------------+-----------------------------------------+
| ``'$code$'``                     | inline Mathics code or other code.      |
+----------------------------------+-----------------------------------------+
| ``<console>$text$</console>``    | a console (shell/bash/Terminal)         |
|                                  | transcript in its own paragraph.        |
+----------------------------------+-----------------------------------------+
| ``<con>$text$</con>``            | an inline console transcript.           |
+----------------------------------+-----------------------------------------+
| ``<em>$text$</em>``              | emphasized (italic) text.               |
+----------------------------------+-----------------------------------------+
| ``<i>$text$</i>``                | the same as ``<em>``.                   |
+----------------------------------+-----------------------------------------+
| ``<url>$url$</url>``             | a URL.                                  |
+----------------------------------+-----------------------------------------+
| ``<img src="$src$" title="$title | an image.                               |
| $" label="$label$">``            |                                         |
+----------------------------------+-----------------------------------------+
| ``<imgpng src="$src$" title="$ti | the same as ``img``.                    |
| tle$" label="$label$">``         |                                         |
+----------------------------------+-----------------------------------------+
| ``<ref label="$label$">``        | a reference to an image.                |
+----------------------------------+-----------------------------------------+
| ``\skip``                        | a vertical skip.                        |
+----------------------------------+-----------------------------------------+
| ``\LaTeX``, ``\Mathematica``,    | special product and company names.      |
| ``\Mathics``                     |                                         |
+----------------------------------+-----------------------------------------+
| ``\'``                           | a single ``'``.                         |
+----------------------------------+-----------------------------------------+

To include images in the documentation, use the ``img`` tag, place an
EPS file ``$sc$.eps`` in ``documentation/images`` and run ``images.sh``
in the ``doc`` directory.

Documentation and tests
=======================

One of the greatest features of Mathics is its integrated documentation
and test system. Tests can be included right in the code as Python
docstrings. All desired functionality should be covered by these tests
to ensure that changes to the code don't break it. Execute

::

    $ python test.py

to run all tests.

During a test run, the results of tests can be stored for the
documentation, both in MathML and LaTeX form, by executing

::

    $ python test.py -o

or

::

    make doc

The XML version of the documentation, which can be accessed in the Web
interface, is updated immediately. To produce the LaTeX documentation
file, run:

::

    $ python test.py -t

You can then create the PDF using LaTeX. All required steps can be
executed by

::

    $ make latex

in the ``doc/tex`` directory, which uses ``latexmk`` to build the LaTeX
document. You just have to adjust the ``Makefile`` and ``latexmkrc`` to
your environment. You need the Asymptote (version 2 at least) to
generate the graphics in the documentation.

You can also run the tests for individual built-in symbols using

::

    $ python test.py -s [name]

This will not re-create the corresponding documentation results,
however. You have to run a complete test to do that.

Requirements
++++++++++++

The following packages are required to build the PDF documentation:

- ``texlive-font-utils``
- ``latexmk``
- ``texlive-xetex``
- ``lmodern``
- ``inkscape``
- ``texlive-latex-extra``
- ``texlive-latex``
- ``texlive-fonts-recommended``
- ``asymptote``

Those can be installed in Debian-based system with

::

    $ apt-get install texlive-font-utils latexmk texlive-xetex lmodern inkscape texlive-latex-extra texlive-latex texlive-fonts-recommended asymptote
