# Documentation and tests

One of the greatest features of Mathics is its integrated documentation and 
test system. Tests can be included right in the code as Python docstrings. All 
desired functionality should be covered by these tests to ensure that changes 
to the code don't break it. Execute

```
$ python test.py
```

to run all tests.

During a test run, the results of tests can be stored for the documentation, 
both in MathML and LaTeX form, by executing

```
$ python test.py -o
```

or

```
make doc
```

The XML version of the documentation, which can be accessed in the Web 
interface, is updated immediately. To produce the LaTeX documentation file, 
run:

```
$ python test.py -t
```

You can then create the PDF using LaTeX. All required steps can be executed by

```
$ make latex
```

in the `doc/tex` directory, which uses `latexmk` to build 
the LaTeX document. You just have to adjust the `Makefile` and 
`latexmkrc` to your environment. You need the Asymptote (version 2 at 
least) to generate the graphics in the documentation.

You can also run the tests for individual built-in symbols using

```
$ python test.py -s [name]
```

This will not re-create the corresponding documentation results, however. You 
have to run a complete test to do that.

## Requirements

The following packages are required to build the PDF documentation:
* `texlive-font-utils`
* `latexmk`
* `texlive-xetex`
* `lmodern`
* `inkscape`
* `texlive-latex-extra`
* `texlive-latex`
* `texlive-fonts-recommended`
* `asymptote`

Those can be installed in Debian-based system with
```
$ apt-get install texlive-font-utils latexmk texlive-xetex lmodern inkscape texlive-latex-extra texlive-latex texlive-fonts-recommended asymptote
```
