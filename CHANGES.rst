CHANGES
=======

0.9
---

New features:

- Improved syntax error messages #329
- SVD, LeastSquares, PseudoInverse #258, #321
- Python 3 support #317
- Improvements to Riffle #313
- Tweaks to PolarPlot #305
- StringTake #285
- Norm #268 #270
- Total, Accumulate, FoldList, Fold #264, #252
- Flatten #253 #269
- Which with symbolic arguments #250
- Min/Max with symbolic arguments # 249

Dependency Updates:

- upgraded to ply 3.8 (issue #246)
- dropped interrupting cow #317
- added six (already required by django) #317

Bug fixes:

- Span issues with negative indices #196 fixed by #263 #325
- SVG export bug fixed by #324
- Django runserver threading issue #158 fixed by #323
- asymptote bug building docs #297 fixed by #317
- Simplify issue #254 fixed by #322
- ParametricPlot bug fixed by #320
- DensityPlot SVG regression in the web interface.
- main function for server.py #288, #289 fixed by #298
- ply table regeneration #294 fixed by #295
- Print bar issue #290 fixed by #293
- Quit[] index error #292 partially fixed by #307
- Quit definition fixed by #286
- Conjugate issue #272 fixed by #281

0.8
---

New features:

- Improvements to 3D Plotting, see #238
- Enable MathJax menu, see #236
- Improvements to documentation

Dependency Updates:

- upgrade to sympy 0.7.6
- upgrade to ply3.6 (new parsetab format, see #246)
- upgrade to mpmath 0.19

Bug Fixes:

- IntegerDigits[0]



0.7
---

New features:

- Readline tab completion
- automatic database initialisation
- support for wildcards in ``Clear`` and ``ClearAll``
- add ``Conjugate``
- More tests and documentation for ``Sequence``
- Context support

Bugs fixed:

- Fix unevaluated index handling (issue #217)
- Fix ``Solve`` treating one solution equal to 1 as a tautology (issue
  #208)
- Fix temporary symbols appearing in the result when taking
  derivatives with respect to t (issue #184)
- typo in save worksheet help text (issue #199)
- Fix mathicsserver wildcard address binding
- Fix ``Dot`` acting on matrices in MatrixForm (issue #145)
- Fix Sum behaviour when using range to generate index values (issue #149)
- Fix behaviour of plot with unevaluated arguments (issue #150)
- Fix zero-width space between factors in MathJax output (issue #45)
- Fix ``{{2*a, 0},{0,0}}//MatrixForm`` crashing in the web interface
  (issue #182)

0.6
---

New features:

- ElementData using data from Wikipedia
- added Switch
- added DSolve and RSolve
- More Timing functions AbsoluteTiming, TimeUsed, SessionTime, Pause
- Date functions DateList, DateString, DateDifference, etc
- Parser rewritten using lex/yacc (PLY)
- Unicode character support 
- PolarPlot
- IPython style (coloured) input 
- VectorAnalysis` Package
- More special functions (Bessel functions and othogonal polynomials)
- More NumberTheory functions
- Import, Export, Get, Needs and other IO related functions
- PyPy compatibility
- added benchmarks (mathics/benchmark.py)
- BaseForm
- DeleteDuplicates
- Depth, Operate Through and other Structure related functions
- Changes to MatrixForm/TableForm printing
- Use interruptingcow to limit evaluation time
- Character Code functions

Bugs fixed:

- Fix divide-by-zero with zero-length plot range
- Fix mathicsserver exception on startup with Django 1.6 (issues #194,
  #205, #209)

0.5
---

- 3D graphics and plots using WebGL in the browser and Asymptote in TeX output
- Plot: adaptive sampling
- MathJax 2.0 and line breaking
- new symbols: Graphics3D etc., Plot3D, ListPlot, ListLinePlot, ParametricPlot, Prime, Names, $Version
- fixed issues: 1, 4, 6, 8-21, 23-27
- lots of minor fixes and improvements
- number of built-in symbols: 386

0.4
---

- compatibility to Sage 4.0 and other latest libraries

0.3 (beta only)
---------------

- resolved several issues

0.1 (alpha only)
----------------

- initial version
