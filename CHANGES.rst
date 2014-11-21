CHANGES
=======

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
- Fix zero-width space between factors in MathJax output (issue #45)
- Fix ``{{2*a, 0},{0,0}}//MatrixForm`` crashing in the web interface
  (issue #182)
- Fix temporary symbols appearing in the result when taking
  derivatives with respect to t (issue #184)
- Fix ``Solve`` treating one solution equal to 1 as a tautology (issue
  #208)
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
