.. pymathics-hello documentation master file, created by
   sphinx-quickstart on Sun Nov 29 14:06:02 2020.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. toctree::
   :maxdepth: 2
   :caption: Contents:

Calling Mathics library from within Python code
===============================================

Mathics is also a Python library implementing a parser and a interpreter
for WL.

For single line code, one stright forward way to interpret, evaluate a
WL expression, and obtaint a simple (text) formatted output is by means
of a system call

.. code:: py

    import subprocess

    expression = "Integrate[Sin[x]/x,x]"
    cmd = ["mathics", "--no-completion", "-q", "--colors", "NOCOLOR"]
    result = subprocess.run(cmd.append(f"\"{expression}\"" )], stdout=subprocess.PIPE).stdout
    result = result.split("\n")[-1]
    result = result[result.find("=")+1:]

This code runs the Mathics interpreter as a subprocess, sending a the
expression as an input parameter, and extracts from the output the
result.

On the other hand, we can do this best by calling directly to the
library:

.. code:: py

    from mathics.session import MathicsSession
    session = MathicsSession(add_builtin=True, catch_interrupt)


    expression = 'Integrate[Sin[x]/x,x]'
    result = session.evaluate(expression)
    session.evaluation.format_output(result)

