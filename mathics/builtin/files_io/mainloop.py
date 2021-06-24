# -*- coding: utf-8 -*-
"""
The Main Loop

An interactive session operates a loop, called the "main loop" in this way:

<ul>
  <li>read input
  <li>process input
  <li>format and print results
  <li>repeat
</ul>

As part of this loop, various global objects in this section are consulted.

There are a variety of "hooks" that allow you to insert functions to be applied to the expresssions at various stages in the main loop.

If you assign a function to the global variable '$PreRead' it will be applied with the input that is read in the first step listed above.

Similarly, if you assign a function to global variable '$Pre', it will be applied with the input before processing the input, the second step listed above.
"""

from mathics.version import __version__  # noqa used in loading to check consistency.

from mathics.builtin.base import Builtin


class HistoryLength(Builtin):
    """
    <dl>
    <dt>'$HistoryLength'
        <dd>specifies the maximum number of 'In' and 'Out' entries.
    </dl>
    >> $HistoryLength
     = 100
    >> $HistoryLength = 1;
    >> 42
     = 42
    >> %
     = 42
    >> %%
     = %3
    >> $HistoryLength = 0;
    >> 42
     = 42
    >> %
     = %7
    """

    name = "$HistoryLength"

    rules = {
        "$HistoryLength": "100",
    }


class In(Builtin):
    """
    <dl>
    <dt>'In[$k$]'
        <dd>gives the $k$th line of input.
    </dl>
    >> x = 1
     = 1
    >> x = x + 1
     = 2
    >> Do[In[2], {3}]
    >> x
     = 5
    >> In[-1]
     = 5
    >> Definition[In]
     = Attributes[In] = {Listable, Protected}
     .
     . In[6] = Definition[In]
     .
     . In[5] = In[-1]
     .
     . In[4] = x
     .
     . In[3] = Do[In[2], {3}]
     .
     . In[2] = x = x + 1
     .
     . In[1] = x = 1
    """

    attributes = ("Listable", "Protected")

    rules = {
        "In[k_Integer?Negative]": "In[$Line + k]",
    }


class IOHookPreRead(Builtin):
    """
    <dl>
    <dt>$PreRead
    <dt> is a global variable whose value, if set, is applied to the \
    text or box form of every input expression before it is fed to the parser.
    <dt>(Not implemented yet)
    </dl>
    """

    name = "$PreRead"
    attributes = ("Unprotected",)


class IOHookPre(Builtin):
    """
    <dl>
    <dt>$Pre
    <dt>is a global variable whose value, if set,
    is applied to every input expression.
    </dl>

    Set $Timing$ as the $Pre function, stores the enlapsed time in a variable,
    stores just the result in Out[$Line] and print a formated version showing the enlapsed time
    >> $Pre := (Print["[Processing input...]"];#1)&
    >> $Post := (Print["[Storing result...]"]; #1)&
     | [Processing input...]
     | [Storing result...]
    >> $PrePrint := (Print["The result is:"]; {TimeUsed[], #1})&
     | [Processing input...]
     | [Storing result...]
    >> 2 + 2
     | [Processing input...]
     | [Storing result...]
     | The result is:
     = {..., 4}
    >> $Pre = .; $Post = .;  $PrePrint = .;  $EnlapsedTime = .;
     | [Processing input...]
    >> 2 + 2
     = 4
    """

    name = "$Pre"
    attributes = ("Unprotected",)


class IOHookPost(Builtin):
    """
    <dl>
    <dt>$Post
    <dt>is a global variable whose value, if set,
    is applied to every output expression.
    </dl>
    """

    name = "$Post"
    attributes = ("Unprotected",)


class IOHookPrePrint(Builtin):
    """
    <dl>
    <dt>$PrePrint
    <dt>is a global variable whose value, if set,
    is applied to every output expression before it is printed.
    </dl>
    """

    name = "$PrePrint"
    attributes = ("Unprotected",)


class IOHookSyntaxHandler(Builtin):
    """
    <dl>
    <dt>$SyntaxHandler
    <dt>is a global variable whose value, if set,
    is applied to  any input string that is found to contain a syntax error.
    <dt>(Not implemented yet)
    </dl>
    """

    name = "$SyntaxHandler"
    attributes = ("Unprotected",)


class Line(Builtin):
    """
    <dl>
    <dt>'$Line'
        <dd>holds the current input line number.
    </dl>
    >> $Line
     = 1
    >> $Line
     = 2
    >> $Line = 12;
    >> 2 * 5
     = 10
    >> Out[13]
     = 10
    >> $Line = -1;
     : Non-negative integer expected.
    """

    name = "$Line"


class Out(Builtin):
    """
    <dl>
    <dt>'Out[$k$]'
    <dt>'%$k$'
        <dd>gives the result of the $k$th input line.
    <dt>'%', '%%', etc.
        <dd>gives the result of the previous input line, of the line before the previous input line, etc.
    </dl>

    >> 42
     = 42
    >> %
     = 42
    >> 43;
    >> %
     = 43
    >> 44
     = 44
    >> %1
     = 42
    >> %%
     = 44
    >> Hold[Out[-1]]
     = Hold[%]
    >> Hold[%4]
     = Hold[%4]
    >> Out[0]
     = Out[0]

    #> 10
     = 10
    #> Out[-1] + 1
     = 11
    #> Out[] + 1
     = 12
    """

    attributes = ("Listable", "Protected")

    rules = {
        "Out[k_Integer?Negative]": "Out[$Line + k]",
        "Out[]": "Out[$Line - 1]",
        "MakeBoxes[Out[k_Integer?((-10 <= # < 0)&)],"
        "    f:StandardForm|TraditionalForm|InputForm|OutputForm]": r'StringJoin[ConstantArray["%%", -k]]',
        "MakeBoxes[Out[k_Integer?Positive],"
        "    f:StandardForm|TraditionalForm|InputForm|OutputForm]": r'"%%" <> ToString[k]',
    }
