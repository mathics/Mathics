# -*- coding: utf-8 -*-
"""
The Main Loop
"""

from mathics.version import __version__  # noqa used in loading to check consistency.
from mathics.builtin.base import Builtin


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
