# -*- coding: utf8 -*-

"""
System functions
"""

from mathics.core.expression import String
from mathics.builtin.base import Predefined
from mathics import get_version_string

class Version(Predefined):
    """
    <dl>
    <dt>'$Version'
        <dd>returns a string with the current Mathics version and the versions of relevant libraries.
    </dl>
    
    >> $Version
     = Mathics ...
    """
    
    name = '$Version'
    
    def evaluate(self, evaluation):
        return String(get_version_string(True))