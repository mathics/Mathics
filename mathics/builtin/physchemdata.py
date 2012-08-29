# -*- coding: utf8 -*-

"""
Physical and Chemical data
"""

from mathics.builtin.base import Builtin
from mathics.core.expression import from_python
from mathics.settings import ROOT_DIR

def load_element_data():
    from csv import reader as csvreader
    element_file = open(ROOT_DIR + 'data/element.csv', 'rb')
    reader = csvreader(element_file, delimiter='\t')
    element_data = []
    for row in reader:
        element_data.append([value for value in row])            
    element_file.close()
    return element_data

_ELEMENT_DATA = load_element_data()

class ElementData(Builtin):
    """
    <dl>
    <dt>'ElementData["name", "property"]
        <dd>gives the value of the specified property for the chemical element "name".  #TODO: Change
    <dt>'ElementData[$n$, "property"]
        <dd>gives the specified property for the $n$th chemical element.                #TODO: Change
    </dl>
    """

    rules = {
        'ElementData[n_]': 'ElementData[n, "StandardName"]',
    }

    messages = {
        'noent': '`1` is not a known entity, class, or tag for ElementData. Use ElementData[] for a list of entities.',
        'noprop': '`1` is not a known property for ElementData. Use ElementData["Properties"] for a list of properties.',
    }

    def apply(self, n, prop, evaluation):
        "ElementData[n_, prop_]"

        py_n = n.to_python()
        py_prop = prop.to_python()

        # Check element specifier n or "name"
        if isinstance(py_n, int):
            if not 1 <= py_n <= 118:
                evaluation.message("ElementData", "noent", n)
                return
        elif isinstance(py_n, unicode):
            pass
        else:
            evaluation.message("ElementData", "noent", n)
            return

        # Check property specifier
        if isinstance(py_prop, str) or isinstance(py_prop, unicode):
            py_prop = str(py_prop)

        if not (isinstance(py_prop, str) and py_prop[0] == py_prop[-1] == '"' and py_prop.strip('"') in _ELEMENT_DATA[0]):
            evaluation.message("ElementData", "noprop", prop)
            return 

        iprop = _ELEMENT_DATA[0].index(py_prop.strip('"'))
        result = _ELEMENT_DATA[py_n][iprop]

        if result == "NOT_AVAILABLE":
            return Expression("Missing", "NotAvailable")
            
        if result == "NOT_APPLICABLE":
            return Expression("Missing", "NotApplicable")
            
        if result == "NOT_KNOWN":
            return Expression("Missing", "Unknown")
            
        from mathics.core.parser import parse
        return parse(result)
