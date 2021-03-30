#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Physical and Chemical data
"""

import os

from csv import reader as csvreader

from mathics.version import __version__  # noqa used in loading to check consistency.
from mathics.builtin.base import Builtin
from mathics.core.expression import (
    Expression,
    from_python,
    Symbol,
    String,
    strip_context,
)
from mathics.settings import ROOT_DIR


class NoElementDataFile(Exception):
    pass


def load_element_data():
    try:
        import mathics_scanner

        datadir = mathics_scanner.__file__[:-11]
        element_file = open(os.path.join(datadir, "data/element.csv"), "r")
    except:
        print(os.path.join(datadir, "data/element.csv"), "  not found.")
        return None
    reader = csvreader(element_file, delimiter="\t")
    element_data = []
    for row in reader:
        element_data.append([value for value in row])
    element_file.close()
    return element_data


_ELEMENT_DATA = load_element_data()

if _ELEMENT_DATA is None:
    raise NoElementDataFile("data/elements.csv is not available.")


class ElementData(Builtin):
    """
    <dl>
    <dt>'ElementData["$name$", "$property$"]'
        <dd>gives the value of the $property$ for the chemical
        specified by $name$.
    <dt>'ElementData[$n$, "$property$"]'
        <dd>gives the value of the $property$ for the $n$th chemical element.
    </dl>

    >> ElementData[74]
     = Tungsten

    >> ElementData["He", "AbsoluteBoilingPoint"]
     = 4.22

    >> ElementData["Carbon", "IonizationEnergies"]
     = {1086.5, 2352.6, 4620.5, 6222.7, 37831, 47277.}

    >> ElementData[16, "ElectronConfigurationString"]
     = [Ne] 3s2 3p4

    >> ElementData[73, "ElectronConfiguration"]
     = {{2}, {2, 6}, {2, 6, 10}, {2, 6, 10, 14}, {2, 6, 3}, {2}}

    The number of known elements:
    >> Length[ElementData[All]]
     = 118

    Some properties are not appropriate for certain elements:
    >> ElementData["He", "ElectroNegativity"]
     = Missing[NotApplicable]

    Some data is missing:
    >> ElementData["Tc", "SpecificHeat"]
     = Missing[NotAvailable]

    All the known properties:
    >> ElementData["Properties"]
     = {Abbreviation, AbsoluteBoilingPoint, AbsoluteMeltingPoint, AtomicNumber, AtomicRadius, AtomicWeight, Block, BoilingPoint, BrinellHardness, BulkModulus, CovalentRadius, CrustAbundance, Density, DiscoveryYear, ElectroNegativity, ElectronAffinity, ElectronConfiguration, ElectronConfigurationString, ElectronShellConfiguration, FusionHeat, Group, IonizationEnergies, LiquidDensity, MeltingPoint, MohsHardness, Name, Period, PoissonRatio, Series, ShearModulus, SpecificHeat, StandardName, ThermalConductivity, VanDerWaalsRadius, VaporizationHeat, VickersHardness, YoungModulus}

    >> ListPlot[Table[ElementData[z, "AtomicWeight"], {z, 118}]]
     = -Graphics-

    ## Ensure all data parses #664
    #> Outer[ElementData, Range[118], ElementData["Properties"]];
    """

    rules = {
        "ElementData[n_]": 'ElementData[n, "StandardName"]',
        "ElementData[]": "ElementData[All]",
        'ElementData["Properties"]': 'ElementData[All, "Properties"]',
    }

    messages = {
        "noent": (
            "`1` is not a known entity, class, or tag for ElementData. "
            "Use ElementData[] for a list of entities."
        ),
        "noprop": (
            "`1` is not a known property for ElementData. "
            'Use ElementData["Properties"] for a list of properties.'
        ),
    }

    def apply_all(self, evaluation):
        "ElementData[All]"
        iprop = _ELEMENT_DATA[0].index("StandardName")
        return from_python([element[iprop] for element in _ELEMENT_DATA[1:]])

    def apply_all_properties(self, evaluation):
        'ElementData[All, "Properties"]'
        return from_python(sorted(_ELEMENT_DATA[0]))

    def apply_name(self, name, prop, evaluation):
        "ElementData[name_?StringQ, prop_]"
        py_name = name.to_python().strip('"')
        names = ["StandardName", "Name", "Abbreviation"]
        iprops = [_ELEMENT_DATA[0].index(s) for s in names]

        indx = None
        for iprop in iprops:
            try:
                indx = [element[iprop] for element in _ELEMENT_DATA[1:]].index(
                    py_name
                ) + 1
            except ValueError:
                pass

        if indx is None:
            evaluation.message("ElementData", "noent", name)
            return

        return self.apply_int(from_python(indx), prop, evaluation)

    def apply_int(self, n, prop, evaluation):
        "ElementData[n_?IntegerQ, prop_]"

        py_n = n.to_python()
        py_prop = prop.to_python()

        # Check element specifier n or "name"
        if isinstance(py_n, int):
            if not 1 <= py_n <= 118:
                evaluation.message("ElementData", "noent", n)
                return
        elif isinstance(py_n, str):
            pass
        else:
            evaluation.message("ElementData", "noent", n)
            return

        # Check property specifier
        if isinstance(py_prop, str):
            py_prop = str(py_prop)

        if py_prop == '"Properties"':
            result = []
            for i, p in enumerate(_ELEMENT_DATA[py_n]):
                if p not in ["NOT_AVAILABLE", "NOT_APPLICABLE", "NOT_KNOWN"]:
                    result.append(_ELEMENT_DATA[0][i])
            return from_python(sorted(result))

        if not (
            isinstance(py_prop, str)
            and py_prop[0] == py_prop[-1] == '"'
            and py_prop.strip('"') in _ELEMENT_DATA[0]
        ):
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

        result = evaluation.parse(result)
        if isinstance(result, Symbol):
            result = String(strip_context(result.get_name()))
        return result
