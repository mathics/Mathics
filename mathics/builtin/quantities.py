# -*- coding: utf-8 -*-

from mathics.version import __version__  # noqa used in loading to check consistency.
from mathics.builtin.base import Builtin, Test, MessageException
from mathics.builtin.randomnumbers import RandomEnv
from mathics.builtin.strings import to_regex, anchor_pattern, ToLowerCase
from mathics.core.expression import (
    Expression,
    String,
    Integer,
    Real,
    Symbol,
    Number,
    strip_context,
)

import os
import re
import itertools
from itertools import chain
import heapq
import math

import pint
from pint import UnitRegistry

ureg = UnitRegistry()
Q_ = ureg.Quantity


class KnownUnitQ(Test):

    """
    <dl>
    <dt>'KnownUnitQ[$unit$]'
        <dd>returns True if $unit$ is a canonical unit, and False otherwise.
    </dl>

    >> KnownUnitQ["Feet"]
     = True

    >> KnownUnitQ["Foo"]
     = False
    """

    def test(self, expr):
        def validate(unit):
            try:
                Q_(1, unit)
            except Exception:
                return False
            else:
                return True

        return validate(expr.get_string_value().lower())


class UnitConvert(Builtin):

    """
    <dl>
    <dt>'UnitConvert[$quantity$, $targetunit$] '
        <dd> converts the specified $quantity$ to the specified $targetunit$.
    <dt>'UnitConvert[quantity]'
        <dd> converts the specified $quantity$ to its "SIBase" units.
    </dl>

    Convert from miles to kilometers:
    >> UnitConvert[Quantity[5.2, "miles"], "kilometers"]
     = 8.36859 kilometer

    Convert a Quantity object to the appropriate SI base units:
    >> UnitConvert[Quantity[3.8, "Pounds"]]
     = 1.72365 kilogram

    #> UnitConvert[Quantity[{3, 10}, "centimeter"]]
     = {0.03 meter, 0.1 meter}

    #> UnitConvert[Quantity[3, "aaa"]]
     : Unable to interpret unit specification aaa.
     = UnitConvert[Quantity[3,aaa]]

    #> UnitConvert[Quantity[{300, 152}, "centimeter"], Quantity[10, "meter"]]
     = {3 meter, 1.52 meter}

    #> UnitConvert[Quantity[{3, 1}, "meter"], "inch"]
     = {118.11 inch, 39.3701 inch}
    """

    messages = {
        "argrx": "UnitConvert called with `1` arguments; 2 arguments are expected"
    }

    def apply(self, expr, toUnit, evaluation):
        "UnitConvert[expr_, toUnit_]"

        def convert_unit(leaves, target):

            mag = leaves[0]
            unit = leaves[1].get_string_value()
            quantity = Q_(mag, unit)
            converted_quantity = quantity.to(target)

            q_mag = converted_quantity.magnitude.evaluate(evaluation).get_float_value()

            # Displaying the magnitude in Integer form if the convert rate is an Integer
            if q_mag - int(q_mag) > 0:
                return Expression("Quantity", Real(q_mag), target)
            else:
                return Expression("Quantity", Integer(q_mag), target)

        if len(evaluation.out) > 0:
            return

        if toUnit.has_form("Quantity", None):
            targetUnit = toUnit.leaves[1].get_string_value().lower()
        elif toUnit.has_form("List", None):
            if not toUnit.leaves[0].has_form("Quantity", None):
                return
            else:
                targetUnit = toUnit.leaves[0].leaves[1].get_string_value().lower()
        elif isinstance(toUnit, String):
            targetUnit = toUnit.get_string_value().lower()
        else:
            return
        if expr.has_form("List", None):
            abc = []
            for i in range(len(expr.leaves)):
                abc.append(convert_unit(expr.leaves[i].leaves, targetUnit))
            return Expression("List", *abc)
        else:
            return convert_unit(expr.leaves, targetUnit)

    def apply_base_unit(self, expr, evaluation):
        "UnitConvert[expr_]"

        def convert_unit(leaves):

            mag = leaves[0]
            unit = leaves[1].get_string_value()

            quantity = Q_(mag, unit)
            converted_quantity = quantity.to_base_units()

            return Expression(
                "Quantity",
                converted_quantity.magnitude,
                String(converted_quantity.units),
            )

        if len(evaluation.out) > 0:
            return
        if expr.has_form("List", None):
            abc = []
            for i in range(len(expr.leaves)):
                abc.append(convert_unit(expr.leaves[i].leaves))
            return Expression("List", *abc)
        else:
            return convert_unit(expr.leaves)


class Quantity(Builtin):
    """
    <dl>
    <dt>'Quantity[$magnitude$, $unit$]'
        <dd>represents a quantity with size $magnitude$ and unit specified by $unit$.
    <dt>'Quantity[$unit$]'
        <dd>assumes the magnitude of the specified $unit$ to be 1.
    </dl>

    >> Quantity["Kilogram"]
     = 1 kilogram

    >> Quantity[10, "Meters"]
     = 10 meter

    >> Quantity[{10,20}, "Meters"]
     = {10 meter, 20 meter}

    #> Quantity[10, Meters]
     = Quantity[10, Meters]

    #> Quantity[Meters]
     : Unable to interpret unit specification Meters.
     = Quantity[Meters]

    #> Quantity[1, "foot"]
     = 1 foot
    """

    attributes = ("HoldRest", "NHoldRest", "Protected", "ReadProtected")

    messages = {
        "unkunit": "Unable to interpret unit specification `1`.",
    }

    def validate(self, unit, evaluation):
        if KnownUnitQ(unit).evaluate(evaluation) == Symbol("False"):
            return False
        return True

    def apply_makeboxes(self, mag, unit, f, evaluation):
        "MakeBoxes[Quantity[mag_, unit_?StringQ], f:StandardForm|TraditionalForm|OutputForm|InputForm]"

        q_unit = unit.get_string_value().lower()
        if self.validate(unit, evaluation):
            return Expression("RowBox", Expression("List", mag, " ", q_unit))
        else:
            return Expression(
                "RowBox", Expression("List", "Quantity", "[", mag, ",", q_unit, "]")
            )

    def apply_n(self, mag, unit, evaluation):
        "Quantity[mag_, unit_?StringQ]"
        expr = Expression("Quantity", mag, unit)

        if self.validate(unit, evaluation):
            if mag.has_form("List", None):
                results = []
                for i in range(len(mag.leaves)):
                    quantity = Q_(mag.leaves[i], unit.get_string_value().lower())
                    results.append(
                        Expression(
                            "Quantity", quantity.magnitude, String(quantity.units)
                        )
                    )
                return Expression("List", *results)
            else:
                quantity = Q_(mag, unit.get_string_value().lower())
                return Expression(
                    "Quantity", quantity.magnitude, String(quantity.units)
                )
        else:
            return evaluation.message("Quantity", "unkunit", unit)

    def apply_1(self, unit, evaluation):
        "Quantity[unit_]"
        if not isinstance(unit, String):
            return evaluation.message("Quantity", "unkunit", unit)
        else:
            return self.apply_n(Integer(1), unit, evaluation)


class QuantityQ(Test):
    """
    <dl>
    <dt>'QuantityQ[$expr$]'
        <dd>return True if $expr$ is a valid Association object, and False otherwise.
    </dl>

    >> QuantityQ[Quantity[3, "Meters"]]
     = True

    >> QuantityQ[Quantity[3, "Maters"]]
     : Unable to interpret unit specification Maters.
     = False

    #> QuantityQ[3]
     = False
    """

    def test(self, expr):
        def validate_unit(unit):
            try:
                Q_(1, unit)
            except Exception:
                return False
            else:
                return True

        def validate(leaves):
            if len(leaves) < 1 or len(leaves) > 2:
                return False
            elif len(leaves) == 1:
                if validate_unit(leaves[0].get_string_value().lower()):
                    return True
                else:
                    return False
            else:
                if isinstance(leaves[0], Number):
                    if validate_unit(leaves[1].get_string_value().lower()):
                        return True
                    else:
                        return False
                else:
                    return False

        return expr.get_head_name() == "System`Quantity" and validate(expr.leaves)


class QuantityUnit(Builtin):
    """
    <dl>
    <dt>'QuantityUnit[$quantity$]'
        <dd>returns the unit associated with the specified $quantity$.
    </dl>

    >> QuantityUnit[Quantity["Kilogram"]]
     = kilogram

    >> QuantityUnit[Quantity[10, "Meters"]]
     = meter

    >> QuantityUnit[Quantity[{10,20}, "Meters"]]
     = {meter, meter}

    #> QuantityUnit[Quantity[10, "aaa"]]
     : Unable to interpret unit specification aaa.
     = QuantityUnit[Quantity[10,aaa]]
    """

    def apply(self, expr, evaluation):
        "QuantityUnit[expr_]"

        def get_unit(leaves):
            if len(leaves) == 1:
                return leaves[0]
            else:
                return leaves[1]

        if len(evaluation.out) > 0:
            return
        if expr.has_form("List", None):
            results = []
            for i in range(len(expr.leaves)):
                results.append(get_unit(expr.leaves[i].leaves))
            return Expression("List", *results)
        else:
            return get_unit(expr.leaves)


class QuantityMagnitude(Builtin):
    """
    <dl>
    <dt>'QuantityMagnitude[$quantity$]'
        <dd>gives the amount of the specified $quantity$.
    <dt>'QuantityMagnitude[$quantity$, $unit$]'
        <dd>gives the value corresponding to $quantity$ when converted to $unit$.
    </dl>

    >> QuantityMagnitude[Quantity["Kilogram"]]
     = 1

    >> QuantityMagnitude[Quantity[10, "Meters"]]
     = 10

    >> QuantityMagnitude[Quantity[{10,20}, "Meters"]]
     = {10, 20}

    #> QuantityMagnitude[Quantity[1, "meter"], "centimeter"]
     = 100

    #> QuantityMagnitude[Quantity[{3,1}, "meter"], "centimeter"]
     = {300, 100}

    #> QuantityMagnitude[Quantity[{300,100}, "centimeter"], "meter"]
     = {3, 1}

    #> QuantityMagnitude[Quantity[{3, 1}, "meter"], "inch"]
     = {118.11, 39.3701}

    #> QuantityMagnitude[Quantity[{3, 1}, "meter"], Quantity[3, "centimeter"]]
     = {300, 100}

    #> QuantityMagnitude[Quantity[3,"mater"]]
     : Unable to interpret unit specification mater.
     = QuantityMagnitude[Quantity[3,mater]]
    """

    def apply(self, expr, evaluation):
        "QuantityMagnitude[expr_]"

        def get_magnitude(leaves):
            if len(leaves) == 1:
                return 1
            else:
                return leaves[0]

        if len(evaluation.out) > 0:
            return
        if expr.has_form("List", None):
            results = []
            for i in range(len(expr.leaves)):
                results.append(get_magnitude(expr.leaves[i].leaves))
            return Expression("List", *results)
        else:
            return get_magnitude(expr.leaves)

    def apply_unit(self, expr, unit, evaluation):
        "QuantityMagnitude[expr_, unit_]"

        def get_magnitude(leaves, targetUnit, evaluation):
            quanity = Q_(leaves[0], leaves[1].get_string_value())
            converted_quantity = quanity.to(targetUnit)
            q_mag = converted_quantity.magnitude.evaluate(evaluation).get_float_value()

            # Displaying the magnitude in Integer form if the convert rate is an Integer
            if q_mag - int(q_mag) > 0:
                return Real(q_mag)
            else:
                return Integer(q_mag)

        if len(evaluation.out) > 0:
            return

        # Getting the target unit
        if unit.has_form("Quantity", None):
            targetUnit = unit.leaves[1].get_string_value().lower()
        elif unit.has_form("List", None):
            if not unit.leaves[0].has_form("Quantity", None):
                return
            else:
                targetUnit = unit.leaves[0].leaves[1].get_string_value().lower()
        elif isinstance(unit, String):
            targetUnit = unit.get_string_value().lower()
        else:
            return

        # convert the quantity to the target unit and return the magnitude
        if expr.has_form("List", None):
            results = []
            for i in range(len(expr.leaves)):
                results.append(
                    get_magnitude(expr.leaves[i].leaves, targetUnit, evaluation)
                )
            return Expression("List", *results)
        else:
            return get_magnitude(expr.leaves, targetUnit, evaluation)
