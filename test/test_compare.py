# -*- coding: utf-8 -*-
from .helper import check_evaluation
import pytest


#  The following tests where generated automatically calling wolframscript -c
#  followed by a combination of expressions.
#  This is the code I used to generate them
#
#  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# import subprocess
# from time import sleep
# exprss = ['2 + 3*a', 'Infinity', '-Infinity', 'Sqrt[I] Infinity', 'a', '"a"', '"1 / 4"', "I", "0", '1 / 4','.25',"Sqrt[2]", "BesselJ[0, 2]", "3+2 I", "2.+ Pi I", "3+I Pi", 'TestFunction["Tengo una vaca lechera"]', "Compile[{x}, Sqrt[x]]", "Graphics[{Disk[{0,0},1]}]"]
# pairs = sum([[(exprss[i], exprss[j]) for j in range(i+1)] for i in range(len(exprss))],[])
# tests = []
#
# for pair in pairs:
#     test = " " + pair[0] + ' == ' + pair[1]
#     result = subprocess.run(['wolframscript', '-c', test], stdout=subprocess.PIPE)
#     sleep(1)
#     res = result.stdout.decode('utf8').strip()
#     if len(res)>0 and res[-1] == '\n':
#         res = res[:-2]
#     newtest = (pair[0], pair[1], res)
#     print(newtest)
#     print("  ", newtest, ",")
#     tests.append(newtest)

tests1 = [
    ("Sqrt[I] Infinity", "2 + 3 a", '"-1 ^ (1 / 4) Infinity == 2 + 3 a"'),
    ("a", "Sqrt[I] Infinity", '"a == -1 ^ (1 / 4) Infinity"'),
    ('"a"', "2 + 3 a", '"a == 2 + 3 a"'),
    ('"a"', "Infinity", '"a == Infinity"'),
    ('"a"', "-Infinity", '"a == -Infinity"'),
    ('"a"', "Sqrt[I] Infinity", '"a == -1 ^ (1 / 4) Infinity"'),
    ('"a"', "a", '"a == a"'),
    ("Graphics[{Disk[{0,0},1]}]", "2 + 3 a", '"-Graphics- == 2 + 3 a"'),
    ("Graphics[{Disk[{0,0},1]}]", "Infinity", '"-Graphics- == Infinity"'),
    ("Graphics[{Disk[{0,0},1]}]", "-Infinity", '"-Graphics- == -Infinity"'),
    (
        "Graphics[{Disk[{0,0},1]}]",
        "Sqrt[I] Infinity",
        '"-Graphics- == -1 ^ (1 / 4) Infinity"',
    ),
    ("Graphics[{Disk[{0,0},1]}]", "a", '"-Graphics- == a"'),
    ("Graphics[{Disk[{0,0},1]}]", '"a"', '"-Graphics- == a"'),
    ("Graphics[{Disk[{0,0},1]}]", '"1 / 4"', '"-Graphics- == 1 / 4"'),
    ("Graphics[{Disk[{0,0},1]}]", "I", '"-Graphics- == I"'),
    ("Graphics[{Disk[{0,0},1]}]", "0", '"-Graphics- == 0"'),
    ("Graphics[{Disk[{0,0},1]}]", "1 / 4", '"-Graphics- == 1 / 4"'),
    ("Graphics[{Disk[{0,0},1]}]", ".25", '"-Graphics- == 0.25"'),
    ("Graphics[{Disk[{0,0},1]}]", "Sqrt[2]", '"-Graphics- == Sqrt[2]"'),
    ("Graphics[{Disk[{0,0},1]}]", "BesselJ[0, 2]", '"-Graphics- == BesselJ[0, 2]"'),
    ("Graphics[{Disk[{0,0},1]}]", "3+2 I", '"-Graphics- == 3 + 2 I"'),
    ("Graphics[{Disk[{0,0},1]}]", "2.+ Pi I", '"-Graphics- == 2. + 3.14159 I"'),
    ("Graphics[{Disk[{0,0},1]}]", "3+I Pi", '"-Graphics- == 3 + I Pi"'),
    (
        "Graphics[{Disk[{0,0},1]}]",
        'TestFunction["Tengo una vaca lechera"]',
        '"-Graphics- == TestFunction[Tengo una vaca lechera]"',
    ),
    (
        "Graphics[{Disk[{0,0},1]}]",
        "Compile[{x}, Sqrt[x]]",
        '"-Graphics- == CompiledFunction[{x}, Sqrt[x], -PythonizedCode-]"',
    ),
    ('"1 / 4"', "2 + 3 a", '"1 / 4 == 2 + 3 a"'),
    ('"1 / 4"', "Infinity", '"1 / 4 == Infinity"'),
    ('"1 / 4"', "-Infinity", '"1 / 4 == -Infinity"'),
    ('"1 / 4"', "Sqrt[I] Infinity", '"1 / 4 == -1 ^ (1 / 4) Infinity"'),
    ('"1 / 4"', "a", '"1 / 4 == a"'),
    ("Sqrt[2]", '"1 / 4"', '"Sqrt[2] == 1 / 4"'),
    ("BesselJ[0, 2]", '"1 / 4"', '"BesselJ[0, 2] == 1 / 4"'),
    ("3+I Pi", '"1 / 4"', '"3 + I Pi == 1 / 4"'),
    (
        'TestFunction["Tengo una vaca lechera"]',
        "2 + 3 a",
        '"TestFunction[Tengo una vaca lechera] == 2 + 3 a"',
    ),
    (
        'TestFunction["Tengo una vaca lechera"]',
        "Infinity",
        '"TestFunction[Tengo una vaca lechera] == Infinity"',
    ),
    (
        'TestFunction["Tengo una vaca lechera"]',
        "-Infinity",
        '"TestFunction[Tengo una vaca lechera] == -Infinity"',
    ),
    (
        'TestFunction["Tengo una vaca lechera"]',
        "Sqrt[I] Infinity",
        '"TestFunction[Tengo una vaca lechera] == -1 ^ (1 / 4) Infinity"',
    ),
    (
        'TestFunction["Tengo una vaca lechera"]',
        "a",
        '"TestFunction[Tengo una vaca lechera] == a"',
    ),
    (
        'TestFunction["Tengo una vaca lechera"]',
        '"a"',
        '"TestFunction[Tengo una vaca lechera] == a"',
    ),
    (
        'TestFunction["Tengo una vaca lechera"]',
        '"1 / 4"',
        '"TestFunction[Tengo una vaca lechera] == 1 / 4"',
    ),
    (
        'TestFunction["Tengo una vaca lechera"]',
        "I",
        '"TestFunction[Tengo una vaca lechera] == I"',
    ),
    (
        'TestFunction["Tengo una vaca lechera"]',
        "0",
        '"TestFunction[Tengo una vaca lechera] == 0"',
    ),
    (
        'TestFunction["Tengo una vaca lechera"]',
        "1 / 4",
        '"TestFunction[Tengo una vaca lechera] == 1 / 4"',
    ),
    (
        'TestFunction["Tengo una vaca lechera"]',
        ".25",
        '"TestFunction[Tengo una vaca lechera] == 0.25"',
    ),
    (
        'TestFunction["Tengo una vaca lechera"]',
        "Sqrt[2]",
        '"TestFunction[Tengo una vaca lechera] == Sqrt[2]"',
    ),
    (
        'TestFunction["Tengo una vaca lechera"]',
        "BesselJ[0, 2]",
        '"TestFunction[Tengo una vaca lechera] == BesselJ[0, 2]"',
    ),
    (
        'TestFunction["Tengo una vaca lechera"]',
        "3+2 I",
        '"TestFunction[Tengo una vaca lechera] == 3 + 2 I"',
    ),
    (
        'TestFunction["Tengo una vaca lechera"]',
        "2.+ Pi I",
        '"TestFunction[Tengo una vaca lechera] == 2. + 3.14159 I"',
    ),
    (
        'TestFunction["Tengo una vaca lechera"]',
        "3+I Pi",
        '"TestFunction[Tengo una vaca lechera] == 3 + I Pi"',
    ),
    (
        "Compile[{x}, Sqrt[x]]",
        "2 + 3 a",
        '"CompiledFunction[{x}, Sqrt[x], -PythonizedCode-] == 2 + 3 a"',
    ),
    (
        "Compile[{x}, Sqrt[x]]",
        "Infinity",
        '"CompiledFunction[{x}, Sqrt[x], -PythonizedCode-] == Infinity"',
    ),
    (
        "Compile[{x}, Sqrt[x]]",
        "-Infinity",
        '"CompiledFunction[{x}, Sqrt[x], -PythonizedCode-] == -Infinity"',
    ),
    (
        "Compile[{x}, Sqrt[x]]",
        "Sqrt[I] Infinity",
        '"CompiledFunction[{x}, Sqrt[x], -PythonizedCode-] == -1 ^ (1 / 4) Infinity"',
    ),
    (
        "Compile[{x}, Sqrt[x]]",
        "a",
        '"CompiledFunction[{x}, Sqrt[x], -PythonizedCode-] == a"',
    ),
    (
        "Compile[{x}, Sqrt[x]]",
        '"a"',
        '"CompiledFunction[{x}, Sqrt[x], -PythonizedCode-] == a"',
    ),
    (
        "Compile[{x}, Sqrt[x]]",
        '"1 / 4"',
        '"CompiledFunction[{x}, Sqrt[x], -PythonizedCode-] == 1 / 4"',
    ),
    (
        "Compile[{x}, Sqrt[x]]",
        "I",
        '"CompiledFunction[{x}, Sqrt[x], -PythonizedCode-] == I"',
    ),
    (
        "Compile[{x}, Sqrt[x]]",
        "0",
        '"CompiledFunction[{x}, Sqrt[x], -PythonizedCode-] == 0"',
    ),
    (
        "Compile[{x}, Sqrt[x]]",
        "1 / 4",
        '"CompiledFunction[{x}, Sqrt[x], -PythonizedCode-] == 1 / 4"',
    ),
    (
        "Compile[{x}, Sqrt[x]]",
        ".25",
        '"CompiledFunction[{x}, Sqrt[x], -PythonizedCode-] == 0.25"',
    ),
    (
        "Compile[{x}, Sqrt[x]]",
        "Sqrt[2]",
        '"CompiledFunction[{x}, Sqrt[x], -PythonizedCode-] == Sqrt[2]"',
    ),
    (
        "Compile[{x}, Sqrt[x]]",
        "BesselJ[0, 2]",
        '"CompiledFunction[{x}, Sqrt[x], -PythonizedCode-] == BesselJ[0, 2]"',
    ),
    (
        "Compile[{x}, Sqrt[x]]",
        "3+2 I",
        '"CompiledFunction[{x}, Sqrt[x], -PythonizedCode-] == 3 + 2 I"',
    ),
    (
        "Compile[{x}, Sqrt[x]]",
        "2.+ Pi I",
        '"CompiledFunction[{x}, Sqrt[x], -PythonizedCode-] == 2. + 3.14159 I"',
    ),
    (
        "Compile[{x}, Sqrt[x]]",
        "3+I Pi",
        '"CompiledFunction[{x}, Sqrt[x], -PythonizedCode-] == 3 + I Pi"',
    ),
    (
        "Compile[{x}, Sqrt[x]]",
        'TestFunction["Tengo una vaca lechera"]',
        '"CompiledFunction[{x}, Sqrt[x], -PythonizedCode-] == TestFunction[Tengo una vaca lechera]"',
    ),
]

tests2 = [
    (r'"\[Mu]"', r'"μ"', "True"),
    ("2 + 3*a", "2 + 3*a", "True"),
    ("Infinity", "2 + 3*a", "Infinity == 2 + 3 a"),
    ("Infinity", "Infinity", "True"),
    ("-Infinity", "2 + 3 a", "-Infinity == 2 + 3 a"),
    ("-Infinity", "Infinity", "False"),
    ("-Infinity", "-Infinity", "True"),
    ("Sqrt[I] Infinity", "Infinity", "False"),
    ("Sqrt[I] Infinity", "-Infinity", "False"),
    ("Sqrt[I] Infinity", "Sqrt[I] Infinity", "True"),
    ("a", "2 + 3 a", "a == 2 + 3 a"),
    ("a", "Infinity", "a == Infinity"),
    ("a", "-Infinity", "a == -Infinity"),
    ("a", "a", "True"),
    ('"a"', '"a"', "True"),
    ('"1 / 4"', '"a"', "False"),
    ('"1 / 4"', '"1 / 4"', "True"),
    ("I", "2 + 3 a", "I == 2 + 3 a"),
    ("I", "Infinity", "False"),
    ("I", "-Infinity", "False"),
    ("I", "Sqrt[I] Infinity", "False"),
    ("I", "a", "I == a"),
    ("I", '"a"', "False"),
    ("I", '"1 / 4"', "False"),
    ("I", "I", "True"),
    ("0", "2 + 3 a", "0 == 2 + 3 a"),
    ("0", "Infinity", "False"),
    ("0", "-Infinity", "False"),
    ("0", "Sqrt[I] Infinity", "False"),
    ("0", "a", "0 == a"),
    ("0", '"a"', "False"),
    ("0", '"1 / 4"', "False"),
    ("0", "I", "False"),
    ("0", "0", "True"),
    ("1 / 4", "2 + 3 a", "1/4 == 2 + 3 a"),
    ("1 / 4", "Infinity", "False"),
    ("1 / 4", "-Infinity", "False"),
    ("1 / 4", "Sqrt[I] Infinity", "False"),
    ("1 / 4", "a", "1/4 == a"),
    ("1 / 4", '"a"', "False"),
    ("1 / 4", '"1 / 4"', "False"),
    ("1 / 4", "I", "False"),
    ("1 / 4", "0", "False"),
    ("1 / 4", "1 / 4", "True"),
    (".25", "2 + 3 a", "0.25 == 2 + 3 a"),
    (".25", "Infinity", "False"),
    (".25", "-Infinity", "False"),
    (".25", "Sqrt[I] Infinity", "False"),
    (".25", "a", "0.25 == a"),
    (".25", '"a"', "False"),
    (".25", '"1 / 4"', "False"),
    (".25", "I", "False"),
    (".25", "0", "False"),
    (".25", "1 / 4", "True"),
    (".25", ".25", "True"),
    ("Sqrt[2]", "2 + 3 a", "Sqrt[2] == 2 + 3 a"),
    ("Sqrt[2]", "Infinity", "False"),
    ("Sqrt[2]", "-Infinity", "False"),
    ("Sqrt[2]", "Sqrt[I] Infinity", "False"),
    ("Sqrt[2]", "a", "Sqrt[2] == a"),
    ("Sqrt[2]", '"a"', 'Sqrt[2] == "a"'),
    ("Sqrt[2]", "I", "False"),
    ("Sqrt[2]", "0", "False"),
    ("Sqrt[2]", "1 / 4", "False"),
    ("Sqrt[2]", ".25", "False"),
    ("Sqrt[2]", "Sqrt[2]", "True"),
    ("BesselJ[0, 2]", "2 + 3 a", "BesselJ[0, 2] == 2 + 3 a"),
    ("BesselJ[0, 2]", "Infinity", "False"),
    ("BesselJ[0, 2]", "-Infinity", "False"),
    ("BesselJ[0, 2]", "Sqrt[I] Infinity", "False"),
    ("BesselJ[0, 2]", "a", "BesselJ[0, 2] == a"),
    ("BesselJ[0, 2]", '"a"', 'BesselJ[0, 2] == "a"'),
    ("BesselJ[0, 2]", "I", "False"),
    ("BesselJ[0, 2]", "0", "False"),
    ("BesselJ[0, 2]", "1 / 4", "False"),
    ("BesselJ[0, 2]", ".25", "False"),
    ("BesselJ[0, 2]", "Sqrt[2]", "False"),
    ("BesselJ[0, 2]", "BesselJ[0, 2]", "True"),
    ("3+2 I", "2 + 3 a", "3 + 2 I== 2 + 3 a"),
    ("3+2 I", "Infinity", "False"),
    ("3+2 I", "-Infinity", "False"),
    ("3+2 I", "Sqrt[I] Infinity", "False"),
    ("3+2 I", "a", "3 + 2 I== a"),
    ("3+2 I", '"a"', "False"),
    ("3+2 I", '"1 / 4"', "False"),
    ("3+2 I", "I", "False"),
    ("3+2 I", "0", "False"),
    ("3+2 I", "1 / 4", "False"),
    ("3+2 I", ".25", "False"),
    ("3+2 I", "Sqrt[2]", "False"),
    ("3+2 I", "BesselJ[0, 2]", "False"),
    ("3+2 I", "3+2 I", "True"),
    ("2.+ Pi I", "Infinity", "False"),
    ("2.+ Pi I", "-Infinity", "False"),
    ("2.+ Pi I", "Sqrt[I] Infinity", "False"),
    ("2.+ Pi I", '"a"', "False"),
    ("2.+ Pi I", '"1 / 4"', "False"),
    ("2.+ Pi I", "I", "False"),
    ("2.+ Pi I", "0", "False"),
    ("2.+ Pi I", "1 / 4", "False"),
    ("2.+ Pi I", ".25", "False"),
    ("2.+ Pi I", "Sqrt[2]", "False"),
    ("2.+ Pi I", "BesselJ[0, 2]", "False"),
    ("2.+ Pi I", "3+2 I", "False"),
    ("2.+ Pi I", "2.+ Pi I", "True"),
    ("3+I Pi", "2 + 3 a", "3 + I Pi == 2 + 3 a"),
    ("3+I Pi", "Infinity", "False"),
    ("3+I Pi", "-Infinity", "False"),
    ("3+I Pi", "Sqrt[I] Infinity", "False"),
    ("3+I Pi", "a", "3 + I Pi == a"),
    ("3+I Pi", "I", "False"),
    ("3+I Pi", "0", "False"),
    ("3+I Pi", "1 / 4", "False"),
    ("3+I Pi", ".25", "False"),
    ("3+I Pi", "Sqrt[2]", "False"),
    ("3+I Pi", "BesselJ[0, 2]", "False"),
    ("3+I Pi", "3+2 I", "False"),
    ("3+I Pi", "2.+ Pi I", "False"),
    ("3+I Pi", "3+I Pi", "True"),
    (
        'TestFunction["Tengo una vaca lechera"]',
        'TestFunction["Tengo una vaca lechera"]',
        "True",
    ),
    ("Compile[{x}, Sqrt[x]]", "Compile[{x}, Sqrt[x]]", "True"),
    ("Graphics[{Disk[{0,0},1]}]", "Graphics[{Disk[{0,0},1]}]", "True"),
    ("3+I Pi", '"a"', '3 + I Pi == "a"'),
    ("2.+ Pi I", "a", "2. + 3.14159265358979 I == a"),
    ("2.+ Pi I", "2 + 3 a", "2. + 3.14159265358979 I == 2 + 3 a"),
]


@pytest.mark.parametrize(
    ("str_lhs", "str_rhs", "str_expected"),
    tests1,
)
def test_cmp1_no_pass(str_lhs, str_rhs, str_expected):
    if str_lhs == str_rhs:
        expr = str_lhs + " == " + str_rhs
        check_evaluation(
            expr, str_expected, to_string_expr=True, to_string_expected=True
        )
    else:
        expr = str_lhs + " == " + str_rhs
        check_evaluation(
            expr, str_expected, to_string_expr=True, to_string_expected=True
        )
        # Checking commutativity
        str_expected_members = str_expected[1:-1].split(" == ")
        if len(str_expected_members) == 2:
            str_expected = (
                '"' + str_expected_members[1] + " == " + str_expected_members[0] + '"'
            )
        expr = str_rhs + " == " + str_lhs
        check_evaluation(
            expr, str_expected, to_string_expr=True, to_string_expected=True
        )


@pytest.mark.parametrize(
    ("str_lhs", "str_rhs", "str_expected"),
    tests2,
)
def test_cmp2_no_pass(str_lhs, str_rhs, str_expected):
    if str_lhs == str_rhs:
        expr = str_lhs + " == " + str_rhs
        check_evaluation(
            expr, str_expected, to_string_expr=False, to_string_expected=False
        )
    else:
        expr = str_lhs + " == " + str_rhs
        check_evaluation(
            expr, str_expected, to_string_expr=False, to_string_expected=False
        )
        # Checking commutativity
        str_expected_members = str_expected.split("==")
        if len(str_expected_members) == 2:
            str_expected = str_expected_members[1] + " == " + str_expected_members[0]
        expr = str_rhs + " == " + str_lhs
        check_evaluation(
            expr, str_expected, to_string_expr=False, to_string_expected=False
        )
