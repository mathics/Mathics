# -*- coding: utf-8 -*-
from .helper import check_evaluation

#  The following tests where generated automatically calling wolframscript -c
#  followed by a combination of expressions.
#  This is the code I used to generate them
#
#  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#  import subprocess
#  from time import sleep
#  exprss = ['2 + 3*a', 'Infinity', '-Infinity', 'Sqrt[I] Infinity', 'a', '"a"', '"1 / 4"', "I", "0", '1 / 4','.25',"Sqrt[2]", "BesselJ[0, 2]", "3+2 I", "2.+ Pi I", "3+I Pi", 'TestFunction["Tengo una vaca lechera"]', "Compile[{x}, Sqrt[x]]", "Graphics[{Disk[{0,0},1]}]"]
#  exprs = [ lhs + ' == '+ rhs for lhs in exprss for rhs in exprss]
#  tests = []
#  for expr in exprs:
#      result = subprocess.run(['wolframscript', '-c', expr], stdout=subprocess.PIPE)
#      sleep(1)
#      res = result.stdout.decode('utf8').strip()
#      if len(res)>0 and res[-1] == '\n':
#          res = res[:-2]
#      tests.append((expr, res))
#  tests
#


# In Mathics, (-1) ^ (1 / 4) Infinity (WMA) ->  System`Times[System`Power[I, 1/2], System`DirectedInfinity[1]]]


# This tests pass with the current implementation
@pytest.mark.parametrize(
    ("str_expr", "str_expected"),
    [
        ("-Infinity == Infinity", "False"),
        ('Sqrt[I] Infinity == "1 / 4"', '(-1) ^ (1 / 4) Infinity  == "1 / 4"'),
        ("Infinity == -Infinity", "False"),
        (".25 == a", "0.25 == a"),
        (".25 == 0", "False"),
        (".25 == 1 / 4", "True"),
        (".25 == 0.25", "True"),
        (".25 == Sqrt[2]", "False"),
        (".25 == BesselJ[0, 2]", "False"),
        (".25 == 2.+ I Pi", "False"),
        (".25 == 3+I Pi", "False"),
        (r'"\[Mu]"=="μ"', "True", ), 
        (
            "I == I",
            "True",
        ),
        (
            'TestFunction["Tengo una vaca lechera"] == Graphics[{Disk[{0,0},1]}]',
            'TestFunction["Tengo una vaca lechera"] == "-Graphics-"',
        ),
        ("Graphics[{Disk[{0,0},1]}] == 2 + 3 a", '"-Graphics-" == 2 + 3 a',),
        ("Graphics[{Disk[{0,0},1]}] == Infinity", '"-Graphics-" == Infinity',),
        ("Graphics[{Disk[{0,0},1]}] == -Infinity", '"-Graphics-" == -Infinity',),
        (
            "Graphics[{Disk[{0,0},1]}] == Sqrt[I] Infinity",
            '"-Graphics-" == (-1) ^ (1 / 4) Infinity',
        ),
        ("Graphics[{Disk[{0,0},1]}] == a", '"-Graphics-" == a'),
        ('Graphics[{Disk[{0,0},1]}] == "a"', '"-Graphics-" == a'),
        ('Graphics[{Disk[{0,0},1]}] == "1 / 4"', '"-Graphics- == 1 / 4"'),
        ("Graphics[{Disk[{0,0},1]}] == I", '"-Graphics- == I"'),
        ("Graphics[{Disk[{0,0},1]}] == 0", '"-Graphics- == 0"'),
        ("Graphics[{Disk[{0,0},1]}] == 1 / 4", '"-Graphics- == 1 / 4"'),
        ("Graphics[{Disk[{0,0},1]}] == 0.25", '"-Graphics- == 0.25"'),
        ("Graphics[{Disk[{0,0},1]}] == Sqrt[2]", '"-Graphics- == Sqrt[2]"',),
        (
            "Graphics[{Disk[{0,0},1]}] == BesselJ[0, 2]",
            '"-Graphics-" == BesselJ[0, 2]',
        ),
        ("Graphics[{Disk[{0,0},1]}] == 3+2 I", '"-Graphics- == 3 + 2 I"'),
        ("Graphics[{Disk[{0,0},1]}] == 2.+ I Pi", '"-Graphics- == 2. + 3.14159 I"',),
        ("Graphics[{Disk[{0,0},1]}] == 3+I Pi", '"-Graphics- == 3 + I Pi"',),
        (
            'Graphics[{Disk[{0,0},1]}] == TestFunction["Tengo una vaca lechera"]',
            '"-Graphics- == TestFunction[Tengo una vaca lechera]"',
        ),
        ("2 + 3 a == 3 + I Pi", "2 + 3 a == 3 + I Pi"),
        ("2 + 3 a == 2 + 3 a", "True",),
        ("2 + 3 a == Infinity", "2 + 3 a == Infinity",),
        ("2 + 3 a == -Infinity", "2 + 3 a == -Infinity",),
        ("2 + 3 a == Sqrt[I] Infinity", "2 + 3 a == (-1) ^ (1 / 4) Infinity",),
        ("2 + 3 a == a", "2 + 3 a == a"),
        ('2 + 3 a == "a"', "2 + 3 a == a"),
        ('2 + 3 a == "1 / 4"', "2 + 3 a == 1 / 4"),
        ("2 + 3 a == I", "2 + 3 a == I"),
        ("2 + 3 a == 0", "2 + 3 a == 0"),
        ("2 + 3 a == 1 / 4", "2 + 3 a == 1 / 4"),
        ("2 + 3 a == 0.25", "2 + 3 a == 0.25"),
        ("2 + 3 a == Sqrt[2]", "2 + 3 a == Sqrt[2]"),
        ("Graphics[{Disk[{0,0},1]}] == Graphics[{Disk[{0,0},1]}]", "True"),
        ("I == I", "True",),
        ("I == 0", "False",),
        ("I + 0 == 1 I - 0", "True",),
        ("I + 5 == I", "False",),
        ('"1 / 4" == Graphics[{Disk[{0,0},1]}]', '"1 / 4 == -Graphics-"'),
        ("I == 2 + 3 a", "I == 2 + 3 a"),
        ("I == a", "I == a"),
        ('I == "a"', "False"),
        ('I == "1 / 4"', "False"),
        ("I == I", "True"),
        ("I == 0", "False"),
        ("I == 1 / 4", "False"),
        ("I == 3+2 I", "False"),
        (
            'I == TestFunction["Tengo una vaca lechera"]',
            'I == TestFunction["Tengo una vaca lechera"]',
        ),
        ("I == Graphics[{Disk[{0,0},1]}]", '"I == -Graphics-"'),
        ("0 == 2 + 3 a", "0 == 2 + 3 a"),
        ("0 == a", "0 == a"),
        ('0 == "a"', "False"),
        ('0 == "1 / 4"', "False"),
        ("0 == I", "False"),
        ("0 == 0", "True"),
        ("0 == 1 / 4", "False"),
        ("0 == 0.25", "False"),
        ("0 == Sqrt[2]", "False"),
        ("0 == BesselJ[0, 2]", "False"),
        (
            '2 + 3 a == TestFunction["Tengo una vaca lechera"]',
            '2 + 3 a == TestFunction["Tengo una vaca lechera"]',
        ),
        (
            "I == Compile[{x}, Sqrt[x]]",
            'I == CompiledFunction[{x}, Sqrt[x], "-PythonizedCode-"]',
        ),
        ("0 == Infinity", "False"),
        ("0 == -Infinity", "False"),
        ("0 == 3+2 I", "False"),
        ("2 + 3 a == Sqrt[I] Infinity", "2 + 3 a == (-1) ^ (1 / 4) Infinity"),
        (
            'Infinity == TestFunction["Tengo una vaca lechera"]',
            'Infinity == TestFunction["Tengo una vaca lechera"]',
        ),
        ("Infinity == Graphics[{Disk[{0,0},1]}]", 'Infinity == "-Graphics-"',),
        ("2 + 3 a == 2.+ I Pi", "2 + 3 a == 2. + 3.14159 I"),
        ("2 + 3 a == BesselJ[0, 2]", "2 + 3 a == BesselJ[0, 2]"),
        ("2 + 3 a == 3 + 2 I", "2 + 3 a == 3 + 2 I"),
        ("2 + 3 a == Graphics[{Disk[{0,0},1]}]", '2 + 3 a == "-Graphics-"',),
        ("Infinity == 2 + 3 a", "Infinity == 2 + 3 a"),
        ("Infinity == Infinity", "True"),
        # For WMA, the next one evaluated to False
        ("Infinity == Sqrt[I] Infinity", "Infinity == Sqrt[I] Infinity"),
        ("Infinity == a", "Infinity == a"),
        ('Infinity == "a"', "Infinity == a"),
        ("Infinity == 0", "False"),
        ("Infinity == 1 / 4", "False"),
        ("Infinity == 0.25", "False"),
        ("Infinity == Sqrt[2]", "False"),
        ("Infinity == BesselJ[0, 2]", "False"),
        ("Infinity == 2.+ I Pi", "False"),
        ("Infinity == 3+I Pi", "False"),
        ("-Infinity == -Infinity", "True"),
        ("-Infinity == 3+I Pi", "False"),
        (
            '-Infinity == TestFunction["Tengo una vaca lechera"]',
            '-Infinity == TestFunction["Tengo una vaca lechera"]',
        ),
        ("-Infinity == Graphics[{Disk[{0,0},1]}]", '-Infinity == "-Graphics-"',),
        ("Sqrt[I] Infinity == 2 + 3 a", "(-1) ^ (1 / 4) Infinity  == 2 + 3 a",),
        ("Sqrt[I] Infinity == a", "(-1) ^ (1 / 4) Infinity  == a"),
        ('Sqrt[I] Infinity == "a"', '(-1) ^ (1 / 4) Infinity  == "a"'),
        ("Sqrt[I] Infinity == Sqrt[I] Infinity", "True"),
        (
            'Sqrt[I] Infinity == TestFunction["Tengo una vaca lechera"]',
            '(-1) ^ (1 / 4) Infinity  == TestFunction["Tengo una vaca lechera"]',
        ),
        (
            "Sqrt[I] Infinity == Graphics[{Disk[{0,0},1]}]",
            '(-1) ^ (1 / 4) Infinity == "-Graphics-"',
        ),
        ("a == 2 + 3 a", "a == 2 + 3 a"),
        ("a == Infinity", "a == Infinity"),
        ("a == -Infinity", "a == -Infinity"),
        ("a == Sqrt[I] Infinity", "a == (-1) ^ (1 / 4) Infinity"),
        ("a == a", "True"),
        ('a == "a"', 'a == "a"'),
        ('a == "1 / 4"', 'a == "1 / 4"'),
        ("a == I", "a == I"),
        ("a == 0", "a == 0"),
        ("a == 1 / 4", "a == 1 / 4"),
        ("a == 0.25", "a == 0.25"),
        ("a == Sqrt[2]", "a == Sqrt[2]"),
        ("a == BesselJ[0, 2]", "a == BesselJ[0, 2]"),
        ("a == 3+2 I", "a == 3 + 2*I"),
        ("a == 2.+ I Pi", "a == 2. + 3.14159 I"),
        ("a == 3+I Pi", "a == 3 + I Pi"),
        (
            'a == TestFunction["Tengo una vaca lechera"]',
            'a == TestFunction["Tengo una vaca lechera"]',
        ),
        ("a == Graphics[{Disk[{0,0},1]}]", 'a == "-Graphics-"'),
        ('"a" == 2 + 3 a', '"a" == 2 + 3 a'),
        ('"a" == Infinity', '"a" == Infinity'),
        ('"a" == -Infinity', '"a" == -Infinity'),
        ('"a" == Sqrt[I] Infinity', '"a" == (-1) ^ (1 / 4) Infinity'),
        ('"a" == a', '"a" == a'),
        ('"a" == "a"', "True"),
        ('"a" == "1 / 4"', "False"),
        ('"a" == I', "False"),
        ('"a" == 0', "False"),
        ('"a" == 1 / 4', "False"),
        ('"a" == Sqrt[2]', '"a" == Sqrt[2]'),
        ('"a" == BesselJ[0, 2]', '"a" == BesselJ[0, 2]'),
        ('"a" == 3+2 I', "False"),
        ('"a" == 3+I Pi', '"a" == 3 + I Pi'),
        (
            '"a" == TestFunction["Tengo una vaca lechera"]',
            '"a" == TestFunction["Tengo una vaca lechera"]',
        ),
        ('"a" == Graphics[{Disk[{0,0},1]}]', '"a == -Graphics-"'),
        ('"1 / 4" == 2 + 3 a', '"1 / 4" == 2 + 3 a'),
        ('"1 / 4" == Infinity', '"1 / 4" == Infinity'),
        ('"1 / 4" == -Infinity', '"1 / 4" == -Infinity'),
        ('"1 / 4" == Sqrt[I] Infinity', '"1 / 4" == (-1) ^ (1 / 4) Infinity '),
        ('"1 / 4" == a', '"1 / 4" == a'),
        ('"1 / 4" == "a"', "False"),
        ('"1 / 4" == "1 / 4"', "True"),
        ('"1 / 4" == I', "False"),
        ('"1 / 4" == 0', "False"),
        ('"1 / 4" == 1 / 4', "False"),
        ('"1 / 4" == Sqrt[2]', '"1 / 4" == Sqrt[2]'),
        ('"1 / 4" == BesselJ[0, 2]', '"1 / 4" == BesselJ[0, 2]'),
        ('"1 / 4" == 3+2 I', "False"),
        ('"1 / 4" == 3+I Pi', '"1 / 4" == 3 + I Pi'),
        (
            '"1 / 4" == TestFunction["Tengo una vaca lechera"]',
            '"1 / 4" == TestFunction["Tengo una vaca lechera"]',
        ),
        ("0 == Graphics[{Disk[{0,0},1]}]", '"0 == -Graphics-"'),
        ("1 / 4 == 2 + 3 a", "1 / 4 == 2 + 3 a"),
        ("1 / 4 == Infinity", "False"),
        ("1 / 4 == -Infinity", "False"),
        ("1 / 4 == a", "1 / 4 == a"),
        ('1 / 4 == "a"', "False"),
        ('1 / 4 == "1 / 4"', "False"),
        ("1 / 4 == I", "False"),
        ("1 / 4 == 0", "False"),
        ("1 / 4 == 1 / 4", "True"),
        ("1 / 4 == 0.25", "True"),
        ("1 / 4 == Sqrt[2]", "False"),
        ("1 / 4 == BesselJ[0, 2]", "False"),
        ("1 / 4 == 3+2 I", "False"),
        ("1 / 4 == Graphics[{Disk[{0,0},1]}]", '"1 / 4 == -Graphics-"'),
        (".25 == 2 + 3 a", "0.25 == 2 + 3 a"),
        (".25 == Infinity", "False"),
        (".25 == -Infinity", "False"),
        (
            "3+2 I == Compile[{x}, Sqrt[x]]",
            '3 + 2 I == CompiledFunction[{x}, Sqrt[x], "-PythonizedCode-"]',
        ),
        ("-Infinity == a", "-Infinity == a"),
        ('-Infinity == "a"', '-Infinity == "a"'),
        ("-Infinity == 0", "False"),
        ("-Infinity == 1 / 4", "False"),
        ("-Infinity == 0.25", "False"),
        ("-Infinity == Sqrt[2]", "False"),
        ("-Infinity == BesselJ[0, 2]", "False"),
        ("-Infinity == 2.+ I Pi", "False"),
        (
            "Compile[{x}, Sqrt[x]] == 3+2 I",
            'CompiledFunction[{x}, Sqrt[x], "-PythonizedCode-"] == 3 + 2 I',
        ),
        (
            "Compile[{x}, Sqrt[x]] == I",
            'CompiledFunction[{x}, Sqrt[x], "-PythonizedCode-"] == I',
        ),
        ("2.+ I Pi == Sqrt[I] Infinity", "False"),
        ('2.+ I Pi == "a"', "False"),
        ('2.+ I Pi == "1 / 4"', "False"),
    ],
)
def test_cmp1_pass(str_expr, str_expected):
    check_evaluation(str_expr, str_expected)


@pytest.mark.parametrize(
    ("str_expr", "str_expected"),
    [
        ("-Infinity == 3+2 I", "False"),
        (
            "Compile[{x}, Sqrt[x]] == BesselJ[0, 2]",
            'CompiledFunction[{x}, Sqrt[x], "-PythonizedCode-"] == BesselJ[0, 2]',
        ),
        (
            "Compile[{x}, Sqrt[x]] == 2.+ I Pi",
            'CompiledFunction[{x}, Sqrt[x], "-PythonizedCode-"] == 2. + 3.14159 I',
        ),
        (
            "Compile[{x}, Sqrt[x]] == 3 + I Pi",
            'CompiledFunction[{x}, Sqrt[x], "-PythonizedCode-"] == 3 + I Pi',
        ),
        (
            'Compile[{x}, Sqrt[x]] == TestFunction["Tengo una vaca lechera"]',
            'CompiledFunction[{x}, Sqrt[x], "-PythonizedCode-"] == TestFunction["Tengo una vaca lechera"]',
        ),
        (
            "Sqrt[I] Infinity == Compile[{x}, Sqrt[x]]",
            '(-1) ^ (1 / 4) Infinity == CompiledFunction[{x}, Sqrt[x], "-PythonizedCode-"]',
        ),
        ("Sqrt[I] Infinity == 0.25", "False"),
        ("Sqrt[I] Infinity == Infinity", "False"),
        ("I == 3+I Pi", "False"),
        (
            '0 == TestFunction["Tengo una vaca lechera"]',
            '0 == TestFunction["Tengo una vaca lechera"]',
        ),
        ("0 == Sqrt[I] Infinity", "False"),
        ("0 == 2.+ I Pi", "False"),
        ("0 == 3+I Pi", "False"),
        (".25 == 3+2 I", "False"),
        (
            ".25 == Compile[{x}, Sqrt[x]]",
            '.25 == CompiledFunction[{x}, Sqrt[x], "-PythonizedCode-"]',
        ),
        (
            "Sqrt[2] == Compile[{x}, Sqrt[x]]",
            'Sqrt[2] == CompiledFunction[{x}, Sqrt[x], "-PythonizedCode-"]',
        ),
        (
            "BesselJ[0, 2] == Compile[{x}, Sqrt[x]]",
            'BesselJ[0, 2] == CompiledFunction[{x}, Sqrt[x], "-PythonizedCode-"]',
        ),
        (
            "2.+ I Pi == Compile[{x}, Sqrt[x]]",
            '2. + 3.14159 I == CompiledFunction[{x}, Sqrt[x], "-PythonizedCode-"]',
        ),
        (
            "3+I Pi == Compile[{x}, Sqrt[x]]",
            '3 + I Pi == CompiledFunction[{x}, Sqrt[x], "-PythonizedCode-"]',
        ),
        ("Sqrt[I] Infinity == -Infinity", "False"),
        ('.25 == "a"', "False"),
        ('.25 == "1 / 4"', "False"),
        (".25 == I", "False"),
        (".25 == Sqrt[I] Infinity", "False"),
        (
            "1 / 4 == Compile[{x}, Sqrt[x]]",
            '1 / 4 == CompiledFunction[{x}, Sqrt[x], "-PythonizedCode-"]',
        ),
        (
            '"a" == Compile[{x}, Sqrt[x]]',
            '"a" == CompiledFunction[{x}, Sqrt[x], "-PythonizedCode-"]',
        ),
        (
            "a == Compile[{x}, Sqrt[x]]",
            'a == CompiledFunction[{x}, Sqrt[x], "-PythonizedCode-"]',
        ),
        ("Infinity == 3+2 I", "False"),
        ("-Infinity == I", "False"),
        (
            "Infinity == Compile[{x}, Sqrt[x]]",
            'Infinity == CompiledFunction[{x}, Sqrt[x], "-PythonizedCode-"]',
        ),
        ("Sqrt[I] Infinity == I", "False"),
        ("Sqrt[I] Infinity == 0", "False"),
        ("Sqrt[I] Infinity == 1 / 4", "False"),
        ("Sqrt[I] Infinity == 3+2 I", "False"),
        ('"a" == 0.25', "False"),
        ('"a" == 2.+ I Pi', "False"),
        ('"1 / 4" == 0.25', "False"),
        ('"1 / 4" == 2.+ I Pi', "False"),
        ("I == Infinity", "False"),
        ("I == -Infinity", "False"),
        ("I == Sqrt[I] Infinity", "False"),
        ("I == 0.25", "False"),
        ("I == Sqrt[2]", "False"),
        ("I == BesselJ[0, 2]", "False"),
        ("I == 2.+ I Pi", "False"),
        (
            '"1 / 4" == Compile[{x}, Sqrt[x]]',
            '"1 / 4" == CompiledFunction[{x}, Sqrt[x], "-PythonizedCode-"]',
        ),
        (
            "0 == Compile[{x}, Sqrt[x]]",
            '0 == CompiledFunction[{x}, Sqrt[x], "-PythonizedCode-"]',
        ),
        (
            "Graphics[{Disk[{0,0},1]}] == Compile[{x}, Sqrt[x]]",
            '"-Graphics-" == CompiledFunction[{x}, Sqrt[x], "-PythonizedCode-"]',
        ),
        ("Compile[{x}, Sqrt[x]] == Compile[{x}, Sqrt[x]]", "True"),
        (
            "Compile[{x}, Sqrt[x]] == Graphics[{Disk[{0,0},1]}]",
            'CompiledFunction[{x}, Sqrt[x], "-PythonizedCode-"] == "-Graphics-"',
        ),
        ("1 / 4 == Sqrt[I] Infinity", "False"),
        ("1 / 4 == 2.+ I Pi", "False"),
        ("1 / 4 == 3+I Pi", "False"),
        (
            '1 / 4 == TestFunction["Tengo una vaca lechera"]',
            '1 / 4 == TestFunction["Tengo una vaca lechera"]',
        ),
        ("-Infinity == Sqrt[I] Infinity", "False"),
        (
            'TestFunction["Tengo una vaca lechera"] == "1 / 4"',
            'TestFunction["Tengo una vaca lechera"] == 1 / 4',
        ),
        (
            'TestFunction["Tengo una vaca lechera"] == 0',
            'TestFunction["Tengo una vaca lechera"] == 0',
        ),
        (
            'TestFunction["Tengo una vaca lechera"] == 1 / 4',
            'TestFunction["Tengo una vaca lechera"] == 1 / 4',
        ),
        (
            'TestFunction["Tengo una vaca lechera"] == Compile[{x}, Sqrt[x]]',
            'TestFunction["Tengo una vaca lechera"] == CompiledFunction[{x}, Sqrt[x], "-PythonizedCode-"]',
        ),
        (
            "Compile[{x}, Sqrt[x]] == 2 + 3 a",
            'CompiledFunction[{x}, Sqrt[x], "-PythonizedCode-"] == 2 + 3 a',
        ),
        (
            "Compile[{x}, Sqrt[x]] == Infinity",
            'CompiledFunction[{x}, Sqrt[x], "-PythonizedCode-"] == Infinity',
        ),
        (
            "Compile[{x}, Sqrt[x]] == -Infinity",
            'CompiledFunction[{x}, Sqrt[x], "-PythonizedCode-"] == -Infinity',
        ),
        (
            "Compile[{x}, Sqrt[x]] == Sqrt[I] Infinity",
            'CompiledFunction[{x}, Sqrt[x], "-PythonizedCode-"] == (-1) ^ (1 / 4) Infinity',
        ),
        (
            "Compile[{x}, Sqrt[x]] == a",
            'CompiledFunction[{x}, Sqrt[x], "-PythonizedCode-"] == a',
        ),
        (
            'Compile[{x}, Sqrt[x]] == "a"',
            'CompiledFunction[{x}, Sqrt[x], "-PythonizedCode-"] == a',
        ),
        (
            'Compile[{x}, Sqrt[x]] == "1 / 4"',
            'CompiledFunction[{x}, Sqrt[x], "-PythonizedCode-"] == 1 / 4',
        ),
        (
            "Compile[{x}, Sqrt[x]] == 0",
            'CompiledFunction[{x}, Sqrt[x], "-PythonizedCode-"] == 0',
        ),
        (
            "Compile[{x}, Sqrt[x]] == 0",
            'CompiledFunction[{x}, Sqrt[x], "-PythonizedCode-"] == 0',
        ),
        (
            "Compile[{x}, Sqrt[x]] == 0.25",
            'CompiledFunction[{x}, Sqrt[x], "-PythonizedCode-"] == 0.25',
        ),
        (
            "Compile[{x}, Sqrt[x]] == Sqrt[2]",
            'CompiledFunction[{x}, Sqrt[x], "-PythonizedCode-"] == Sqrt[2]',
        ),
    ],
)
def test_cmp1_no_pass(str_expr, str_expected):
    check_evaluation(str_expr, str_expected)


@pytest.mark.parametrize(
    ("str_expr", "str_expected"),
    [
        ("2.+ I Pi == I", "False"),
        ("2.+ I Pi == 0", "False"),
        ("2.+ I Pi == 1 / 4", "False"),
        ("2.+ I Pi == 3+2 I", "False"),
        ("BesselJ[0, 2] == I", "False"),
        ("BesselJ[0, 2] == 3+2 I", "False"),
        ("Sqrt[2] == 3+2 I", "False"),
        ("Sqrt[2] == I", "False"),
        ("3+2 I == Infinity", "False"),
        ("3+2 I == -Infinity", "False"),
        ("3+2 I == Sqrt[I] Infinity", "False"),
        ("3+2 I == 0.25", "False"),
        ("3+2 I == Sqrt[2]", "False"),
        ("3+2 I == BesselJ[0, 2]", "False"),
        ("3+2 I == 2.+ I Pi", "False"),
        ("3+2 I == 3+I Pi", "False"),
        ("3+I Pi == I", "False"),
        ("3+I Pi == 0", "False"),
        ("3+I Pi == 1 / 4", "False"),
        ("3+I Pi == 3+2 I", "False"),
        (
            "I + 5 == I",
            "False",
        ),
        ("Sqrt[I] Infinity == 3+I Pi", "False"),
        ("Sqrt[I] Infinity == Sqrt[2]", "False"),
        ("Sqrt[I] Infinity == BesselJ[0, 2]", "False"),
        ("Sqrt[I] Infinity == 2.+ I Pi", "False"),
        ('Infinity == "1 / 4"', '"Infinity == 1 / 4"'),
        ("-Infinity == Compile[{x}, Sqrt[x]]", "-Infinity == Compile[{x}, Sqrt[x]]"),
        ('-Infinity == "1 / 4"', '"-Infinity == 1 / 4"'),
        ("BesselJ[0, 2] == Sqrt[I] Infinity", "False"),
        ("Sqrt[2] == Sqrt[I] Infinity", "False"),
        ("3+I Pi == Sqrt[I] Infinity", "False"),
    ],
)
def test_cmp2_no_pass(str_expr, str_expected):
    check_evaluation(str_expr, str_expected)
