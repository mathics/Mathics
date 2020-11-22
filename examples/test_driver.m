(* ****************************************************************************

    Test driver code for examples.

    by Brian Beckman. License the same as for mathics.

    Oct 2020

    This originally called GS0.m in symbolic_logic/gries_schneider
    but it is not application specific.

 *************************************************************************** *)

(* ***************************************************************************

    This file contains tooling so we can write 'expected' and 'actual' in our
    examples. You don't need to understand how this works. You just need to know
    how to use it, and you'll see how in the examples in GS1.m, GS2.m, GS3.m
    of symbolic_logic/gries_schneider.

 *************************************************************************** *)

ClearAll[expect, totalRight, totalWrong, totalTests];
SetAttributes[ expect, HoldAllComplete ];
totalRight = totalWrong = totalTests = 0;
expect[expected_, actual_] := (* <~~~ Here's the API *)
   Module[{evalActualOnce = actual,
           evalExpectedOnce = expected},
      totalTests += 1;
      Print[ {"Test[" <> ToString[totalTests] <> "]:=\n",
              HoldForm[actual],
              "\nexpected", HoldForm[expected],
              "\neval'd expected", evalExpectedOnce,
              "\neval'd actual  ", evalActualOnce,
              "\nright?",   evalExpectedOnce === evalActualOnce} ];
      Print[ "" ]; (* newline *)
      If[ evalExpectedOnce === evalActualOnce,
          totalRight += 1,
          totalWrong += 1 ];
      {"total right", totalRight, "total wrong", totalWrong}
      ];
