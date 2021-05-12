(* ****************************************************************************

    by Brian Beckman. License the same as for mathics.

    Oct 2020

    These examples are adapted from Gries & Schnedier, "A Logical Approach
    to Discrete Math." The bibtex entry for this book follows:

    @Book{gries1993a,
     author = {Gries, David},
     title = {A Logical Approach to Discrete Math},
     publisher = {Springer New York},
     year = {1993},
     address = {New York, NY},
     isbn = {978-1-4757-3837-7}}

 *************************************************************************** *)

(* ***************************************************************************

    This file contains tooling so we can write 'expected' and 'actual' in our
    examples. You don't need to understand how this works. You just need to know
    how to use it, and you'll see how in the examples in GS1.m, GS2.m, GS3.m.

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
