(* Self-contained test for testing mathicscript <file>

  Recursive GCD from https://mathematica.stackexchange.com/questions/156990/gcd-using-euclidean-algorithm
  Test harness is the same used in Gries Schnieder testing.
 *)

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

RecursiveGCD[a_, 0] := a;
RecursiveGCD[a_, b_] := RecursiveGCD[b, Mod[a, b]];

expect[6, RecursiveGCD[24, 18]]
expect[1, RecursiveGCD[3, 5]]

x = RandomInteger[{-100, 100}]
expect[x, RecursiveGCD[x, 0]]
expect[x, RecursiveGCD[2 x, x]]
expect[x, RecursiveGCD[x 2, x]]
(** uncomment to see a failure **)
(*** expect[3 x, RecursiveGCD[x 2, x]] ***)
Print["Total right: ", totalRight, ". Total wrong: ", totalWrong, ". Total tests: ", totalTests]
If[ Or[totalTests <= 0, totalTests != totalRight], Quit[1], Quit[0]]
