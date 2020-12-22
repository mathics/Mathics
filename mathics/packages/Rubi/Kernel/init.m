(* Mathematica Init file    *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(*The code here is intended to vastly improve Rubi's loading time.
* After loading all .m files, we store the complete state of Rubi as MX files which can be loaded in under a second.
* The MX file depends on the OS, the Mathematica version and the Steps/Elementary Function configuration of Rubi.
* For each of those configurations, we write the mx after the first loading, so that the next loading of the Rubi package
* is instantaneous.
*
* MX files can be removed by using the RubiClearMemoryImages[] function.
*
* If you intend to develop Rubi's rules and *always* want to load the code from the .m files, please load the Rubi.m
* file from the parent directory with Get. This will prevent the caching mechanism here.
* *)
Rubi`Private`$kernelDir = DirectoryName[System`$InputFileName];
Rubi`Private`$RubiVersionNumber = Version /. List @@ Get[FileNameJoin[{Rubi`Private`$kernelDir, "..", "PacletInfo.m"}]];
Rubi`Private`$MXFile = FileNameJoin[{
  Rubi`Private`$kernelDir,
  StringJoin[
    $SystemID,
    "-" <> ToString[$VersionNumber],
    "-Rubi-",
    Rubi`Private`$RubiVersionNumber,
    If[If[ValueQ[$LoadShowSteps], TrueQ@$LoadShowSteps, True], "-Steps", ""],
    If[If[ValueQ[$LoadElementaryFunctionRules], TrueQ@$LoadElementaryFunctionRules, True], "-ElementaryFunctions.mx", ".mx"]
  ]
}];

Quiet[
  Unprotect["Int"];
  Remove["Int"];
];

If[FileExistsQ[Rubi`Private`$MXFile],
  Quiet@ClearAll["Rubi`*"];
  Get[Rubi`Private`$MXFile],
  Get["Rubi`Rubi`"];
  DumpSave[Rubi`Private`$MXFile, "Rubi`"];
]
