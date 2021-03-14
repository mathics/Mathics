
Begin["System`Convert`B64Dump`"]


Options[B64Export] = {
    "CharacterEncoding" :> $CharacterEncoding
};

B64Export[filename_, expr_, OptionsPattern[]] :=
  Module[{strm, data},
    strm = OpenWrite[filename];
    If[strm === $Failed, Return[$Failed]];
    data = B64Encode[expr];
    WriteString[strm, data];
    Close[strm];
  ]

ImportExport`RegisterExport[
    "Base64",
	System`Convert`B64Dump`B64Export,
	FunctionChannels -> {"FileNames"},
	Options -> {"CharacterEncoding", "ByteOrderMark"},
	DefaultElement -> "Plaintext",
	BinaryFormat -> False
]



End[]
