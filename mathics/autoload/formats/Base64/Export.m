
Begin["System`Convert`B64Dump`"]


Options[B64Export] = {
    "CharacterEncoding" :> $CharacterEncoding
};

B64Export[strm_, expr_, OptionsPattern[]] :=
  Module[{data},
    If[strm === $Failed, Return[$Failed]];
    data = B64Encode[expr];
    WriteString[strm, data];
    Close[strm];
  ]

ImportExport`RegisterExport[
    "Base64",
	System`Convert`B64Dump`B64Export,
	FunctionChannels -> {"Streams"},
	Options -> {"CharacterEncoding", "ByteOrderMark"},
	DefaultElement -> "Plaintext",
	BinaryFormat -> True
]



End[]
