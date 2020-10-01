Begin["System`Convert`B64Dump`"]


Options[B64Import] = {
    "CharacterEncoding" :> $CharacterEncoding
};

B64Import[filename_, OptionsPattern[]] :=
    Module[{strm,data, grid},
	strm = OpenRead[filename];
	If[strm === $Failed, Return[$Failed]];
	data = Read[strm, Record, RecordSeparator->{}];
	Close[strm];
        { "Data" -> B64Decode[data] }
]

ImportExport`RegisterImport[
    "Base64",
    B64Import,
    (* Sources -> ImportExport`DefaultSources["Table"], *)
    FunctionChannels -> {"FileNames"},
    AvailableElements -> {"Data"},
    DefaultElement -> "Data",
    Options -> {
        "CharacterEncoding"
    }
]



End[]
