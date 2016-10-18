(* Text Importer *)

Begin["System`Convert`TextDump`"]


DataImport[stream_]:=
    Module[{data},
        data = ReadList[stream, String];
        {"Data" -> data}
    ]

LinesImport[stream_]:=
    Module[{lines},
        lines = ReadList[stream, String];
        {"Lines" -> lines}
    ]

PlaintextImport[stream_]:=
    Module[{plaintext},
        plaintext = Read[stream, Record, RecordSeparators -> {}];
        {"Plaintext" -> plaintext}
    ]

StringImport[stream_]:=
    Module[{string},
        string = Read[stream, Record, RecordSeparators -> {}];
        {"String" -> string}
    ]

WordsImport[stream_]:= 
    Module[{words},
        words = ReadList[stream, Word];
        {"Words" -> words}
    ]

ImportExport`RegisterImport[
    "Text",
    {
        "Data" :> System`Convert`TextDump`DataImport,
        "Lines" :> System`Convert`TextDump`LinesImport,
        "Plaintext" :> System`Convert`TextDump`PlaintextImport,
        "String" :> System`Convert`TextDump`StringImport,
        "Words" :> System`Convert`TextDump`WordsImport,
        System`Convert`TextDump`PlaintextImport
    },
    {},
	AvailableElements -> {"Data", "Lines", "Plaintext", "String", "Words"},
	BinaryFormat -> True,
	DefaultElement -> "Plaintext",
    FunctionChannels -> {"Streams"},
	Options -> {"CharacterEncoding"}
]


End[]
