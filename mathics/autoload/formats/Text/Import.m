(* Text Importer *)

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

RegisterImport[
    "Text",
    {
        "Data" :> DataImport,
        "Lines" :> LinesImport,
        "Plaintext" :> PlaintextImport,
        "String" :> StringImport, 
        "Words" :> WordsImport,
        PlaintextImport
    },
    {},
	AvailableElements -> {"Data", "Lines", "Plaintext", "String", "Words"},
	BinaryFormat -> True,
	DefaultElement -> "Plaintext",
    FunctionChannels -> {"Streams"}
]

