(* Text Importer *)

DataImport[filename_String]:=
    Module[{stream, data},
        stream = OpenRead[filename];
        data = ReadList[stream, String];
        Close[stream];
        {"Data" -> data}
    ]

LinesImport[filename_String]:=
    Module[{stream, lines},
        stream = OpenRead[filename];
        lines = ReadList[stream, String];
        Close[stream];
        {"Lines" -> lines}
    ]

PlaintextImport[filename_String]:=
    Module[{stream, plaintext},
        stream = OpenRead[filename];
        plaintext = Read[stream, Record, RecordSeparators -> {}];
        Close[stream];
        {"Plaintext" -> plaintext}
    ]

StringImport[filename_String]:=
    Module[{stream, string},
        stream = OpenRead[filename];
        string = Read[stream, Record, RecordSeparators -> {}];
        Close[stream];
        {"String" -> string}
    ]

WordsImport[filename_String]:= 
    Module[{stream, words},
        stream = OpenRead[filename];
        words = ReadList[stream, Word];
        Close[stream];
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
    } (*,
    "FunctionChannels" -> {"Streams"},
	"AvailableElements" -> {"Data", "Lines", "Plaintext", "String", "Words"},
	"DefaultElement" -> "Plaintext",
	"BinaryFormat" -> True *)
]

Import["ExampleData/ExampleData.txt", "Words"]
