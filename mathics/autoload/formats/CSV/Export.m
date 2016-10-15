(* CSV Exporter *)

Begin["System`Convert`TableDump`"]

Options[CSVExport] = {
    "CharacterEncoding" :> $CharacterEncoding,
    "FieldSeparators" -> ","
};

CSVExport[filename_String, data_, OptionsPattern[]]:=
    Module[{strm, char, wraplist, sep = OptionValue["FieldSeparators"]},
        strm = OpenWrite[filename, CharacterEncoding -> OptionValue["CharacterEncoding"]];
        If[strm === $Failed, Return[$Failed]];
        wraplist[x_] := If[Head[x] === List, x, {x}];
        char = Map[ToString, wraplist /@ wraplist[data], {2}];
        char = StringJoin[Riffle[Riffle[#, sep] & /@ char, "\n"]];
        WriteString[strm, char];
        Close[strm];
    ]

ImportExport`RegisterExport[
    "CSV",
    System`Convert`TableDump`CSVExport,
    FunctionChannels -> {"FileNames"},
    Options -> {"ByteOrderMark"},
    DefaultElement -> "Plaintext",
    BinaryFormat -> True,
    Options -> {
        "CharacterEncoding",
        "FieldSeparators"
    }
]


End[]
