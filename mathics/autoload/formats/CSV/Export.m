(* CSV Exporter *)

CSVExport[filename_String, data_, opts___]:=
  Module[{strm, char, wraplist},
    strm = OpenWrite[filename];
    If[strm === $Failed, Return[$Failed]];
    wraplist[x_] := If[Head[x] === List, x, {x}];
    char = Map[ToString, wraplist /@ wraplist[data], {2}];
    char = StringJoin[Riffle[Riffle[#, ","] & /@ char, "\n"]];
    WriteString[strm, char];
    Close[strm];
  ]

RegisterExport[
  "CSV",
  CSVExport,
  FunctionChannels -> {"FileNames"},
  Options -> {"ByteOrderMark"},
  DefaultElement -> "Plaintext",
  BinaryFormat -> True
]
