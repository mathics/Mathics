(* Text Exporter *)

Begin["System`Convert`Asy`"]

Options[AsyExport] = {
		      "CharacterEncoding" :> $CharacterEncoding, (*Not used by now...*)
};

AsyExport[strm_OutputStream, expr_, OptionsPattern[]]:=
  Module[{strout, wraplist, encoding=OptionValue["CharacterEncoding"]},
        If[strm === $Failed, Return[$Failed]];
	 strout = ToString[(If[Not[MemberQ[{Graphics, Graphics3D},Head[expr]]],Graphics[{Text[expr]}],expr]),TeXForm,
			  CharacterEncoding->encoding];
	strout = StringReplace[strout, {"\\begin{asy}"->"", "\\end{asy}"->""}];
	WriteString[strm, strout];
    ]


ImportExport`RegisterExport[
    "asy",
    System`Convert`Asy`AsyExport,
    FunctionChannels -> {"Streams"},
    Options -> {"ByteOrderMark"},
    DefaultElement -> "Plaintext",
    BinaryFormat -> False,
    Options -> {
        "CharacterEncoding"
    }
]

End[]
