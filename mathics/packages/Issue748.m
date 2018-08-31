ClearAll[parse]

(* Consider the following, which shows that {"p", "q"} matches the pattern
                                 {tok : Alternatives @@ {"p", "q", "r", "s"}, toks___}
                           *)

Print[MatchQ[{"p", "q"},
             {tok : Alternatives @@ {"p", "q", "r", "s"}, toks___}]]

(* However, the pattern-matching seems not to work with the same pattern in the
                                 when definining a DownValue for a symbol *)

parse[{tok : Alternatives @@ {"p", "q", "r", "s"}, toks___}] :=
Print[{"2", tok, {toks}}]

Print[DownValues[parse]]

(* Here is the proof that it doesn't work: the following does not reduce.
                                               However, it DOES reduce, as expected, in Mathematica. *)

Print[parse[{"p", "q"}]]

(* The following works in both mathics and in Mathematica *)

parse[{tok : "p" | "q" | "r" | "s", toks___}] :=
Print[{"2", tok, {toks}}]

Print[DownValues[parse]]

Print[parse[{"p", "q"}]]
