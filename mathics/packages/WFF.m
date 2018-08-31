(* ****************************************************************************
   Digression into WFF-N-PROOF


   From the famous game "Wff-n-Proof:
   https://www.amazon.com/Wffn-Proof-Book-Layman-Allen/dp/B0007FIH94


   DEFINITIONS

    A grammar is a function from non-terminal symbols to productions.

        Example: the grammar P is a function defined on a set of five
        non-terminals, "Start", "Binary", "Unary", "Proposition", and "Start".

    A production is an ordered sequence of alternatives.

        Example: The production "P[Wff]", the value of the function P at the
        non-terminal Wff, is a sequence of three alternatives "{{Proposition},
        {Unary}, {Binary}}". We model a sequence with a list in curly braces.

    An alternative is an ordered sequence of terms.

        Example: The alternative "{Proposition}" is a sequence of one term.

    A term is either a terminal symbol or a non-terminal symbol.

        Example: The term "Proposition" is a non-terminal symbol.

    A terminal symbol is a literal token like "p" or "q".

        Example: The value "P[Proposition]" of our function P at the
        non-terminal "Proposition", is the sequence of terminals "{{p}, {q},
        {r}, {s}}"

    A non-terminal symbol recurses back into the grammar.

        Example: The alternative {"C", Wff, Wff} is a sequence of three terms:
        one terminal, "C", and two non-terminals, Wff.

    An instance (utterance) is a string consisting entirely of terminal symbols.

    A context-free grammar is one whose productions may be applied anywhere in
    an instance without regard to preceding or following input. Such grammars
    are the only ones of interest, here.

    The start symbol is the special, distinguished name Start. This name is not
    available for user-defined symbols.

 *************************************************************************** *)
<<"GS0.m"


ClearAll[P, Wff, Proposition, Unary, Binary, T,
           nonTerminalsFromGrammar, terminalsFromGrammar];



(* We call our grammar "P" for "Propositional Calculus". We "Set" five values
   for P because our grammar has five non-terminals. "Set" is mathics's word for
   "assign," more precisely, to "define" the value of a pattern. If the pattern
   is the pattern constant "Wff", then the expression

       P[Wff] = {{Proposition}, {Unary}, {Binary}};

   meanss "define the value of the pattern P[Wff] to be {{Proposition}, {Unary},
   {Binary}}." In turn, this means "whenever you see 'P[Wff]', please replace it
   with '{{Proposition}, {Unary}, {Binary}}'." *)



P[Wff] = {{Proposition}, {Unary}, {Binary}};
P[Proposition] = {{"p"}, {"q"}, {"r"}, {"s"}};
P[Unary] = {{"N", Wff}};
P[Binary] = {{"C", Wff, Wff},
             {"A", Wff, Wff},
             {"K", Wff, Wff},
             {"E", Wff, Wff}};
P[Start] = P[Wff];



(* Non-Terminal Symbols *)

(* "DownValues" sniffs the lookup keys out of a bunch of definitions, retrieving
    the domain of the grammar function. I map #[[1, 1, 1]]& down the list of
    downvalues because that's all I want from them. Type "DownValues[P]" if
    you're curious about every detail in there. I just want to fish out the
    non-terminals. *)

ClearAll[nonTerminalsFromGrammar];
nonTerminalsFromGrammar[ps_] := #[[1, 1, 1]] & /@ DownValues[ps]



expect[
        {Start, Binary, Unary, Proposition, Wff}
      ,
        nonTerminalsFromGrammar @ P
]



(* Terminal Symbols *)

(* The terminals in a grammar is the complement of the non-terminals against the
   set of all symbols.

   To see how I got the list of all symbols, Type DownValues[P] in the console
   to see the list of everything, then figure out why I mapped #[[2]]& down the
   list, then why I flattened it, then why I fed it to "Union". *)

ClearAll[allSymbols];
allSymbols[ps_] := Union @ Flatten[#[[2]] & /@ DownValues[ps]]

ClearAll[terminalsFromGrammar];
terminalsFromGrammar[ps_] :=
    Complement[
        allSymbols @ ps,
        nonTerminalsFromGrammar @ ps]



(* T = set of terminals: Global Variable *)

expect [(T = terminalsFromGrammar @ P)
        ,
        {"A", "C", "E", "K", "N", "p", "q", "r", "s"}
]



ClearAll[injectGenerationProbabilities];
injectGenerationProbabilities[grammar_, probsFromAlternatives_] :=
    Module[{newTable, nonTerminals = nonTerminalsFromGrammar@grammar},
       Scan[Function[nonTerminal,
           Module[{
               alternatives = grammar[nonTerminal],
               probabilities = probsFromAlternatives[grammar[nonTerminal]]},
                   newTable[nonTerminal] =
                   MapThread[
                       Function[{prob, alt},
                                {"probability" -> prob,
                                 "alternative" -> alt}],
                       {probabilities, alternatives} ]
           ]],
            nonTerminals];
       newTable]

ClearAll[equiProbabilities];
equiProbabilities[list_List] :=
    Module[{l = Length@list}, Table[N[1/l], {l}]]

ClearAll[iP]; (iP = injectGenerationProbabilities[P, equiProbabilities]);

DownValues[Evaluate[iP]]



(*    __                       ___
 ____/ /  ___  ___  ___ ___   / _/______  __ _
/ __/ _ \/ _ \/ _ \(_-</ -_) / _/ __/ _ \/  ' \
\__/_//_/\___/\___/___/\__/ /_//_/  \___/_/_/_/
       ____                    __  _
 ___ _/ / /____ _______  ___ _/ /_(_)  _____ ___
/ _ `/ / __/ -_) __/ _ \/ _ `/ __/ / |/ / -_|_-<
\_,_/_/\__/\__/_/ /_//_/\_,_/\__/_/|___/\__/___/
 *)

ClearAll[chooseFromAlternatives];



(* If there is only one alternative, choose it: *)

chooseFromAlternatives[{probabilizedAlternative_}, dieRoll_] :=
  "alternative" /. probabilizedAlternative;



(* If there are many, pick the first if its cumulative probability is greater
   than or equal to the dieRoll. Track cumulative probability by decrementing
   dieRoll by each non-cumulative probability as we recurse the sequence of
   alternatives. *)

chooseFromAlternatives[{probabilizedAlternative_, rest___}, dieRoll_] :=
    Module[{p = "probability" /. probabilizedAlternative},
           If[dieRoll < p,
              (* then *)"alternative" /. probabilizedAlternative,
              (* else *)chooseFromAlternatives[{rest}, dieRoll - p]]];

chooseFromAlternatives[badArgs___] :=
  Throw[{"CHOOSE:BADARGS: ", {badArgs}}];


(*    __        _                                    _
 ____/ /  ___ _(_)__    _____ __ ___  ___ ____  ___ (_)__  ___
/ __/ _ \/ _ `/ / _ \  / -_) \ // _ \/ _ `/ _ \(_-</ / _ \/ _ \
\__/_//_/\_,_/_/_//_/  \__/_\_\/ .__/\_,_/_//_/___/_/\___/_//_/
                              /_/
 *)

(* groundTerm is the term to force when the recursion limit is exceeded. Cdr
   down the sequence of terms in the production, randomly choosing a branch to
   explore. *)

ClearAll[chainExpansion];



(* Case: exhausted the production: *)

chainExpansion[iP_, groundTerm_, T_, production : {},
               sentenceAccumulator_, i_, iLim_] := sentenceAccumulator;



(* Case: we have a term to consider in the production and we're under the*)
(*recursion limit ( /; (i < iLim) ): *)

chainExpansion[
    iP_, groundTerm_, Terminals_, production : {term_, rest___},
    sentenceAccumulator_, i_, iLim_    /;    (i < iLim)] :=
    (If[MemberQ[Terminals, term],
        (* If the term is a terminal symbol (if it's in the set "T"), then
           append it to the sentence being accumulated and recurse on the rest
           of the production. *)
        chainExpansion[
            iP, groundTerm, Terminals, {rest},
            Append[sentenceAccumulator, term], i + 1, iLim],
        (* Otherwise, the term is a non-terminal symbol. Choose, non-uniformly,
           randomly, from the alternatives of the non-terminal and recurse. *)

        Module[{randomAlt = chooseFromAlternatives[iP[term], RandomReal[]]},
               chainExpansion[
                   iP, groundTerm, Terminals, {rest},
                   Join[sentenceAccumulator,
                       (* start a new sentence accumulator in this recursion: *)
                        chainExpansion[
                            iP, groundTerm, Terminals, randomAlt, {},
                            i + 1, iLim]],
                   i + 1, iLim]]])



(* Case: exceeded the recursion limit: *)

chainExpansion[iP_, groundTerm_, T_, production_, sentence_, i_, iLim_] :=
    chooseFromAlternatives[iP[groundTerm], RandomReal[]];



ClearAll[generateSentence];
generateSentence[iP_, groundTerm_, T_, recursionLimit_: 100] :=
    chainExpansion[iP, groundTerm, T, {Start}, {}, 0, recursionLimit]



SeedRandom[44];
expect [
    ToString /@
    {C, A, N, N, N, A, N, s, E, N, N, N, r, C, N, p, q, E, A, N, C, K, K,
     E, C, N, p, A, C, q, q, r, s, s, C, K, q, N, K, K, C, N, p, s, q, N, N, r,
     r, N, q, C, q, A, p, q, r, N, s}
  ,
    generateSentence[iP, Proposition, T, 500]
]



expect [
    ToString /@
    {A, C, N, q, A, p, C, N, A, N, K, p, r, K, E, N, N, C, N, r, N,
     C, A, C, p, A, A, N, p, N, K, r, K, A, E, p, N, N, q, N, C, E, N, N, N, C,
     N, K, N, p, s, K, s, N, r, r, N, K, s, r, q, N, C, N, K, N, A, p, N, E, s,
     A, N, N, N, s, N, C, N, C, q, p, s, N, q, p, q, N, A, N, q, N, r, N, s, p,
     N, E, p, A, q, N, A, r, K, q, N, q, N, s}
  ,
    Block[{$RecursionLimit=500},
          SeedRandom[56];
          generateSentence[iP, Proposition, T, 500]]
]



ClearAll[expressionStringFromSentenceRules, expressionStringFromSentence];
expressionStringFromSentenceRules = {};
expressionStringFromSentence[sentence_] :=
  sentence /. expressionStringFromSentenceRules // StringJoin;

(*                  __                  __  __
  _______ ____  ___/ /__  __ _    __ __/ /_/ /____ _______ ____  _______
 / __/ _ `/ _ \/ _  / _ \/  ' \  / // / __/ __/ -_) __/ _ `/ _ \/ __/ -_)
/_/  \_,_/_//_/\_,_/\___/_/_/_/  \_,_/\__/\__/\__/_/  \_,_/_//_/\__/\__/
 *)

randomUtterance[len_: 500] :=
    expressionStringFromSentence @
        generateSentence[iP, Proposition, T, len]



(* __                __                __       __
  / /  ___ ____  ___/ /__________  ___/ /__ ___/ / ___  ___ ________ ___ ____
 / _ \/ _ `/ _ \/ _  /___/ __/ _ \/ _  / -_) _  / / _ \/ _ `/ __(_-</ -_) __/
/_//_/\_,_/_//_/\_,_/    \__/\___/\_,_/\__/\_,_/ / .__/\_,_/_/ /___/\__/_/
                                                /_/
 *)

(* Given an utterance, build a tree of the non-terminals that generate it. *)

(* Strategy: top-down, recursive descent. There is one "pattern" for each
   non-terminal. *)
                                                                                             (* First, tokenize the utterance. In our example, this is trivial since each
   term is a single character. *)

ClearAll[tokens, parse, remToks, partTree];
tokens[utterance_] := Characters[utterance];
remToks[list_List] := First@list;
partTree[list_List] := First@Rest@list;

(* ClearAll[parse] *)

(* parse[{tok : Alternatives @@ {"p", "q", "r", "s"}, toks___}] := *)
(*     Print[{"2", tok, {toks}}] *)

(* Print[MatchQ[{"p", "q"}, *)
(*              {tok : Alternatives @@ {"p", "q", "r", "s"}, toks___}]] *)

(* parse[{"p", "q"}] *)

(* TODO: See Issue #748: https://github.com/mathics/Mathics/issues/748. Parse
   cannot be defined in mathics using the "Alternatives" notation, even though
   MatchQ succeeds. We must use the "|" syntax. This limitation dooms the
   data-driven parser and parser generator. *)

ClearAll[parse]

parse[{}, tree_] := {{}, tree};

parse[{tok : "p" | "q" | "r" | "s", toks___},
   tree_: Null] :=
  {{toks}, Proposition[tok]};

parse[{tok : "N", toks___}, tree_: Null] :=
  Module[{R = parse[{toks}]},
   {remToks@R, Unary[tok, partTree@R]}];

parse[{tok : "C" | "A" | "K" | "E", toks___},
   tree_: Null] :=
  Module[{L = parse[{toks}]},
   Module[{R = parse[remToks@L]},
    {remToks@R, Binary[tok, partTree@L, partTree@R]}]];

parse[xs___] := Throw[{"PARSE: CATASTROPHE: ", xs}];

Print[parse[tokens["App"]]]

(* Import["https://raw.github.com/lshifr/CodeFormatter/master/CodeFormatter.m"]

   TODO: It will be a lot of work to make this work in mathics.
 *)

(*                                                         __
   ___  ___ ________ ___ ____  ___ ____ ___  ___ _______ _/ /____  ____
  / _ \/ _ `/ __(_-</ -_) __/ / _ `/ -_) _ \/ -_) __/ _ `/ __/ _ \/ __/
 / .__/\_,_/_/ /___/\__/_/    \_, /\__/_//_/\__/_/  \_,_/\__/\___/_/
/_/                          /___/

   TODO: this is doomed until issue #748 is solved:
   TODO: https://github.com/mathics/Mathics/issues/748

 *)

ClearAll[
    prefix, suffix,
    terminalsFromRule,
    nonTerminalHeadFromRule,
    arityFromPrefixRule];

prefix = First; suffix = Rest;

terminalsFromRule[rule_, nonTerminals_] :=
    Select[prefix /@ rule[[2]], ! MemberQ[nonTerminals, #] &];



expect [
    {{}, ToString/@{C, A, K, E}, ToString/@{N}, ToString/@{p, q, r, s}, {}}
  ,
    terminalsFromRule[#, nonTerminalsFromGrammar@P] & /@ DownValues[P]
]



nonTerminalHeadFromRule[rule_, nonTerminals_] :=
    Module[{h = rule[[1, 1, 1]]},
         If[! MemberQ[nonTerminals, h],
            Throw[{"NON-TERMINAL HEAD FROM RULE: CATASTROPHE", nonTerminals,
                   h}]];
         h];



expect[
    {Start, Binary, Unary, Proposition, Wff}
  ,
    nonTerminalHeadFromRule[#, nonTerminalsFromGrammar@P] & /@
        DownValues[P]
]



arityFromPrefixRule[rule_] :=
    Module[{lens = Union[Length /@ suffix /@ rule[[2]]]},
         If[Length@lens > 1,
            Throw[{"ARITY FROM PREFIX RULE: CATASTROPHE", lens}]];
         First@lens]



expect[
    {0, 2, 1, 0, 0}
  ,
    arityFromPrefixRule /@ DownValues[P]
]



ClearAll[genParserBody, parserDefFromGrammarRule];

genParserBody[0, tok_, toks_, head_, parts_, parse_] :=
 {toks,
  head[tok, Sequence @@ parts]}

genParserBody[arity_?(# > 0 &), tok_, toks_, head_, parts_, parse_] :=
  Module[{rec = parse[toks]},
  genParserBody[arity - 1, tok, remToks@rec, head,
    Join[parts, {partTree@rec}], parse]]

parserDefFromGrammarRule[parserTargetSym_, rule_, nonTerminals_] :=
 Module[{tok, toks, tree, parts, xs},
  Module[{
    ts = terminalsFromRule[rule, nonTerminals],
    h = nonTerminalHeadFromRule[rule, nonTerminals],
    a = arityFromPrefixRule[rule]},
   If[Length @ ts =!= 0,
    (* then *)
    Module[{
      firstPattern = {tok : Alternatives @@ ts, toks___},
      secondPattern = tree_: Null},
     parserTargetSym[firstPattern, secondPattern] :=
      genParserBody[a, tok, {toks}, h, {}, parserTargetSym]],
    (* else *)
    parserTargetSym[{}, tree_] := {{}, tree}]];
  parserTargetSym[xs___] :=
   Throw[{ToString @ parserTargetSym <> ": CATASTROPHE: ", xs}]]



(* Ad-hoc symbol for new test parser *)

ClearAll[$parse]
Scan[parserDefFromGrammarRule[
    $parse, #, nonTerminalsFromGrammar@P] &, DownValues@P]

(* ****************************************************************************
 _____ _          _____                                        ___         _
|_   _| |_  ___  |_   _|__ _ __  _ __  ___ _ _ __ _ _ _ _  _  | __|_ _  __| |
  | | | ' \/ -_)   | |/ -_) '  \| '_ \/ _ \ '_/ _` | '_| || | | _|| ' \/ _` |
  |_| |_||_\___|   |_|\___|_|_|_| .__/\___/_| \__,_|_|  \_, | |___|_||_\__,_|
                                |_|                     |__/
 *************************************************************************** *)

(* We leave this at the very bottom so we can get a count of right and wrong
results during development. The final result of the script, no matter what its
intermediate state, is the result of the following tautology. *)

expect[ true, true ]
