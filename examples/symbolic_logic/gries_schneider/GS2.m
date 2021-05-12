(* ****************************************************************************

    by Brian Beckman. License the same as for mathics.

    Oct 2020

    These examples are adapted from Gries & Schneider, "A Logical Approach
    to Discrete Math." The bibtex entry for this book follows:

    @Book{gries1993a,
     author = {Gries, David},
     title = {A Logical Approach to Discrete Math},
     publisher = {Springer New York},
     year = {1993},
     address = {New York, NY},
     isbn = {978-1-4757-3837-7}}

    In the prose below, the term "G&S" means that book. Also, references to
    page numbers, even without "G&S", refer to that book.

    These examples can be understood, with extra effort, without the book.
    However, the book is very helpful. It is not, to my knowledge, available
    online.

    I created these examples on the request of someone I am mentoring. Much of
    my explanations are just about understanding mathics because my mentee did
    not know it. My prose tone is conversational rather than formal, but the
    examples, of course, are formal in the sense of "mechanically checked."

 *************************************************************************** *)

<< "GS0.m"

(* Chaper 2, Boolean Expressions, page 25

   Section 2.1, Syntax and evaluation of Boolean expression, page 25
 ___           _                 ___                        _
| _ ) ___  ___| |___ __ _ _ _   | __|_ ___ __ _ _ ___ _____(_)___ _ _  ___
| _ \/ _ \/ _ \ / -_) _` | ' \  | _|\ \ / '_ \ '_/ -_|_-<_-< / _ \ ' \(_-<
|___/\___/\___/_\___\__,_|_||_| |___/_\_\ .__/_| \___/__/__/_\___/_||_/__/
                                        |_|

    Mathics has a full range of Boolean operators.

    Look at the example at the bottom of page 28. Code This directly using ||
    for \/ ('or') and && for /\ ('and'). Generate a table will all combinations
    of truth values for the three variables x, y, and z, flatten the table and
    apply And to the result. If any of the eight combinations of truth values
    yielded False, the result would be False. "And @@ <list-of-truth-values>,"
    syntax for "Apply[And, <list-of-truth-values>>]", is idiom for checking that
    all the truth values are True.

    That's an exhaustive test for equality of two symbolic Boolean expressions.

    f @ x is shorthand for f[x] and is handy for avoiding the need to put
    brackets on the ends of big expressions. Thus, "Flatten @ <long-expr>" is
    syntax for "Flatten[<long-expr>]".

 *************************************************************************** *)

expect[
  True,
  And @@ Flatten @ Table[(x || (y && z)) === ((x || y) && (x || z)),
    {x, {True, False}}, {y, {True, False}}, {z, {True, False}}]
]

(* Section 2.3, Satisfiability, validity, and duality, page 31
 ___       _   _     __ _      _    _ _ _ _
/ __| __ _| |_(_)___/ _(_)__ _| |__(_) (_) |_ _  _
\__ \/ _` |  _| (_-<  _| / _` | '_ \ | | |  _| || |_
|___/\__,_|\__|_/__/_| |_\__,_|_.__/_|_|_|\__|\_, ( )
                                              |__/|/
__   __    _ _    _ _ _                         _
\ \ / /_ _| (_)__| (_) |_ _  _     __ _ _ _  __| |
 \ V / _` | | / _` | |  _| || |_  / _` | ' \/ _` |
  \_/\__,_|_|_\__,_|_|\__|\_, ( ) \__,_|_||_\__,_|
                          |__/|/
 ___            _ _ _
|   \ _  _ __ _| (_) |_ _  _
| |) | || / _` | | |  _| || |
|___/ \_,_\__,_|_|_|\__|\_, |
                        |__/

    We must make up prefix forms for Boolean operations. Mathics does not have
    "Symbolize", Mathematica's way to define new notation.

    TODO: would be nice to add "Symbolize" to mathics.

    First, let's do the four unary operators and the sixteen binary operators on
    pages 25 and 26. I will give names to the operators that G&S leaves unnamed.

    As an aside, the number of different total functions f from a set A to a set
    B is

        ||B||  **  ||A||  =def=  ||A -> B||

    where ||B|| is the size or cardinality or number-of-elements in set B, and
    ||A|| is the size of set A, and the set of functions is denoted A->B. That's
    because for every element a in A, there are ||B|| different choices for the
    target f(a). The number of different ways to assign ||B|| values to ||A||
    values is ||B|| ** ||A||: ||B|| different possibilities for the first value
    a_1 in A, ||B|| different possibilities for the second value a_2 in A, and
    so on, up to ||B|| different possibilities for the ||A||-th value a_||A|| in
    A.

    For the unary functions, A is the set {true, false}, of size 2, and B is the
    set {true, false}, of size two. Thus there are 2 ** 2 == four different
    unary functions. For the binary functions, A is the set of pairs of true and
    false, and there are four such pairs; and B is the set {true, false}. Thus,
    there are 2 ** 4 == sixteen different binary functions.

    I start with inert "true" and "false" to avoid evaluation leaks, i.e., to
    prevent mathics from reducing expessions that have active "True" and
    "False".

*************************************************************************** *)

ClearAll[
  true,     (* inert version of True *)
  t,        (* shorthand for "true"; easier to compare against the book *)
  false,    (* inert version of False *)
  f,        (* shorthand for "false"; easier to compare against the book *)
  id,       (* function that returns its argument *)
  not,      (* unary that negates its argument *)
  tconst,   (* function that returns true on any input; first binary on p26 *)
  or,       (* second binary on p26 *)
  because,  (* left doublestruck arrow; G&S call this "consequence"; 3rd bin *)
  fst,      (* function that returns its first argument; fourth binary on p26 *)
  implies,  (* right-pointing doublestruck arrow; fifth binary on p26 *)
  snd,      (* function that returns its second argument; sixth binary on p26 *)
  eqv,      (* associative triple equals in G&S; seventh binary on p26 *)
  eq,       (* conjunctive double equals in G&S; seventh binary on p26 *)
  and,      (* eighth binary on p26 *)
  nand,     (* ninth binary on p26 *)
  neqv,     (* tenth binary on p26 *)
  neq,      (* tenth binary on p26 *)
  nsnd,     (* function that negates its second argumentn; 11th binary on p26 *)
  nimplies, (* function that negates "implies"; 12th binary on p26 *)
  nfst,     (* function that negates its first argument; 13th binary on p26 *)
  nbecause, (* function that negates "because"; 14th binary on p26 *)
  nor,      (* 15th binary on p26 *)
  fconst    (* function that returns false on any input; 16th binary on p26 *)
]

(* Now the truth table for the unary functions *)

unaryFunctionTruthTable =
    Table[{tconst[b], id[b], not[b], fconst[b]}, {b, {true, false}}]

(* Evaluation rules to drive mathics to reduce *)

boolRules = {
        tconst[_] :> True,     (* notice application of the 'rule of thumb' *)
        id        -> Identity, (*   that it's better to use ":>" for any    *)
        not       -> Not,      (*   rule with pattern variables like "_".   *)
        fconst[_] :> False,    (* ditto                                     *)
        true      -> True,
        false     -> False}

expect[
  { {True, True,  False, False},
    {True, False, True,  False} }
  ,
  unaryFunctionTruthTable //. boolRules
]

(*

    TRUTH TABLE FOR THE BINARY FUNCTIONS

    NEW EVALUATION RULES FOR EASIER COMPARISON WITH THE BOOK

    We build up rules in terms of other ones in a way that the book does not.
    Our build-up requires "//.", "ReplaceAllRepeated", to reduce expressions,
    because some built-up replacements cannot reduce in one step.

    We also see, here, the first need for mathics conditionals in the rewrite
    rules. When a replacement rule checks an argument against t or f with a
    mathics "If", we must first confirm that the argument is t or f. If we
    don't, then something like this

        and[a_, b_] := If[(a === t) && (b === t), t, f]

    called as follows

        and[not[f], t]

    bogusly produces f because not[f] doesn't "===" t before reduction by the
    rule for "not". We can't (and shouldn't need to) predict the order of
    application of the rules. We instead check that a and b each belong to the
    set {t, f}.

    One syntax for the conditional part of a rule is as follows

        fn   args           condition                 replacement
        -- -------- ------------------------- -----------------------------
        and[a_, b_] /; (boolQ[a] && boolQ[b]) :> If[(a===t)&&(b===t), t, f]

    It's safest to type-check arguments on all functions, but there is a certain
    elegance to minimal type-checking, especially because types are checked at
    run time in mathics and that's not free.

*)

ClearAll[boolQ]
boolQ[x_] := ((x === t) || (x === f))

comparisonBoolRules = {
(*fn      args    condition                replacement                        *)
(*------- ------- ------------------------ -----------------------------------*)
  id                                       -> Identity,
  not     [b_]    /;boolQ[b]               :> If[b === f, t, f],
  tconst  [a_, b_]                         :> t,
  tconst  [p_]                             :> t,
  or      [a_, b_]/;(boolQ[a] && boolQ[b]) :> If[(a === t) || (b === t), t, f],
  because [a_, b_]                         :> or[not[b], a],
  fst     [a_, b_]                         :> a,
  implies [a_, b_]                         :> or[not[a], b],
  snd     [a_, b_]                         :> b,
  eqv     [a_, b_]/;(boolQ[a] && boolQ[b]) :> If[a === b, t, f],
  eq                                       -> eqv,
  and     [a_, b_]/;(boolQ[a] && boolQ[b]) :> If[(a === t) && (b === t), t, f],
  nand    [a_, b_]                         :> not[and[a, b]],
  neqv    [a_, b_]                         :> not[eqv[a, b]],
  neq                                      -> neqv,
  nsnd    [a_, b_]                         :> not[snd[a, b]],
  nimplies[a_, b_]                         :> not[implies[a, b]],
  nfst    [a_, b_]                         :> not[fst[a, b]],
  nbecause[a_, b_]                         :> not[because[a, b]],
  nor     [a_, b_]                         :> not[or[a, b]],
  fconst  [a_, b_]                         :> f,
  fconst  [p_]                             :> f
}

binaryFunctionList = {tconst, or, because, fst, implies, snd, eqv, eq, and,
nand, neqv, neq, nsnd, nimplies, nfst, nbecause, nor, fconst}

(*

    We massage results with "Flatten", "Transpose", "Last" and "Partition" so
    that they can be compared directly with the table in the book. Remove that
    massaging if you want to see a more verbose output.

*)

(*  Mathics has an open bug preventing evaluation of the binary truth table
    when the input is a variable.

    https://github.com/mathics/Mathics/issues/960

    binaryTruthTable =
      Table[{ToString[fn[a, b]], fn[a, b]},
        {fn, binaryFunctionList},
        {a, {t, f}}, {b, {t, f}}]

    But I can unblock by pasting the binaryFunctionList explicitly. This will
    always work, even after they fix the bug, so we're not going to sweat it.
*)

binaryTruthTable =
  Table[{ToString[fn[a, b]], fn[a, b]},
    {fn, {tconst, or, because, fst, implies, snd, eqv, eq, and,
          nand, neqv, neq, nsnd, nimplies, nfst, nbecause, nor, fconst}},
    {a, {t, f}}, {b, {t, f}}]


expect[
    {{t, t, t, t},
     {t, t, t, f},                   (* or *)
     {t, t, f, t},                   (* because *)
     {t, t, f, f},
     {t, f, t, t},                   (* implies *)
     {t, f, t, f},
     {t, f, f, t}, {t, f, f, t},     (* eqv, eq *)
     {t, f, f, f},                   (* and *)
     {f, t, t, t},                   (* nand *)
     {f, t, t, f}, {f, t, t, f},     (* neqv, neq *)
     {f, t, f, t},
     {f, t, f, f},
     {f, f, t, t},
     {f, f, t, f},
     {f, f, f, t},                   (* nor *)
     {f, f, f, f}}
    ,
    Partition[Last @ Transpose @ Flatten[
        binaryTruthTable //. comparisonBoolRules, 2], 4]
]

(* Dual: Definition 2.2, page 31
 ___            _
|   \ _  _ __ _| |
| |) | || / _` | |
|___/ \_,_\__,_|_|

    Consider the pattern in "dual[head_[args__]]", namely "head_[args__]". It
    matches something like "and[p, q]" with "head" matching "and" and "{args}"
    matching the list "{p, q}"; "args", without curly braces, is bound to
    "Sequence[p, q]", a special form for argument splicing; we don't need to get
    into that now.

    Our rule for "dual" recursively dualizes the head and the args. Rewriting
    proceeds until things stop changing, so the recursion eventually bottoms out
    into one of the hard rules like dual[or]. We are reminded of a fact about
    mathics, a fact we covered in GS1.m, the first example file in this series:

    I M P O R T A N T   F A C T   A B O U T   M A T H I C S

    Applying a named rule like "dual" is like applying an unnamed rule with
    "//.", "ReplaceAllRepeated".

*)

ClearAll[dual]

dual[true]     = false
dual[t]        = f
dual[false]    = true
dual[f]        = t

dual[or]       = and
dual[and]      = or

dual[eqv]      = neqv
dual[neqv]     = eqv

dual[eq]       = neq
dual[neq]      = eq

dual[implies]  = nbecause
dual[nimplies] = because
dual[because]  = nimplies
dual[nbecause] = implies

(*

   These last few are redundant. "implies[p, q]" means "we cannot have q without
   p," or "p is a sufficient condition for q," or "q is a necessary condition of
   p," or "~(p /\ ~q)", which reduces, by De Morgan's laws, to "(~p \/ q)", or
   to "or[not[p], q]".

   The dual of "implies[p, q]" is "(~p /\ q)", which is "~(p \/ ~q)" (by De
   Morgan), which is "not[because[p, q]]". So we can derive the rules above from
   the duals of "or" and "and".

*)

dual[head_[args__]] := Apply[dual[head], dual /@ {args}]

dual[var_] := var

(* Table 2.1, page 31 *)

expect[and[p, q],                   dual[or[p, q]]]
expect[nbecause[p, q],              dual[implies[p, q]]]
expect[neqv[p,not[p]],              dual[eqv[p,not[p]]]]
expect[eqv[t, and[f, p]],           dual[neqv[f, or[t, p]]]]
expect[neqv[or[not[p], not[q]], r], dual[eqv[and[not[p], not[q]], r]]]

(*

   Let's have some fun making up random boolean expressions, then checking that
   the duals of their duals reproduce the originals.

   To see the random expressions, inspect the console output.

   D E P E N D E N C Y   W A R N I N G

   You must have numpy installed for the following to work in mathics.

   The results will be different in Mathematica because the random-number
   generators differ between Mathematica and mathics. There is also a difference
   in "RandomChoice", but I worked around it by always calling "RandomChoice"
   with the second argument, which specifies the length of a list returned. In
   Mathematica, RandomChoice with no second argument returns a randomly chosen
   element of the list in the first argument. In mathics, RandomChoice with no
   second argument returns a singleton list containing an element. In both,
   RandomChoice with the second argument equal to 1 returns a singleton list. We
   use "First" to extract the randomly chosen element.

*)

ClearAll[randomUnaryFunction, randomBinaryFunction, randomBooleanVariable]

randomUnaryFunction[] := First @ RandomChoice[{tconst, id, not, fconst}, 1]

randomBinaryFunction[] := First @ RandomChoice[{ (* skip 'eq' and 'neq' *)
    tconst,  or,       because, fst,
    implies, snd,      eqv,     and,
    nand,    neqv,     nsnd,    nimplies,
    nfst,    nbecause, nor,     fconst}, 1]

randomBooleanVariableOrConstant[] :=
    Symbol @ First @ RandomChoice[Characters["pqrsft"], 1]

SeedRandom[45] (* for repeatability *)

randomBooleanExpression[] :=
    Module[{roll = RandomReal[]},
        If[ roll < 0.40,
            randomBooleanVariableOrConstant[],
            If[ roll < 0.70,
                randomBinaryFunction[][
                    randomBooleanExpression[],
                    randomBooleanExpression[]],
                randomUnaryFunction[][
                    randomBooleanExpression[]]]]]

expect[fconst[fconst[s, tconst[t]]], randomBooleanExpression[]]
expect[p, randomBooleanExpression[]]
expect[p, randomBooleanExpression[]]
expect[because[nfst[implies[p, implies[f, fconst[f, p]]], t], id[not[q]]],
       randomBooleanExpression[]]

(* check repeatability *)

SeedRandom[45]
expect[fconst[fconst[s, tconst[t]]], randomBooleanExpression[]]
expect[p, randomBooleanExpression[]]
expect[p, randomBooleanExpression[]]
expect[because[nfst[implies[p, implies[f, fconst[f, p]]], t], id[not[q]]],
       randomBooleanExpression[]]

(* check that dual is self-inverse *)

SeedRandom[45]
expect[fconst[fconst[s, tconst[t]]],
    dual @ dual @ randomBooleanExpression[]]
expect[p,
    dual @ dual @ randomBooleanExpression[]]
expect[p,
    dual @ dual @ randomBooleanExpression[]]
expect[because[nfst[implies[p, implies[f, fconst[f, p]]], t], id[not[q]]],
    dual @ dual @ randomBooleanExpression[]]

Module[{expressions = Table[randomBooleanExpression[], {20}]},
  Module[{dduals = dual /@ dual /@ expressions},
    MapThread[expect, {expressions, dduals}]]]

(* The Superman example, page 37; we'll consider all 64 states. Mathics has a
   nice, rendered 'implication' arrow. The ASCII input syntax for it is
   "\[Implies]". This might render well on your screen if you have a good
   Unicode font and UTF-8 encoding. Also, we're using mathics logical operators
   and allowing them to reduce, rather than manipulating our own expressions.
   We'll do that later. The book gets back to "Superman" on page 89.
   *)

expect[
        True
        ,
        And @@
        Flatten @
                Table[( ( (a && w) \[Implies] p ) &&
                        ( (!a \[Implies] i) && (!w \[Implies] m) ) &&
                        ( !p ) &&
                        ( e \[Implies] (!i && !m) )
                      ) \[Implies] !e,
                      {a, {True, False}},
                      {w, {True, False}},
                      {i, {True, False}},
                      {m, {True, False}},
                      {p, {True, False}},
                      {e, {True, False}} ]
]

(* Metatheorem 2.3, page 32 ***************************************************
 __  __     _        _   _
|  \/  |___| |_ __ _| |_| |_  ___ ___ _ _ ___ _ __
| |\/| / -_)  _/ _` |  _| ' \/ -_) _ \ '_/ -_) '  \
|_|  |_\___|\__\__,_|\__|_||_\___\___/_| \___|_|_|_|
 ___            _ _ _
|   \ _  _ __ _| (_) |_ _  _
| |) | || / _` | | |  _| || |
|___/ \_,_\__,_|_|_|\__|\_, |
                        |__/
 *************************************************************************** *)

ClearAll[dualTheorem]
dualTheorem[theorem_] := not[dual[theorem]]

(* Table 2.2, Using Duality to Generate Valid Expressions ****************** *)

expect[ dualTheorem[true],           not[false] ]
expect[ dualTheorem[or [p, true]],   not[and[p, false]] ]
expect[ dualTheorem[or [p, not[p]]], not[and[p, not[p]]] ]

(* G&S slip a fast one on us, here, by implicitly reducing not[neqv[...]] to
   eqv[...] and vice versa. Computers are dumb, must be told exactly what to do.
   we tweak the output of "dualTheorem" with a ad-hoc rewrite rule. *)

expect[ dualTheorem[eqv[true, true]]
            //. not[neqv[x_, y_]] :> eqv[x, y]
        ,
        eqv[false, false] ]



expect[ dualTheorem[eqv[or[p, q], or[q, p]]]
            //. not[neqv[x_, y_]] :> eqv[x, y]
        ,
        eqv[and[p, q], and[q, p]] ]



expect[ dualTheorem[eqv[eqv[p, q], eqv[q, p]]]
            //. not[neqv[x_, y_]] :> eqv[x, y]
        ,
        eqv[neqv[p, q], neqv[q, p]] ]



expect[ dualTheorem[eqv[not[or[p, q]], and[not[p], not[q]]]]
            //. not[neqv[x_, y_]] :> eqv[x, y]
        ,
        eqv[not[and[p, q]], or[not[p], not[q]]] ]     // Print


(* You do the exercises in Chapter 2. *)


Exit[Min[totalWrong, 255]]
