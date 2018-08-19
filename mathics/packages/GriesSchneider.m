(* ****************************************************************************

    Please see

    https://github.com/rebcabin/Mathics/blob/master/mathics/packages/GriesSchneider.m

    for the most up-to-date version. Changes will be committed there from now
    on.

    When mathics itself is updated, you must reinstall it:

        python setup.py install

    You can run unit tests as follows:

        python setup.py test

   ****************************************************************************

    This is an extended transcription of Gries & Schnedier, "A Logical Approach
    to Discrete Math," into mathics (https://goo.gl/wSm1wt), a free clone of
    Mathematica (https://goo.gl/0uvLZ), written in Python. I got mathics to run
    on Python 3.5 and not on Python 3.6.

    @Book{gries1993a,
     author = {Gries, David},
     title = {A Logical Approach to Discrete Math},
     publisher = {Springer New York},
     year = {1993},
     address = {New York, NY},
     isbn = {978-1-4757-3837-7}}

    Why are we doing this? Gries & Schnedier is a great example of a formal
    method. Formal methods means "machine-checked proofs." Formal Methods help
    you write better software. They can help you avoid billion-dollar mistakes,
    like crashing the Mars Climate Observer because the units of measure
    "newton" and "pound-force" were not checked by machine.

    Fall in love with formal methods, please! They're related to static
    type-checking (that's a little formal method in your compiler, proving
    little theorems about types in your code), and great things like
    Clojure.spec (https://goo.gl/sttnFC). I think a lot of people know those are
    good, but there are lots of other, lesser-known formal methods like
    Statecharts (https://statecharts.github.io/) and TLA+
    (https://goo.gl/dx32Mw). TLA+ saved Amazon's Dynamo DB a catastrophic
    failure (https://goo.gl/pTpZYT). Many mistakes have been found in published
    protocols when subjected to formal methods (no citation).

 *************************************************************************** *)
(* Section 1.2 Textual substitution, page 8
 _____        _             _   ___      _       _   _ _        _   _
|_   _|____ _| |_ _  _ __ _| | / __|_  _| |__ __| |_(_) |_ _  _| |_(_)___ _ _
  | |/ -_) \ /  _| || / _` | | \__ \ || | '_ (_-<  _| |  _| || |  _| / _ \ ' \
  |_|\___/_\_\\__|\_,_\__,_|_| |___/\_,_|_.__/__/\__|_|\__|\_,_|\__|_\___/_||_|

    Gries & Schneider         Us                        Expected Output
    ------------------------- ------------------------- ----------------------
    x[x := x + 2]             x /. {x -> x + 2}         x + 2
    (x+y)[x := z + 2]         x + y /. {x -> z + 2}     z + 2 + y
    (x*y)[x := z + 2]         x * y /. {x -> z + 2}     (z + 2) * y

    To test that we've got this working, load this here file, the one you're
    reading right now, into mathics. Let's say you've stored the file in
    "~/some/directory/GriesSchneider.m". Then, then run mathics at the terminal
    and load the file; you should see approximately the following:

        $ mathics

        Mathics 1.1.dev0
        on CPython 3.5.5 |Anaconda, Inc.| (default, May 13 2018, 21:12:35)
        using SymPy 1.0, mpmath 1.0.0

        Copyright (C) 2011-2016 The Mathics Team.
        This program comes with ABSOLUTELY NO WARRANTY.
        This is free software, and you are welcome to redistribute it
        under certain conditions.
        See the documentation for the full license.

        Quit by pressing CONTROL-D

        In[1]:= <<"some/directory/GriesSchneider.m"
        2 + x
        2 + y + z
        y (2 + z)
        In[2]:=

    at the top of the output.

    Just what Gries and Schneider said we should see, only with a little
    rearranging because mathics knows that '+' and '*' are commutative. This
    rearranging won't bother us until later, when we make a way to control it.

 *************************************************************************** *)

Print[ x /. {x -> x + 2} ]

Print[ x + y /. {x -> z + 2} ]

Print[ x * y /. {x -> z + 2} ]

(* Bottom of page 8: *********************************************************

    Gries & Schneider         Us                        Expected Output
    ------------------------- ------------------------- ----------------------
    (z+y)[z,y := 5, 6]        z+y /. {z->5, y->6}       11

 *************************************************************************** *)

Print[ z+y /. {z->5, y->6} ]

(* ***************************************************************************

    Let's do a little tooling so we can write 'expected' and 'actual' in our
    examples. You don't need to understand this.

 *************************************************************************** *)

ClearAll[expect, totalRight, totalWrong];
SetAttributes[ expect, HoldAllComplete ];
totalRight = totalWrong = 0;
expect[expected_, actual_] := (* <~~~ Here's the API *)
   Module[{evalActualOnce = actual,
           evalExpectedOnce = expected},
      Print[ {"expression", HoldForm[actual],
              "\nexpected", HoldForm[expected],
              "\neval'd expected", evalExpectedOnce,
              "\neval'd actual  ", evalActualOnce,
              "\nright?",   evalExpectedOnce === evalActualOnce} ];
      Print[ "" ]; (* newline *)
      If[ evalExpectedOnce === evalActualOnce,
          totalRight += 1,
          totalWrong += 1 ];
      {"total right", totalRight, "total wrong", totalWrong}
       (* That's the return value of 'expect'. *) ];

(* ~~~> Here's an example <~~~ *)

expect[11,   z+y /. {z->5, y->6}]

(* ***************************************************************************

    Note: You don't have to write stars for commutative multiplication in
    mathics: all the following are the same: 2 * y, y * 2, 2 y, 2y. You'll
    notice that even in the PRINTING of the expressions, the stars are removed
    (despite my HoldAllComplete and HoldForm, mathics always does a little
    simplification). You do need to leave spaces between symbols, however; 4a c
    is 4*a*c, whereas 4ac is 4*ac, where ac is a symbol with two characters.

 *************************************************************************** *)

expect[2y,   2 * y    ]
expect[2y,   2 y      ]
expect[2y,   y * 2    ]
expect[2y,   y 2      ]
expect[4a c, 4 * a * c]
expect[4ac,  4 * ac   ]

(* Top of page 10: ************************************************************

    Left Associativity:

    Gries & Schneider         Us                        Expected Output
    ------------------------- ------------------------- ----------------------
    (x + 2y)[x,y := y,x]      (x+2y)/.{x->y, y->x}      y + 2x
    (x + 2y)[x := y][y := x]  (x+2y)/.{x->y}/.{y->x}    3x

 *************************************************************************** *)

expect[y + 2x,   (x + 2y) /. {x -> y, y -> x}    ]
expect[3x,       (x + 2y) /. {x -> y} /. {y -> x}]

(* Textual Substitution and Hidden Variables, page 10 *************************

       -b + Sqrt[ b^2 - 4 a c ]
    Q: ------------------------
                 2a

    Gries & Schneider         Us                        Expected Output
    ------------------------- ------------------------- ----------------------
    (x = Q)[b := 5]           (x = Q)/.{b->5}           (-5+Sqrt[25-4a c])/(2a)

    Be careful: in mathics, as with most programming languages, 1/2a == a/2.
    This is counter to the accepted typographical conventions of mathematics and
    physics (I don't have the reference handy, but Author's manuals for both the
    American Physical Society and the American Mathematical Society declare that
    1/2a means 1/(2a). This serious problem leads to subtle bugs!).

 *************************************************************************** *)

Q = (-b + Sqrt[ b^2 - 4 a c ]) / (2a)

expect[ (-5 + Sqrt[25-4a c]) / (2a),   (x = Q)/.{b->5} ]

(* In the above, we have made assignments to Q and x. We have modified the
   global state of this session. We need to clear out those assignment so that
   we can use the variables Q and x later. *)

ClearAll[Q, x]

(* Examples, box bottom of page 10: *******************************************

    We're getting comfortable with the mathics syntax for substitution, so I'm
    just going to skip the English write-up.

 *************************************************************************** *)

expect[ 35, 35 /. {x -> 2} ]
expect[ y,   y /. {x -> 2} ]
expect[ 2,   x /. {x -> 2} ]
expect[ (c + y) * (c + y) + y,   (x*x + y) /. {x -> c + y} ]
expect[ (x + y)^2 + y^2 + (x + y)^3,
        (x^2 + y^2 + x^3) /. {x -> x + y} ]

expect[ z + w + w,      (x + y + y) /. {x -> z, y -> w}    ]
expect[ 2y + x z + x z, (x + y + y) /. {x -> 2y, y -> x z} ]
expect[ y + 2x,         (x + 2y)    /. {x -> y, y -> x}    ]
expect[ z + 2 x y,      (x + 2 y z) /. {x->z, y->x, z->y}  ]

(* Inference Rule Substitution (1.1), page 10
 ___       __                           ___      _
|_ _|_ _  / _|___ _ _ ___ _ _  __ ___  | _ \_  _| |___
 | || ' \|  _/ -_) '_/ -_) ' \/ _/ -_) |   / || | / -_)
|___|_||_|_| \___|_| \___|_||_\__\___| |_|_\\_,_|_\___|
 ___      _       _   _ _        _   _
/ __|_  _| |__ __| |_(_) |_ _  _| |_(_)___ _ _
\__ \ || | '_ (_-<  _| |  _| || |  _| / _ \ ' \
|___/\_,_|_.__/__/\__|_|\__|\_,_|\__|_\___/_||_|

    The inference rule 'Substitution' takes in a top-line expression e, a list
    of variables v, and a corresponding list of replacement expressions f, and
    spits out the bottom line conclusion e[v:=f]. The following is the
    definition of the inference-rule scheme, followed by the example on page 11.

    (we shouldn't use capital letters as the first characters of names in
    mathics because the system defines many of them. For example, E is defined
    as the Euler constant 2.71828... So, we shall take some liberties with Gries
    & Schneider (G&S) and write their formulas with small (uncial) letters).

 *************************************************************************** *)

ClearAll[inferenceRuleSubstitution];
SetAttributes[inferenceRuleSubstitution, HoldFirst]
inferenceRuleSubstitution[e_, v_:List, f_:List] :=
    Module[{ rules = MapThread[ Rule, {v, f} ] },
        Print[{
            "inference rule", "Substitution 1.1, page 11",
            "\ntop line", HoldForm[e],
            "\nrules", rules,
            "\noutput", HoldForm[e] /. rules}];
        Unevaluated[e] /. rules ] (* <~~~ possible evaluation leak *)

(* ****************************************************************************

    Here comes the example, but first, a note:

    To write theorem expressions, assertions of equality, we have == and === in
    mathics, short for Equal and SameQ, respectively. Equal returns itself if
    the expressed equality is not true, whereas SameQ returns False:

 *************************************************************************** *)

expect[ x == y, x == y ]

expect[ False, x === y ]

(* ****************************************************************************

    Almost all the time we want the behavior of SameQ. So now we can apply the
    inference rule. You'll see a beautiful proof when you run this through
    mathics (or even Mathematica). Notice that mathics does a little
    rearranging, rewriting j + 5 as 5 + j. It's OK for now.

 *************************************************************************** *)

expect[ 2 (j + 5) / 2 === j + 5,
        inferenceRuleSubstitution[
            2 x / 2 === x,
            {x},
            {j + 5}
        ] ]

(* ****************************************************************************

    You should see something like this:

    {inference rule, Substitution 1.1, page 11,
            top line, 2 x / 2 === x,
            rules, {x -> 5 + j},
            output, 2 (5 + j) / 2 === 5 + j}
    {expression, inferenceRuleSubstitution[2 x / 2 === x, {x}, {j + 5}],
            expected, 2 (j + 5) / 2 === j + 5,
            actual, True,
            right?, True}

 *************************************************************************** *)

(* Section 1.3 Textual substitution and equality, page 11
 _____        _             _   ___      _       _   _ _        _   _
|_   _|____ _| |_ _  _ __ _| | / __|_  _| |__ __| |_(_) |_ _  _| |_(_)___ _ _
  | |/ -_) \ /  _| || / _` | | \__ \ || | '_ (_-<  _| |  _| || |  _| / _ \ ' \
  |_|\___/_\_\\__|\_,_\__,_|_| |___/\_,_|_.__/__/\__|_|\__|\_,_|\__|_\___/_||_|
              _   ___                _ _ _
 __ _ _ _  __| | | __|__ _ _  _ __ _| (_) |_ _  _
/ _` | ' \/ _` | | _|/ _` | || / _` | | |  _| || |
\__,_|_||_\__,_| |___\__, |\_,_\__,_|_|_|\__|\_, |
                        |_|                  |__/

    Going forward, we want more inference and less evaluation. This is tricky in
    mathics, which is super aggressive about evaluation. Stopping it requires a
    large zoo of "evaluation-control" functions. Here is a partial list:

    Hold              HoldAll           HoldFirst         HoldRest
    HoldComplete      HoldAllComplete   HoldForm          Evaluate
    Unevaluated       ReleaseHold       HoldPattern       SequenceHold
    Defer             Update            many more ...

    Most of the work of using an expression evaluator like mathics as a proof
    assistant is in preventing evaluation until the right time. Rules like
    Reflexivity (G&S 1.2), Symmetry (G&S 1.3), and Transitivity (G&S 1.4) are
    hard-coded into mathics, which will apply the rules without notification.
    Common factors in division expressions are canceled without notification
    right in the middle of trying to prove cancellation; and so on.

    An "evaluation leak" is an inadvertent early evaluation. Evaluation leaks
    don't affect the truth value of a theorem, they affect display of steps of a
    proof because mathics applies built-in rules as soon as it can, interrupting
    our flow of human reasoning. We must control evaluation ourselves, watching
    our rules at work and putting them to work explicitly.

    We can thwart evaluation either with the zoo of Holds, or by not using
    built-ins, at least not until we want to. It's a trade-off: using the zoo of
    Holds, we can retain pretty infix syntax in expressions, but we risk
    evaluation leaks. Experience shows that even the pros get this wrong often
    (search "evaluation leak" on mathematica.stackexchange.com). It also forces
    us to learn more than we want about the evaluator (see
    https://goo.gl/L7Gz3h), including its bugs, which we may have to fix.

    On the other hand, if we avoid built-ins, we must write, for example, the
    inert expression div[2 (j+5), 2] instead of 2 (j + 5) / 2 to prevent early
    cancellation. We lose pretty syntax, at least until we do something like the
    following:

*************************************************************************** *)

expect [ j + 5,
         div[ 2 (j+5), 2 ] /. {div[a_, b_] -> a / b} ]

(* ****************************************************************************

    The mathics rule to the right of /. has pattern variables a_ and b_, which
    are replaced by 2 (j+5) and 2, respectively, when /. is evaluated on t
    he
    inert div[2(j+5), 2] and that rule.

    This is a new kind of rule we don't see above, but we will use rules like it
    to control evaluation below. This kind of rule is very much like application
    of a function or lambda expression in Python, but not exactly the same. The
    distinction shouldn't matter as we proceed.

    So let's do an extended experiment with inert sameq, plus, times, div, etc.,
    replacing them only explicitly with built-ins when desired using rules like
    div immediately above. We lose some pretty syntax; the purpose of the
    experiment is to see whether losing that syntax is worth avoiding struggles
    and bugs with evaluation leaks. We at least get the mitigation that our work
    will be totally explicit.

    Let's first redo the substitution rule (G&S 1.1). During this experiment, we
    avoid the zoo of Holds, plus SetAttributes (https://goo.gl/Zt3KbB), upvalues
    (https://goo.gl/4bgm65), and more arcana. We used some of those in the prior
    definition of inferenceRuleSubstitution, which we're leaving behind now.

    So much for mathics (for now). The G&S "language" has fine distinctions
    between Laws, Inference Rules, and Axioms that are not always crystal-clear.
    It's clear that some Laws, like Transitivity (1.4) are Inference Rules.
    However, there may be churn as I discover subtleties around these
    distinctions.

 *************************************************************************** *)

ClearAll[substitutionInferenceRule];
substitutionInferenceRule[e_, v_:List, f_:List] :=
    Module[{ rules = MapThread[ Rule, {v, f} ] },  (* <~~~ don't use "With" *)
        Module [{ output = e /. rules },           (* <~~~ don't use "With" *)
            Print[{
                "inference rule", "Substitution 1.1, page 11",
                "\ntop line", e,
                "\nrules", rules,
                "\noutput", output}];
            output ] ]

expect[ sameq[ div[2(j+5), 2], j+5 ],
        substitutionInferenceRule[
            sameq[ div[2 x, 2], x],
            {x},
            {j + 5} ] ]

(* ****************************************************************************

    You should see something like this:

    {inference rule, Substitution 1.1, page 11,
            top line, sameq[div[2 x, 2], x],
            rules, {x -> 5 + j},
            output, sameq[div[2 (5 + j), 2], 5 + j]}
    {expression, substitutionInferenceRule[sameq[div[2 x, 2], x], {x}, {j + 5}],
            expected, sameq[div[2 (j + 5), 2], j + 5],
            actual, sameq[div[2 (5 + j), 2], 5 + j],
            right?, True}

    We found a difference between mathics and Mathematica, here: "With" instead
    of "Module" works in Mathematica but not in mathics. This should not be a
    problem in the following because we will not rely on the differences between
    "With" and "Module". If you wish to dig in, feel free, but you will be
    debugging mathics itself rather than working on G&S.

    A point about semicolons. They are syntax for CompoundExpression:

 *************************************************************************** *)

expect[ CompoundExpression[x, y, z],    (* Expression 25 *)
        (x; y; z) ]

(* ****************************************************************************

    A CompoundExpression evaluates all its terms but produces only the last one
    as final result:

 *************************************************************************** *)

expect[ z, (x; y; z) ]

(* ****************************************************************************

    Notice in the printout from "expect" in Expression 25 above that mathics
    aggressively rewrites "CompoundExpression[x, y, z]" as "x ; y ; z" despite
    the fact that "expect" has attribute "HoldAllComplete" and "HoldForm". This
    is an example of an evaluation leak that would be very time-consuming to
    fix. Mathematica does the same thing. We won't bother to fix it, because
    we're trying to make the following point:

    We may absentmindedly put a semicolon after a global definition or a
    "ClearAll". The presence or absence of such terminal semicolons does not
    make a difference at the global level, but it does inside Expressions. Don't
    be too concerned about this, just be aware that semicolons are sometimes
    important and sometimes not.

 *************************************************************************** *)

(* 1.2, page 12 *)
ClearAll[reflexivityLaw];
reflexivityLaw[x_] := sameq[x, x]

(* 1.3, page 12 *)
ClearAll[symmetryLaw];
symmetryLaw[x_, y_] := sameq[ sameq[x, y], sameq[y, x] ]

(* 1.4, page 12 *)
ClearAll[transitivityLaw];
transitivityLaw [ and [ sameq[x_, y_], sameq[y_, z_] ] ] := sameq[x, z]

expect[ sameq[ x+y, 7 ],
        transitivityLaw [ and [ sameq [ x+y, w+1 ],
                                sameq [ w+1, 7 ] ] ] ]

(* ****************************************************************************

    You should see something like this:

        {expression, transitivityLaw[and[sameq[x + y, w + 1], sameq[w + 1, 7]]],
            expected, sameq[x + y, 7],
            actual, sameq[x + y, 7],
            right?, True}

    A nice victory, with no evaluation drama.

    We now reproduce the theorem on page 4. This will require more machinery.
    Remember that all the laws above all hard-coded in the mathics evaluator, so
    we need to avoid triggering them. We supply our own rewrite rules for plus,
    times, sameq, and so on, leaving them inert symbolic expressions that
    mathics doesn't know how to reduce until we tell it explicitly.

    U N N A M E D   R U L E S

    We may write two kinds of rewrite rules: named and unnamed. Many of the ones
    above are unnamed. Such rules have the form "pattern -> result", for example
    "x -> 2y", where the pattern is "x" and must match the target of the rule
    exactly (the target of such a rule is the left-hand side of a "/.",
    "ReplaceAll", or of a "//.", "ReplaceAllRepeated").

    Another example of an unnamed rule is "div[a_, b_] -> a / b", where the
    pattern is "div[a_, b_]", containing pattern variables a_ and b_. Those
    variables match anything. When the rule is applied, the pattern variables
    are replaced on the right-hand side of the arrow by the things they match.

    "a -> b" is syntax for "Rule". There is another kind of arrow, namely ":>",
    syntax for "RuleDelayed", which means "don't evaluate the right-hand side of
    the rule now, only later, when we apply the rule."

    Unnamed rules are usually "ad-hoc," created just to solve a problem that
    arises once in some computation, and are therefore not worth naming and
    saving away for many uses.

    N A M E D   R U L E S

    A named rule has the form "head[pattern] := result". Search backwards for
    instances of ":=". Every place we used that, we defined a name, which
    becomes the head of the rule. For example, in "transitivityLaw[blahblah] :=
    yaketyyak", the "head" is "transitivityLaw" and we invoke the rule as in
    "transitivityLaw[and[sameq[x+y, blahblah]]]". The rest of the rule works
    just like an unnamed rule with a ":>" arrow.

    We could write a named rule as "head[pattern] = result", syntax for "Set",
    when we want the result (right-hand side) evaluated at definition time (now)
    instead of at application time (later), but it's usually considered bad
    practice. Using ":=", syntax for "SetDelayed", instead of "=" sweeps a bunch
    of early-evaluation drama under the rug.

    By default, we write unnamed rules with an eager arrow, "->", and named
    rules with a lazy "SetDelayed", i.e., ":=", but we have the other options
    when we need them.

    This issue is a "meta-evaluation leak", noise about evaluation rising to our
    attention. But we can ignore the noise much of the time.

    Back to our theorem from page 4, we'll write our new machinery with some
    named rewrite rules that don't trigger the built-in reductions:

 *************************************************************************** *)

ClearAll[leftTimesIsAssociativeLaw]
leftTimesIsAssociativeLaw              [
    times [ times [ a_, b_ ], c_ ]     ] :=
    times [ a,  times [ b,  c  ] ]

ClearAll[rightTimesIsAssociativeLaw]
rightTimesIsAssociativeLaw             [
    times [ a_, times [ b_, c_ ] ]     ] :=
    times [ times [ a,  b  ], c  ]

ClearAll[leftTimesUnitLaw]
leftTimesUnitLaw                       [
    times [ 1, m_ ]                    ] := m

ClearAll[rightTimesUnitLaw]
rightTimesUnitLaw                      [
    times [ m_, 1 ]                    ] := m

ClearAll[divideBothSidesByNonZeroLaw]
divideBothSidesByNonZero               [
    sameq [ a_, b_ ],  d_              ] :=
    sameq [ div[a, d], div[b, d] ]

ClearAll[cancelNonZeros]
cancelNonZeros                         [
    div [ a_, a_ ]                     ] := 1

(* Rather than do a bunch more laws (at least eight) for associativity between
   div and times, it's easier to rewrite div _as_ times. G&S finesse over this
   point, but computers are stupid and must be told exactly what to do all the
   time. *)

ClearAll[divAsTimes]
divAsTimes                             [
    div [ a_, b_ ]                     ] :=
    times [ a, power [ b, -1 ] ]

ClearAll[timesAsDiv]
timesAsDiv                             [
    times [ a_, power [ b_, -1 ] ]     ] :=
    div [ a, b ]

(* ****************************************************************************

    Let's start by applying the rule "divideBothSidesByNonZero" and checking
    that it meets our long-winded representation of e/c^2 === (m c^2)/c^2:

        sameq [ div [ e, c^2 ], div [ times [ m, c^2 ], c^2 ] ].

    Call this expression "target" so we don't have to write it out long over and
    over again, remembering to ClearAll it along with other definitions at the
    end of this block of work:

 *************************************************************************** *)

target = sameq [ div [ e, c^2 ],
                 div [ times [ m, c^2 ], c^2 ] ]

premise = sameq [ e, times [ m, c^2 ] ]

expect [ target,
         divideBothSidesByNonZero [
             premise,
             c^2 ] ]

(* ****************************************************************************

    To avoid (1) deeply nesting expressions, and (2) copy-pasting expressions
    over and over (https://goo.gl/4PJnbK), introduce "Postfix notation for
    application of rules." Instead of

        divideBothSidesByNonZero[premise, c^2]

    we can write

        premise // divideBothSidesByNonZero [ #1, c^2 ] &

    "x // f" means "apply function f to argument x"

    It chains nicely (associates to the left), as we see below. That will make
    proofs look more like those in the book instead of like deeply nested
    function applications staircasing off to the right (https://goo.gl/MVBwaV).

        e[#1, blahblah] &

    means "a function with body e[#1, blahblah], with slot #1 for the first
    argument."

 *************************************************************************** *)

expect [ divideBothSidesByNonZero [ premise, c^2 ],
         premise // divideBothSidesByNonZero [ #1, c^2 ] &
       ]

(* ****************************************************************************

    We still need a little parenthesizing to group up ad-hoc rules because
    "ReplaceAll" and "ReplaceAllRepeated" don't always associate well with
    postfix application "//". Here's the first shot at the theorem:

 *************************************************************************** *)

expect [

sameq [ div [ e, c^2 ], m ],

(premise                                                       //

     divideBothSidesByNonZero [ #1, c^2 ] &                    //

     Map[divAsTimes, #1] &                                   ) /. (* ad-hoc *)

     sameq[a_, b_] -> sameq[a, leftTimesIsAssociativeLaw[b]]   /.

     sameq[a_, times[b_, c_]] ->                                  (* ad-hoc *)
         sameq[timesAsDiv[a], times[b, timesAsDiv[c]]]         /.

     sameq[a_, times[b_, c_]] ->                                  (* ad-hoc *)
         sameq[a, times[b, cancelNonZeros[c]]]                 /.

     sameq[a_, b_] -> sameq[a, rightTimesUnitLaw[b]]              (* ad-hoc *)
]

(* ****************************************************************************

    You should see something like this (the original indentation is ugly; I
    reindented it):

      {expression,
        (divAsTimes /@ #1&)[(divideBothSidesByNonZero[#1, c ^ 2]&)[premise]] /.
         sameq[a_, b_] -> sameq[a, leftTimesIsAssociativeLaw[b]] /.
         sameq[a_, times[b_, c_]] ->
           sameq[timesAsDiv[a], times[b, timesAsDiv[c]]] /.
         sameq[a_, times[b_, c_]] -> sameq[a, times[b, cancelNonZeros[c]]] /.
         sameq[a_, b_] -> sameq[a, rightTimesUnitLaw[b]],
        expected, sameq[div[e, c ^ 2], m],
               actual, sameq[div[e, c ^ 2], m],
               right?, True}

    That recaps the theorem and is our first substantial proof.

    We made several ad-hoc rules because we needed to apply laws _inside_ other
    expressions. We'll make some ways to mitigate that below, because we want
    our proof assistant eventually to search for opportunities to apply rules,
    but we probably don't want to get into dynamically generating ad-hoc rules.
    Rather, we want to write higher-order rules that search inside nested
    expressions for ways to apply named and ad-hoc rules.

    Let's add a little machinery to nicely display intermediate results so we
    get a display similar to but more detailed than that on page 4. We need more
    parentheses, unfortunately.

 *************************************************************************** *)

ClearAll[dump]
dump[e_] := (Print[e]; e)

(((((premise                                                   // dump  //
     divideBothSidesByNonZero [ #1, c^2 ] &                    // dump  //
     Map[divAsTimes, #1] &                                     // dump) /.
     sameq[a_, b_] -> sameq[a, leftTimesIsAssociativeLaw[b]] ) // dump) /.
     sameq[a_, times[b_, c_]] ->
         sameq[timesAsDiv[a], times[b, timesAsDiv[c]]]         // dump) /.
     sameq[a_, times[b_, c_]] ->
         sameq[a, times[b, cancelNonZeros[c]]]                 // dump) /.
     sameq[a_, b_] -> sameq[a, rightTimesUnitLaw[b]]           // dump

(* ****************************************************************************

    That works --- you should see a pretty display of all the steps when you
    evaluate that expression like that below. Make sure you understand both the
    input and output. These steps are not annotated. We'll fix that later. This
    proof is short enough that lack of annotation should not matter.

sameq[e, times[m, c ^ 2]]
sameq[div[e, c ^ 2], div[times[m, c ^ 2], c ^ 2]]
sameq[times[e, power[c ^ 2, -1]], times[times[m, c ^ 2], power[c ^ 2, -1]]]
sameq[times[e, power[c ^ 2, -1]], times[m, times[c ^ 2, power[c ^ 2, -1]]]]
sameq[div[e, c ^ 2], times[m, div[c ^ 2, c ^ 2]]]
sameq[div[e, c ^ 2], times[m, 1]]
sameq[div[e, c ^ 2], m]

    Let's see what happens when we let mathics evaluate the intermediate
    steps. We'll do that with a variation of "dump" called "eump" that replaces
    our inert operators "div", "times", and "power" with mathics's live
    versions. We'll have to use "ReplaceAllRepeated" in "eump", with syntax
    "//.", so we get every nested instance of our inert operators.
    "ReplaceAllRepeated" keeps applying the rules until nothing changes any more
    (technical jargon, "until normal-form is reached, via the 'confluence'
    property of conditional term rewriting, which is the evaluation algorithm
    of mathics"). We don't use it in inference rules because a substitution like
    x -> x + 1 will loop forever.

    You might be able to see that we are actually writing an evaluator for
    mathics expressions in mathics itself. Such a thing is called a
    'metacircular evaluator'. See https://goo.gl/VnyaiU.

 *************************************************************************** *)

ClearAll[erules]
erules = {div[a_, b_] -> a / b, times -> Times, power -> Power}

ClearAll[eump]
eump[e_] := (Print[{e //. erules, e}]; e)

ClearAll[newline]
newline[] := Print[""]

newline[]
Print["WITH MATHICS EVALUATION"]
(((((premise                                                   // eump  //
     divideBothSidesByNonZero [ #1, c^2 ] &                    // eump  //
     Map[divAsTimes, #1] &                                     // eump) /.
     sameq[a_, b_] -> sameq[a, leftTimesIsAssociativeLaw[b]] ) // eump) /.
     sameq[a_, times[b_, c_]] ->
         sameq[timesAsDiv[a], times[b, timesAsDiv[c]]]         // eump) /.
     sameq[a_, times[b_, c_]] ->
         sameq[a, times[b, cancelNonZeros[c]]]                 // eump) /.
     sameq[a_, b_] -> sameq[a, rightTimesUnitLaw[b]]           // eump
newline[]

(* ****************************************************************************

   Except for my manual indentation, you should see something like this;

WITH MATHICS EVALUATION
{sameq[e, c ^ 2 m],
  sameq[e, times[m, c ^ 2]]}
{sameq[e / c ^ 2, m],
  sameq[div[e, c ^ 2], div[times[m, c ^ 2], c ^ 2]]}
{sameq[e / c ^ 2, m],
  sameq[times[e, power[c ^ 2, -1]], times[times[m, c ^ 2], power[c ^ 2, -1]]]}
{sameq[e / c ^ 2, m],
  sameq[times[e, power[c ^ 2, -1]], times[m, times[c ^ 2, power[c ^ 2, -1]]]]}
{sameq[e / c ^ 2, m],
  sameq[div[e, c ^ 2], times[m, div[c ^ 2, c ^ 2]]]}
{sameq[e / c ^ 2, m],
  sameq[div[e, c ^ 2], times[m, 1]]}
{sameq[e / c ^ 2, m],
  sameq[div[e, c ^ 2], m]}

    Mathics takes away all our fun, proving the theorem right away in the second
    step. That's ok, we already knew that would happen, and we won't let it
    interfere with our pedantic, detailed proofs.

    Don't forget to clear out defined symbols. If you leave them in, you can get
    unwelcome surprises later, when you might think they're clean.

 *************************************************************************** *)

ClearAll[target, premise]

(* ****************************************************************************

    The first version of Leibniz's law / rule:

 *************************************************************************** *)

(* 1.5, page 12 *)
ClearAll[leibniz]
leibniz[ sameq[x_, y_], e_, z_ ] :=
    sameq[e /. {z -> x}, e /. {z -> y}]

expect [
    sameq [ d + b + 3,   d + c + 5 ],
    leibniz [ sameq [ b + 3,   c + 5 ],
              d + z,
              z ] ]

(*  Here is a cheat sheet of all the rules we've defined so far.
  ___ _             _     ___ _            _                __
 / __| |_  ___ __ _| |_  / __| |_  ___ ___| |_   ___ ___   / _|__ _ _ _
| (__| ' \/ -_) _` |  _| \__ \ ' \/ -_) -_)  _| (_-</ _ \ |  _/ _` | '_|
 \___|_||_\___\__,_|\__| |___/_||_\___\___|\__| /__/\___/ |_| \__,_|_|

    (* 1.1 *) substitutionInferenceRule[e_, v_:List, f_:List] :=
        Module[{ rules = MapThread[ Rule, {v, f} ] }, e /. rules ]
    (* 1.2 *) reflexivityLaw[x_] := sameq[x, x]
    (* 1.3 *) symmetryLaw[x_, y_] := sameq[ sameq[x, y], sameq[y, x] ]
    (* 1.4 *) transitivityLaw [ and [ sameq[x_, y_], sameq[y_, z_] ] ] :=
        sameq[x, z]
    (* 1.5 *) leibniz[ sameq[x_, y_], e_, z_ ] :=
        sameq[e /. {z -> x}, e /. {z -> y}]
    leftTimesIsAssociativeLaw              [
        times [ times [ a_, b_ ], c_ ]     ] :=
        times [ a,  times [ b,  c  ] ]
    rightTimesIsAssociativeLaw             [
        times [ a_, times [ b_, c_ ] ]     ] :=
        times [ times [ a,  b  ], c  ]
    leftTimesUnitLaw                       [
        times [ 1, m_ ]                    ] := m
    rightTimesUnitLaw                      [
        times [ m_, 1 ]                    ] := m
    divideBothSidesByNonZero               [
        sameq [ a_, b_ ],  d_              ] :=
        sameq [ div[a, d], div[b, d] ]
    cancelNonZeros                         [
        div [ a_, a_ ]                     ] := 1
    divAsTimes                             [
        div [ a_, b_ ]                     ] :=
        times [ a, power [ b, -1 ] ]
    timesAsDiv                             [
        times [ a_, power [ b_, -1 ] ]     ] :=
        div [ a, b ]

 *************************************************************************** *)

(* Section 1.4 Leibniz's rule and function evaluation, page 13
 _        _ _         _    _      ___      _                    _
| |   ___(_) |__ _ _ (_)__( )___ | _ \_  _| |___   __ _ _ _  __| |
| |__/ -_) | '_ \ ' \| |_ //|_ / |   / || | / -_) / _` | ' \/ _` |
|____\___|_|_.__/_||_|_/__| /__| |_|_\\_,_|_\___| \__,_|_||_\__,_|
 ___             _   _            ___          _           _   _
| __|  _ _ _  __| |_(_)___ _ _   | __|_ ____ _| |_  _ __ _| |_(_)___ _ _
| _| || | ' \/ _|  _| / _ \ ' \  | _|\ V / _` | | || / _` |  _| / _ \ ' \
|_| \_,_|_||_\__|\__|_\___/_||_| |___|\_/\__,_|_|\_,_\__,_|\__|_\___/_||_|

    Start at the bottom of page 13. A lot of this we can do without prose, now,
    because we're getting accustomed to the style.

    In the below, we modify "erules", adding a rule for "plus" and a rule for
    "apply". After that modification, erules doesn't mean the same thing as it
    does above. Modification of global variables is risky, in general, because
    it introduces global dependence on order of evaluation. If we accidentally
    use the new "erules" on expressions defined above, we could conceivably have
    a problem. Not in this case, because we didn't use "plus" and "apply" in
    above. However, in general, modifying things is "code smell" to be avoided.

    We introduce our first use of "RuleDelayed", syntax ":>". It isn't really
    necessary; "->" would work just as well as ":>" in this rule for "apply".
    But it's stylistically better because it makes ad-hoc rules more like named
    rules, which use "SetDelayed", syntax ":=". We bring it up here just to
    start getting used to it.

    We also use a Module to define the function "g" to avoid polluting the
    global namespace. A Module defines local names that vanish when the
    evaluator finishes with it.

 *************************************************************************** *)

Module [{ g = Function[z, plus[times[3, z], 6]] },
          apply[g, 5]
       ]

expect [ apply  [ Function[z, plus[times[3, z], 6]],
                  5
                ],

         Module [{ g = Function[z, plus[times[3, z], 6]] },
                   apply[g, 5]
                ]
       ]

erules = Join [ { plus -> Plus,                  (* a list of new rules ...  *)
                  apply[f_, a_] :> Apply[f, {a}] (* including this fancy     *)
                },                               (*   delayed one            *)
                erules                           (* to the old list of rules *)
              ]

expect [ 21,
    Module [{ g = Function[z, plus[times[3, z], 6]] },
              apply[g, 5]] //. erules
       ]

expect [ 3 (y + 2) + 6,
    Module[{ g = Function[z, plus[times[3, z], 6]] },
             apply[g, y+2]] //. erules
       ]

(* ****************************************************************************

    To write the functional version of Leibiz's law / rule, we don't need to
    delete (ClearAll) the old, substitution version. Mathics can distinguish the
    old version, which has three arguments, from the new version, which has two,
    when the rule is invoked. In some languages, multiple, distinguishable
    versions of the same symbol with the same names are called "overloads." Some
    languages distinguish the overload based on the types of arguments as well
    as on the numbers of arguments. So far, we're not using any kind of types in
    mathics, though mathics is capable of type analysis through its
    "conditional" term rewriting. All rules we've define so far are
    unconditional, with one exception. In defining "substitutionInferenceRule",
    we stipulated that the terms "f" and "v" must have type "List" (review the
    definition). We make greater use of conditions below. Mathic's conditional
    facility is very powerful, encompassing things like "dependent types,"
    beyond all but experimental research programming languages.

    We introduce a three-term overload for "apply", which was inert, in terms of
    substitution, following definition 1.7 on page 14 of the book. The existing,
    two-term usages above will not be affected.

    Be aware that this practice, of introducing new overloads and definitions is
    generally a code smell because it's modifying global state in a way that
    depends on order of evaluation. We're being careful, here, but you must be
    aware that subtle bugs often occur if terms prior to the new definition are
    changed.

 *************************************************************************** *)

(* 1.7, page 14 *)
ClearAll[apply]
apply[g_, z_, x_] := g /. {z :> x}

expect [
    plus [ times [ 3, 5 ], 6 ],
    apply [ plus[times[3, z], 6], z, 5 ] ]

expect [
    21,
    apply [ plus[times[3, z], 6], z, 5 ] //. erules ]

(* 1.8, page 14 *)
leibniz[ sameq[x_, y_], g_ ] := sameq [ apply [ g, x ], apply [ g, y ] ]

expect [
    sameq [ apply [f, 42], apply [f, times[6, 7]] ],
    leibniz [ sameq [ 42, times[6, 7] ], f ] ]

expect [
    sameq [ times[42, 42], 1764 ] //. erules,
    leibniz [ sameq [ 42, times[6, 7] ], Function[x, x * x] ] //. erules ]

(* Section 1.5, Reasoning with Leibniz's rule, INTENTIONALLY SKIPPED *)

(* Section 1.6, The assignment statement, page 16
   _          _                         _
  /_\   _____(_)__ _ _ _  _ __  ___ _ _| |_
 / _ \ (_-<_-< / _` | ' \| '  \/ -_) ' \  _|
/_/ \_\/__/__/_\__, |_||_|_|_|_\___|_||_\__|
               |___/

    Here we exhibit the first instance of a "type.". It's just a head and some
    args, for example,

        hoareTriple[precondition, statement, postcondition]

    If we don't have a named rule bound to the name "hoareTriple", then mathics
    can't reduce "hoareTriple" nor the entire expression. However, it may reduce
    the arguments "precondition", "statement", and "postcondition" by its usual
    aggressive, eager evaluation strategy. If we don't want them reduced, we
    must take care that they're inert.

    We can write rules like "isValid" that "dispatch on type" like Clojure
    multimethods or Python multimethods in my "locutius" package on PyPI.
    Because we simulate types with unbound heads, a multimethod looks like a
    bunch of definitions:

        isValid [ hoareTriple [p_, s_, q_] ] := blahblah
        isValid [ somethingElse [ x_, y_, z_ ] := yaketyyak

    Mathics stores these as separate overloads of "isValid" because they have
    different patterns, each requiring the literal head that's in evidence.

    The following is "multiple assignments" on page 19. We treat single
    assignments as a special case of multiple assignments at the cost of
    enclosing our variables in curly braces (List brackets).

 *************************************************************************** *)

ClearAll[assign]
assign[variables_:List, expressions_:List, postcondition_] :=
    hoareTriple [
        substitutionInferenceRule [
            postcondition,
            variables,
            expressions
        ],
        assignmentStatement[variables, expressions],
        postcondition ]

(* Examples, page 18 ******************************************************* *)

(* Notice we don't have to use inert forms of expressions when we know that
   mathics can't reduce what we write. We've been extra pedantic with inert
   forms, proving that (e === m c^2) === (e c^2 === m), because we didn't want
   mathics doing the proof for us. But there are certainly several places where
   we could have been more lax. *)

expect [
    hoareTriple [
        (* Consider a state in which the postcondition is false, say x = 5.
           Make the substitution specified by the assignment, namely
           x -> x + 1, on that postcondition, rendering truth. That is now
           a true precondition. *)
        x + 1 > 5,
        assignmentStatement [{x}, {x + 1}],
        x > 5
     ],
     assign [{x}, {x+1}, x > 5] ]

expect [
    hoareTriple [
        ne [ 5, 5 ], (* Here we don't want to say 5 != 5 because it will
                        reduce to mathics False. *)
        assignmentStatement [{x}, {5}],
        ne [ 5, 5 ]
    ],
    assign [{x}, {5}, ne[5, 5]]
]

expect [
    hoareTriple [
        x^2 > x^2 y,
        assignmentStatement [{x}, {x^2}],
        x > x y
    ],
    assign [{x}, {x^2}, x > x y]
]

(* Examples, page 20 ******************************************************* *)

expect [
    hoareTriple [
        y > x,
        assignmentStatement [{x, y}, {y, x}],
        x > y
    ],
    assign [{x, y}, {y, x}, x > y]
]

expect [
    hoareTriple [
        sameq [ x + i,   1 + 2 + ellipsis + (i + 1 - 1) ],
        assignmentStatement [{x, i}, {x+i, i+1}],
        sameq [ x,       1 + 2 + ellipsis + (i - 1) ]
    ],
    assign [{x, i}, {x+i, i+1}, sameq [ x, 1 + 2 + ellipsis + (i - 1) ] ]
]

expect [
    hoareTriple [
        sameq [ x + i,   1 + 2 + ellipsis + (i + 1 - 1) ],
        assignmentStatement [{x, i}, {x+i, i+1}],
        sameq [ x,       1 + 2 + ellipsis + (i - 1) ]
    ],
    assign [{x, i}, {x+i, i+1}, sameq [ x, 1 + 2 + ellipsis + (i - 1) ] ]
]

expect [
    hoareTriple [
        sameq [ x + i,   1 + 2 + ellipsis + (i + 1 - 1) ],
        assignmentStatement [{i, x}, {i+1, x+i}],
        sameq [ x,       1 + 2 + ellipsis + (i - 1) ]
    ],
    assign [{i, x}, {i+1, x+i}, sameq [ x, 1 + 2 + ellipsis + (i - 1) ] ]
]

(* A lot of these print out in an ugly way. Eventually, we'll write a
PrettyPrint. See https://goo.gl/4txWex *)

(* Exercises for Chapter 1, pages 21-23
 ___                _               ___ _        _
| __|_ _____ _ _ __(_)___ ___ ___  / __| |_     / |
| _|\ \ / -_) '_/ _| (_-</ -_|_-< | (__| ' \ _  | |
|___/_\_\___|_| \__|_/__/\___/__/  \___|_||_(_) |_|

 *************************************************************************** *)

(* Exercise 1.7(a) ************************************************************

   Here, we need a little chicanery to force mathics to do arithmetic inside our
   inert "sameq"; "Expand /@ blahblah" means "Map[Expand, blahblah]" and forces
   the arithmetic expander inside the "sameq" expression to yield our expected
   result. With out it, mathics would not reduce 4(x+2) and we get a bogusly
   failed example. None of this would be an issue if we were using our pedantic
   inert forms for the arithmetic.

 *************************************************************************** *)

expect [

    sameq [ 4x + y,   8 + 4x + y ], (* E[z:=X] === E[z:=Y] *)

    Expand /@ leibniz [
              sameq [ x,   x + 2 ], (* X === Y             *)
              4z + y,               (* E(z)                *)
              z ]                   (* z                   *)
]

(* Exercise 1.7(b) ********************************************************* *)

expect [

    sameq [ x + (2y+1) w,   x + 5w ], (* E[z:=X] === E[z:=Y] *)

    leibniz [
        sameq [ 2y + 1,   5 ],        (* X === Y             *)
        x + z w,                      (* E(z)                *)
        z ]                           (* z                   *)
]

(* Exercise 1.7(c) ********************************************************* *)

expect [

    sameq [ 3(x+1) + 3x + 1,   3y + 3(y-1) + 1 ], (* E[z:=X] === E[z:=Y] *)

    leibniz [
        sameq [ x + 1,   y ],                     (* X === Y             *)
        3z + 3(z-1) + 1,                          (* E(z)                *)
        z ]                                       (* z                   *)
]

(* Exercise 1.7(d) ********************************************************* *)

expect [

    sameq [ x + x,   x + y ],

    leibniz [
        sameq [ x,   y ],
        z + x,
        z ]
]

expect [

    sameq [ x + x,   y + y ],

    leibniz [
        sameq [ x,   y ],
        z + z,
        z ]
]

expect [

    sameq [ x + x,   y + x ],

    leibniz [
        sameq [ x,   y ],
        x + z,
        z ]
]

(* Exercise 1.7(e) ********************************************************* *)

expect [

    sameq [ 7x + 7y,   x (y+1) + y (y+1) ],

    leibniz [
        sameq [ 7,   y + 1],
        z x + z y,
        z ]
]

expect [

    Expand /@ sameq [ 7x + 7y,   x (y+1) + y (y+1) ],

    Expand /@ leibniz [
        sameq [ 7,   y + 1],
        z ( x + y ),
        z ]
]

expect [

    Expand /@ sameq [ 7x + 7y,   x (y+1) + y (y+1) ],

    Expand /@ leibniz [
        sameq [ 7,   y + 1],
        ( x + y ) z,
        z ]
]

(* Exercise 1.8 ************************************************************ *)

expect [

    sameq [ x + y + w,              (* E[z := X]                             *)
            b + c + y + w ],        (* E[z := Y] <~~~ answer to the exercise *)

    leibniz [
        sameq [ x, b + c ],         (* X === Y (hint)                        *)
        z + y + w,                  (* E(z) <~~~ part of the answer         *)
        z                           (*   z                                   *)
    ]
]

(* "Simplify' is an alternative to "Expand" (Mathematics produces different  *)
(* but equally correct results, here).                                       *)

expect [

    Simplify /@ sameq [
        x + y + w,                  (* E[z := X]                             *)
        x + 2(y + w) - b c          (* E[z := Y] <~~~ answer to the exercise *)
    ],
    Simplify /@ leibniz [
        sameq [ b c,                (* X <~~~ part of the hint               *)
                y + w               (* Y <~~~ part of the hint               *)
        ],
        z - b c + x + y + w,        (* E(z) <~~~ part of the answer          *)
        z                           (*   z                                   *)
    ]
]

expect [

    Simplify /@ sameq [
        x + y + w,                  (* E[z := X]                             *)
        x + 2(y + w) - b c          (* E[z := Y] <~~~ answer to the exercise *)
    ],
    Simplify /@ leibniz [
        sameq [ b c,                (* X <~~~ part of the hint               *)
                y + w               (* Y <~~~ part of the hint               *)
        ],
        z - b c + x + y + w,        (* E(z) <~~~ part of the answer          *)
        z                           (*   z                                   *)
    ]
]

expect [

    Simplify /@ sameq [
        x (x + y),                  (* E[z := X]                             *)
        x (y + x)                   (* E[z := Y] <~~~ answer to the exercise *)
    ],
    Simplify /@ leibniz [
        sameq [ x + y,              (* X <~~~ part of the hint               *)
                y + x               (* Y <~~~ part of the hint               *)
        ],
        x z,                        (* E(z) <~~~ part of the answer          *)
        z                           (*   z                                   *)
    ]
]

expect [

    Simplify /@ sameq [
        (x + y) w,                  (* E[z := X]                             *)
        x y (x + y)                 (* E[z := Y] <~~~ answer to the exercise *)
    ],
    Simplify /@ leibniz [
        sameq [ w,                  (* X <~~~ part of the hint               *)
                x y                 (* Y <~~~ part of the hint               *)
        ],
        (x + y) z,                  (* E(z) <~~~ part of the answer          *)
        z                           (*   z                                   *)
    ]
]

(* The next one is challenging. We will need the functional form of Leibniz.
   I am not going to do this one for you, but here is a hint:

       leibniz[
         sameq[
           plus[x,y],  (* Here is the problem; we need to pretend that     *)
           plus[y,x]   (* 'plus' is not commutative for this exercise      *)
         ],            (* to be meaningful, if I understand the authors.   *)
         plus[x_,y_] :> plus[y,x]  (* think of this as a function!         *)
       ] /. {apply[f_,a_] :> ReplaceAll[a,f]}  (* this is how to apply it! *)

   Note that RuleDelayed, :>, is necessary here. Rule, ->, won't do. This is the
   first time we have run across such a necessity.

   I'm leaving the rest of the exercises in Ch. 1 to you, the reader, also.
   You are advised to do them all!

*)

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

expect [
  True,
  And @@ Flatten @ Table[(x || (y && z)) === ((x || y) && (x || z)),
    {x, {True, False}}, {y, {True, False}}, {z, {True, False}}]
]

(* Section 2.3, Satisfiability, validity, and duality, page 31
 ___       _   _     __ _      _    _ _ _ _
/ __| __ _| |_(_)___/ _(_)__ _| |__(_) (_) |_ _  _
\__ \/ _` |  _| (_-<  _| / _` | '_ \ | | |  _| || |_
p|___/\__,_|\__|_/__/_| |_\__,_|_.__/_|_|_|\__|\_, ( )
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

    As an aside, the number of different functions from a set A to a set B is

        ||B||  **  ||A||  =def=  ||A -> B||

    where ||B|| is the size or cardinality or number-of-elements in set B, and
    ||A|| is the size of set A, and the set of functions is denoted A->B.

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

boolRules = {tconst[_] :> True, id -> Identity, not -> Not, fconst[_] :> False,
    true -> True, false -> False}

expect [

  { {True, True,  False, False},
    {True, False, True,  False} },

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
    rule for "not". Because we can't easily predict the order of application of
    the rules, we check that a and b each belong to the set {t, f}.

    One syntax for the conditional part of a rule is as follows

        fn   args           condition                 replacement
        -- -------- ------------------------- -----------------------------
        and[a_, b_] /; (boolQ[a] && boolQ[b]) :> If[(a===t)&&(b===t), t, f]

    It's safest to type-check all arguments on all functions, but there is a
    certain elegance to minimal type-checking, especially because the types are
    checked at run time in mathics and that's not free.

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

    We do a littlen massaging of the result with "Flatten", "Transpose", "Last"
    and "Partition" so that they can be compared directly with the table in the
    book. Remove that massaging if you want to see a more verbose output.

*)

binaryTruthTable =
  Table[{ToString[fn[a, b]], fn[a, b]},
    {fn, binaryFunctionList},
    {a, {t, f}}, {b, {t, f}}]

expect [
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
     {f, f, f, f}},

    Partition[Last @ Transpose @ Flatten[
        binaryTruthTable //. comparisonBoolRules, 2], 4]
]

(* Dual: Definition 2.2, page 31
 ___            _
|   \ _  _ __ _| |
| |) | || / _` | |
|___/ \_,_\__,_|_|

    The only interesting new feature of mathics illustrated here is the pattern
    variable in dual[head_[args__]]. The pattern head_[args__] matches something
    like and[p, q] with "head" bound to "and" and "{args}" bound to "{p, q}".
    Args is bound to Sequence[p, q], a special form for argument splicing; we
    don't need to get into that now. That rule for dual recursively dualizes the
    head and the args. Rewriting proceeds until things stop changing, so the
    recursion eventually bottoms out into one of the hard rules like dual[or].

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
   p," or "~(p /\ ~q)", which reduces, by de Morgan's laws, to "(~p \/ q)", or
   "or[not[p], q]".

   The dual of "implies[p, q]" is "(~p /\ q)", which is "~(p \/ ~q)" (by de
   Morgan), which is "not[because[p, q]]". So we can derive the rules above from
   the duals of "or" and "and", and that's why they're redundant.

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

   You must have numpy installed for the following to work in mathics.

*)

ClearAll[randomUnaryFunction, randomBinaryFunction, randomBooleanVariable]

randomUnaryFunction[] := First @ RandomChoice[{tconst, id, not, fconst}]

randomBinaryFunction[] := First @ RandomChoice[{ (* skip 'eq' and 'neq' *)
    tconst,  or,       because, fst,
    implies, snd,      eqv,     and,
    nand,    neqv,     nsnd,    nimplies,
    nfst,    nbecause, nor,     fconst}]

randomBooleanVariableOrConstant[] :=
    Symbol @ First @ RandomChoice[Characters["pqrsft"]]

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
"\[Implies]". This might render well on your screen if you have a good Unicode
font and UTF-8 encoding. Also, we're using mathics logical operators and
allowing them to reduce, rather than manipulating our own expressions. We'll do
that later. *)

expect [
True,

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

expect[ dualTheorem[true],                    not[false] ]
expect[ dualTheorem[or [p, true]],            not[and[p, false]] ]
expect[ dualTheorem[or [p, not[p]]],          not[and[p, not[p]]] ]

(* G&S slip a fast one on us, here, by implicitly reducing not[neqv[...]] to
eqv[...] and vice versa. Computers are dumb, and must be told exactly what to
do, so we need to tweak the output of "dualTheorem" with a ad-hoc rewrite rule.
We may end up, later, having to do some more gymnastics like this. *)

expect[ dualTheorem[eqv[true, true]] //. not[neqv[x_, y_]] :> eqv[x, y],
        eqv[false, false] ]

expect[ dualTheorem[eqv[or[p, q], or[q, p]]] //. not[neqv[x_, y_]] :> eqv[x, y],
        eqv[and[p, q], and[q, p]] ]

expect[ dualTheorem[eqv[eqv[p, q], eqv[q, p]]]
            //. not[neqv[x_, y_]] :> eqv[x, y],
        eqv[neqv[p, q], neqv[q, p]] ]

expect[ dualTheorem[eqv[not[or[p, q]], and[not[p], not[q]]]]
            //. not[neqv[x_, y_]] :> eqv[x, y],
        eqv[not[and[p, q]], or[not[p], not[q]]] ]

(* You do the exercises in chapter 2. *)


(* Chaper 3, Propositional Calculus, page 41 **********************************
 ___                      _ _   _               _
| _ \_ _ ___ _ __  ___ __(_) |_(_)___ _ _  __ _| |
|  _/ '_/ _ \ '_ \/ _ (_-< |  _| / _ \ ' \/ _` | |
|_| |_| \___/ .__/\___/__/_|\__|_\___/_||_\__,_|_|
            |_|
  ___      _         _
 / __|__ _| |__ _  _| |_  _ ___
| (__/ _` | / _| || | | || (_-<
 \___\__,_|_\__|\_,_|_|\_,_/__/
 *************************************************************************** *)

(*

   Equational logic, E

   We've written the laws leibniz, substitutionInferenceRule, and
   transitivityLaw a functions (from the cheat sheet above):

    (* 1.1 *) substitutionInferenceRule[e_, v_:List, f_:List] :=
        Module[{ rules = MapThread[ Rule, {v, f} ] }, output ]
    (* 1.4 *) transitivityLaw [ and [ sameq[x_, y_], sameq[y_, z_] ] ] :=
        sameq[x, z]
    (* 1.5 *) leibniz[ sameq[x_, y_], e_, z_ ] :=
        sameq[e /. {z -> x}, e /. {z -> y}]

   because we want to drive expressions from premises above the line to
   conclusions below the line, as in page 41.

   In Chapter 2, we introduced the inert symbol "eqv" in preference to the old
   "sameq" from Chapter 1. We'll write new versions of the three laws,
   shortening the names, to make following through Chapter 3 easier.

*)

(* Section 3.1, Preliminaries ********************************************** *)

(* new leibniz *)
leibniz[ eqv[x_, y_], e_, z_ ] := eqv[e /. {z -> x}, e /. {z -> y}]

(* new transitivity *)
transitivityLaw [ and [ eqv[x_, y_], eqv[y_, z_] ] ] := eqv[x, z]

(* new substitution *)
substitution[e_, v_:List, f_:List] := e /. MapThread [ Rule, {v, f} ]

(* Associativity of eqv *)
ClearAll[associativityAxiom]
associativityAxiom[p_, q_] := eqv[ eqv[ eqv[p, q], r ], eqv[ p, eqv[q, r] ] ]

(* Symmetry of eqv *)
ClearAll[symmetryAxiom]
symmetryAxiom[p_, q_] := eqv[ eqv[p, q], eqv[q, p] ]

(* Identity of eqv, page 44 *)
ClearAll[identityAxiom]
identityAxiom[q_] := eqv[ true, eqv [q, q] ]

(*

   NOTES

   The fact that we've parenthesized these in particular ways may get us into
   trouble.

   For any values of the pattern variables, the axioms yield static expressions,
   but the way they're used in proofs, they drive expressions from one form to
   another, that is, as rewrite rules, replacing expressions with equivalents.
   We will create those rewrite rules, named or unnamed as we go along. G&S is
   not terribly explicit about this point, but it's critical in undestanding how
   to encode proofs in mathics.

   We note, in passing, that we can automate the associativity and symmetry
   axioms with a couple of mathics Attributes: Flat and Orderless. Here is a
   dummy eqv that demonstrates this:

*)

ClearAll[deqv]
SetAttributes[deqv, {Flat, Orderless}]

expect [
    True,
    deqv[p, q] === deqv[q, p]
]

expect [
    True,
    deqv[ deqv[p, q], r ] === deqv[ p, deqv[q, r] ]
]

(*

   Checking this automation, however, requires us to use === instead of deqv
   itself, so we're not yet sure whether or how we want to use it.

*)

ClearAll[deqv]

(* Theorems, page 44 **********************************************************

   We invent a little more dumping machinery that prints out intermediate
   results in a chain of derivations with annotations a little closer, though
   nowhere near close enough, to the book. As we develop, we will slim this down
   and make it more palatable. However, it is functional for the moment. You
   don't need to understand how "fump" and "gump" work; just look at the
   printout in the console and convince yourself that the steps of the proof are
   being adequately presented.

 *************************************************************************** *)

ClearAll[fump, gump]
SetAttributes[fump, HoldAllComplete]
fump[x_] := (
    Print[ToString[Unevaluated[x]] <> " ~~>\n" <> ToString[x]];
    x)
gump[x_, r_] := (
    Print[ToString[x] <> " /. " <> ToString[r] <> " ~~>\n" <> ToString[x/.r]];
    x /. r)

expect [

  eqv[true, eqv[q, q]],

  fump[    identityAxiom[true] ]  //
  gump[#1, eqv[x_, y_] :> y    ]& //
  gump[#1, eqv[true, true] :> eqv[true, identityAxiom[q][[2]]]]&

]

(* ****************************************************************************
 _____ _          ___         _
|_   _| |_  ___  | __|_ _  __| |
  | | | ' \/ -_) | _|| ' \/ _` |
  |_| |_||_\___| |___|_||_\__,_|
 *************************************************************************** *)

(* We leave this at the very bottom so we can get a count of right and wrong
results during development. The final result of the script, no matter what its
intermediate state, is the result of the following tautology. *)

expect[ true, true ]
