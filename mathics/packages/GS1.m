(* ****************************************************************************

    Please see

    https://github.com/rebcabin/Mathics/blob/master/mathics/packages/

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
    "newton" and "pound-force" were not checked by machine. Like losing customer
    data in a cloud database because of an unanticipated edge-case thirty-five
    steps into a leader-election protocol.

    Fall in love with formal methods, please! They're related to static
    type-checking (that's a little formal method in your compiler, proving
    little theorems about types in your code), and great things like
    Clojure.spec (https://goo.gl/sttnFC). I think a lot of people know those are
    good, but there are lots of other, lesser-known formal methods like
    Statecharts (https://statecharts.github.io/) and TLA+
    (https://goo.gl/dx32Mw). Statecharts allowed me to formally prove that an
    embedded controller for a robot had no bugs. TLA+ saved Amazon's Dynamo DB a
    catastrophic failure (https://goo.gl/pTpZYT). Many mistakes have been found
    in published algorithms and protocols at the foundational layer of the
    internet and cloud computing when those protocols were subjected to formal
    methods (no citation).

 *************************************************************************** *)

<<"GS0.m"

(* Section 1.2 Textual substitution, page 8
 _____        _             _   ___      _       _   _ _        _   _
|_   _|____ _| |_ _  _ __ _| | / __|_  _| |__ __| |_(_) |_ _  _| |_(_)___ _ _
  | |/ -_) \ /  _| || / _` | | \__ \ || | '_ (_-<  _| |  _| || |  _| / _ \ ' \
  |_|\___/_\_\\__|\_,_\__,_|_| |___/\_,_|_.__/__/\__|_|\__|\_,_|\__|_\___/_||_|

    Mathics gives us direct syntax for Gries & Schneider's (G&S)'s "textual
    substitution:"

    Gries & Schneider         Us                        Expected Output
    ------------------------- ------------------------- ----------------------
    x[x := x + 2]             x /. {x -> x + 2}         x + 2
    (x+y)[x := z + 2]         x + y /. {x -> z + 2}     z + 2 + y
    (x*y)[x := z + 2]         x * y /. {x -> z + 2}     (z + 2) * y

    In our syntax, "target /. rules" means "ReplaceAll[target, rules]", where
    "target" is some expression like "x + y" and "rules" is a list of rules in
    curly braces like "{x -> x + 2}": that's a list of rules with one element in
    the list, the rule's being "x -> x + 2". The rule means

        please replace "x" with "x + 2" in the target.

    To test that we've got this working, load this here file, the one you're
    reading right now, into mathics. Let's say you've stored the file in
    "~/some/directory/GriesSchneider.m". Then, run mathics at the terminal and
    load the file; you should see approximately the following:

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
        ... more ...

    at the top of the output.

    Just what Gries and Schneider said we should see, only with a little
    rearranging because mathics knows that '+' and '*' are commutative. This
    rearranging won't bother us until later, when we make a way to control it.

 *************************************************************************** *)

Print[     x /. {x -> x + 2} ]

Print[ x + y /. {x -> z + 2} ]

Print[ x * y /. {x -> z + 2} ]

(* Bottom of page 8 in G&S: **************************************************

    Here's a case where the list of rules has more than one element. These rules
    are applied in parallel.

    Gries & Schneider         Us                        Expected Output
    ------------------------- ------------------------- ----------------------
    (z+y)[z,y := 5, 6]        z+y /. {z->5, y->6}       11

 *************************************************************************** *)

Print[ z+y /. {z->5, y->6} ]

(* ***************************************************************************

    Let's do a little tooling so we can write 'expected' and 'actual' in our
    examples. You don't need to understand how this works. You just need to know
    how to use it, and you'll see how in the examples that follow.

 *************************************************************************** *)

ClearAll[expect, totalRight, totalWrong, totalTests];
SetAttributes[ expect, HoldAllComplete ];
totalRight = totalWrong = totalTests = 0;
expect[expected_, actual_] := (* <~~~ Here's the API *)
   Module[{evalActualOnce = actual,
           evalExpectedOnce = expected},
      totalTests += 1;
      Print[ {"Test[" <> ToString[totalTests] <> "]:=\n",
              HoldForm[actual],
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
    mathics: all the following are the same: 2 * y, y * 2, 2 y, 2y. Notice that
    even in the PRINTING of the expressions, the stars are removed (despite my
    "HoldAllComplete" and "HoldForm", mathics always does a little
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
   global state of this session. We must clear out those assignment so that we
   can use the variables Q and x below without having to know or remember that
   we gave them values above. *)

ClearAll[Q, x]

(* At this point, Q and x have no values. In most programming languages, a
   variable with no value is an error. In mathics, however, a variable with no
   value is just itself: a symbolic constant. Mathics keeps rewriting
   expressions until they don't change any more. Integers evaluate to
   themselves, strings evaluate to themselves, and symbols that don't have
   values (or rules) attached to them evaluate to themselves. What does it mean
   to have a "value or rule" attached to a symbol? Having a value attached is
   just like assignment in ordinary programming languages: if we say

       x = 42

   then x has the value 42 attached to it, at least until we clear it:

       ClearAll[x]

   Now x has no value attached to it.

   A "rule" is mathics's way of defining rewrites. Rewrites are a lot like
   functions, but not exactly the same. We explain below bit-by-bit as we
   encounter rewriting rules. *)

(* Examples, box bottom of page 10: *******************************************

    We're getting comfortable with the mathics syntax for substitution, so I
    skip the English write-up.

 *************************************************************************** *)

expect[ 35, 35 /. {x -> 2} ] (* no change because "x" does not appear in "35"*)
expect[ y,   y /. {x -> 2} ] (* ditto *)
expect[ 2,   x /. {x -> 2} ] (* this time, x appears, so we replace it with 2*)

(* We can replace x inside complex expression like "x*x + y". *)

expect[ (c + y) * (c + y) + y,   (x*x + y) /. {x -> c + y} ]

(* Here's one broken over two lines because it's long: *)

expect[ (x + y)^2 + y^2 + (x + y)^3,

        (x^2 + y^2 + x^3)                  /. {x -> x + y} ]

(* Here are some more copied from the book. *)

expect[ z + w + w,      (x + y + y) /. {x -> z, y -> w}    ]
expect[ 2y + x z + x z, (x + y + y) /. {x -> 2y, y -> x z} ]
expect[ y + 2x,         (x + 2y)    /. {x -> y, y -> x}    ]
expect[ z + 2 x y,      (x + 2 y z) /. {x->z, y->x, z->y}  ]

(* Inference Rule Substitution (1.1), page 10
 ___       __                           ___      _     _
|_ _|_ _  / _|___ _ _ ___ _ _  __ ___  | _ \_  _| |___(_)
 | || ' \|  _/ -_) '_/ -_) ' \/ _/ -_) |   / || | / -_)_
|___|_||_|_| \___|_| \___|_||_\__\___| |_|_\\_,_|_\___(_)
 ___      _       _   _ _        _   _
/ __|_  _| |__ __| |_(_) |_ _  _| |_(_)___ _ _
\__ \ || | '_ (_-<  _| |  _| || |  _| / _ \ ' \
|___/\_,_|_.__/__/\__|_|\__|\_,_|\__|_\___/_||_|

    The inference rule 'Substitution' takes in a top-line expression e, a list
    of variables v, and a corresponding list of replacement expressions f, and
    spits out the bottom line conclusion e[v:=f].

                           E
        Substitution: -----------
                       E[v := F]

    (we shouldn't use capital letters as the first characters of names in
    mathics because the system defines many names beginning with capital
    letters. For example, E is defined as the Euler constant 2.71828... We shall
    take some liberties with G&S and write their formulas with small (uncial)
    letters).

    The following is the definition of this inference-rule scheme, followed by
    the example on page 11.

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

    To write assertions of equality, we have == and === in mathics, short for
    Equal and SameQ, respectively. Equal returns itself if the expressed
    equality is not true, whereas SameQ returns False:

 *************************************************************************** *)

expect[ x == y, x == y ] (* return the input expression because x != y       *)

expect[ False, x === y ] (* return False because x =!= y                     *)

(* ****************************************************************************

    Almost all the time we want the behavior of SameQ. Now we can apply the
    inference rule. You'll see a beautiful proof when you run this through
    mathics (or Mathematica). Notice again that mathics does a little
    rearranging, rewriting j + 5 as 5 + j. It's OK for now, but later we won't
    be able to assume commutativity.

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
    {Test[22]:=
         , inferenceRuleSubstitution[2 x / 2 === x, {x}, {j + 5}],
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
    built-in, hard-coded into mathics, which will apply the rules without
    notification. Common symbolic factors in division expressions are canceled
    without notification right in the middle of trying to prove cancellation;
    mathics just assumes they're non-zero! This fact, alone, renders mathics
    unsuitable as a rigorous theorem-prover on its own, but it's still very
    useful. We simply avoid the things we know to be risky, like automatic
    cancellation.

    An "evaluation leak" is an inadvertent early evaluation. Evaluation leaks
    might not affect the truth value of a theorem, say when we know on the side
    that some divisor is non-zero. But evaluation leaks always affect display of
    steps of a proof. Mathics applies built-in rules as soon as it can,
    interrupting our flow of human reasoning. We must control evaluation
    ourselves, watching our rules at work and putting them to work explicitly.

    We can control evaluation either with the zoo of Holds, or by not using
    built-ins, at least not until we want to. It's a trade-off: using the zoo of
    Holds, we can retain pretty infix syntax in expressions, but we risk
    evaluation leaks. Experience shows that even the pros get this wrong often
    (search "evaluation leak" on mathematica.stackexchange.com). Using the zoo
    also forces us to learn more than we want to learn about the evaluator (see
    https://goo.gl/L7Gz3h).

    On the other hand, if we avoid built-ins, we must write, for example, the
    inert expression div[2 (j+5), 2] instead of 2 (j + 5) / 2 to prevent early
    cancellation. Mathics doesn't have a definition for "div", so can't reduce
    the expression. The expression just evaluates to itself, and that's what we
    mean by "inert." We lose pretty syntax, at least until we do something like
    the following:

*************************************************************************** *)

expect[ j + 5,
        div[ 2 (j+5), 2 ] /. {div[a_, b_] -> a / b} ]

(* ****************************************************************************

    The mathics rule to the right of "/." has pattern variables "a_" and "b_".
    Mathics replaces them with "2 (j+5)" and "2", respectively, by matching the
    pattern expression

        div[a_, b_]

    against the target

        div[ 2 (j+5), 2 ]

    Unlike most programming languages, the underscore in mathics is not an
    ordinary character. It's syntax for "Blank[]", a pattern that can match any
    expression. Let's decode the syntax "a_" via "FullForm":

        In[2]:= FullForm[a_]
        Out[2]= Pattern[a, Blank[]]

    "FullForm" is useful for decoding any syntax in mathics: it will tell you
    the fundamental, bottom meaning of any expression. We see that "a_" is a
    pattern named "a" and able to match any expression, because that's what
    "Blank[]" does.

    The name "a" is saved in case the right-hand side of a rule refers to it. We
    explain this in more detail, below, when we discuss "Rule" and
    "RuleDelayed". For now, just think that the rule

        div[a_, b_] -> a / b

    means "match div[a_, b_], binding the symbols 'a' and 'b' to the actual
    values that appear in the target of the match."

    This is a new kind of rule we haven't seen above, but is very much like
    application of a function or lambda expression in Python, but not exactly
    the same. We must be aware of _when_ the right-hand side, "a / b", is
    evaluated.

    E X T E N D E D   E X P E R I M E N T

    Let's do an extended experiment with inert symbols instead of the zoo of
    Holds: sameq, plus, times, div, etc., replacing them explicitly with
    built-ins only when we want, using rules like "div" above. The purpose of
    the experiment is to see whether losing pretty syntax is worth avoiding
    struggles and bugs with evaluation leaks and the zoo of Holds.

    Let's first redo the substitution rule (G&S 1.1). During this experiment, we
    avoid the zoo of Holds, and avoid SetAttributes (https://goo.gl/Zt3KbB),
    upvalues (https://goo.gl/4bgm65), and more arcana. We used some of those in
    the prior definition of "inferenceRuleSubstitution", which we won't use any
    more.

 *************************************************************************** *)

ClearAll[substitutionInferenceRule];
substitutionInferenceRule[e_, v_:List, f_:List] :=
    Module[{ rules = MapThread[ Rule, {v, f} ] },  (* <~~~ don't use "With" *)
        Module [{ output = e /. rules },           (* <~~~ don't use "With" *)
            Print["inference: Substitution 1.1, page 11"];
            Print["premise:   " <> ToString[e]];
            Print["rules:     " <> ToString[rules]];
            Print["result:    " <> ToString[output]];
            output ] ]

expect[ sameq[ div[2(j+5), 2], j+5 ],
        substitutionInferenceRule[
            sameq[ div[2 x, 2], x],
            {x},
            {j + 5} ] ]

(* ****************************************************************************

    You should see something like this:

        inference: Substitution 1.1, page 11
        premise:   sameq[div[2 x, 2], x]
        rules:     {x -> 5 + j}
        result:    sameq[div[2 (5 + j), 2], 5 + j]
        {Test[24]:=
             , substitutionInferenceRule[sameq[div[2 x, 2], x], {x}, {j + 5}],
             expected, sameq[div[2 (j + 5), 2], j + 5],
             actual, sameq[div[2 (5 + j), 2], 5 + j],
             right?, True}

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

    Notice in the printout from this "expect" in Expression 25 that mathics
    aggressively rewrites "CompoundExpression[x, y, z]" as "x ; y ; z" despite
    the fact that "expect" has attribute "HoldAllComplete" and "HoldForm". This
    is an example of an evaluation leak that would be very time-consuming to
    fix. Mathematica does the same thing. We won't bother to fix it, because
    we're trying to make the following point:

    We may absentmindedly put a semicolon after a global definition or a
    "ClearAll". The presence or absence of such terminal semicolons does not
    make a difference at the global level, but it does inside expressions. Don't
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

        {Test[27]:=
            , transitivityLaw[and[sameq[x + y, w + 1], sameq[w + 1, 7]]],
            expected, sameq[x + y, 7],
            actual, sameq[x + y, 7],
            right?, True}

    A nice victory, with no evaluation drama.

    We now reproduce the theorem on page 4. This requires more machinery.
    Remember that all the laws above are hard-coded in the mathics evaluator, so
    we need to avoid triggering them. We supply our own rewrite rules for plus,
    times, sameq, and so on, leaving them inert symbolic expressions in
    lower-case until we want to reduce them by force; mathics doesn't know how
    to reduce such expressions until we tell it explicitly with rewrite rules.

    U N N A M E D   R U L E S

    We have two kinds of rewrite rules: named and unnamed. Many of the ones
    above are unnamed. Many such rules have the form "pattern -> result", for
    example "x -> 2y". The pattern is "x" and must match the target of the rule
    exactly. It's a pattern constant, not a pattern variable. We know that
    because there is no underscore; it's not "x_". The "target"" of such a rule
    is the left-hand side of a "/.", "ReplaceAll", or the left-hand side of a
    "//.", "ReplaceAllRepeated" expression. For example, in the expression

        x + z /. {x -> 2y}

    "x + z" is the target of the rule "x -> 2y". The result of evaluating the
    entire expression is "2y + z" because the "x" pattern in the rule exactly
    matches the "x" in the target "x + z". "FullForm" explains the syntax (here
    we must use a HoldForm to prevent the expression from being evaluated before
    it's passed to FullForm):

        In[8]:= FullForm[HoldForm[x + z /. {x -> 2y}]]

        Out[8]= ReplaceAll[Plus[x, z], List[Rule[x, Times[2, y]]]]

    The right-hand sides of "/." or a "//." must be either a rule or a list of
    rules. The curly braces put the rule "x -> 2y" in a list; "{x -> 2y}" is a
    list of rules, a list with one element, a singleton list. A slightly
    simpler, and equivalent, form, is

        In[70]:= FullForm[HoldForm[x + z /. x -> 2y]]

        Out[70]= ReplaceAll[Plus[x, z], Rule[x, Times[2, y]]]

    I find "x + z /. {x -> 2y}" to be more readable than "x + z /. x -> 2y", so
    I usually put singleton rules inside curly braces, even though it's not
    necessary. If there are multiple rules to apply in parallel, they must be
    enclosed in curly braces:

        In[72]:= FullForm[HoldForm[x + z /. {x -> 2y, z -> 3y}]]

        Out[72]= ReplaceAll[Plus[x, z], List[Rule[x, Times[2, y]],
                                             Rule[z, Times[3, y]]]]

    Another example of an unnamed rule is "div[a_, b_] -> a / b", where the
    pattern is "div[a_, b_]", containing pattern variables a_ and b_. Those
    variables match anything. When the rule is applied, the pattern variables
    are replaced on the right-hand side of the arrow by the things they match:

        In[73]:= div[x, y] /. div[a_, b_] -> a / b

        Out[73]= x / y

        In[74]:= div[42, 7] /. div[a_, b_] -> a / b

        Out[74]= 6

    "a -> b" is syntax for "Rule[a, b]". There is another kind of arrow, namely
    "a :> b", syntax for "RuleDelayed[a, b]", which means "don't evaluate the
    right-hand side of the rule now, only later, when we apply the rule."

    Sometimes, it doesn't matter which of the two you use. In the example
    "div[a_, b_] -> a / b", the right-hand side is evaluated when the rule
    itself is parsed, before it is applied to any target. Consider the following
    expression:

        div[1764, 42] /. {div[a_, b_] -> a / b}

    In a clean environment, where "a" and "b" have no definitions, evaluating
    the right-hand side of the rule, namely a / b, produces just a / b, itself,
    an expression in terms of symbolic constants that mathics cannot further
    reduce. The whole expression produces the expected "42". Mathics replaces
    the pattern variables "a_" and "b_" with actual arguments "1764" and "42",
    respectively, then evaluates the right-hand side "a / b" again.

        In[6]:= div[1764, 42] /. {div[a_, b_] -> a / b}

        Out[6]= 42

    If, however, "b" had been defined somewhere, we don't get what we expect:

        In[10]:= b = 6
        In[11]:= div[1764, 42] /. {div[a_, b_] -> a / b}

        Out[11]= 294

    That's because the right-hand side "a / b" is evaluated early, when In[11]
    is processed, and becomes "a / 6". At replacement time, "a_" and "b_" are
    replaced with "1764" and "42", as before, but, this time the right-hand side
    is a / 6; "b" no longer appears, it's been eaten up, and we get "1764 / 6"
    or "294".

    One way to fix this is to ClearAll the symbols "a" and "b" of the pattern
    variables "a_" and "b_" before evaluating the expression:

        In[14]:= b = 6
        In[15]:= ClearAll[a, b]
        In[16]:= div[1764, 42] /. {div[a_, b_] -> a / b}

        Out[16]= 42

    But this is ugly and risky --- "a" and "b" might be legitimately used
    somewhere else, and we just cleared them. In general, statements that have
    "non-local" effects make software "rigid" and "brittle," difficult to
    maintain and modify because any person doing a modification must have more
    than local knowledge to get things right.

    Much safer and prettier is a "RuleDelayed" or ":>". This works no matter
    what's been defined in the global environment, because the replacement of
    the pattern variables happens before evaluation of the right-hand side of
    the rule, not after evaluation of the right-hand side, as with "Rule" or
    "->":

        In[17]:= b = 6
        In[18]:= div[1764, 42] /. {div[a_, b_] :> a / b}

        Out[18]= 42

    Unnamed rules are usually "ad-hoc," created just to solve a problem that
    arises once in some computation, and are therefore not worth naming and
    saving away for other uses.

    N A M E D   R U L E S

    A named rule has the form "head[pattern] := result". Every place we used
    ":=", syntax for "SetDelayed", we defined a name, which becomes the "head"
    of the rule. For example, in "transitivityLaw[blahblah] := yaketyyak", the
    "head" is "transitivityLaw" and we invoke the rule as in
    "transitivityLaw[and[sameq[x+y, blahblah]]]". The rest of the rule works
    just like an unnamed rule with a "RuleDelayed", ":>" arrow.

    We could write some named rules as "head[pattern] = result", syntax for
    "Set", not using ":=", "SetDelayed". We would do this when we want the
    result (right-hand side) evaluated at definition time (now) instead of at
    application time (later), just as we can write some unnamed rules with "->"
    instead of with ":>". Using "=" when it's not necessary is bad style. Using
    ":=", syntax for "SetDelayed", instead of "=", even when it's not necessary,
    is good style because it sidesteps a bunch of early-evaluation drama.

    As a rule of thumb, if a rule, named or unnamed, has pattern variables, it's
    best to use the delayed forms ":=" and ":>", even if they're not strictly
    needed. That practice avoids any need for global omniscience of the global
    environment of variables and their values at definition time and at
    evaluation time on the part of the programmer.

    R E P L A C E A L L   V E R S U S   R E P L A C E A L L R E P E A T E D

    One more important fact must be emphasized: when named rules are applied,
    mathics keeps rewriting until nothing changes any more. That is the normal
    evaluation strategy. Sometimes, we want a single application of a rule, and
    for that, we must explicitly use "ReplaceAll".

    Consider the following:

        neqv[p_, q_] := not[eqv[p, q]]
        eqv[p_, q_] := not[neqv[p, q]]

    Mathics will rewrite an occurrence of neqv as not[eqv[...]], then rewrite
    the internal occurrence of eqv, producing not[not[neqv[...]]], and so on,
    forever:

        In[77]:= neqv[x, y]
        $RecursionLimit::reclim: Recursion depth of 200 exceeded.
        Out[77]= $Aborted

    We get around this by defining, instead of the above,

        neqvRule = neqv[p_, q_] :> not[eqv[p, q]]
        eqvRule  = eqv[p_, q_]  :> not[neqv[p, q]]

    retaining some benefits of naming. We apply the rule when _we_ want to, and
    only as many times as we want to:

        In[82]:= neqv[x, y] /. neqvRule
        Out[82]= not[eqv[x, y]]

    If we want to keep going without stopping, then we use "//.",
    "ReplaceAllRepeated".

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

expect[ target,
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

    "x // f" means "apply function (or rule) f to argument x"

    (rules act just like functions much of the time).

    It chains nicely (associates to the left), as we see below. That will make
    proofs look more like those in the book instead of like deeply nested
    function applications staircasing off to the right (https://goo.gl/MVBwaV).

        e[#1, blahblah] &

    means "a function (or rule) with body e[#1, blahblah], with slot #1 for the
    first argument."

 *************************************************************************** *)

expect[ divideBothSidesByNonZero [ premise, c^2 ],
        premise // divideBothSidesByNonZero [ #1, c^2 ] &
      ]

(* ****************************************************************************

    We still need a little parenthesizing to group up ad-hoc rules because
    "ReplaceAll" and "ReplaceAllRepeated" don't always associate well with
    postfix application "//". Here's the first shot at the theorem:

 *************************************************************************** *)

expect[

sameq [ div [ e, c^2 ], m ]
,
(premise                                                       //

     divideBothSidesByNonZero [ #1, c^2 ] &                    //

     Map[divAsTimes, #1] &                                   ) /. (* ad-hoc *)

     sameq[a_, b_] :> sameq[a, leftTimesIsAssociativeLaw[b]]   /.

     sameq[a_, times[b_, c_]] :>                                  (* ad-hoc *)
         sameq[timesAsDiv[a], times[b, timesAsDiv[c]]]         /.

     sameq[a_, times[b_, c_]] :>                                  (* ad-hoc *)
         sameq[a, times[b, cancelNonZeros[c]]]                 /.

     sameq[a_, b_] :> sameq[a, rightTimesUnitLaw[b]]              (* ad-hoc *)
]

(* ****************************************************************************

    You should see something like this (the original indentation is ugly; I
    reindented it):

      {Test[30]:=
        , (divAsTimes /@ #1&)[(divideBothSidesByNonZero[#1, c ^ 2]&)[premise]] /.
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
    expressions. We'll make some ways to mitigate that below. We want our proof
    assistant to search for opportunities to apply rules, but we don't want to
    get into dynamically generating ad-hoc rules. Rather, we want to write
    higher-order rules that search inside nested expressions for ways to apply
    named and ad-hoc rules.

    Let's add a little machinery to nicely display intermediate results so we
    get a display similar to but more detailed than the display on page 4. We
    need more parentheses, unfortunately.

 *************************************************************************** *)

ClearAll[dump]
dump[e_] := (Print[e]; e)

(((((premise                                                   // dump  //
     divideBothSidesByNonZero [ #1, c^2 ] &                    // dump  //
     Map[divAsTimes, #1] &                                     // dump) /.
     sameq[a_, b_] :> sameq[a, leftTimesIsAssociativeLaw[b]] ) // dump) /.
     sameq[a_, times[b_, c_]] :>
         sameq[timesAsDiv[a], times[b, timesAsDiv[c]]]         // dump) /.
     sameq[a_, times[b_, c_]] :>
         sameq[a, times[b, cancelNonZeros[c]]]                 // dump) /.
     sameq[a_, b_] :> sameq[a, rightTimesUnitLaw[b]]           // dump

(* ****************************************************************************

    That works --- you should see a pretty display like that below of all the
    steps. Make sure you understand both the input and output. These steps are
    not annotated. We'll fix that later. This proof is short enough that lack of
    annotation should not matter.

sameq[e, times[m, c ^ 2]]
sameq[div[e, c ^ 2], div[times[m, c ^ 2], c ^ 2]]
sameq[times[e, power[c ^ 2, -1]], times[times[m, c ^ 2], power[c ^ 2, -1]]]
sameq[times[e, power[c ^ 2, -1]], times[m, times[c ^ 2, power[c ^ 2, -1]]]]
sameq[div[e, c ^ 2], times[m, div[c ^ 2, c ^ 2]]]
sameq[div[e, c ^ 2], times[m, 1]]
sameq[div[e, c ^ 2], m]

    What happens when we let mathics evaluate the intermediate steps? Consider a
    variation of "dump" called "eump" that replaces our inert operators "div",
    "times", and "power" with mathics's live versions. We'll have to use
    "ReplaceAllRepeated" in "eump", with syntax "//.", so we get every nested
    instance of our inert operators. "ReplaceAllRepeated" keeps applying the
    rules until nothing changes any more. We don't use it in inference rules
    because a substitution like x -> x + 1 will loop forever.

    (Technical jargon, "ReplaceAllRepeated keeps apply the rules until 'normal
    form' is reached, via the 'confluence' property of conditional term
    rewriting, which is the evaluation algorithm of mathics")

    You might be able to see that we are actually writing an evaluator for
    mathics expressions in mathics itself. Such a thing is called a
    'metacircular evaluator'. See https://goo.gl/VnyaiU.

 *************************************************************************** *)

ClearAll[erules]
erules = {div[a_, b_] :> a / b, times -> Times, power -> Power}

ClearAll[eump]
eump[e_] := (Print[{e //. erules, e}]; e)

ClearAll[newline]
newline[] := Print[""]

newline[]
Print["WITH MATHICS EVALUATION"]
(((((premise                                                   // eump  //
     divideBothSidesByNonZero [ #1, c^2 ] &                    // eump  //
     Map[divAsTimes, #1] &                                     // eump) /.
     sameq[a_, b_] :> sameq[a, leftTimesIsAssociativeLaw[b]] ) // eump) /.
     sameq[a_, times[b_, c_]] :>
         sameq[timesAsDiv[a], times[b, timesAsDiv[c]]]         // eump) /.
     sameq[a_, times[b_, c_]] :>
         sameq[a, times[b, cancelNonZeros[c]]]                 // eump) /.
     sameq[a_, b_] :> sameq[a, rightTimesUnitLaw[b]]           // eump
newline[]

(* ****************************************************************************

   Except for my manual re-indentation, you should see something like this;

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

(* example *)

expect[
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

    In the below, we add a rule for "plus" and a rule for "apply" to "erules".

    After that modification, erules doesn't mean the same thing as it does
    above. Modification of global variables is risky, in general, because it
    introduces global dependence on order of evaluation. If we accidentally use
    the new "erules" on expressions defined above, we could conceivably have a
    problem. Not in this case, because we didn't use "plus" and "apply" above.
    However, in general, modifying things is "code smell" to be avoided.

    Notice "Module" for defining the function "g" locally, to avoid polluting
    the global namespace. A Module defines local names that vanish when the
    evaluator finishes with the Module.

    A mathics "Function" is a real lambda expression, as opposed to a rewrite
    rule that acts mostly like a lambda expression. Consider the rewrite rule:

        square[x_] := x * x

    This means "whenever you see an expression like 'square[42]', rewrite it as
    42 * 42, by (i) matching the pattern variable x_ to the actual argument 42,
    (ii) saving the value in a temporary variable named x, (iii) replacing x in
    the right-hand side x * x. Then, keep evaluating until nothing changes. That
    is, evaluate 42 * 42 getting 1764, then evaluate 1764 getting 1764, then
    stop."

    On the other hand,

        square = Function[x, x * x]

    means "bind the global name square to the Function (or lambda expression)
    that will return x * x when called on any actual argument, which will get
    bound to the parameter x". Note that the right-hand side of the assignment
    is evaluated _now_, at definition time, because we wrote "=" and not ":=".
    The latter would be silly because "Function[x, x * x]" would get
    re-evaluated every time we invoked "square".

    These two forms are invoked exactly the same way, with syntax like

        square[42]

    Most of the time, we can't tell the difference between a straightforward
    rewrite rule like the first "square" and a Function like the second
    "square". However, if we set up a complex example where rewriting has side
    effects, we could tell the difference. That's off-topic for now, but worth
    remembering for later.

 *************************************************************************** *)

Module [{ g = Function[z, plus[times[3, z], 6]] },
          apply[g, 5]
       ] (* the local definition of g vanishes here *)



expect[ apply  [ Function[z, plus[times[3, z], 6]],
                 5
               ]
        ,
        Module [{ g = Function[z, plus[times[3, z], 6]] },
                  apply[g, 5]
               ]
      ]



erules = Join [ { plus -> Plus,                  (* join a list of new rules *)
                  apply[f_, a_] :> Apply[f, {a}] (* ... including this fancy *)
                },                               (*   delayed one            *)
                erules                           (* to the old list of rules *)
              ]



expect[ 21,
    Module [{ g = Function[z, plus[times[3, z], 6]] },
              apply[g, 5]] //. erules
      ]



expect[ 3 (y + 2) + 6,
    Module[{ g = Function[z, plus[times[3, z], 6]] },
             apply[g, y+2]] //. erules
      ]

(* ****************************************************************************

    To write the functional version of Leibiz's law / rule, we don't need to
    delete (ClearAll) the old, substitution version. Mathics can distinguish the
    old version, which has three arguments, from the new version, which has two
    arguments, when the rule is invoked.

    In some languages, multiple, distinguishable versions of the same symbol
    with the same names are called "overloads." Some languages distinguish the
    overload based on the types of arguments as well as on the numbers of
    arguments. So far, we're not using any kind of types in mathics, though
    mathics is capable of type analysis through its "conditional" term
    rewriting. All rules we've define so far are unconditional, with one
    exception. In defining "substitutionInferenceRule", we stipulated that the
    terms "f" and "v" must have type "List" (review the definition). We make
    greater use of conditions below. Mathics's conditional facility is very
    powerful, encompassing things like "dependent types," which are beyond all
    other programming languages except for a few research languages like Agda.

    We introduce a three-term overload for "apply", which was inert, in terms of
    substitution, following definition 1.7 on page 14 of the book. The existing,
    two-term "apply" in "erules" above will not be affected.

    Be aware that this practice, of introducing new overloads and definitions is
    code smell. It is modifying global state in a way that depends on order of
    evaluation. We're being careful, here, but you must be aware that subtle
    bugs often occur if you try to evaluate terms above the change using the new
    definition. You will thank me someday for reiterating this warning.

 *************************************************************************** *)

(* 1.7, page 14 *)
ClearAll[apply]

(* g applied to X (g.X) is E[z := X] if g.z : E defines g of z. *)

apply[g_, z_, x_] := g /. {z :> x}



expect[
    plus [ times [ 3, 5 ], 6 ],
    apply [ plus[times[3, z], 6], z, 5 ]
]



expect[
    21,
    apply [ plus[times[3, z], 6], z, 5 ] //. erules
]



(* 1.8, page 14 *)

leibniz[ sameq[x_, y_], g_ ] := sameq [ apply [ g, x ], apply [ g, y ] ]



expect[
    sameq [ apply [f, 42], apply [f, times[6, 7]] ],
    leibniz [ sameq [ 42, times[6, 7] ], f ]
]



expect[
    sameq [ times[42, 42], 1764 ] //. erules,
    leibniz [ sameq [ 42, times[6, 7] ], Function[x, x * x] ] //. erules
]



(* Section 1.5, Reasoning with Leibniz's rule, ********************************

    G&S writes Leibniz as follows:

                X = Y
        ---------------------
        E[z := X] = E[z := Y]

    or, as

           X = Y
        -----------
        g(X) = g(Y)

    (I don't like G&S's  notation g.X for function application; I'll use "g(X)".")

    We write the same thing as follows

        leibniz [
            sameq[x_, y_], e_, z_ ] :=
            sameq[ e /. {z -> x}, e /. {z -> y} ]

    which is a rewrite rule that will drive an expression that matches the
    pattern above the line, namely

        sameq[x_, y_]                            (* X == Y *)

    to the expression below the line, namely

        sameq[ e /. {z -> x}, e /. {z -> y} ]    (* E[z := X] = E[z := Y] *)

    In a chain of reasoning, G&S write Leibniz like this:

            E[z := X]      (* Here is a premise *)
        = < X = Y >        (* Here is a "justification" in angle brackets *)
            E[z := Y]      (* Here is a conclusion *)

    which means that they're using Leibniz to rewrite an expression like

        E[z := X]

    into one like

        E[z := Y]

    Why? To drive (not derive) the proof forward. Without that rearrangement,
    Leibniz is just a dead statement of fact. To drive proofs in mathics, we
    need a version of Leibniz that just picks out E[z := Y] from the consequent
    below the line.

    First, we write a use of Leibniz like that at the bottom of page 14:

 *************************************************************************** *)

leibniz[ sameq[ m, 2j ],    (* premise, above the line; pattern-matching ... *)
                            (* against sameq[x_, y_], in lower case ...      *)
                                     (* ... instantiates x to m and y to 2j  *)
         sameq[div[z, 2], 2(j-1) ],  (* E[z] of Leibniz                      *)
         z ]                         (* the independent variable is z        *)

(* ****************************************************************************

   Now, we write a new version of Leibniz that Prints to the console an
   annotated proof fragment and returns only the right half of the conclusion,
   namely E[z := Y]. This new version has the same function signature (the same
   API) as the old "leibniz".

   Note the "[[1]]" and "[[2]]" syntax, shorthand for "Part", which retrieves
   the first and second part of any expression, respectively. In the case of
   "sameq[x, y]", the first part, "sameq[x, y][[1]]", is "x", and the second
   part, "sameq[x, y][[2]]", is "y". The zeroth part, "sameq[x, y][[0]]", is the
   "head", namely "sameq".

   In general, of course, "expr[[i]]" retrieves the i-th part of any expr.

 *************************************************************************** *)

ClearAll[leibnizE]
leibnizE[ premise:sameq[ x_, y_ ], e_, z_ ] :=
    Module[{conclusion = leibniz[premise, e, z]},
        Print["  E[z := X]: " <> ToString[conclusion[[1]]]];
        Print["=   <X = Y>: " <> ToString[premise]];
        Print["  E[z := Y]: " <> ToString[conclusion[[2]]]];
        conclusion[[2]]]



expect[
    sameq[div[2j, 2], 2(j-1)]
    ,
    leibnizE[sameq[m, 2j],             (* premise sameq[X, Y], by 0.1 *)
             sameq[div[z, 2], 2(j-1)], (* E(z)                        *)
             z]
]

(* ****************************************************************************

    You should see something like this on the console

          E[z := X]: sameq[div[m, 2], 2 (-1 + j)]
        =   <X = Y>: sameq[m, 2 j]
          E[z := Y]: sameq[div[2 j, 2], 2 (-1 + j)]
        Out[150]= sameq[div[2 j, 2], 2 (-1 + j)]

    To use "leibnizE" with substitution, as on page 15, use an ad-hoc rule:

 *************************************************************************** *)

expect[
    sameq[j, 2(j-1)]
    ,
    leibnizE[sameq[div[2x, 2], x     ] /. {x -> j}, (* premise               *)
             sameq[z,          2(j-1)],             (* E(z)                  *)
             z]                                     (* independent variable  *)
]

(* ****************************************************************************

    To use "leibniz" or "leibnizE", you need to supply an E(z) that converts the
    premise into something you want. The best way to figure out E(z) is to
    examine the inputs and the outputs of the deduction: G&S write, top of page
    15:

          2j/2 = 2(j-1)
        =   <(1.9), with x := j>
          j = 2(j-1)

    The middle line is clear: it's theorem 1.9, namely 2x/2 = x (written with
    inert "div", "sameq[div[2x,2],x])" with a substitution "x :> j". The middle
    line is really "2j/2 = j", and we mentally rewrite the fragment as

          2j/2 = 2(j-1)
        =   <2j/2 = j>
          j = 2(j-1)

    Remember the middle line is the premise and is always of the form "X = Y".
    Now we clearly see that X is 2j/2 and Y is j. The top line is X = 2(j-1);
    the bottom line is Y = 2(j-1). A suitable E(z), then, is z = 2(j-1), because
    the top line is always E(z)[z := X] and the bottom line is always
    E(z)[z := Y]. More concisely (using the functional variant of Leibniz), the
    top line is always E(X) and the bottom line is always E(Y). The output of
    "leibnizE" is E(Y) and we figure out an E(z) by looking at the premise, with
    or without a substitution. There is still a bunch of implicit "head math"
    going on; we will have to be more explicit below.

    That kind of thinking will be critical as you go through the exercises for
    Chapter 2. Watch out because E(z) is often not unique.

    Equivalences, theorem, axioms, and the like are permanent, immutable,
    eternal statements; there is no aspect of "change" --- no time dependence,
    no causality, no drive to rewrite one from the other.

    But we want to drive computations, to rewrite one expression into another,
    to prove consequences from premises. A rewrite rule has a preferred
    direction: it converts one expression X into another expression Y that
    happens to be equivalent but is closer in form to the objective. If we wish
    to go the other way, we need another rewrite rule. That's why computational
    machinery is more verbose than logical machinery: we often must write rules
    for each direction. The connection between a theorem and applications
    rewrite rules is as follows: A theorem in the propositional calculus is
    (from page 42)

    (i) an axiom

    (ii) the conclusion of a inference (rewrite) rule whose premises are
    theorems

    (iii) a boolean expression that, using the inference rules, is proved equal
    to an axiom or to a previously proved theorem.

 *************************************************************************** *)

(* Section 1.6, The assignment statement, page 16
   _          _                         _
  /_\   _____(_)__ _ _ _  _ __  ___ _ _| |_
 / _ \ (_-<_-< / _` | ' \| '  \/ -_) ' \  _|
/_/ \_\/__/__/_\__, |_||_|_|_|_\___|_||_\__|
               |___/

    Here we exhibit the first instance of a "type." It's just a head and some
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
   forms, proving that (e === m c^2) === (e / c^2 === m), because we didn't want
   mathics doing the proof for us. But there are certainly several places where
   we could have been more lax. *)



expect[
    hoareTriple [
        (* Consider a state in which the postcondition is false, say x = 5.
           Make the substitution specified by the assignment, namely
           x -> x + 1, on that postcondition, rendering truth. That is now
           a true precondition. *)
        x + 1 > 5,
        assignmentStatement [{x}, {x + 1}],
        x > 5
    ],
    assign [{x}, {x+1}, x > 5]
]



expect[
    hoareTriple [
        ne [ 5, 5 ], (* Here we don't want to say 5 != 5 because it will
                        reduce to mathics False. *)
        assignmentStatement [{x}, {5}],
        ne [ 5, 5 ]
    ]
    ,
    assign [{x}, {5}, ne[5, 5]]
]



expect[
    hoareTriple [
        x^2 > x^2 y,
        assignmentStatement [{x}, {x^2}],
        x > x y
    ]
    ,
    assign [{x}, {x^2}, x > x y]
]



(* Examples, page 20 ******************************************************* *)

expect[
    hoareTriple [
        y > x,
        assignmentStatement [{x, y}, {y, x}],
        x > y
    ]
    ,
    assign [{x, y}, {y, x}, x > y]
]



expect[
    hoareTriple [
        sameq [ x + i,   1 + 2 + ellipsis + (i + 1 - 1) ],
        assignmentStatement [{x, i}, {x+i, i+1}],
        sameq [ x,       1 + 2 + ellipsis + (i - 1) ]
    ]
    ,
    assign [{x, i}, {x+i, i+1}, sameq [ x, 1 + 2 + ellipsis + (i - 1) ] ]
]



expect[
    hoareTriple [
        sameq [ x + i,   1 + 2 + ellipsis + (i + 1 - 1) ],
        assignmentStatement [{x, i}, {x+i, i+1}],
        sameq [ x,       1 + 2 + ellipsis + (i - 1) ]
    ]
    ,
    assign [{x, i}, {x+i, i+1}, sameq [ x, 1 + 2 + ellipsis + (i - 1) ] ]
]



expect [
    hoareTriple [
        sameq [ x + i,   1 + 2 + ellipsis + (i + 1 - 1) ],
        assignmentStatement [{i, x}, {i+1, x+i}],
        sameq [ x,       1 + 2 + ellipsis + (i - 1) ]
    ]
    ,
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

   Here, we need to force mathics to do arithmetic inside our inert "sameq";
   "Expand /@ blahblah" means "Map[Expand, blahblah]" and forces "Expand", the
   arithmetic expander, inside the "sameq" expression. Without it, mathics would
   not reduce 4(x+2) and we get a bogusly failed example. None of this would be
   an issue if we were using our pedantic inert forms for the arithmetic.

 *************************************************************************** *)

expect[
    sameq [ 4x + y,   8 + 4x + y ]  (* E[z:=X] === E[z:=Y] *)
    ,
    Expand /@ leibniz [
              sameq [ x,   x + 2 ], (* X === Y             *)
              4z + y,               (* E(z)                *)
              z ]                   (* z                   *)
]

(* Exercise 1.7(b) ********************************************************* *)

expect[
    sameq [ x + (2y+1) w,   x + 5w ]  (* E[z:=X] === E[z:=Y] *)
    ,
    leibniz [
        sameq [ 2y + 1,   5 ],        (* X === Y             *)
        x + z w,                      (* E(z)                *)
        z ]                           (* z                   *)
]

(* Exercise 1.7(c) ********************************************************* *)

expect[
    sameq [ 3(x+1) + 3x + 1,   3y + 3(y-1) + 1 ]  (* E[z:=X] === E[z:=Y] *)
    ,
    leibniz [
        sameq [ x + 1,   y ],                     (* X === Y             *)
        3z + 3(z-1) + 1,                          (* E(z)                *)
        z ]                                       (* z                   *)
]

(* Exercise 1.7(d) ********************************************************* *)

expect[
    sameq [ x + x,   x + y ]
    ,
    leibniz [
        sameq [ x,   y ],
        z + x,
        z ]
]



expect[
    sameq [ x + x,   y + y ]
    ,
    leibniz [
        sameq [ x,   y ],
        z + z,
        z ]
]



expect[
    sameq [ x + x,   y + x ]
    ,
    leibniz [
        sameq [ x,   y ],
        x + z,
        z ]
]

(* Exercise 1.7(e) ********************************************************* *)

expect[
    sameq [ 7x + 7y,   x (y+1) + y (y+1) ]
    ,
    leibniz [
        sameq [ 7,   y + 1],
        z x + z y,
        z ]
]



expect[
    Expand /@ sameq [ 7x + 7y,   x (y+1) + y (y+1) ]
    ,
    Expand /@ leibniz [
        sameq [ 7,   y + 1],
        z ( x + y ),
        z ]
]



expect[
    Expand /@ sameq [ 7x + 7y,   x (y+1) + y (y+1) ]
    ,
    Expand /@ leibniz [
        sameq [ 7,   y + 1],
        ( x + y ) z,
        z ]
]

(* Exercise 1.8 ************************************************************ *)

expect[
    sameq [ x + y + w,              (* E[z := X]                             *)
            b + c + y + w ]         (* E[z := Y] <~~~ answer to the exercise *)
    ,
    leibniz [
        sameq [ x, b + c ],         (* X === Y (hint)                        *)
        z + y + w,                  (* E(z) <~~~ part of the answer         *)
        z                           (*   z                                   *)
    ]
]

(* "Simplify' is an alternative to "Expand" (Mathematics produces different  *)
(* but equally correct results, here).                                       *)

expect[
    Simplify /@ sameq [
        x + y + w,                  (* E[z := X]                             *)
        x + 2(y + w) - b c          (* E[z := Y] <~~~ answer to the exercise *)
    ]
    ,
    Simplify /@ leibniz [
        sameq [ b c,                (* X <~~~ part of the hint               *)
                y + w               (* Y <~~~ part of the hint               *)
        ],
        z - b c + x + y + w,        (* E(z) <~~~ part of the answer          *)
        z                           (*   z                                   *)
    ]
]

expect[
    Simplify /@ sameq [
        x + y + w,                  (* E[z := X]                             *)
        x + 2(y + w) - b c          (* E[z := Y] <~~~ answer to the exercise *)
    ]
    ,
    Simplify /@ leibniz [
        sameq [ b c,                (* X <~~~ part of the hint               *)
                y + w               (* Y <~~~ part of the hint               *)
        ],
        z - b c + x + y + w,        (* E(z) <~~~ part of the answer          *)
        z                           (*   z                                   *)
    ]
]

expect[
    Simplify /@ sameq [
        x (x + y),                  (* E[z := X]                             *)
        x (y + x)                   (* E[z := Y] <~~~ answer to the exercise *)
    ]
    ,
    Simplify /@ leibniz [
        sameq [ x + y,              (* X <~~~ part of the hint               *)
                y + x               (* Y <~~~ part of the hint               *)
        ],
        x z,                        (* E(z) <~~~ part of the answer          *)
        z                           (*   z                                   *)
    ]
]

expect[
    Simplify /@ sameq [
        (x + y) w,                  (* E[z := X]                             *)
        x y (x + y)                 (* E[z := Y] <~~~ answer to the exercise *)
    ]
    ,
    Simplify /@ leibniz [
        sameq [ w,                  (* X <~~~ part of the hint               *)
                x y                 (* Y <~~~ part of the hint               *)
        ],
        (x + y) z,                  (* E(z) <~~~ part of the answer          *)
        z                           (*   z                                   *)
    ]
]
