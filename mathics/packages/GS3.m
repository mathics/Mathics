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

(* ****************************************************************************

   Equational logic, E

   We've written the laws leibniz, substitutionInferenceRule, and
   transitivityLaw as rewrite rules (from the cheat sheet in GS1.m):

    (* 1.1 *) substitutionInferenceRule[e_, v_:List, f_:List] :=
        Module[{ rules = MapThread[ Rule, {v, f} ] }, e /. rules  ]

    (* 1.4 *) transitivityLaw [
        and [ sameq[x_, y_], sameq[y_, z_] ]     ] :=
        sameq[x, z]

    (* 1.5 *) leibniz[ sameq[x_, y_], e_, z_     ] :=
        sameq[e /. {z -> x}, e /. {z -> y}]

   because we want evaluation to drive expressions in a particular direction:
   from premises above the line to conclusions below the line, as on page 41.

   In Chapter 2, we introduced the inert symbol "eqv" to replace the old "sameq"
   from Chapter 1. We now write new versions of the three laws in terms of
   "eqv", also shortening the names, to make Chapter 3 easier.

   ClearAll symbols before redefining them to avoid surprises debugging
   lingering, prior definitions. This is cheap paranoia.

 *************************************************************************** *)

<<"GS0.m"

(* Section 3.1, Preliminaries ********************************************** *)
(* ___          ___       _               _
  / _ \_______ / (_)_ _  (_)__  ___ _____(_)__ ___
 / ___/ __/ -_) / /  ' \/ / _ \/ _ `/ __/ / -_|_-<
/_/  /_/  \__/_/_/_/_/_/_/_//_/\_,_/_/ /_/\__/___/
 *)
ClearAll[eqv]
ClearAll[leibniz]

(* 1.5 Leibniz : convert an eqv of X and Y to an eqv of E[z:=X] and E[z:=Y],
   where E is an arbitrary expression. Replaces the old leibniz, which used
   "sameq" instead of "eqv". Otherwise the same. *)

leibniz[ eqv[x_, y_], e_, z_ ] :=
        ((* Print[{"leibniz", "x", x, "y", y, "e", e,
                "conclusion", eqv[e /. {z -> x}, e /. {z -> y}]}]; *)
         eqv[e /. {z -> x}, e /. {z -> y}])

(*

   Instrumented version of Leibniz that prints a trace. Works best with a binary
   "e", presumably an "eqv". Informally and in practical application, uses the
   premise X=Y to drive E[z:=X] to E[z:=Y].

 *)
ClearAll[leibnizE]
leibnizE[ premise:eqv[ x_, y_ ], e_, z_ ] :=
        Module[{conclusion = leibniz[premise, e, z]},
               Print[{"leibnizE:","x", x, "y", y}];
               Print["  E(z)     : " <> ToString[e]];
               Print["  E[z := X]: " <> ToString[conclusion[[1]]]];
               Print["=   <X = Y>: " <> ToString[premise]];
               Print["  E[z := Y]: " <> ToString[conclusion[[2]]]];
               conclusion[[2]]]

ClearAll[transitivity]
transitivity[ and [ eqv[x_, y_], eqv[y_, z_] ] ] := eqv[x, z]

ClearAll[substitution]
substitution[e_, v_:List, f_:List] := e /. MapThread [ Rule, {v, f} ]

(* Section 3.2, Equivalence and true *************************************** *)

(* ____          _           __                   ____      __
  / __/__ ___ __(_)  _____ _/ /__ ___  _______   / __/___  / /_______ _____
 / _// _ `/ // / / |/ / _ `/ / -_) _ \/ __/ -_)  > _/_ _/ / __/ __/ // / -_)
/___/\_, /\_,_/_/|___/\_,_/_/\__/_//_/\__/\__/  |_____/   \__/_/  \_,_/\__/
      /_/
 *)

(* (3.1) Axiom, Associativity of eqv; both directions (computers are dumb) *)

ClearAll[associativity]
associativity[eqv[ eqv[p_, q_], r_ ]] := eqv[ p, eqv[q, r] ]
associativity[eqv[ p_, eqv[q_, r_] ]] := eqv[ eqv[p, q], r ]

(* (3.2) Axiom, Symmetry of eqv *)
(* p === q === q === p *)

ClearAll[symmetryOfEqv]
symmetryOfEqv[eqv[p_, q_]] := eqv[q, p]

(* (3.3) Axiom, Identity of eqv, page 44 *)
(* true === q === q *)

ClearAll[identity]
identity[eqv[q_, q_]] := eqv[true, eqv[q, q]]

(*

   Note that we can automate the associativity and symmetry axioms with mathics
   Attributes: Flat and Orderless. Here is a dummy eqv that demonstrates this.

*)

ClearAll[deqv]
SetAttributes[deqv, {Flat, Orderless}]

expect[
    True,
    deqv[p, q] === deqv[q, p]
]

expect[
    True,
    deqv[ deqv[p, q], r ] === deqv[ p, deqv[q, r] ]
]

(*

   Checking this automation, however, requires === instead of deqv itself, so
   it's delegating too much to mathics and spoiling our fun (for now). We'll
   revisit later when the work of constantly explicitly applying associativity
   and symmetry becomes annoying.

*)

ClearAll[deqv]

(* Theorems, pages 43-44 ******************************************************

   Need a little more display machinery that prints out annotations. That brings
   us a little closer, though nowhere near close enough, to the book. As we
   develop, we will slim this down and make it more palatable. You don't need to
   understand how the display machinery works.

   Remember that "Reasoning with Leibniz," Section 1.5, page 14, allows us to
   "replace equals with equals" via the rewrite rule "leibnizE."

 *************************************************************************** *)

ClearAll[fump]
SetAttributes[fump, HoldAllComplete]
fump[e_] := (
    Print[ToString[Unevaluated[e]] <> " ~~>\n" <> ToString[e]];
    e)
dump[annotation_, e_] := (
    Print[annotation <> " ~~> ", e];
    e)

expect[ eqv[eqv[p, q, q], p]
,
Module[{proposition = eqv[p, eqv[p, q, q]]}, (* the prop. I want to prove *)
  proposition                          // fump                  //
  symmetryOfEqv[#1]&                   // dump["symmetryOfEqv", #1]& //
  leibnizE[#1, eqv[p, z], z]&          //
  leibnizE[proposition, eqv[p, z], z]& //
  symmetryOfEqv[#1]&                   // dump["symmetryOfEqv", #1]&
]]

(* ****************************************************************************

   A few notes about this proof are necessary. First, there are many ways to
   parenthesize the Axiom of Symmetry into binary uses of eqv, i.e., "===":

       (p === q) === (q === p)    (* presumably G&S's intention *)
       p === (q === (q === p))
       p === ((q === q) === p)
       (p === (q === q)) === p
       ((p === q) === q) === p

       eqv[ eqv[p, q], eqv[q, p] ]
       eqv[ p, eqv[ q, eqv[q, p] ] ]
       eqv[ p, eqv[ eqv[q, q], p ] ]
       eqv[ eqv[ p, eqv[q, q] ], p ]
       eqv[ eqv[ eqv[p, q], q ], p ]

   The number of ways is the Catalan number C_3, https://goo.gl/cNExhR, which is
   five https://goo.gl/rzbfwD, https://goo.gl/b2ZXVE, so I got them all. These
   numbers increase quickly with the number of terms in an expression, but
   that's not the hazard for us. If each associativity step of a theorem has
   five branches, then our proof will have at least 5^s independent threads,
   where s is the number of associative steps. That's bad.

   They're all equivalent by the Axiom of Associativity, 3.1, which precedes
   Symmetry, 3.2. G&S use them freely and implicitly in the proof at the bottom
   of page 43. They even say

       "Associativity allows us to be informal and insert or delete pairs of
       parenthesis in sequences of equivalences, just as we do with sequences of
       additions"

   That's not good enough for us right now because we're doing explicit, formal
   calculations. Instead, we will pick one particular parenthesization per step
   and be satisfied at the end that we got them all.

   In this case, we'll be willing, mentally, to accept eqv[p, eqv[p, q, q]] as
   the statement of the proposition we're trying to prove, and
   eqv[eqv[p, q, q], p] as the particular parenthization of the Axiom of
   Symmetry we reduce to, that is, if we're satisfied that if we can reduce

       eqv[p, eqv[p, q, q]]

   to

       eqv[eqv[p, q, q], p],

   then we have proved the theorem eqv[p, eqv[p, q, q]] by reducing it to an
   axiom, one of the ways of proving theorem listed at the top of page 42.

   That leaves open the question of the meaning of eqv[a, b, c], a ternary eqv,
   which we have not defined. We define it now to mean eqv[eqv[a, b], c] or
   eqv[a, eqv[b, c]], but we won't explicitly reduce it that way because we
   don't want to invoke "or" for this proof. If you're willing to go with this
   definition, then we may proceed.

   Before explaining the proof line-by-line, we note that simply one invocation
   of symmetry suffices, namely

       symmetryOfEqv[eqv[p, eqv[p, q, q]]] ~~> eqv[eqv[p, q, q], p]

   but G&S invoke symmetry twice, with an intermediate step of eqv[p, p], and
   we're going to take a dirty road to get there with two invocations of
   symmetry and two invocations of leibnizE.

   First, we feed our proposition through symmetry, failing to notice that we're
   done:

       proposition$10423 ~~>  (* ignore the dollar sign and numbers *)
                 eqv[p, eqv[p, q, q]]
       symmetryOfEqv ~~> eqv[eqv[p, q, q], p]

   We then use leibnizE, with X = eqv[p, q, q] and Y = p, to produce the
   intermediate form:

       {leibnizE:, x, eqv[p, q, q], y, p}
         E(z)     : eqv[p, z]
         E[z := X]: eqv[p, eqv[p, q, q]]
       =   <X = Y>: eqv[eqv[p, q, q], p]    <~~~ THAT'S AN AXIOM
         E[z := Y]: eqv[p, p]

   We then feed the proposition once more through leibnizE, this time with X = p
   and Y = eqv[p, q, q], noting mentally, informally that our intermediate form
   eqv[p, p] is present (and eaten up) as E[z := X]:

       {leibnizE:, x, p, y, eqv[p, q, q]}
         E(z)     : eqv[p, z]
         E[z := X]: eqv[p, p]
       =   <X = Y>: eqv[p, eqv[p, q, q]]
         E[z := Y]: eqv[p, eqv[p, q, q]]

   That produces our proposition, again, but no mind, we're trying to follow the
   book, and G&S say we must do two applications of symmetry and two
   applications of leibniz. Finally, we end with our final application of
   symmetry:

       symmetryOfEqv ~~> eqv[eqv[p, q, q], p]
       Out[207]= eqv[eqv[p, q, q], p]

   and you should see something like that on your console.

   Here is the whole proof again, laid out slightly differently:

 *************************************************************************** *)

expect[ eqv[eqv[p, q, q], p]
      ,
        Module[{proposition = eqv[p, eqv[p, q, q]]},
               (* the prop. I want to prove *)
               proposition
               // fump                              //
               symmetryOfEqv[#1]&
               // dump["symmetryOfEqv", #1]&        //
               leibnizE[#1, eqv[p, z], z]&          //
               leibnizE[proposition, eqv[p, z], z]& //
               symmetryOfEqv[#1]&
               // dump["symmetryOfEqv", #1]&
        ]
]

(* ****************************************************************************

   That's more than a little round-about, but at least we avoided an explosion
   of rules for associativity of binary combinations. That need may come back to
   haunt us, and we may get into Attribute Flat, as we did with "deqv" above.

 *************************************************************************** *)


(* (3.4) Theorem, _true_ *************************************************** *)

Module[{proposition = true},
       proposition
       // fump //
       identity[eqv[#1, #1]]&       (* eqv[true, eqv[true, true]] *)
       // dump["identity", #1]& //
       leibnizE[#1, z, z]&      //  (* eqv[true, true]            *)
       leibnizE[identity[eqv[q, q]], eqv[true, z], z]&
(*
      E(z)     : eqv[true, z]
      E[z := X]: eqv[true, true]
    =   <X = Y>: eqv[true, eqv[q, q]]    <~~~ PREMISE = identity[eqv[q, q]]
      E[z := Y]: eqv[true, eqv[q, q]]    <~~~ reduced to axiom of identity

*)
]

(* ****************************************************************************

   While the above accomplishes the proof in our minds, it's not a full
   calculation inside mathics, not fully formal, because of the line

       leibnizE[identity[eqv[q, q]], eqv[true, z], z]&

   That line doesn't actually depend on the preceding line. We constructed
   E[z:=x] as eqv[true, true] in our minds and mentally wrote it out as the
   hidden E[z:=x] branch of eqv[true, z]. We need a new version of Leibniz that
   takes E[z:=x], E(z) and <x=y>, and constructs for us E[z:=y]. We want to say

       eqv[true, true]        Hey, Leibniz, that's E[z:=x]
       eqv[true, eqv[q, q]]   Hey, Leibniz, that's x === y
       eqv[true, z]           Hey, Leibniz, that's E(z)
                              Hey, Leibniz, what's E[z:=y]?

   Our previous versions of Leibniz internally calculated E[z:=x], but we want
   to compare a previous line in the proof with that internal calculation. We'll
   do that with a call to mathics built-in "SameQ". We may later need "MatchQ"
   if we go meta, but not for now. We'll tighten up the display a bit, too.

   This is a new, four-term overload of leibniz.

 *************************************************************************** *)



ClearAll[leibnizF]
leibnizF[ eXForZcheck_, premise:eqv[ x_, y_ ], e_, z_ ] :=
        Module[{conclusion = leibniz[premise, e, z]},
               If[ Not[SameQ[eXForZcheck, conclusion[[1]]]],
                   Print["LEIBNIZ CHECK FAIL: "
                         <> ToString[conclusion[[1]]]
                         <> " is supposed to be the same as "
                         <> ToString[eXForZcheck] ] ]
                   Print[{"leibnizF:", "x", x, "y", y}];
                   Print["  E(z)   : " <> ToString[e]];
                   Print["  E[z:=X]: " <> ToString[conclusion[[1]]]];
                   Print["=   <X=Y>: " <> ToString[premise]];
                   Print["  E[z:=Y]: " <> ToString[conclusion[[2]]]];
                   conclusion[[2]]]



(* (3.4) Theorem, _true_ *************************************************** *)

expect[ (* Reduce the proposition to the Axiom of Identity.                  *)
        identity[eqv[q, q]]     (*    true === q === q                       *)
        ,
        Module[{proposition = true},

               (* The proposition is "true":                                 *)
               proposition                                           // fump //

               (* Instantiate the Axiom of Identity with the proposition:    *)
               (* producing eqv[true, eqv[true, true]]                       *)
               identity[eqv[#1, #1]]&               // dump["identity", #1]& //

               (* Use Leibniz to pick out the second term, eqv[true, true]:  *)
               leibnizE[#1, z, z]&                                           //

               (* Use Leibniz again with a known truth, "identity", the      *)
               (* deduction so far, eqv[true, true], and a crafted           *)
               (* E(z) = eqv[true, z] such that eqv[q, q] is subbed for z.   *)
               (* This reduces to an instance of identity so we're done.     *)
               leibnizF[#1, identity[eqv[q, q]], eqv[true, z], z]&
        ]
]

(* ****************************************************************************

   Now we see the deduction convincingly in the output:

       proposition$1466 ~~> true
       identity ~~> eqv[true, eqv[true, true]]
       {leibnizE:, x, true, y, eqv[true, true]}
         E(z)     : z
         E[z := X]: true
       =   <X = Y>: eqv[true, eqv[true, true]]
         E[z := Y]: eqv[true, true]
       {leibnizF:, x, true, y, eqv[q, q]}
         E(z)   : eqv[true, z]
         E[z:=X]: eqv[true, true]
       =   <X=Y>: eqv[true, eqv[q, q]]
         E[z:=Y]: eqv[true, eqv[q, q]]

   but the input is not obvious enough. We'd like to write intermediate
   expectations, something like this:

       Module[{proposition = true},

         (* The proposition is "true": *)

         proposition
         // expectI[true] //

         (* Instantiate the Axiom of Identity with the proposition: *)
         (* producing eqv[true, eqv[true, true]]                    *)

         identity[eqv[#1, #1]]&
         // expectBy[eqv[true, eqv[true, true]], "identity"] //

         (* Use Leibniz to pick out the second term, eqv[true, true]: *)

         leibnizE[#1, z, z]&
         // expectBy[eqv[true, true], "leibniz"] //

         (* Use Leibniz again with a known truth, "identity", the    *)
         (* deduction so far, eqv[true, true], and a crafted         *)
         (* E(z) = eqv[true, z] such that eqv[q, q] is subbed for z. *)
         (* This reduces to an instance of identity so we're done.   *)

         leibnizF[#1, identity[eqv[q, q]], eqv[true, z], z]&
         // expectBy[identity[eqv[q, q]], "leibniz"]
       ]

   You don't need to understand how expectI and expectBy work, but you need to
   know that they return Functions that are invoked by the "//" postfix
   function-application operator.

 *************************************************************************** *)



ClearAll[expectI]
expectI[expected_] :=
        Function[
                actual, (* Notice parameter of Function is not a pattern     *)
                        (* variable, that is, not "actual_".                 *)
                Print["expected: " <> ToString[expected]
                      <> "\nactual: "<> ToString[actual] ];
                If[Not[SameQ[expected, actual]],
                   Print["\nFAILED EXPECTATION"]];
                actual ]



ClearAll[expectBy]
expectBy[expected_, by_] :=
        Function[
                actual, (* Notice parameter of Function is not a pattern     *)
                        (* variable, that is, not "actual_".                 *)
                Print["expected: " <> ToString[expected]
                      <> "\nactual: "<> ToString[actual]
                      <> "\n by " <> ToString[by]];
                If[Not[SameQ[expected, actual]],
                   Print["\nFAILED EXPECTATION"]];
                actual ]



(* (3.4) Theorem, _true_ ( new proof ) ************************************* *)

expect[ identity[eqv[q, q]] (* true === q === q *)
        ,
        Module[{proposition = true},

               (* The proposition is "true":                                 *)

               proposition
               // expectI[true] //

               (* Instantiate the Axiom of Identity with the proposition:    *)

               identity[eqv[#1, #1]]&

               (* Notice how we say explicitly in code what we expect, not just
                  in comments *)

               // expectBy[eqv[true, eqv[true, true]], "identity"] //

               (* Use Leibniz to pick out the second term, eqv[true, true]:  *)

               leibnizE[#1, z, z]&

               // expectBy[eqv[true, true], "leibniz"] //

               (* Use Leibniz again with a known truth, "identity", the      *)
               (* deduction so far, eqv[true, true], and a crafted           *)
               (* E(z) = eqv[true, z] such that eqv[q, q] is subbed for z.   *)
               (* This reduces to an instance of identity so we're done.     *)

               leibnizF[#1, identity[eqv[q, q]], eqv[true, z], z]&

               // expectBy[identity[eqv[q, q]], "leibniz"]
        ] ]



(* ****************************************************************************

   By writing our new "expectI" and "expectBy" as rewrite rules that return
   Functions (that print by side effect), we've gotten rid of some "#1" and "&"
   syntax. We can use the same idea to improve our mechanization of the
   preliminaries in section 3.1. However, without a significant foray into
   metaprogramming (functions or rules that return functions or rules or even
   install functions and rules into the global environment), we can't preserve
   the pattern-matching that makes them powerful, so we will skip that for now.

 *************************************************************************** *)

(* (3.5) Theorem, Reflexivity of eqv: eqv[p, p] **************************** *)

expect[ identity[eqv[p, p]]     (* eqv[true, eqv[p, p]] *)
        ,
        Module[{proposition = eqv[p, p]},

               proposition

               // expectI[eqv[p, p]] //

               substitution[#1, {p}, {true}]&

               // expectBy[eqv[true, true], "substitution"] //

               (* Ues Leibniz again with a known truth, "identity", the      *)
               (* deduction so far, eqv[true, true], and a crafted           *)
               (* E(z) = eqv[true, z] such that eqv[p, p] is subbed for z.   *)
               (* This reduces to an instance of identity so we're done.     *)

               leibnizF[#1, identity[eqv[p, p]], eqv[true, z], z]&

               // expectBy[identity[eqv[p, p]], "leibniz"]
        ] ]

(* ****************************************************************************

   Let's re-do the theorem on page 43 using our new instrumentation:

 *************************************************************************** *)

expect[
        eqv[eqv[p, q, q], p]
        ,
        Module[{proposition = eqv[p, eqv[p, q, q]]},

               proposition

               // expectI[eqv[p, eqv[p, q, q]]] //

               symmetryOfEqv[#1]&

               // expectBy[eqv[eqv[p, q, q], p], "symmetryOfEqv"] //

               leibnizE[#1, eqv[p, z], z]&

               // expectBy[eqv[p, p], "leibniz"] //

               leibnizF[#1, proposition, eqv[p, z], z]&

               // expectBy[eqv[p, eqv[p, q, q]], "leibniz"] //

               symmetryOfEqv[#1]&

               // expectBy[eqv[eqv[p, q, q], p], "symmetryOfEqv"]
        ]]

(* ****************************************************************************

   We can see that the last call of leibnizF depends on TWO inputs, the derived
   fact eqv[p, p] and the proposition. This is much better formal mechanization
   than just carrying one of the inputs in our minds.

   Now, we're going to give up on explicit uses and choices of associativity by
   setting the Flat Attribute on eqv. This is OK because G&S are going
   "informal" about it (presumably they know about Catalan explosion, without
   saying so). We don't have to go informal! But we have to trust mathics to "do
   the right thing." We will trust _and_ verify.

   In the first shot, we prove the theorem by reducing ppqq to pqqp in one step
   of symmetry. We noticed that before; this time, we go with it.

 *************************************************************************** *)

SetAttributes[eqv, Flat]

expect[ eqv[eqv[p, q, q], p]
        ,
        Module[{proposition = eqv[p, eqv[p, q, q]]},

               proposition

               // expectI[eqv[p, eqv[p, q, q]]] //

               symmetryOfEqv[#1]&

               // expectBy[eqv[eqv[p, q, q], p], "symmetryOfEqv"]
        ]]

(* ****************************************************************************

   Because of the new Flat Attribute, this is what we get:

       expected: eqv[p, p, q, q]
                actual: eqv[p, p, q, q]
       expected: eqv[p, q, q, p]
                actual: eqv[p, q, q, p]
                 by symmetryOfEqv

   Pretty cool. What if we continue with our old, redundant proof?

       proposition

       // expectI[eqv[p, eqv[p, q, q]]] //

       symmetryOfEqv[#1]&

       // expectBy[eqv[eqv[p, q, q], p], "symmetryOfEqv"] //

       leibnizE[#1, eqv[p, z], z]&

       // expectBy[eqv[p, p], "leibniz"]

   Bad stuff:

       {leibnizE:, x, p, y, eqv[q, q, p]}
         E(z)     : eqv[p, z]
         E[z := X]: p
       =   <X = Y>: eqv[p, q, q, p]
         E[z := Y]: p
       expected: eqv[p, p]
                actual: p
                 by leibniz
                FAILED EXPECTATION

   This happened because we fed eqv[p, q, q, p] as the premise into Leibniz. Our
   implementation, leibniz[ eqv[x_, y_], e_, z_ ] := ..., pattern-matches x to p
   and y to eqv[q, q, p]

       In[81]:= eqv[p, q, q, p] /. eqv[x_, y_] :> {x, y}
       Out[81]= {p, eqv[q, q, p]}

   Good thing we decided to trust AND verify.
    _      _____   ___  _  _______  _______
   | | /| / / _ | / _ \/ |/ /  _/ |/ / ___/
   | |/ |/ / __ |/ , _/    // //    / (_ /
   |__/|__/_/ |_/_/|_/_/|_/___/_/|_/\___/

   This is a bug in mathics; it should produce {eqv[p], eqv[q, q, p]}; see
   https://github.com/mathics/Mathics/issues/747. The bug does not affect the
   current analysis, but be aware that we depart from Mathematica whenever we
   use pattern-matching with Flat symbols; Mathematica will produce different
   results. (UPDATE: we now believe that this bug concerns mathics's treatment
   of attribute "OneIdentity". Check with the issue above for the most
   up-to-date progress on the bug))

   What's going wrong?

   The next step in "leibniz" is to construct the conclusion

       with x = p and y = eqv[q, q, p]:

       eqv[ e         /. {z -> x}, e       /. {z -> y} ]
         ~~>
       eqv[ eqv[p, z] /. {z -> x}, e[p, z] /. {z -> y} ]
         ~~>
       eqv[ eqv[p, p],             eqv[p, eqv[q, q, p]]]
         ~~>
       eqv[p, p, p, q, q, p]

   because eqv is now "Flat". Conclusion[[1]] (inside the implementation of
   leibnizF) is "p"; conclusion[[2]] (also inside the implementation of
   leibnizF) is "p", just as we see in the output from the instrumented leibnizE
   above.

   OK, what's wrong and what can we do about it? Well, it's not really "wrong."
   The old leibniz and leibnizE just weren't designed for flat eqv. We'll need
   to be smarter. The good news is that automating associativity has forced out
   the redundant extra steps in the old G&S proof, the steps that caused us some
   angst above.

 *************************************************************************** *)

(* (3.7) Metatheorem. Any two theorems are equivalent (eqv), page 45 **********

   A theorem is (i) an axiom, (ii) the conclusion of an inference rule whose
   premises are [previously proved] theorems, (iii) a boolean expression that,
   using the inference rules, is proved equal to an axiom or [to] a previously
   proved theorem.

   INFORMAL PROOF OF THE METATHEOREM

   By the "Proof Technique" 3.6, a theorem P is proved by reducing it to a known
   theorem Q via a sequence of steps. Each step is an assertion of equivalence
   (eqv).

   The right-hand version of the technique on page 45 begins with the
   proposition "true", which equivales "P === P" by the Axiom of Identity 3.3.
   This proposition, "P === P", is converted to "P === Q" by the same
   equivalence steps in the left-hand version. Because "true" is a theorem and
   equivales both "P" and "Q", which are arbitrary, all theorems are equivalent
   to "true".

 *************************************************************************** *)

(* Section 3.3, Negation, inequivalence, and false ************************* *)

(* _  __              __  _
  / |/ /__ ___ ____ _/ /_(_)__  ___
 /    / -_) _ `/ _ `/ __/ / _ \/ _ \_
/_/|_/\__/\_, /\_,_/\__/_/\___/_//_( )
         /___/                     |/
   ____                   _           __                     ____
  /  _/__  ___ ___ ___ __(_)  _____ _/ /__ ___  _______     / __/___
 _/ // _ \/ -_) _ `/ // / / |/ / _ `/ / -_) _ \/ __/ -_)    > _/_ _/
/___/_//_/\__/\_, /\_,_/_/|___/\_,_/_/\__/_//_/\__/\__( )  |_____/
               /_/                                    |/
   ___     __
  / _/__ _/ /__ ___
 / _/ _ `/ (_-</ -_)
/_/ \_,_/_/___/\__/
 *)

(* (3.8) Axiom, Definition of "false", page 45 ***************************** *)

ClearAll[false, falseRule, falseDef]
falseRule = {false -> not[true]}
falseDef  = eqv[false, not[true]]

(* (3.9) Axiom, Distributivity of "not" over "eqv" ****************************

   By (3.2) Symmetry of eqv, we can write distributivity in both directions. If
   we wrote these two directions as global, named, multi-shot rewrite rules ---
   the foo[pat] := rhs syntax --- the evaluator, which implicitly applies
   "ReplaceRepeated", would bounce not[eqv[p, q]] and eqv[not[p, q]] back and
   forth forever because the evaluator keeps rewriting until nothing changes. To
   prevent that infinite bouncing, we'll represent these two directions as
   one-shot rules that we must apply explicitly with "ReplaceAll", just one
   time, as needed.

 *************************************************************************** *)

ClearAll[notRule, invNotRule]
notRule    = (not[eqv[p_, q_]] :> eqv[not[p], q])
invNotRule = (eqv[not[p_], q_] :> not[eqv[p, q]])

(* (3.10) Axiom, Definition of "neqv" ************************************** *)

ClearAll[neqv, neqvRule, invNeqvRule]
neqvRule    = neqv[p_, q_]     :> not[eqv[p, q]]
invNeqvRule = not[eqv[p_, q_]] :> neqv[p, q]

(* (3.11) Unnamed theorem ************************************************** *)

expect[ eqv[not[q], p]
        ,
        Module[{proposition =  eqv[not[p], q]},

               (((proposition
                  // expectI[  eqv[not[p], q]   ]) /.

                invNotRule)

                // expectBy[   not[eqv[p, q]]   , "invNotRule"] //

                symmetryOfEqv /@ #1 &

                // expectBy[   not[eqv[q, p]]   , "internal symmetryOfEqv"]) /.

                notRule

               // expectBy[   eqv[not[q], p]   , "notRule"]
        ] ]

(* ****************************************************************************

   We needed an ad-hoc "wheel-hash-ampersand" function to shove symmetry inside
   the outer "not". We also needed some parentheses to ensure proper nesting of
   alternating postfix application "//" and "ReplaceAll", "/.". We can mitigate
   excess parentheses with ad-hoc functions wrapping the rules:

 *************************************************************************** *)

(* (3.11) Unnamed theorem (reduced parentheses) **************************** *)

expect[ eqv[not[q], p]
        ,
        Module[{proposition = eqv[not[p], q]},

               proposition

               // expectI[    eqv[not[p], q]   ] //

               #1 /. invNotRule &

               // expectBy[   not[eqv[p, q]]   , "invNotRule"] //

               symmetryOfEqv /@ #1 &

               // expectBy[   not[eqv[q, p]]   , "internal symmetryOfEqv"] //

               #1 /. notRule &

               // expectBy[   eqv[not[q], p]   , "notRule"]
        ] ]

(* ****************************************************************************

   That's much neater and cleaner. It's also much closer to what we'll need when
   we start constructing proofs automatically. So far, all we're doing is
   checking proofs; we're still constructing the proofs by hand. Yes, that's a
   teaser, but we have a long way to go before we can construct proofs
   automatically.

 *************************************************************************** *)

(* (3.12) Double negation: ****************************************************

   We found the old leibniz wasn't suited to a flat eqv when there are more than
   two arguments. What did we use leibniz for, most of the time? Picking out the
   other side of an eqv with true. For instance:

        In[18]:= leibnizF[true, eqv[true, p], z, z]
        {leibnizF:, x, true, y, p}
          E(z)   : z
          E[z:=X]: true
        =   <X=Y>: eqv[true, p]
          E[z:=Y]: p
        Out[18]= p

   For now, we'll just turn off the flatness of eqv. We prove theorem 3.12 by
   reducing it to "true", already a theorem. This proof corresponds to the
   right-hand column on the bottom of page 44.

 *************************************************************************** *)

ClearAll[eqv]                   (* Turn off the flatness for now. *)

expect[ true
    ,
    Module[{proposition = eqv[not[not[p]], p]}, (* double negation *)

       proposition

       // expectI[    eqv[not[not[p]], p]           ] //

       #1 /. invNotRule &

       // expectBy[   not[eqv[not[p], p]]           , "invNotRule"] //

       symmetryOfEqv /@ #1 &

       // expectBy[   not[eqv[p, not[p]]]           , "internal symmetryOfEqv"] //

       #1 /. notRule &

       // expectBy[   eqv[not[p], not[p]]           , "notRule"] //

       identity

       // expectBy[   eqv[true, eqv[not[p], not[p]]], "identity"] //

       symmetryOfEqv

       // expectBy[   eqv[eqv[not[p], not[p]], true], "symmetryOfEqv"] //

       leibnizF[eqv[not[p], not[p]], #1, z, z] & (* ugly; pick first *)

       // expectBy[   true                          , "leibniz"]
    ] ]

(* ****************************************************************************

   I haven't found a proof doing straight conversion, as in the left-hand column
   at the bottom of page 44.

   The laste leibnizF is still ugly because it required the human to notice the
   premise eqv[not[p], not[p]]. We're going to have to face this, soon, with
   another reworking of formal Leibniz.

   Having proved the theorem, we may now enshrine it in a rule. Later, when we
   construct proofs automatically, we'll use metaprogramming tricks to install
   rules automatically for proved theorems.

 *************************************************************************** *)

ClearAll[doubleNegation]
doubleNegation[not[not[p_]]] := p;

(* (3.13) Negation of false ************************************************ *)

expect[ true                    (* reduce not[false] to true *)
      ,
        Module[{proposition = not[false]}, (* then it will be a theorem *)

               proposition /. falseRule

               // expectBy[    not[not[true]]    , "axiom def. of false"] //

               doubleNegation

               // expectBy[    true              , "double negation"]
        ]
]

(* (3.14) Unnamed Theorem eqv[ neqv[p, q], eqv[ not[p], q] ] *************** *)
(* Transform neqv[p, q] to eqv[not[p], q]                                    *)

expect[ eqv[not[p], q]          (* produce this at the end *)
      ,
        Module[{proposition = neqv[p, q]},

               proposition /. neqvRule

               // expectBy[    not[eqv[p, q]]    , "def. of neqv"] //

               #1 /. notRule &

               // expectBy[    eqv[not[p], q]    , "notRule"]
        ]
]

(* (3.15) Unnamed Theorem eqv[ eqv[ not[p], p ], false] ******************** *)

expect[ false
      ,
        Module[{proposition = eqv[not[p], p]},

               proposition

               // expectI[    eqv[not[p], p]    ] //

               #1 /. invNotRule &

               // expectBy[   not[eqv[p, p]]               , "invNotRule"] //

               identity /@ #1 & (* line 3, internally at level 1 *)

               // expectBy[   not[eqv[true, eqv[p, p]]]    , "identity"] //

               symmetryOfEqv /@ #1 & (* line 4, internally at level 1 *)

               // expectBy[   not[eqv[eqv[p, p], true]]    , "symmetryOfEqv"] //

               (leibnizF[eqv[p, p], #1, z, z]&) /@ #1 & (* line 5 *)

               // expectBy[   not[true]                    , "leibniz"] //

               leibnizF[#1, symmetryOfEqv[falseDef], z, z] &

               // expectBy[   false    , "leibniz(symmetryOfEqv(falseDef))"]
        ]
]

(* ****************************************************************************

   Our uses of Leibniz are still only partially formal. We, as humans, are still
   providing one of the inputs to each invocation. I continue to nag about this
   until we understand how to do better.

   The proof of 3.15 introduced several new structures. First, we've seen the
   pattern of "identity, symmetry, leibniz" before, in the proof of 3.12, double
   negation. This pattern introduces an equivalence to "true" in the first
   position, i.e., converts eqv[p, p] into eqv[true, eqv[p, p]], then swaps true
   into the second position, i.e., to generate eqv[eqv[p, p], true], then
   finally extracts the "true" via Leibniz, demonstrating that eqv[p, p] reduces
   to true. In this proof, of 3.15, we use that pattern _inside_ a not, and
   that's by _mapping_ identity, symmetry, and leibniz on lines 3, 4, and 5
   above. Finally, we apply leibniz with a premise of symmetry[falseDef] to
   reduce eqv[not[p], p] to false.

   We capture the pattern of "identity, symmetry, leibniz" in a "lemma
   generator," extractTrue (looking ahead to page 53). This lemma generator
   encapsulates and hides one of our partially informal uses of Leibniz.
   Externally, its use is entirely formal. Such encapsulation and hiding is a
   justified formalization of the previously informal.

 *************************************************************************** *)

ClearAll[extractTrue]
extractTrue[eqv[p_, p_]] :=
        Module[{proposition = eqv[p, p]},

               proposition

               // expectBy[    eqv[p, p], "(lemma) prop"] //

               identity

               // expectBy[   eqv[true, eqv[p, p]], "(lemma) identity"] //

               symmetryOfEqv

               // expectBy[   eqv[eqv[p, p], true], "(lemma) symmetryOfEqv"] //

               leibnizF[eqv[p, p], #1, z, z] &

               // expectBy[   true                , "(lemma) leibniz"]
        ]

(* ****************************************************************************

   Now, a new proof of 3.12:

 *************************************************************************** *)

expect[ true
      ,
        Module[{proposition = eqv[not[not[p]], p]},

               proposition

               // expectI[    eqv[not[not[p]], p]] //

               #1 /. invNotRule &

               // expectBy[   not[eqv[not[p], p]], "invNotRule"] //

               symmetryOfEqv /@ #1 &

               // expectBy[   not[eqv[p, not[p]]], "internal symmetryOfEqv"] //

               #1 /. notRule &

               // expectBy[   eqv[not[p], not[p]], "notRule"] //

               extractTrue
        ]
]

(* ****************************************************************************

   and an alternative proof for 3.15:

 *************************************************************************** *)

expect[ false
      ,
        Module[{proposition = eqv[not[p], p]},

               proposition

               // expectI[    eqv[not[p], p]    ] //

               #1 /. invNotRule &

               // expectBy[   not[eqv[p, p]]    , "invNotRule"] //

               extractTrue /@ #1 & (* replaces original lines 3, 4, and 5 *)

               // expectBy[   not[true]         , "lemma"] //

               leibnizF[#1, symmetryOfEqv[falseDef], z, z] &

               // expectBy[   false    , "leibniz(symmetryOfEqv(falseDef))"]
        ]
]

(* ****************************************************************************

   That proof still has an ugly, semi-formal use of Leibniz.

 *************************************************************************** *)



(* (3.16) Symmetry of neqv ************************************************* *)

expect[ neqv[q, p]
      ,
        Module[{proposition = neqv[p, q]},

               proposition /. neqvRule

               // expectBy[    not[eqv[p, q]], "3.10, def of neqv"] //

               symmetryOfEqv /@ #1 &

               // expectBy[    not[eqv[q, p]], "internal symmetryOfEqv"] //

               #1 /. invNeqvRule &

               // expectBy[    neqv[q, p]    , "inverse neqv rule"]
        ]
]

(* (3.17) Associativity of neqv ***********************************************

   For this one, we'll use a different kind of proof. We'll reduce the two
   sides, neqv[neqv[p, q], r] and neqv[p, neqv[q, r]] to the same thing, namely
   eqv[p, eqv[q, r]], then double-check the sameness with "SameQ", i.e., "===".
   That's a tiny bit of cheating, because SameQ isn't in our metacircular
   evaluator, yet, but it does prove the theorem, informally, at the human
   level. The tools we're building, so far, are "proof assistants" or "proof
   checkers", as opposed to fully formal "theorem provers." That means that it's
   OK, so far, if parts of the proof are informal, not fully machine-checked
   with the apparatus at hand. We're proceeding like this so that we can
   discover the correct patterns to enshrine in the theorem provers we shall
   ultimately build.

   We shall also need (for this proof) to Map "neqvRule" at the second level of
   nesting, inside a "not" and an "eqv", to convert

       not[eqv[neqv[q, r], p]]

   to

       not[eqv[not[eqv[q, r]], p]]

   The proper way to do this is with a third argument to "Map" (see
   http://reference.wolfram.com/language/ref/Map.html?q=Map):

       Map[(#1 /. neqvRule)&, #1, {2}] & (* Map at second nest level *)

   The "{2}", a singleton list, means "at the second level only, please".

 *************************************************************************** *)



ClearAll[simpleAssociativityEqvRule]
simpleAssociativityEqvRule = eqv[eqv[p_, q_], r_] :> eqv[p, eqv[q, r]]



expect [ True
         ,
Module[{leftHalf =
        Module[{proposition = neqv[neqv[p, q], r]},

               proposition

               // expectBy [ neqv[neqv[p, q], r], "left prop" ] //

               (#1 /. neqvRule)& /@ #1 & (* inside, at level 1 *)

               // expectBy [ neqv[not[eqv[p, q]], r], "3.10, def of neqv"] //

               (#1 /. neqvRule)&

               // expectBy [ not[eqv[not[eqv[p, q]], r]], "def of neqv"] //

               (#1 /. invNotRule)& /@ #1 & (* inside, at level 1 *)

               // expectBy [ not[not[eqv[eqv[p, q], r]]], "3.9, inv distr"] //

               doubleNegation

               // expectBy [ eqv[eqv[p, q], r], "3.12, double negation"] //

               associativity

               // expectBy [ eqv[p, eqv[q, r]], "associativity"]
        ],
        rightHalf =
        Module[{proposition = neqv[p, neqv[q, r]]},

               proposition

               // expectBy [ neqv[p, neqv[q, r]], "right prop" ] //

               (#1 /. neqvRule)&

               // expectBy [ not[eqv[p, neqv[q, r]]], "3.10, def of neqv"] //

               symmetryOfEqv /@ #1 &

               // expectBy [ not[eqv[neqv[q, r], p]], "internal symmetryOfEqv"] //

               Map[(#1 /. neqvRule)&, #1, {2}] & (* Map at second nest level *)

               // expectBy [ not[eqv[not[eqv[q, r]], p]], "def of neqv"] //

               (#1 /. invNotRule)& /@ #1 &

               // expectBy [ not[not[eqv[eqv[q, r], p]]], "3.9, inv distr"] //

               doubleNegation

               // expectBy [ eqv[eqv[q, r], p], "3.12, double negation"] //

               symmetryOfEqv

               // expectBy [ eqv[p, eqv[q, r]], "symmetryOfEqv" ]
        ]}
       ,
         leftHalf === rightHalf]
]

(* ****************************************************************************

   We notice some repeated code:

       (#1 /. neqvRule)&
       (#1 /. invNotRule)&

   appear more than once, each. It's a good idea to capture that pattern.
   Remembering the discussion in Chapter 1 (GS1.m) about one-shot rules, we go
   one more level up and define a "function" (actually a global, named,
   multi-shot rule) that applies any one-shot rule just once. This function
   "returns another function" that fires the rule once on an argument.
   Technically, this kind of definition is a "curried" global, named, multi-shot
   rewrite rule. "Curried" just means that it takes its arguments one at a time.
   If you think of it as a function that returns another function, that will be
   close enough to the truth because we often do not distinguish functions from
   rewrite rules. Mathematica's documentation is also not scrupulous about this
   distinction, usually calling anything that's invoked by square brackets a
   "function" even when it's technically a named rewrite rule.

   We also close up the layout to make it more concise. As we get more
   accustomed to the proof style, we need less whitespace.

 *************************************************************************** *)

ClearAll[fireRule]
fireRule[rule_][arg_] := arg /. rule

expect [ True
         ,
Module[{leftHalf =
        Module[{proposition = neqv[neqv[p, q], r]},
               proposition
               // expectBy [ neqv[neqv[p, q], r], "left prop" ] //
               fireRule[neqvRule] /@ #1 &
               // expectBy [ neqv[not[eqv[p, q]], r], "3.10, def of neqv"] //
               fireRule[neqvRule]
               // expectBy [ not[eqv[not[eqv[p, q]], r]], "def of neqv"] //
               fireRule[invNotRule] /@ #1 &
               // expectBy [ not[not[eqv[eqv[p, q], r]]], "3.9, inv distr"] //
               doubleNegation
               // expectBy [ eqv[eqv[p, q], r], "3.12, double negation"] //
               associativity
               // expectBy [ eqv[p, eqv[q, r]], "associativity"]
        ],
        rightHalf =
        Module[{proposition = neqv[p, neqv[q, r]]},
               proposition
               // expectBy [ neqv[p, neqv[q, r]], "right prop" ] //
               fireRule[neqvRule]
               // expectBy [ not[eqv[p, neqv[q, r]]], "3.10, def of neqv"] //
               symmetryOfEqv /@ #1 &
               // expectBy [ not[eqv[neqv[q, r], p]], "internal symmetryOfEqv"] //
               Map[fireRule[neqvRule], #1, {2}] & (* Map at second nest level *)
               // expectBy [ not[eqv[not[eqv[q, r]], p]], "def of neqv"] //
               fireRule[invNotRule] /@ #1 &
               // expectBy [ not[not[eqv[eqv[q, r], p]]], "3.9, inv distr"] //
               doubleNegation
               // expectBy [ eqv[eqv[q, r], p], "3.12, double negation"] //
               symmetryOfEqv
               // expectBy [ eqv[p, eqv[q, r]], "symmetryOfEqv" ]
        ]}
       ,
         leftHalf === rightHalf] ]

(* ****************************************************************************

   That's a lot easier to stomach; we're getting rid of "wheels" "/@", "hashes"
   "#1", and ampersands "&" little by little. Let's capture mapping, as well, in
   an overload of fireRule:

 *************************************************************************** *)

fireRule[rule_, level_][arg_] := Map[fireRule[rule], arg, {level}]

expect [ True
         ,
Module[{leftHalf =
        Module[{proposition = neqv[neqv[p, q], r]},
               proposition
               // expectBy [ neqv[neqv[p, q], r], "left prop" ] //
               fireRule[neqvRule, 1]
               // expectBy [ neqv[not[eqv[p, q]], r], "3.10, def of neqv"] //
               fireRule[neqvRule]
               // expectBy [ not[eqv[not[eqv[p, q]], r]], "def of neqv"] //
               fireRule[invNotRule, 1]
               // expectBy [ not[not[eqv[eqv[p, q], r]]], "3.9, inv distr"] //
               doubleNegation
               // expectBy [ eqv[eqv[p, q], r], "3.12, double negation"] //
               associativity
               // expectBy [ eqv[p, eqv[q, r]], "associativity"]
        ],
        rightHalf =
        Module[{proposition = neqv[p, neqv[q, r]]},
               proposition
               // expectBy [ neqv[p, neqv[q, r]], "right prop" ] //
               fireRule[neqvRule]
               // expectBy [ not[eqv[p, neqv[q, r]]], "3.10, def of neqv"] //
               symmetryOfEqv /@ #1 &
               // expectBy [ not[eqv[neqv[q, r], p]], "internal symmetryOfEqv"] //
               fireRule[neqvRule, 2] (* Map at second nest level *)
               // expectBy [ not[eqv[not[eqv[q, r]], p]], "def of neqv"] //
               fireRule[invNotRule, 1]
               // expectBy [ not[not[eqv[eqv[q, r], p]]], "3.9, inv distr"] //
               doubleNegation
               // expectBy [ eqv[eqv[q, r], p], "3.12, double negation"] //
               symmetryOfEqv
               // expectBy [ eqv[p, eqv[q, r]], "symmetryOfEqv" ]
        ]}
       ,
         leftHalf === rightHalf] ]

(* ****************************************************************************

   There is just one more "wheel, hash, ampersand" to get rid of:
   "symmetry /@ #1 &". This one comes from the fact that we defined "symmetry"
   as a named rule. Seemed good, at the time, but now, the pressure to unify
   syntax makes that choice less good. Let's redefine "symmetry" --- and, while
   we're at it, every axiom and theorem we've defined previously --- as a named
   one-shot rule. We'll leave inference rules as global, named, multi-shot
   rules, for now, but it's a good guess that they're on their way out,
   eventually, too.

 *************************************************************************** *)

ClearAll[associativity, leftAssociativity, rightAssociativity]
leftAssociativity  = eqv[ eqv[p_, q_], r_ ] :> eqv[ p, eqv[q, r] ]
rightAssociativity = eqv[ p_, eqv[q_, r_] ] :> eqv[ eqv[p, q], r ]

(* (3.2) Axiom, Symmetry of eqv, p === q === q === p *)
ClearAll[symmetryOfEqv]
symmetryOfEqv = eqv[p_, q_] :> eqv[q, p]

(* (3.3) Axiom, Identity of eqv, page 44, true === q === q *)
ClearAll[identity]
identity = eqv[q_, q_] :> eqv[true, eqv[q, q]]

(* (3.12) Theorem, Double negation, page 46 *)
ClearAll[doubleNegation]
doubleNegation = not[not[p_]] :> p

expect [ True
         ,
Module[{leftHalf =
        Module[{proposition = neqv[neqv[p, q], r]},
               proposition
               // expectBy [ neqv[neqv[p, q], r], "left prop" ] //
               fireRule[neqvRule, 1]
               // expectBy [ neqv[not[eqv[p, q]], r], "3.10, def of neqv"] //
               fireRule[neqvRule]
               // expectBy [ not[eqv[not[eqv[p, q]], r]], "def of neqv"] //
               fireRule[invNotRule, 1]
               // expectBy [ not[not[eqv[eqv[p, q], r]]], "3.9, inv distr"] //
               fireRule[doubleNegation]
               // expectBy [ eqv[eqv[p, q], r], "3.12, double negation"] //
               fireRule[leftAssociativity]
               // expectBy [ eqv[p, eqv[q, r]], "left associativity"]
        ],
        rightHalf =
        Module[{proposition = neqv[p, neqv[q, r]]},
               proposition
               // expectBy [ neqv[p, neqv[q, r]], "right prop" ] //
               fireRule[neqvRule]
               // expectBy [ not[eqv[p, neqv[q, r]]], "3.10, def of neqv"] //
               fireRule[symmetryOfEqv, 1]
               // expectBy [ not[eqv[neqv[q, r], p]], "internal symmetryOfEqv"] //
               fireRule[neqvRule, 2] (* Map at second nest level *)
               // expectBy [ not[eqv[not[eqv[q, r]], p]], "def of neqv"] //
               fireRule[invNotRule, 1]
               // expectBy [ not[not[eqv[eqv[q, r], p]]], "3.9, inv distr"] //
               fireRule[doubleNegation]
               // expectBy [ eqv[eqv[q, r], p], "3.12, double negation"] //
               fireRule[symmetryOfEqv]
               // expectBy [ eqv[p, eqv[q, r]], "symmetryOfEqv" ]
        ]}
       ,
         leftHalf === rightHalf] ]

(* ****************************************************************************

   One final bit of syntax unification will make future automation easier: "Map"
   at level zero is exactly application. Now, every line looks the same: as the
   application of a rule at an appropriate level.

 *************************************************************************** *)

expect [ True
         ,
Module[{leftHalf =
        Module[{proposition = neqv[neqv[p, q], r]},
               proposition
               // expectBy [ neqv[neqv[p, q], r], "left prop" ] //
               fireRule[neqvRule, 1]
               // expectBy [ neqv[not[eqv[p, q]], r], "3.10, def of neqv"] //
               fireRule[neqvRule, 0]
               // expectBy [ not[eqv[not[eqv[p, q]], r]], "def of neqv"] //
               fireRule[invNotRule, 1]
               // expectBy [ not[not[eqv[eqv[p, q], r]]], "3.9, inv distr"] //
               fireRule[doubleNegation, 0]
               // expectBy [ eqv[eqv[p, q], r], "3.12, double negation"] //
               fireRule[leftAssociativity, 0]
               // expectBy [ eqv[p, eqv[q, r]], "left associativity"]
        ],
        rightHalf =
        Module[{proposition = neqv[p, neqv[q, r]]},
               proposition
               // expectBy [ neqv[p, neqv[q, r]], "right prop" ] //
               fireRule[neqvRule, 0]
               // expectBy [ not[eqv[p, neqv[q, r]]], "3.10, def of neqv"] //
               fireRule[symmetryOfEqv, 1]
               // expectBy [ not[eqv[neqv[q, r], p]], "internal symmetryOfEqv"] //
               fireRule[neqvRule, 2] (* Map at second nest level *)
               // expectBy [ not[eqv[not[eqv[q, r]], p]], "def of neqv"] //
               fireRule[invNotRule, 1]
               // expectBy [ not[not[eqv[eqv[q, r], p]]], "3.9, inv distr"] //
               fireRule[doubleNegation, 0]
               // expectBy [ eqv[eqv[q, r], p], "3.12, double negation"] //
               fireRule[symmetryOfEqv, 0]
               // expectBy [ eqv[p, eqv[q, r]], "symmetryOfEqv" ]
        ]}
       ,
         leftHalf === rightHalf] ]

(* (3.18) Mutual associativity of eqv and neqv ********************************

   We add mathics built-in "Identity", not to be confused with our axiom of
   identity 3.3, at the end of every pair of lines, so they all look like

      fireRule[ ... ]
      // expect ... //

   this way, we don't have to remember to remove the trailing // on the last
   line of a chain of steps.

   Again, this will help future "code generation," i.e., automating the process
   of producing proofs.

 *************************************************************************** *)

expect[ True
,
  Module[{
    leftHalf = Module[{proposition = eqv[neqv[p, q], r]},
       proposition
       // expectBy [ eqv[neqv[p, q], r], "left prop" ] //
       fireRule[neqvRule, 1]
       // expectBy [ eqv[not[eqv[p, q]], r], "def of neqv" ] //
       fireRule[invNotRule, 0]
       // expectBy [ not[eqv[eqv[p, q], r]], "def of neqv" ] //
       fireRule[leftAssociativity, 1]
       // expectBy [ not[eqv[p, eqv[q, r] ]], "left assoc" ] //
       Identity],
    rightHalf = Module[{proposition = neqv[p, eqv[q, r]]},
       proposition
       // expectBy [ neqv[p, eqv[q, r]], "right prop" ] //
       fireRule[neqvRule, 0]
       // expectBy [ not[eqv[p, eqv[q, r]]], "inv of neqv" ] //
       Identity]}

 , leftHalf === rightHalf] ]

(* (3.19) Mutual interchangeability *******************************************

   Now that we've proved associativity of neqv, we can make both eqv and neqv
   Flat again, and simplify proofs. Notice that we present the rightHalf before
   the leftHalf so that our statement of 3.19 is not only closer to the book but
   also close to the proof of 3.18. We constructed the proof of 3.19 by
   copy-pasting the proof of 3.18 (immediately above) and doing some
   associativity by hand.

 *************************************************************************** *)

SetAttributes[eqv, Flat]
SetAttributes[neqv, Flat]

expect[ True
  ,
  Module[{
    rightHalf = Module[{proposition = eqv[neqv[p, q], r]},
        proposition
        // expectBy [ eqv[neqv[p, q], r], "right prop" ] //
        fireRule[neqvRule, 1]
        // expectBy [ eqv[not[eqv[p, q]], r], "def of neqv" ] //
        fireRule[invNotRule, 0]
        // expectBy [ not[eqv[p, q, r]], "def of neqv" ] //
        Identity],
    leftHalf = Module[{proposition = neqv[p, eqv[q, r]]},
        proposition
        // expectBy [ neqv[p, eqv[q, r]], "left prop" ] //
        fireRule[neqvRule, 0]
        // expectBy [ not[eqv[p, q, r]], "inv of neqv" ] //
        Identity]}

 , leftHalf === rightHalf] ]

(* ****************************************************************************

   The heuristic, 3.21, says, just "pattern-match" the proposition against
   existing axioms and theorems. Well, that's exactly what we're doing with
   mathics. You can begin to see the madness behind the method: we're developing
   pattern-matching abstractions. We plan ultimately to automate all the proof
   heuristics by searching for matching patterns, i.e., by
   meta-pattern-matching.

   Let's see how our tools are working out. We'll do a new proof of 3.11, an
   unnamed theorem. First, we rewrite the old proof with our new "fireRule"
   construction. G&S say "Note that Symmetry of === is used in the second step
   of the proof, without explicit mention." We could automate the two symmetry
   properties we know of, for eqv and for neqv, with the Attribute "Orderless."
   That will interfere with the proof of the unnamed theorem because our notRule
   and invNotRule for Axiom 3.9 only allow us to distribute "not" over the first
   slot in an eqv. We cannot control what mathics puts in the first slot with an
   orderless eqv. We shall have to continue with explicit applications of
   symmetry, at least for a while. Eventually, we will learn mathics's
   meta-rules for ordering the slots (it's alphabetical and by complexity: unary
   first, binary next, ternary next, and so on).

 *************************************************************************** *)

(* (3.11) Unnamed theorem (cleaner version of old proof) ******************* *)

expect[ eqv[not[q], p]
      ,
        Module[{proposition = eqv[not[p], q]},
               proposition
               // expectBy[    eqv[not[p], q]   , "proposition" ] //
               fireRule[invNotRule   , 0]
               // expectBy[    not[eqv[p, q]]   , "invNotRule"] //
               fireRule[symmetryOfEqv, 1]
               // expectBy[    not[eqv[q, p]]   , "internal symmetryOfEqv"] //
               fireRule[notRule      , 0]
               // expectBy[    eqv[not[q], p]   , "notRule"] //
               Identity
        ] ]



(* Cheat sheet so far                                                        *)

(* leibniz[ eqv[x_, y_], e_, z_ ] := eqv[e /. {z -> x}, e /. {z -> y}])      *)
(* transitivity[ and [ eqv[x_, y_], eqv[y_, z_] ] ] := eqv[x, z]             *)
(* substitution[e_, v_:List, f_:List] := e /. MapThread [ Rule, {v, f} ]     *)
(*                                                                           *)
(* (3.1) Axiom, Associativity of eqv                                         *)
(* leftAssociativity  = eqv[ eqv[p_, q_], r_ ] :> eqv[ p, eqv[q, r] ]        *)
(* rightAssociativity = eqv[ p_, eqv[q_, r_] ] :> eqv[ eqv[p, q], r ]        *)
(*                                                                           *)
(* (3.2) Axiom, Symmetry of eqv                                              *)
(* p === q === q === p                                                       *)
(* symmetryOfEqv = eqv[p_, q_] :> eqv[q, p]                                  *)
(*                                                                           *)
(* (3.3) Axiom, Identity of eqv, page 44                                     *)
(* true === q === q                                                          *)
(* identity = eqv[q_, q_] :> eqv[true, eqv[q, q]]                            *)
(*                                                                           *)
(* (3.4) Theorem, _true_                                                     *)
(* (3.5) Theorem, Reflexivity of eqv: eqv[p, p]                              *)
(* (3.9) Axiom, Distributivity of "not" over "eqv"                           *)
(* notRule    = (not[eqv[p_, q_]] :> eqv[not[p], q])                         *)
(* invNotRule = (eqv[not[p_], q_] :> not[eqv[p, q]])                         *)
(*                                                                           *)
(* (3.10) Axiom, Definition of "neqv"                                        *)
(* neqvRule    = neqv[p_, q_]     :> not[eqv[p, q]]                          *)
(* invNeqvRule = not[eqv[p_, q_]] :> neqv[p, q]                              *)
(*                                                                           *)
(* (3.11) Unnamed theorem eqv[ eqv[not[p], q], eqv[p, not[q] ]               *)
(* (3.12) Double negation: eqv[ not[not[p]], p ]                             *)
(* doubleNegation = not[not[p_]] :> p                                        *)
(*                                                                           *)
(* (3.13) Negation of false                                                  *)
(* (3.14) Unnamed Theorem eqv[ neqv[p, q], eqv[ not[p], q] ]                 *)
(* (3.15) Unnamed Theorem eqv[ eqv[ not[p], p ], false]                      *)
(* (3.18) Mutual associativity of eqv and neqv                               *)
(* (3.19) Mutual interchangeability                                          *)



(*
 __  __                       __           _        _ _         _
|  \/  |__ _ __ _ _ ___ ___  / _|___ _ _  | |   ___(_) |__ _ _ (_)___
| |\/| / _` / _| '_/ _ (_-< |  _/ _ \ '_| | |__/ -_) | '_ \ ' \| |_ /
|_|  |_\__,_\__|_| \___/__/ |_| \___/_|   |____\___|_|_.__/_||_|_/__|

 *)

(* ****************************************************************************

   Alternatives for 3.15 (page 48)

   We're going to replace our lemma generator "extractTrue" because so much has
   changed between then and now. We will just add a rule for the axiom of
   identity that allows us to USE the first eqv in eqv[true, eqv[p, p]] to
   replace any eqv[p_, p_] with true. Every time we USE an eqv, we are
   implicitly applying Leibniz. That is not a relaxation of formality, but it is
   a hand-coding, akin to a "macro," thus a justifiable formalization of a
   previous informality. G&S don't talk about this.

 *************************************************************************** *)

ClearAll[identity, invIdentity]
identity = eqv[p_, p_] :> true
(* A variant, justified by associativity: *)
invIdentity = eqv[true, p_] :> p

expect[ false
      ,
        Module[{proposition = eqv[not[p], p]},
               proposition
               // expectBy[   eqv[not[p], p]    , "proposition"] //
               fireRule[invNotRule, 0]
               // expectBy[   not[eqv[p, p]]    , "invNotRule"] //
               fireRule[identity, 1]
               // expectBy[   not[true]         , "lemma macro"] //
               leibnizF[#1, falseDef /. symmetryOfEqv, z, z] &
               // expectBy[   false    , "leibniz(symmetryOfEqv(falseDef))"] //
               Identity
        ] ]



(* ****************************************************************************

   Alternatives for 3.15 (page 48)

   We can replace the last use of Leibniz with another macro. The old definition
   of false is eqv[false, not[true]]. To USE the definition, we need another use
   of Leibniz, the last line above "Identity." Let's turn that use of Leibniz
   into a "macro":

 *************************************************************************** *)

ClearAll[falseDef, invFalseDef]
falseDef = not[true] -> false
invFalseDef = false -> not[true]

expect[ false
      ,
        Module[{proposition = eqv[not[p], p]},
               proposition
               // expectBy[   eqv[not[p], p]    , "proposition"] //
               fireRule[invNotRule, 0]
               // expectBy[   not[eqv[p, p]]    , "invNotRule"] //
               fireRule[identity, 1]
               // expectBy[   not[true]         , "lemma macro"] //
               fireRule[falseDef, 0]
               // expectBy[   false             , "falseDef Leibniz macro"] //
               Identity
        ] ]



(* And now, new rules implied by the theorem. We must name the unnamed theorem,
now *)

ClearAll[contradictionByNot, invContradictionByNot]
contradictionByNot         = eqv[not[p_], p_] :> false
invContradictionByNot[p_] := false -> eqv[not[p], p]



(* ****************************************************************************

   Alternative for 3.16 (page 49)

 *************************************************************************** *)

expect[ neqv[q, p]
      ,
        Module[{proposition = neqv[p, q]},
               proposition
               // expectBy[    neqv[p, q]    , "proposition"] //
               fireRule[neqvRule, 0]
               // expectBy[    not[eqv[p, q]], "3.10, def of neqv"] //
               fireRule[symmetryOfEqv, 1]
               // expectBy[    not[eqv[q, p]], "3.2 internal symmetryOfEqv"] //
               fireRule[invNeqvRule, 0]
               // expectBy[    neqv[q, p]    , "3.10, inverse neqv rule"] //
               Identity
        ] ]



(* Section 3.4, Disjunction ************************************************ *)

(* ___  _       _               __  _
  / _ \(_)__   (_)_ _____  ____/ /_(_)__  ___
 / // / (_-<  / / // / _ \/ __/ __/ / _ \/ _ \
/____/_/___/_/ /\_,_/_//_/\__/\__/_/\___/_//_/
          |___/

   Write the axioms as one-shot rules:

 *)


ClearAll[or]


(* (3.24) Axiom, Symmetry of \/, page 49 *)
ClearAll[symmetryOfDisjunction]
symmetryOfDisjunction = or[p_, q_] :> or[q, p]


(* (3.25) Axiom, Associativity of \/, page 49 *)
ClearAll[leftAssociativityOfDisjunction, rightAssociativityOfDisjunction]
leftAssociativityOfDisjunction  = or[or[p_, q_], r_] :> or[p, or[q, r]]
rightAssociativityOfDisjunction = or[p_, or[q_, r_]] :> or[or[p, q], r]


(* (3.26) Axiom, Idempotency of \/, page 49 *)
ClearAll[idempotencyOfDisjunction, invIdempotencyOfDisjunction]
idempotencyOfDisjunction = or[p_, p_] :> p;
invIdempotencyOfDisjunction = p_ :> or[p, p]


(* (3.27) Axiom, Distributivity of \/ over eqv, page 49  *)
ClearAll[multiplyingOutDisjunction, factoringDisjunction]
multiplyingOutDisjunction = or[p_, eqv[q_, r_]] :> eqv[or[p, q], or[p, r]];
factoringDisjunction = eqv[or[p_, q_], or[p_, r_]] :> or[p, eqv[q, r]]


(* (3.28) Axiom, Excluded Middle *)
ClearAll[excludedMiddle, invExcludedMiddle]
excludedMiddle[p_] := true -> or[p, not[p]];
invExcludedMiddle   = or[p_, not[p_]] :> true;

(* We must introduce "true" to the definition of the excluded middle to
   mechanize this rule. G&S do this implicitly because they're working merely
   with pencil and paper. *)



(* (3.29) Theorem, Zero of \/, page 49 *)

expect[true
     ,
       Module[{proposition = or[p, true]},
              proposition
              // expectBy[    or[p, true]           , "proposition"] //
              fireRule[excludedMiddle[p], 1]
              // expectBy[    or[p, or[p, not[p]]]  , "3.28, excluded middle"] //
              fireRule[rightAssociativityOfDisjunction, 0]
              // expectBy[    or[or[p, p], not[p]]  , "3.25, associativity"] //
              fireRule[idempotencyOfDisjunction, 1]
              // expectBy[    or[p, not[p]]         , "3.26, idempotency"] //
              fireRule[invExcludedMiddle, 0]
              // expectBy[    true                  , "3.28 excluded middle"] //
              Identity
       ] ]



(* (3.30) Theorem, Identity of \/ *)

(*

   We need a new version of 3.15. To help the proof of 3.30, we want to replace
   "eqv[false, p]" with "not[p]". The original statement and proof were that
   "false" and "eqv[p, not[p]]" are eqv. By associativity, we can arrange this
   as an equivalence of "eqv[false, p]" and "not[p]", and leibniz allows us to
   replace equivalents with one another. After seeing a couple of macros of
   leibniz, we're comfortable that such replacements are always allowed. The
   exact way we want to mechanize that blanket allowance is not yet clear. But,
   for now, we will freely use "Part" to implement leibniz; for example, a lemma
   like "eqv[p, q]" allows us to replace "p = eqv[p, q][[1]]" with
   "q = eqv[p, q][[2]]" and vice versa. Another mechanization might be a rule
   that returns a rule, a metarule (we mentioned meta-programming earlier):

       eqv[p_, q_] :> (p :> q)

   We'll get to that later.

 *)

expect[not[p]
     ,
       Module[{proposition = eqv[false, p]},
              proposition
              // expectBy[   eqv[false, p]    , "proposition"] //
              fireRule[invFalseDef, 1]
              // expectBy[   eqv[not[true], p], "def of false (macro)"] //
              fireRule[invNotRule, 0]
              // expectBy[   not[eqv[true, p]], "3.9, distributivity"] //
              fireRule[invIdentity, 1]
              // expectBy[   not[p]           , "3.3, identity"] //
              Identity
       ] ]

(*

   Because of this fantastic little theorem, we're able to replace eqv[false, p]
   with not[p] and vice versa. We'll need rules that we can invoke for doing
   that. Because we need rules, we can now see a big meta-meta-rule:
    __  __     _                      _          ___      _
   |  \/  |___| |_ __ _ ___ _ __  ___| |_ __ _  | _ \_  _| |___
   | |\/| / -_)  _/ _` |___| '  \/ -_)  _/ _` | |   / || | / -_)
   |_|  |_\___|\__\__,_|   |_|_|_\___|\__\__,_| |_|_\\_,_|_\___|

   Every theorem must automatically introduce rules (probably always one-shot
   rules) for applying that theorem in downstream proofs. Every (successful)
   invocation of "expect" must generate and install a pair of new (one-shot)
   rules allowing transformations back and forth between the two terms of the
   "expect". "Expect" will become a metarule: a generator of rules. This is one
   big insight we've been working towards.

   We're not quite ready to implement it, but we now more about know what we're
   doing. First, we'll finish Chapter 3 by hand, then we will rewrite Chapter 3
   with the new meta-meta rule (unless we find out better).

*)

ClearAll[notPRule, invNotPRule]
notPRule    = not[p_] :> eqv[false, p]
invNotPRule = eqv[false, p_] :> not[p]

(* Back to theorem 3.30 *)

expect[true
 ,
  Module[{proposition = eqv[or[p, false], p]},
    proposition
    // expectBy[    eqv[or[p, false], p]          , "proposition"] //
    eqv[#1[[1]], #1[[2]] /. invIdempotencyOfDisjunction] &
    // expectBy[    eqv[or[p, false], or[p, p]]   , "3.26, idempotency"]//
    fireRule[factoringDisjunction, 0]
    // expectBy[    or[p, eqv[false, p]]     , "3.27, distributivity of \/"] //
    fireRule[invNotPRule, 1]
    // expectBy[    or[p, not[p]]            , "3.15, unnamed theorem"] //
    fireRule[invExcludedMiddle, 0]
    // expectBy[    true                     , "3.28, excluded middle"] //
    Identity
    ] ]

(*

   Notice we needed a new hash, ampersand rule to get idempotency of
   disjunction, 3.26, applied just to the second part of its input. We'll have
   to modify "fireRule" to mechanize this subtlety. Mixing Part and level will
   be a challenge, so we will break up the challenge, getting it right in
   stages.

   EMPHASIS:

   For parts greater than 1, the following "fireRuleOnPart" only works on
   associative (Flat) heads like eqv, because it recurses on head[args],
   introducing inner copies of "head".

   It uses a new pattern, "args___", which matches zero or more things in an
   argument list enclosed in square brackets (a "Sequence"). I leave it to you
   to study "Sequence" in the documentation for mathics and for Mathematica. You
   can find the Mathematica documentation here:

       https://reference.wolfram.com/language/ref/Sequence.html?q=Sequence

   I've left in some debugging Print expressions. Uncomment them if you want to
   see how it's working inside.

 *)

ClearAll[fireRuleOnPart]
fireRuleOnPart[rule_, part_][head_[arg_, args___]] :=
(   (* Print[{"part", part, "head", head, "arg", arg, "{args}", {args}}]; *)
    If[part === 1,
       (   (* Print[{"rule", rule, "arg/.rule", arg/.rule}]; *)
           head[arg /. rule, args]),
       head[arg, fireRuleOnPart[rule, part-1][head[args]]]
    ])

(* Alternative proof of theorem 3.30 *)

expect[true
     ,
       Module[{proposition = eqv[or[p, false], p]},
              proposition
              // expectBy[    eqv[or[p, false], p]          , "proposition"] //
              fireRuleOnPart[invIdempotencyOfDisjunction, 2]
              // expectBy[    eqv[or[p, false], or[p, p]]   , "3.26, idempotency"]//
              fireRule[factoringDisjunction, 0]
              // expectBy[    or[p, eqv[false, p]]     , "3.27, distributivity of \/"] //
              fireRule[invNotPRule, 1]
              // expectBy[    or[p, not[p]]            , "3.15, unnamed theorem"] //
              fireRule[invExcludedMiddle, 0]
              // expectBy[    true                     , "3.28, excluded middle"] //
              Identity
       ] ]

(* Again, this works only at top level. We will need new rules (later) to apply
   "on-part" rules at lower levels. *)

(* We introduce a couple of rules, as usual, for use downstream. Note that the
   specific form of the proof would not make it easy to generate these rules
   mechanically. We'll have to address this issue as we reach for greater
   automation. Most likely, we'll standardize a few proof heuristics, then write
   metarules for each form to generate rules from proofs. *)

ClearAll[identityOfDisjunction, invIdentityOfDisjunction]
identityOfDisjunction         = or[p_, false] :> p
invIdentityOfDisjunction[p_] := p -> or[p, false]



(* (3.31) Theorem, Distributivity of \/ over \/ *)

expect[
    or[or[p, q], or[p, r]]
   ,
Module[{proposition = or[p, or[q, r]]},
       proposition
       // expectBy[   or[p, or[q, r]], "proposition"] //
       fireRuleOnPart[invIdempotencyOfDisjunction, 1]
       // expectBy[   or[or[p, p], or[q, r]], "3.26 idempotency [[1]]"] //
       fireRule[leftAssociativityOfDisjunction   , 0]
       // expectBy[   or[p, or[p, or[q, r]]], "3.25 associativity /@ 0"] //
       fireRule[rightAssociativityOfDisjunction  , 1]
       // expectBy[   or[p, or[or[p, q], r]], "3.25 associativity /@ 1"] //
       fireRule[symmetryOfDisjunction            , 1]
       // expectBy[   or[p, or[r, or[p, q]]], "3.24 symmetry of \/ /@ 1"] //
       fireRule[rightAssociativityOfDisjunction  , 0]
       // expectBy[   or[or[p, r], or[p, q]], "3.25 associativity /@ 0"] //
       fireRule[symmetryOfDisjunction            , 0]
       // expectBy[   or[or[p, q], or[p, r]], "3.24 symmetry of \/ /@ 0"] //
       Identity
] ]

ClearAll[distributivityOfOrOverOr, invDistributivityOfOverOr]
distributivityOfOrOverOr  = or[p_, or[q_, r_]] :> or[or[p, q], or[p, r]]
invDistributivityOfOverOr = or[or[p_, q_], or[p_, q_]] :> or[p, or[q, r]]

(* (3.32) Unnamed Theorem *)

expect[p
     ,
Module[{proposition = eqv[or[p, q], or[p, not[q]]]},
       proposition
       // expectBy[   eqv[or[p, q], or[p, not[q]]], "proposition"] //
       fireRule[factoringDisjunction , 0]
       // expectBy[   or[p, eqv[q, not[q]]], "3.27 distributivity /@ 0"] //
       fireRule[symmetryOfEqv        , 1]
       // expectBy[   or[p, eqv[not[q], q]], "3.2 symmetry of eqv /@ 0"] //
       fireRule[contradictionByNot    , 1] (* 0 would work here, too. *)
       // expectBy[   or[p, false],          "3.15 contradictionByNot /@ 1"] //
       fireRule[identityOfDisjunction, 0]
       // expectBy[   p,                     "3.30 identity of \/ /@ 0"] //
       Identity
] ]

(*

   Note we have made free use of the heuristics and of principle 3.34 on page
   50, without commentary. This entire project is work-in-progress, so we move
   freely between more and less formality as we discover ultimate, formal rules,
   metarules, and meta-metarules.

   Throughout the entire programme, so far, is the gradual liberalization of
   Leibniz. First, just a formal rule of inference. Next, a method to derive
   posterior replacements from premises and prior replacements ("prior" and
   "posterior" are new jargon to help us move forward). Finally, an informal,
   implicit justification for proof heuristics, that is, for concluding that
   eqv[p, q] by transforming p into q with fireRule and fireRuleOnPart. We shall
   be forced, eventually, to formalize this final use of Leibniz as we automate
   more of the proof process.

 *)



(* Section 3.5, Conjunction ************************************************ *)

(*___           _              _   _
 / __|___ _ _  (_)_  _ _ _  __| |_(_)___ _ _
| (__/ _ \ ' \ | | || | ' \/ _|  _| / _ \ ' \
 \___\___/_||_|/ |\_,_|_||_\__|\__|_\___/_||_|
             |__/
 *)

(* (3.35) Axiom, Golden rule *)

ClearAll[goldenRule1, invGoldenRule1]
goldenRule1    = and[p_, q_] :> eqv[p, q, or[p, q]]
invGoldenRule1 = eqv[p_, q_, or[p_, q_]] :> and[p, q]

ClearAll[goldenRule2]
goldenRule2 = eqv[p_, q_] :> eqv[and[p, q], or[p, q]]

(* (3.36) Symmetry of /\ *)

expect[ and[q, p]
      ,
Module[{proposition = and[p, q]},
       proposition
       // expectBy[and[p, q], "proposition"] //
       fireRule[goldenRule1, 0]
       // expectBy[eqv[p, q, or[p, q]], "3.35 golden rule /@ 0"] //
       fireRule[symmetryOfDisjunction, 0]
       // expectBy[eqv[p, q, or[q, p]], "3.24 symmetry of \/"] //
       fireRule[eqv[p_, q_, r_] :> eqv[q, p, r], 0]
       // expectBy[eqv[q, p, or[q, p]], "3.2 ad-hoc symmetry of eqv"] //
       fireRule[invGoldenRule1, 0]
       // expectBy[and[q, p], "3.35 golden rule /@ 0"] //
       Identity
] ]



(* (3.37) Associativity of /\ *)

(* Because the Golden Rule expands into a three-part expression, our proofs can
   get very long, with many steps just re-arranging and flattening combinations
   of "eqv" and "or". At this point, we've seen enough and we trust mathics to
   automate associativity ("Flat") and symmetry ("Orderless"). Mathematica
   implements Orderless by a trick: put terms in alphabetical order. That way,
   they don't have to search over combinatorially large spaces of alternatives.
   I'm not sure mathics does that. Sergey Markelov fixed a bug in mathics that
   broke "Orderless" (https://github.com/mathics/Mathics/issues/749). In his
   investigations, it looks like mathics might be looping over all permutations.
   We're not sure, yet.

   In the offing, We'll add "OneIdentity", which allows a symbol's "Default" to
   be filled in on a pattern match
   (https://reference.wolfram.com/language/ref/OneIdentity.html). "OneIdentity"
   doesn't work properly in mathics, but its defects also won't get in our way
   for now (https://github.com/mathics/Mathics/issues/747). *)

SetAttributes[or,  {Flat, Orderless, OneIdentity}]
SetAttributes[eqv, {Flat, Orderless, OneIdentity}]

expect[True,
Module[
{left = Module[{proposition = and[and[p, q], r]},
       proposition
       // expectBy[and[and[p, q], r], "proposition"] //
       fireRule[goldenRule1, 1]
       // expectBy[
           and[eqv[p, q, or[p, q]], r],
           "3.35 golden rule"] //
       fireRule[goldenRule1, 0]
       // expectBy[
           eqv[p, q, r, or[p, q], or[r, eqv[p, q, or[p, q]]]],
           "3.35 golden rule"] //
       fireRuleOnPart[multiplyingOutDisjunction, 5]
       // expectBy[
           eqv[p, q, r, or[p, q], or[p, r], or[r, eqv[q, or[p, q]]]],
           "3.27 distributivity of \/ over eqv"] //
       fireRuleOnPart[multiplyingOutDisjunction, 6]
       // expectBy[
           eqv[p, q, r, or[p, q], or[p, q, r], or[p, r], or[q, r]],
           "3.27 distributivity of \/ over eqv"] //
       Identity
],
right = Module[{proposition = and[p, and[q, r]]},
       proposition
       // expectBy[and[p, and[q, r]], "proposition"] //
       fireRule[goldenRule1, 1]
       // expectBy[
           and[p, eqv[q, r, or[q, r]]], "3.35 golden rule"] //
       fireRule[goldenRule1, 0]
       // expectBy[
           eqv[p, q, r, or[p, eqv[q, r, or[q, r]]], or[q, r]],
           "3.35 golden rule"] //
       fireRuleOnPart[multiplyingOutDisjunction, 4]
       // expectBy[
           eqv[p, q, r, or[q, r], or[p, eqv[r, or[q, r]]], or[p, q]],
           "3.27 distributivity of \/ and implicit associativity"] //
       fireRuleOnPart[multiplyingOutDisjunction, 5]
       // expectBy[
           eqv[p, q, r, or[p, q], or[p, q, r], or[p, r], or[q, r]],
           "3.27 distributivity of \/ and implicit associativity"] //
       Identity
]}],
left === right]



(* (3.38) Idempotency of /\ : p /\ p === p *)

expect[p
,
Module[{proposition = and[p, p]},
       proposition
       // expectBy[ and[p, p], "proposition" ] //
       fireRule[goldenRule1, 0]
       // expectBy[ eqv[p, p, or[p, p]], "3.35 golden rule" ] //
       fireRule[identity, 0]
       // expectBy[ eqv[true, or[p, p]], "identity of eqv"  ] //
       leibnizF[true, #1, z, z] &
       // expectBy[or[p, p], "1.5 leibniz"] //
       fireRule[idempotencyOfDisjunction, 0]
       // expectBy[p, "3.26 idempotency of \/"] //
       Identity
] ]

(*

   This proof brings up again the question whether we need a rule form for
   leibnizF. There have been a few recent proofs where we brought in leibnizF in
   "hash-ampersand" form just for the purpose of picking one side of an
   equivalence. If we start with eqv[a, b] and we know a is true, then we pick b
   going forward, or vice-versa.

   We can, instead, imagine a "pick" rule that extracts the part of a Leibniz
   application we want. With Flat, "multiary" eqv (new jargon, meaning an eqv
   with any number of arguments), we will extract the First and Rest of a
   premise (lispers will recognize "car" and "cdr"). Mathics bug #747 won't
   affect us because in both mathics and Mathematica,

       First[eqv[p, q, r]] === p
       Rest[eqv[p, q, r]] === eqv[q, r]]

   for all combinations of {Flat, Orderless, OneIdentity}. Proof:

   Generate all combinations as follows:

       In[66]:=
       Flatten[
         Table[
           Union[
             Sort/@Permutations[{Flat,Orderless,OneIdentity}, {i}]],
           {i,3}],
         1]
       Out[66]= {{Flat},
                 {OneIdentity},
                 {Orderless},
                 {Flat, OneIdentity},
                 {Flat, Orderless},
                 {OneIdentity, Orderless},
                 {Flat, OneIdentity, Orderless}}

   Then check as follows:

      In[67]:=
      Table[
        Module[{e = eqv[p,q,r]},    <~~~ HERE IS THE MONEY
          ClearAll[eqv];
          SetAttributes[eqv,j];
          {j, First@e, Rest@e}],
       {j, ... the above ...}]

      Out[67]= {{{Flat},                         p, eqv[q, r]},
                {{OneIdentity},                  p, eqv[q, r]},
                {{Orderless},                    p, eqv[q, r]},
                {{Flat, OneIdentity},            p, eqv[q, r]},
                {{Flat, Orderless},              p, eqv[q, r]},
                {{OneIdentity, Orderless},       p, eqv[q, r]},
                {{Flat, OneIdentity, Orderless}, p, eqv[q, r]}}

   We only get differences, and subtle ones, when we pattern-match:

       Table[
         Module[{ e = (eqv[p,q,r] /. {eqv[x_,y_] :> {x,y}}) },    <~~~ DIFF'T
           ClearAll[eqv];
           SetAttributes[eqv,j];
           {j, First@e, Rest@e}],
        {j, ... the above ...}]

   produces, in Mathematica

       {{{Flat},                         p, {eqv[q, r]}},
        {{OneIdentity},                  eqv[p], {eqv[q, r]}},
        {{Orderless},                    p, eqv[q, r]},
        {{Flat, OneIdentity},            p, eqv[q, r]},
        {{Flat, Orderless},              p, {eqv[q, r]}},
        {{OneIdentity, Orderless},       q, {eqv[p, r]}},
        {{Flat, OneIdentity, Orderless}, p, eqv[q, r]}}

   but, in mathics

       {{{Flat},                         p, {eqv[q, r]}},
        {{OneIdentity},                  p, {eqv[q, r]}},    <~~~ OOPS
        {{Orderless},                    p, eqv[q, r]},
        {{Flat, OneIdentity},            p, eqv[q, r]},
        {{Flat, Orderless},              p, {eqv[q, r]}},
        {{OneIdentity, Orderless},       p, {eqv[q, r]}},    <~~~ OOPS
        {{Flat, OneIdentity, Orderless}, p, eqv[q, r]}}

   There are enough subtleties that making mathics consistent with Mathematica
   is a lengthy debugging exercise if not a total rewrite of mathics because
   everything depends on pattern-matching.

   However, we note the good fortune that pattern-matching produces the same
   results when our symbols are Flat, OneIdentity, Orderless, so we can postpone
   the debugging or rewriting work so long as we only use such symbols.

   LESSON:

   Use "Part" instead of pattern-matching for Leibniz when possible. When
   pattern-matching, ensure Flat, OneIdentity, and Orderless.

   WHAT WE WANT:

       Module[{proposition = and[p, p]},
              proposition
              // expectBy[ and[p, p], "proposition" ] //
              fireRule[goldenRule1, 0]
              // expectBy[ eqv[p, p, or[p, p]], "3.35 golden rule" ] //
              fireRule[identity, 0]
              // expectBy[ eqv[true, or[p, p]], "identity of eqv"  ] //
       ~~~>   fireRule[leibnizPick[2][true]]    <~~~ THE MONEY SHOT
              // expectBy[or[p, p], "1.5 leibniz"] //
              fireRule[idempotencyOfDisjunction, 0]
              // expectBy[p, "3.26 idempotency of \/"] //
              Identity
       ]

   When we unpack a Flat premise, say eqv[true, or[p, p]], with First and Rest,
   we'll get

       First[premise] ~~> true
       Rest[premise]  ~~> eqv[or[p, p]]

   When we say "leibnizPick[2]," we want the second part of the premise. We
   really want the first part of the recursively unpacked eqv, i.e., the first
   part of eqv[or[p, p]]. We'll do a non-recursive version first, one that only
   works at the top level of a proof.

   Note that we check the condition (part >= 2) in the definition.

*)

ClearAll[leibnizPick]
leibnizPick[part_ /; (part >= 2)][proposition_] =
premise_ :> Module[{x = First[premise], y = Rest[premise]},
                   If[Not[SameQ[proposition, x]],
                      Print["\nFAILED MATCH OF PROPOSITION"];
                      Print[ToString[proposition]];
                      Print["WITH FIRST TERM OF EQV"];
                      Print[ToString[x]],
                      (* else *)
                      y[[part - 1]]
                   ] ]

expect[p
     ,
       Module[{proposition = and[p, p]},
              proposition
              // expectBy[ and[p, p], "proposition" ] //
              fireRule[goldenRule1, 0]
              // expectBy[ eqv[p, p, or[p, p]], "3.35 golden rule" ] //
              fireRule[identity, 0]
              // expectBy[ eqv[true, or[p, p]], "identity of eqv"  ] //
              fireRule[leibnizPick[2][true]]
              // expectBy[or[p, p], "1.5 leibniz"] //
              fireRule[idempotencyOfDisjunction, 0]
              // expectBy[p, "3.26 idempotency of \/"] //
              Identity
       ] ]

(* ****************************************************************************
 _____ _          _____                                        ___         _
|_   _| |_  ___  |_   _|__ _ __  _ __  ___ _ _ __ _ _ _ _  _  | __|_ _  __| |
  | | | ' \/ -_)   | |/ -_) '  \| '_ \/ _ \ '_/ _` | '_| || | | _|| ' \/ _` |
  |_| |_||_\___|   |_|\___|_|_|_| .__/\___/_| \__,_|_|  \_, | |___|_||_\__,_|
                                |_|                     |__/
 *************************************************************************** *)

(* we leave this at the very bottom so we can get a count of right and wrong
results during development. The final result of the script, no matter what its
intermediate state, is the result of the following tautology. *)

expect[ true, true ]
