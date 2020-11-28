(* ::Package:: *)

(* :Title: RSolve *)

(* :Author:  Marko Petkovsek *)

(* :Summary:
This package provides the function RSolve plus other tools for
solving recurrence (difference) equations.
*)

(* :Context: DiscreteMath`RSolve` *)

(* :Package Version: 1.5 *)

(* :Copyright: Copyright 1990-2007, Wolfram Research, Inc. *)

(* :History:
  Alpha test version by Marko Petkovsek, June 1990.
  Modified for use under V2.0 by ECM, Wolfram Research, Inc., January 1991.
  More SeriesTerm, PowerSum, and ExponentialPowerSum rules added;
	package symbol HypergeometricF replaced by kernel symbol
	HypergeometricPFQ; package symbol K for indexing sums deleted as this
	is part of the kernel in V3.0; eliminated syntax of representing valid
	range of n for an equation with nonstandard Condition;
	eliminated MakeReal and UseApart options;  eliminated PrecisionST
	and PrecisionHS options so that RSolve, HSolve, and other functions
	are more like Solve and DSolve;  replaced UseMod symbol with
	IntegerFunctions option in SeriesTerm and RSolve;  replaced Methods
	symbol with Method option in RSolve;  added GeneratedParameters,
	HSolveConstants, GeneratingFunctionConstants, and
	ExponentialGeneratingFunctionConstants options;  eliminated Info
	and replaced its functionality by adding support for the Assumptions
	option to SeriesTerm:   ECM, Wolfram Research, Inc., 1994-1995.
   New PartialFractions based on Series, Wolfram Research, Nov. 1996.
   New SeriesTerm capabilities based on Dan Lichtblau's nthTerm.m and other
	enhancements: ECM, Wolfram Research, Jan. 1997.
   Modified SeriesTerm to call the new kernel function InverseZTransform
	first before internal iSeriesTerm rules.  Added new iPowerSum rule
	that tries Sum and ZTransform.  Added new iExponentialPowerSum rule
	that tries Sum.  ECM, WRI, Jan. 1998.
*)

(* :Keywords:
  recurrence, difference, discrete, solve, equation
*)

(* :Warning:
	In V3.0, the syntax for representing the valid range of n is
	changed from using the nonstandard Condition to inferring the
	range from the problem.
*)

(* :Mathematica Version: 4.0 *)

(* :Limitation:
*)

(* :Source:
Finding Closed-Form Solutions of Difference Equations by Symbolic Methods,
  M. Petkovsek, PhD Thesis, Carnegie Mellon University, Sept. 20, 1990.
Theory & Application of the z-transform method,
  E.I.Jury, John Wiley & Sons, Inc.
nthTerm.m, package by Dan Lichtblau, Wolfram Research, Inc., Dec. 1996.
*)


(* :Discussion:
RSolve calls iRSolve, which calls iMethodGF and iMethodEGF.
iMethodGF calls FunctionSolve (with PowerSum) and then internalSeriesTerm; that
	is, it solves for the generating function and then finds an expression
	for the coefficients for the series expansion of
	the generating function.
iMethodEGF calls FunctionSolve (with ExponentialPowerSum) and then
	internalSeriesTerm;
	that is, it solves for the exponential generating function and then
	finds an expression for the coefficients for the series expansion of
	the exponential generating function.
FunctionSolve calls either PowerSum or ExponentialPowerSum, followed by
	 Solve, DSolve and iHSolve.

HSolve calls iHSolve.

GeneratingFunction calls FunctionSolve with PowerSum.
ExponentialGeneratingFunction calls FunctionSolve with ExponentialPowerSum.

PowerSum calls iPowerSum which includes a rule based on ZTransform and Sum.
ExponentialPowerSum calls iExponentialPowerSum which includes a rule
	based on Sum.

SeriesTerm calls internalSeriesTerm.

internalSeriesTerm calls InverseZTransform if
	either Assumptions -> Automatic or Assumptions ->  {n >= 0}.
	If there are other Assumptions or InverseZTransform
	fails, then iSeriesTerm rules are used.
	( NOTE: According to Victor Adamchik 8/19/98
	"Z transform package does assume that n>=0.")

*)

Off[General::obspkg]
Message[General::obspkg, "DiscreteMath`RSolve`"]

BeginPackage["DiscreteMath`RSolve`"]

Unprotect[PowerSum, ExponentialPowerSum, GeneratingFunction,
	ExponentialGeneratingFunction, Gf, EGf, HSolve, HypergeometricF,
	SeriesTerm, RealQ, ISolve, PartialFractions, Even, Odd]


(*
RSolve::usage =
"RSolve[eqn, a, n] solves a recurrence equation for the function a,
with independent variable n. RSolve[{eqn1, eqn2, ...}, {a1, a2, ...}, n]
solves a list of recurrence equations."

GeneratedParameters::usage =
"GeneratedParameters is an option to RSolve that determines the constants in the
the returned result. The default is GeneratedParameters -> C."
*)

PowerSum::usage =
"PowerSum[expr, {z, n, n0:0}] gives Sum[expr z^n, {n, n0, Infinity}]. \
If a function representing a sequence of the form a[n] is in expr, \
the name Gf[a][z] is used by PowerSum to denote \
Sum[a[n] z^n, {n, 0, Infinity}]. PowerSum is threaded over lists and \
equations."

(* Note: the usage here changed between V3.0.X and V4.0. *)
ExponentialPowerSum::usage =
"ExponentialPowerSum[expr, {z, n, n0:0}] gives Sum[expr z^n / n!, \
{n, n0, Infinity}]. If a[n] is a  sequence, the name EGf[a][z] is used by \
ExponentialPowerSum to denote Sum[a[n] z^n / n!, {n, 0, Infinity}]. \
ExponentialPowerSum is threaded over lists and equations."

GeneratingFunction::usage =
"GeneratingFunction[eqn, a[n], n, z] gives the ordinary generating functions \
Sum[a[n] z^n, {n, n0, Infinity}] for the functions a[n] solving eqn, with \
independent variable n. GeneratingFunction[{eqn1, eqn2, ...}, \
{a1[n], a2[n], ...}, n, z] gives the ordinary generating functions \
Sum[ai[n] z^n, {n, n0, Infinity}] for the functions ai[n] solving \
eqn1, eqn2, ..., with independent variable n. Here n0 denotes the least \
value of n such that a[n] (or ai[n]) appears in the equation(s)."

GeneratingFunctionConstants::usage =
"GeneratingFunctionConstants is an option to GeneratingFunction that determines \
the constants in the the returned result. The default is \
GeneratingFunctionConstants -> C."

(* Note: the usage here changed between V3.0.X and V4.0. *)
ExponentialGeneratingFunction::usage =
"ExponentialGeneratingFunction[eqn, a[n], n, z] gives the exponential \
generating functions Sum[a[n] z^n / n!, {n, n0, Infinity}] for the \
functions a[n] solving eqn, with independent variable n. \
ExponentialGeneratingFunction[{eqn1, eqn2, ...}, {a1[n], a2[n], ...}, n, z] \
gives the exponential generating functions Sum[ai[n] z^n / n!, \
{n, n0, Infinity}] for the functions ai[n] solving eqn1, eqn2, ..., with \
independent variable n. Here n0 denotes the least value of n such that a[n] \
(or ai[n]) appears in the equation(s)."

ExponentialGeneratingFunctionConstants::usage =
"ExponentialGeneratingFunctionConstants is an option to \
ExponentialGeneratingFunction that determines the constants in the the returned \
result. The default is ExponentialGeneratingFunctionConstants -> C."

Gf::usage =
"Gf[a][z] is used by PowerSum to denote Sum[a[n] z^n, {n, 0, Infinity}]."

EGf::usage =
"EGf[a][z] is used by ExponentialPowerSum to denote Sum[a[n] z^n / n!, \
{n, 0, Infinity}]."

HSolve::usage =
"HSolve[eqn, f[z], z] gives a hypergeometric formal series solution to \
differential equation eqn in function f[z] with independent variable z. \
HSolve gives {} when it determines that there is no series solution."

HSolveConstants::usage =
"HSolveConstants is an option to HSolve that determines the constants in the \
the returned result. The default is HSolveConstants -> C."

HypergeometricF::usage =
"HypergeometricF[{a1, a2, ...}, {b1, b2, ...}, z] is obsolete. It is \
superseded by HypergeometricPFQ[{a1, a2, ...}, {b1, b2, ...}, z]."

SeriesTerm::usage =
"SeriesTerm[f, {z, z0, n}, opts] gives the coefficient of the term \
(z-z0)^n in the Laurent series expansion for f about the point z = z0."
(* SeriesTerm[expr, {z, a, n}, opts] gives the n-th coefficient of the
   Laurent series expansion of expr around z = a, where n may be symbolic. *)

RealQ::usage = "'RealQ[x] = True' serves to declare that x is real."

Info::usage =
"Info[n] is obsolete. It is superseded by the use of the Assumptions \
option in SeriesTerm to specify information about n."

ISolve::usage =
"ISolve[eqs] solves a Boolean combination of equalities and/or inequalities \
in a single integer-valued variable. The answer is given as a disjunction of \
disjoint inclusive inequalities."

(* NOTE: for backward compatibility, this has the same name as the
	V1.2 - V2.2 function, but internally it is the same as
	PartialFractionExpand in Calculus`Common`TransformCommon`. *)
PartialFractions::usage =
"PartialFractions[f, z] gives a partial fraction decomposition of \
f[z] over the complex functions."

MethodGF::usage =
"MethodGF is a possible value for the Method option of RSolve. It denotes \
the method of ordinary generating functions."

MethodEGF::usage =
"MethodEGF is a possible value for the Method option of RSolve. It denotes \
the method of exponential generating functions."

PrecisionHS::usage =
"PrecisionHS is an obsolete option of RSolve, HSolve, GeneratingFunction, and \
ExponentialGeneratingFunction. Now all solutions are exact as they are with \
Solve and DSolve."

PrecisionST::usage =
"PrecisionST is an obsolete option of RSolve and SeriesTerm. Now all \
results are exact as they are with Solve and DSolve."

MakeReal::usage =
"MakeReal is an obsolete option of RSolve and SeriesTerm. RSolve and \
SeriesTerm automatically try to replace pairs of conjugate complex quantities \
with double their real parts."

UseApart::usage =
"UseApart is an obsolete option of RSolve and SeriesTerm. It was used to \
control the application of Apart prior to power series expansion. \
MethodGF automatically uses Apart without a second argument and \
MethodEGF does not use Apart at all."

(*
old usage for UseApart:		It specifies whether Apart
should be used before attempting expansion into power series, and in what form.
UseApart -> None means don't use Apart at all, UseApart -> All means use Apart
with second argument, and UseApart -> Automatic (default) means use Apart
without second argument.  By default, MethodGF specifies UseApart -> Automatic
and MethodEGF specifies UseApart -> None.  UseApart -> All is useful when the
function to be expanded contains parameters."
*)

UseMod::usage =
"UseMod is an obsolete option of RSolve and SeriesTerm, superseded by \
IntegerFunctions."

IntegerFunctions::usage =
"IntegerFunctions is an option of RSolve and SeriesTerm specifying \
whether results will be expressed in terms of the integer functions \
Mod, Even, and Odd."

SpecialFunctions::usage =
"SpecialFunctions is an option of RSolve and SeriesTerm specifying \
whether results will be expressed in terms of special functions \
such as Legendre polynomials."

Even::usage =
"Even[n] is equivalent to EvenQ[n] except that it remains unevaluated when \
n is not numerical."

Odd::usage =
"Odd[n] is equivalent to OddQ[n] except that it remains unevaluated when \
n is not numerical."


Begin["`Private`"]

issueObsoleteFunMessage[fun_, context_] :=
	(*
        (Message[fun::obspkgfn, fun, context];
         )
	 *)
	Null;

(* ====================================================================== *)


(*
Options[RSolve] := {Method -> Automatic, GeneratedParameters -> C,
			IntegerFunctions -> True, SpecialFunctions -> True}
*)
Options[HSolve] := {HSolveConstants -> C}
Options[GeneratingFunction] := {GeneratingFunctionConstants -> C}
Options[ExponentialGeneratingFunction] :=
	 {ExponentialGeneratingFunctionConstants -> C}
Options[SeriesTerm] =  {Assumptions -> Automatic,
			IntegerFunctions -> True,
			SpecialFunctions -> True}



(* =============================== RSOLVE =================================== *)

(*
RSolve[eqn_, fcn_, n_Symbol, opts___Rule] :=
 Block[{result, oList, fcnn, rule, pureFcnSyntaxRule, function, slot},
   (
   (* handle pure-function syntax *)
   result = result /. {Rule -> rule};
   pureFcnSyntaxRule = If[VectorQ[fcn],
             Map[If[FreeQ[#, n],
		    rule[#[n], x_] :> rule[#, function[x /. {n:>slot}]],
		    {}]&, fcn],
	     If[FreeQ[fcn, n],
		    rule[fcn[n], x_] :> rule[fcn, function[x /. {n:>slot}]],
		    {}]
	  ];
   pureFcnSyntaxRule = Flatten[pureFcnSyntaxRule];

   result = result /. pureFcnSyntaxRule;
   result = result /. {slot -> Slot[1]};
   result = result /. {function -> Function};
   result = result /. {rule -> Rule};
   result
   )	  /; ( oList = {opts};  fcnn = fcn;
	       If[!FreeQ[oList, PrecisionHS] || !FreeQ[oList, PrecisionST],
			 Message[RSolve::obsoptprec]];
               If[!FreeQ[oList, MakeReal], Message[RSolve::obsoptreal] ];
               If[!FreeQ[oList, UseApart], Message[RSolve::obsoptusea] ];
	       If[!FreeQ[oList, UseMod],
		  oList = oList /. {(UseMod -> x_) :> (IntegerFunctions -> x)};
		  Message[RSolve::obsoptusem, IntegerFunctions /. oList ]    ];
	       (* handle pure-function syntax *)
	       fcnn = If[VectorQ[fcn],
		         Map[If[FreeQ[#, n], #[n], #]&, fcn],
			 If[FreeQ[fcn, n], fcn[n], fcn]		];
	       (result = iRSolve[eqn, fcnn, n,
				 Sequence @@ oList]) =!= $Failed &&
	       FreeQ[result, SeriesTerm]
	     )
 ] /; If[FreeQ[eqn, Fail] && FreeQ[eqn, Condition], True,
            Message[RSolve::obssyn]; False] &&
 	(Head[n] =!= Blank && Head[n] =!= Pattern)
*)

(*
RSolve::obsoptprec =
"Warning: PrecisionHS and PrecisionST are obsolete options.
The result of RSolve is always exact."

RSolve::obsoptreal = "Warning: MakeReal is an obsolete option.  RSolve
always tries to replace pairs of conjugate complex quantities with double their
real parts."

RSolve::obsoptusea = "Warning: UseApart is an obsolete option used to
control the application of Apart prior to power series expansion.
MethodGF automatically uses Apart without a second argument and
MethodEGF does not use Apart at all."

RSolve::obsoptusem = "Warning: UseMod is an obsolete option, superseded by
IntegerFunctions.  Using IntegerFunctions -> ``."

RSolve::obssyn =
"Warning: It is obsolete to specify an equation eqn valid for a condition cond
using the syntax Condition[eqn, cond] or eqn /; cond."
*)

iRSolve[eqn_, fcn_, n_, opts___Rule] :=
    Block[{recur, conds, unknowns, startValues,
	   method, rsolvec, if, sf, methods,
	   result, elist, eqlist, flist, solvelist, prs},

	elist = MakeList[eqn];  flist = MakeList[fcn];
	If[!Apply[And, Map[(Head[#] === Equal)&, elist]],
	   Message[RSolve::eqn, elist];  Return[$Failed] ];
   (* escape for nonrecursive equations to Solve *)
    eqlist = Select[elist, !FreeQ[#, n]&]; (* ignore initial conds *)
    solvelist = Map[Union[Cases[eqlist, #, Infinity]]&, flist /. n -> Blank[]];
    If[And @@ Map[Length[#] === 1 &, solvelist],
       Return[fixindices[Solve[eqlist, Flatten[solvelist]], n]]
           (* note: might be nice to verify initial conds with soln, warn
                    if inconsistent *)
    ];
   (* Parse equations to manageable form *)
        prs = Parse[elist, flist, n];
        If[Head[prs] === Fail,
	   Message[RSolve::recur, prs[[1]]];
	   Return[$Failed],
          {recur, conds, unknowns, startValues} = prs ];
        {method, rsolvec, if, sf} =
	   {Method, GeneratedParameters, IntegerFunctions, SpecialFunctions} /.
		 {opts} /. Options[RSolve];
	methods = Switch[method,
		MethodGF, {iMethodGF},
		MethodEGF, {iMethodEGF},
		Automatic, {iMethodGF, iMethodEGF},
	        _, (Message[RSolve::method];
		    {iMethodGF, iMethodEGF})];
	result = Scan[
             Block[{temp},
                temp = # @@ {recur, conds, unknowns, startValues, n,
				 rsolvec, if, sf};
                If[temp =!= $Failed,
                        Return[ Map[Thread[fcn->#]&,temp] ]
                ]
             ]&,
             methods];
        If[result === Null, $Failed,
	   result
	]
    ]

(* fixindices : utility to convert form
    a[f[n]] -> g[n] to a[f^(-1)[n]] -> g[f^(-1)[n]] *)
fixindices[eqns_, n_] :=
 eqns/.Rule[a_, b_] :> fixindices[a, b, n]

fixindices[a:(_[n_]), b_, n_] := a -> b
fixindices[a_[ne_], b_, n_] :=
  Module[{m}, a[n] -> ((b/.First[Solve[ne == m, n]])/. m -> n)]

(*
RSolve::eqn = "`` is not an equation or a system of equations."

RSolve::recur =
"Unable to shift recurrence equations to solve for `` in terms of the
initial conditions."

RSolve::method =
"Warning: Method must be either MethodGF, MethodEGF, or Automatic.  Taking
Method to be Automatic (MethodGF followed by MethodEGF)."
*)

(* ============================= METHOD GF ================================= *)

iMethodGF[recur_, conds_, unknowns_, startValues_, n_, constant_, if_, sf_] :=
    Block[{rec, con, genf, start, temp, mm, z, i, sum},
        {rec, con} = Reset[recur, conds, unknowns, startValues, 0];
	(* NOTE: the old syntax called FunctionSolve with iPowerSum. *)
        genf = FunctionSolve[rec, con, unknowns, n, PowerSum, z, constant];
        If[FreeQ[genf, $Failed],
           (Evaluate[start /@ unknowns]) = startValues;
           temp = Table[(
			 mm /: iInfo[mm] = (mm >= startValues[[i]]);
                         st = internalSeriesTerm[#[[i]],
				 {z, 0, mm - startValues[[i]]},
			          {}, if, sf];
			 UpValues[mm] = {};
			 st
			),
                         {i, Length[unknowns]}
                  ]& /@ genf;
           If[!FreeQ[temp, iSeriesTerm] || !FreeQ[temp, $Failed],
		 Return[$Failed]];
	   (* FunctionSolve returns expressions in terms of
                HypergeometricF... replace with HypergeometricPFQ in case
                temp still contains that internal symbol (it should not).
                This replacement must be done after SeriesTerm. *)
	   temp = temp /. Sum -> sum;
	   temp = temp /. mm -> n;
           temp = temp //. {HypergeometricF :> HypergeometricPFQ};
           temp = temp /. aa_?(MemberQ[unknowns, #]&)[kk_] :>
                           aa[Expand[kk + start[aa]]];
	   temp = temp /. sum -> Sum;
	   temp,
	   If[Length[genf] > 0, Message[RSolve::intind, genf[[1]]] ];
           $Failed
        ]
    ]


(*
RSolve::intind =
"The initial conditions `` must have integral indices."
*)

(* =============================== METHOD EGF ============================= *)

iMethodEGF[recur_, conds_, unknowns_, startValues_, n_, constant_, if_, sf_] :=
    Block[{rec, con, genf, start, temp, mm, z, i, sum},
        {rec, con} = Reset[recur, conds, unknowns, startValues, 0];
	(* NOTE: the old syntax called FunctionSolve with
		 iExponentialPowerSum. *)
        genf = FunctionSolve[rec, con, unknowns, n, ExponentialPowerSum,
                             z, constant];
        If[FreeQ[genf, $Failed],
           (Evaluate[start /@ unknowns]) = startValues;
           temp = Table[(mm /: iInfo[mm] = (mm >= startValues[[i]]);
		         st = internalSeriesTerm[#[[i]],
				 {z, 0, mm - startValues[[i]]},
			         {}, if, sf];
                         (mm - startValues[[i]])! st
			),	{i, Length[unknowns]}
		  ]& /@ genf;
           If[!FreeQ[temp, iSeriesTerm] || !FreeQ[temp, $Failed],
	       Return[$Failed]];
	   (* FunctionSolve returns expressions in terms of
                HypergeometricF... replace with HypergeometricPFQ in case
                temp still contains that internal symbol (it should not).
                This replacement must be done after SeriesTerm. *)
	   temp = temp /. Sum -> sum;
	   temp = temp /. mm -> n;
           temp = temp //. {HypergeometricF :> HypergeometricPFQ};
           temp = temp /. aa_?(MemberQ[unknowns, #]&)[kk_] :>
                           aa[Expand[kk + start[aa]]];
	   temp = temp /. sum -> Sum;
	   temp,
	   If[Length[genf] > 0, Message[RSolve::intind, genf[[1]]] ];
           $Failed
	]
    ]


(* ============================= ROOTS OF UNITY =========================== *)

Omega[n_] := Exp[2 Pi I / n]



(* ==================== SIMPLIFICATION OF EXP[LOG[_]] ===================== *)

SimplifyExpLog = {E^((a_. Log[b_] + c_.)d_.) -> b^(a d) E^(c d)}



(* ========================== FUNCTIONAL SOLVE ============================ *)

FunctionSolve[recur_, conds_, unknowns_, n_, iTransform_, z_, constant_] :=
    Block[{soln, G, functions, hC, dC},


     functions = G[#][z]& /@ unknowns;

     Block[{eqns, init, subst, fnHeads, times, cC, svars},

        (* transform recurrence equations into functional equations *)

        eqns = Expand[#[[1]] - #[[2]]]& /@ recur;
        eqns = Function[xx,
                   Block[{hom, inhom, yy},
                       yy = MakeList[xx, Plus];
                       inhom = Select[yy, FreeListQ[#, unknowns]&];
                       hom = Together[Plus @@ Complement[yy, inhom]];
                       Numerator[hom] + Expand[(Plus @@
                           inhom) Denominator[hom]] ]] /@ eqns;
	(* Next do either PowerSum[eqns, {z, n}] or
		ExponentialPowerSum[eqns, {z, n}]. *)
        eqns = iTransform[eqns, {z, n}];
        If[!FreeListQ[PatternList[eqns, iTransform[__]], unknowns],
            Return[$Failed]];
        Which[iTransform === PowerSum,            G = Gf,
              iTransform === ExponentialPowerSum, G = EGf];

        (* substitute values explicitly given by initial conditions *)

        init = Select[conds, Function[xx, MatchQ[xx,
            a_?(MemberQ[unknowns, #]&)[_?NumberQ] == _?NumberQ]]];
        subst = Solve[init, Union @@
		 (PatternList[init, #[_]]& /@ unknowns)];
        If[subst === {}, Return[$Failed], eqns = eqns /. First[subst]];

        (* remember the unknown functions *)

        fnHeads = G /@ unknowns;

        (* solve the equations using Solve, DSolve, or HypergeometricSolve *)

        If[FreeQ[eqns, Derivative],

	    (* In V4.0,
                Solve[{-(1 - z)^(-1) - (a[0] - Gf[a][z])/z - 4*Gf[a][z] -
                 3*Gf[b][z] == 0,
                -(z/(-1 + z)^2) - (a[0] - Gf[a][z])/z - (b[0] - Gf[b][z])/z +
                 Gf[b][z] == 0}, {Gf[a][z], Gf[b][z]}]
                gives a Solve::svars message. *)
	    svars = (Head[Solve::svars] === $Off);
            Off[Solve::svars];
            soln = Solve[Thread[eqns == 0], functions];
	    If[!svars, On[Solve::svars]],

            (* before using differential equation solvers, integrate
               equations of the form  lhs' == rhs'  *)

            times = Integrable[#, fnHeads, z]& /@ eqns;
            eqns = PreIntegrate[#[[1]], fnHeads, {z, #[[2]]}]& /@
                Thread[List[eqns, times]];
            eqns = Table[Sum[cC[ii, jj] z^(jj - 1), {jj, times[[ii]]}] +
                eqns[[ii]], {ii, Length[eqns]} ];
            eqns = eqns /. Derivative[kk_][ff_] :> D[ff, {z, kk}];
            eqns = Thread[eqns == 0];

            (* use iHSolve then DSolve *)

	    soln = iHSolve[eqns, functions, z, hC];
        If[!FreeQ[soln, $Failed],
                soln = DSolve[eqns, functions, z, GeneratedParameters -> dC]
	    ]
	];
        If[soln === {}, Return[{}]];
        If[!FreeListQ[soln, {Solve, DSolve, $Failed, Indeterminate, Integrate}],
		 Return[$Failed]];
        If[!FreeListQ[functions /. soln, fnHeads], Return[$Failed]];
        soln = soln //. SimplifyExpLog;

 	(* NOTE: need to do the following simplification so that problems like
		RSolve[{x[n] == 0.5*x[-2 + n] + 0.4*x[-1 + n] + y[-1 + n],
         		y[n] == x[-1 + n] + 0.7*y[-2 + n] + 0.3*y[-1 + n]},
		       {x[n], y[n]}, n] work. *)
        soln = {Map[(#[[1]] -> Chop[Together[#[[2]]]])&, soln[[1]]]}

     ]; (* end Block *)


     Block[{initials, pinitials, solvedFcns, constants, vars, order, series,
	    ii, seriesEqns, xx, var1, var2, lL, nN, svars,
	    initvals, var3, constant3, result},

        (* find all initial terms occurring in conditions, equations,
           or solutions, sort initials by falling indices *)

        initials = Union @@ (PatternList[{conds, eqns,
            functions /. soln}, #[_]]& /@ unknowns);
	If[!Apply[And, Map[IntegerQ[#]&, Map[First, initials] ]],
	   (* Return initials so that error message can give the
		initial conditions causing the problem. *)
           Return[$Failed[Evaluate[initials]]] ];
	(* sort initials by falling indices *)
        initials = Join @@ (Append[Cases[initials, _[#]]& /@
            Reverse[Union[PatternList[initials, _?NumberQ]]], {}]);


	(* determine values of initials, eliminate constants of integration *)

	solvedFcns = functions /. soln;
	constants = Cases[solvedFcns, hC[_] | dC[_] | cC[__], Infinity];
	constants = Union[constants];
	vars = Join[constants, initials];

        (* `order' is the number of series terms needed to determine initials *)
        (order[#] = Max[Append[First /@ PatternList[initials, #[_]], 0]])& /@
            	unknowns;


        series = soln /. { (G[aa_?(MemberQ[unknowns, #]&)][z] -> bb_) :>
            		   (aa -> Taylor[bb, z, 0, order[aa]]) };
        If[!FreeListQ[series, {Series, $Failed}], Return[$Failed]];
        If[G === EGf,
            series = series /. { (aa_?(MemberQ[unknowns, #]&) -> bb_) :>
               			 (aa -> Table[ii!, {ii, 0, order[aa]}] bb) } ];


        pinitials = Select[initials, (#[[1]] >= 0)&];
	seriesEqns = Function[xx,
	     Union[conds,
		 (# == (Head[#] /. xx)[[First[#] + 1]])& /@ pinitials
	     ] 	     ] 		/@ series;


	var1 = {};  var2 = {};
	Scan[With[{eqList = #},
              AppendTo[var1, Select[vars, ((!FreeQ[eqList, #])&)]];
              AppendTo[var2, Select[vars, FreeQ[eqList, #]&]]
             ]&, seriesEqns];

	nN = Length[soln];
	(* In V4.0,
		Solve[{P[0] == 1, P[0] == dC[1] - x*Log[1 - x] +
		Log[1 - x]*P[1], P[1] == x,
		P[1] == -x + P[1] + x*(dC[1]- x*Log[1 - x] + Log[1 - x]*P[1])},
		{dC[1], P[1], P[0]}]
	   gives a Solve::svars message. *)
	svars = (Head[Solve::svars] === $Off);
        Off[Solve::svars];
	initvals = Table[(
		Solve[seriesEqns[[ii]], var1[[ii]]]
			 ), {ii, nN}];
	If[!svars, On[Solve::svars]];
	result = Table[ (functions /. soln[[ii]]) /. initvals[[ii]], {ii, nN}];
	result = Select[Union @@ result, (Head[#] === List)& ];

	(* NOTE: This Collect just makes the result more complicated. *)
	(*
	var2 = Flatten[var2];
 	result = Collect[result, var2];
	*)

	(* var3 are the variables that remain in the result
		after substituting initial or boundary conditions.
	   Put these variables in terms of the specified constant. *)
	var3 = Select[vars, (!FreeQ[result, #])&];
	constant3 = Table[constant[ii], {ii, Length[var3]}];
	result /. Thread[Rule[var3, constant3]]

     ] (* end of Block *)

] (* end of FunctionSolve *)
    (* iHSolve may return result in terms of HypergeometricF, thus
	FunctionSolve may also return result in terms of HypergeometricF.
	Functions calling FunctionSolve must replace HypergeometricF
	with HypergeometricPFQ at some point. *)


(* =============================== TAYLOR =============================== *)

Taylor[f_, z_, a_, n_] :=
    Block[{tmp, kk, poly, series, result},
      series = SafeSeries[f, {z, a, n}];
      poly = (Head[General::poly] === $Off);
      Off[General::poly];
      tmp = CoefficientList[series,z];
      If[SameQ[Head[tmp],CoefficientList],
             tmp = CoefficientList[Normal[series],z];
             If[SameQ[Head[tmp],CoefficientList],
                     tmp = {Normal[series]}
      ]];
      If[!poly, On[General::poly]];
      If[Length[tmp] > n + 1, tmp = Take[tmp, n + 1]];
      result = Join[tmp, Table[0, {kk, n + 1 - Length[tmp]}]];
      result
    ]


(* =================== INTEGRABLE and PREINTEGRATE ======================= *)

Integrable[a_Plus, b___] := Min[Integrable[#, b]& /@ (List @@ a)]

Integrable[a_, functions_, z_] := Infinity /; FreeListQ[a, functions]

Integrable[a_ b_, functions_, z_] :=
    Integrable[b, functions, z] /; FreeQ[a, z]

Integrable[a_, functions_, z_] :=
    FirstArg[a] /; FirstHead[a] === Derivative

Integrable[a___] := 0


PreIntegrate[a_, functions_, {z_, 0}] := a

PreIntegrate[a_Plus, b___] := PreIntegrate[#, b]& /@ a

PreIntegrate[a_, functions_, {z_, n_}] :=
    Nest[Integrate[#, z]&, a, n] /; FreeListQ[a, functions]

PreIntegrate[a_ b_, functions_, {z_, n_}] :=
    a PreIntegrate[b, functions, {z, n}] /; FreeQ[a, z]

PreIntegrate[a_, functions_, {z_, n_}] :=
     MapAt[(# - n)&, a,
         Append[First[Position[a, Derivative[_]]], 1]] /;
                             FirstHead[a] === Derivative



(* ==================== FIRSTHEAD, FIRSTARG and HEADCOUNT ================= *)

FirstHead[a_] := Nest[Head, a, HeadCount[a]]



FirstArg[a_] := If[Length[a] == 0, Null,
                          First[Nest[Head, a, HeadCount[a] - 1]] ]


HeadCount[a_] := If[Length[a] == 0, 0, HeadCount[Head[a]] + 1 ]



(* ============================== POWER SUM ================================ *)
(* Note that PowerSum[a[n], {z, n}] gives Gf[a][z],
	but (ZTransform[a[n], n, w] /. {w :> 1/z})
	and Sum[a[n] z^n, {n, 0, Infinity}] remain unevaluated.
   So PowerSum is more useful than ZTransform or Sum for symbolic
	functions of n.   This is why internal iPowerSum rules
	are tried before trying ZTransform or Sum.
*)



    (* lists and equations *)

PowerSum[expr_, {z_, n_Symbol, n0_:0}] :=
	(
	PowerSum[#, {z, n, n0}]& /@ expr
	) /; Head[expr]===List || Head[expr]===Equal


    (* apply internal iPowerSum rules;  these rules include
	application of Sum and ZTransform *)

(* NOTE: eliminated rules transforming Even or Odd to SymbolicMod expressions *)
PowerSum[expr_, {z_, n_Symbol, n0_:0}] :=
   (issueObsoleteFunMessage[PowerSum, "DiscreteMath`RSolve`"];
    Block[{e, e0, sum, if, result},
     (
	   result
     )	/; (
		      e = expr //. {Sum :> sum, If :> if};
		      e0 = e /. {n :> n + n0};
		      e0 = e0 /.
                            {a_Symbol?(Context[#] =!= "System`" && # =!= K &&
                                # =!= Even && # =!= Odd &)[m_] :> a[Expand[m]]};
		      e0 = e0 /. {sum :> Sum, if :> If};
		      e0 = e0 /. {Sum[a_, {k_, m_}] :> Sum[a, {k, 1, m}],
				  Mod :> SymbolicMod};
		      FreeQ[result = z^n0 iPowerSum[e0, z, n], iPowerSum]
	   )
    ] /; !(Head[expr]===List || Head[expr]===Equal))


(* ========================= internal POWER SUM ============================ *)

    (* lists and equations *)

iPowerSum[expr_, z_, n_, n0_:0] :=
        (
        iPowerSum[#, z, n, n0]& /@ expr
        ) /; Head[expr]===List || Head[expr]===Equal


    (* non-zero starting point *)
    (* (This rule is needed because the "convolution" iPowerSum rule
		returns a 4-argument iPowerSum.) *)

iPowerSum[expr_, z_, n_, n0_] :=
 Module[{e, sum, if},
    (
    e = expr //. {Sum :> sum, If :> if};
    e = e /. {n :> n + n0};
    e = e /. {a_Symbol?(Context[#] =!= "System`" && # =!= K &&
                   # =!= Even && # =!= Odd &)[m_] :> a[Expand[m]]};
    e = e //. {sum :> Sum, if :> If};
    z^n0 iPowerSum[e, z, n]
    )
 ]  /; !(Head[expr]===List || Head[expr]===Equal) && NumberQ[N[n0]]


    (* the geometric series *)

iPowerSum[1, z_, n_] := 1/(1 - z)


    (* linearity *)

iPowerSum[c_ expr_., z_, n_] :=
    c iPowerSum[expr, z, n]  	/; FreeListQ[c, {n, Blank}]

iPowerSum[x_Plus, z_, n_] :=
  Module[{xlist = Apply[List, x], xn, xconstant, result, result2},
    xn = Apply[Plus, Select[xlist, (!FreeQ[#, n])&]];
    xconstant = Apply[Plus, Select[xlist, FreeQ[#, n]&]];
    result = xconstant/(1-z)  +
	If[Head[xn]===Plus,
                Map[iPowerSum[#, z, n]&, xn],
                iPowerSum[xn, z, n]
        ];
    If[Head[result] === Plus,
          result2 = Expand[result];
          If[Head[result2] =!= Plus ||
             Length[result2] < Length[result],
                result = result2
          ]
    ];
    result
  ]   /; !FreeQ[x, n] && FreeQ[z, n]

iPowerSum[x_Plus f_, z_, n_] :=
  Module[{xlist = Apply[List, x], xn, xconstant},
    (
    xn = Apply[Plus, Select[xlist, (!FreeQ[#, n])&]];
    xconstant = Apply[Plus, Select[xlist, FreeQ[#, n]&]];
    xconstant iPowerSum[f, z, n] + iPowerSum[Expand[xn f], z, n]
    ) /; Expand[x f] =!= x f
  ]   /; !FreeQ[x, n] && FreeQ[z, n] && !MatchQ[f, 1/n] && FreeQ[f, BernoulliB]

iPowerSum[Sum[a_, {k_, lo_, hi_}] b_., z_, n_] :=
   (
    Sum[Evaluate[iPowerSum[Expand[a b], z, n]], Evaluate[{k, lo, hi}]]
   ) /; FreeQ[{lo, hi}, n] && FreeQ[b, k]


    (* powers *)

iPowerSum[c_^((a_ + b_)d_) e_., rest__] :=
	(
    iPowerSum[c^(a d + b d) e, rest]
	)

iPowerSum[c_^(a_ + b_) e_., z_, n_] :=
	(
    c^a iPowerSum[c^b e, z, n]
	)	/; FreeListQ[c, {n, Blank}] && FreeListQ[a, {n, Blank}]

iPowerSum[(a_ + b_)^c_Integer?Positive d_., rest__] :=
	(
    iPowerSum[Expand[(a + b)^c d], rest]
	)


    (* exponentials *)

iPowerSum[e_. c_^((a_. n_ + b_.)d_.), z_, n_] :=
    (
	c^(b d) iPowerSum[e, z, n] /. z -> c^(a d) z
    )	/;	(
		 (FreeListQ[#, {n, Blank}]& /@ And[a, b, c, d]) &&
		 FreeListQ[e, {z, Blank}]
		)

iPowerSum[(1 - Exp[(k_ n_)]) / n_, z_, n_] :=
  Module[{alpha},
    (
	alpha + Log[(1/z - Exp[-alpha])/(1/z - 1)]
    ) /; (alpha = Coefficient[-k n, n];
	  MatchQ[alpha, _Symbol] || (NumberQ[N[alpha]] && N[alpha] > 0)
	 )
  ] /; FreeQ[k, n] && !TrueQ[k > 0]	(* Jury 34, new in V3.0 *)


    (* binomials *)

iPowerSum[Binomial[a_, n_ + k_.], z_, n_] :=
    (
    z^(-k) * ((1 + z)^a -
	Sum[Evaluate[Binomial[a, m] z^m], Evaluate[{m, 0, k - 1}]])
    )	/;	FreeQ[a, n] && IntegerQ[k]

iPowerSum[Binomial[a_, b_], z_, n_] :=
  Block[{e = Expand[n - b], r = Expand[a - 2 b], m},
   (
    z^e (   ((1 - Sqrt[1 - 4 z])/(2 z))^r / Sqrt[1 - 4 z] -
            	Sum[Evaluate[Binomial[2 m + r, m] z^m],
		    Evaluate[{m, 0, -(e + 1)}]]		)
   ) /; IntegerQ[e] && IntegerQ[r]
  ]


    (* trinomials *)

iPowerSum[Sum[a_. b_Binomial c_Binomial, {k_, 0, n_}], z_, n_] :=
    Block[{aa = a^(1/k)},
     (
    	1/Sqrt[1 - 2 (2 aa + 1) z + z^2]
     )		/; (FreeListQ[aa, {k, n}] &&
        	    MakeTrinomial[b c] == Sort[Multinomial[k, k, n - k]])
    ]

iPowerSum[Sum[a_. b_Binomial c_Binomial, {k_, 0, n_}], z_, n_] :=
    Block[{aa = a^(1/(n - k))},
     (
    	1/Sqrt[1 - 2 (2 aa + 1) z + z^2]
     )		/;  (FreeListQ[aa, {k, n}] &&
       		     MakeTrinomial[b c] == Sort[Multinomial[k, n - k, n - k]])
    ]

iPowerSum[Sum[a_. b_Binomial c_Binomial, {k_, 0, n_}], z_, n_] :=
    Block[{aa = a^(1/k)},
     (
    	1/Sqrt[1 - 2 z + (1 - 4 aa) z^2]
     )		 /; (FreeListQ[aa, {k, n}] &&
        	     MakeTrinomial[b c] == Sort[Multinomial[k, k, n - 2 k]])
    ]

iPowerSum[Sum[a_. b_Binomial c_Binomial, {k_, 0, n_}], z_, n_] :=
    Block[{aa = a^(1/(n - k))},
     (
    	1/Sqrt[1 - 2 z + (1 - 4 aa) z^2]
     )	/; (FreeListQ[aa, {k, n}] &&
            MakeTrinomial[b c] == Sort[Multinomial[n - k, n - k, 2 k - n]])
    ]


    (* iPowerSum is the inverse of iSeriesTerm *)

iPowerSum[HoldPattern[iSeriesTerm[f_, z_, n_ + m_.]], z_, n_] :=
  Block[{k},
   (
    z^(-m) (f - Sum[Evaluate[SeriesTerm[f, {z, 0, k}] z^k],
		    Evaluate[{k, 0, m - 1}]])
   ) /; PoleMultiplicity[f, {z, 0}] == 0 && IntegerQ[m]
  ]


    (* trigonometric rules *)

iPowerSum[Cos[n_], rest__] :=
    iPowerSum[(E^(I n) + E^(-I n))/2, rest]

iPowerSum[Sin[n_], rest__] :=
    iPowerSum[(E^(I n) - E^(-I n))/2/I, rest]

iPowerSum[Sin[alpha_. n_]/n_, z_, n_] :=
  Module[{},
	alpha + ArcTan[Sin[alpha]/(z^(-1) - Cos[alpha])]
  ] /;	!TrueQ[alpha < 0] (* Jury 34, new in V3.0 *)


    (* hyperbolic rules *)	(* new in V3.0 *)

iPowerSum[Cosh[n_], rest__] :=
    iPowerSum[(E^n + E^(-n))/2, rest]

iPowerSum[Sinh[n_], rest__] :=
    iPowerSum[(E^n - E^(-n))/2, rest]


    (* factorial inhomogeneity *)

iPowerSum[expr_./(n_ + a_.)!, z_, n_] :=
  Block[{k, eps, sum},
   (
    (eps - sum) / z^a
   ) /; FreeQ[eps = ExponentialPowerSum[Expand[expr /. n -> n - a], {z, n}],
			ExponentialPowerSum] &&
	FreeQ[sum = Sum[(expr /. n -> k - a) z^k / k!, {k, 0, a - 1}], Sum]
  ] /;  IntegerQ[a]


    (* other factorial rules *)

iPowerSum[1/((2 n_)!), z_, n_] := Cosh[Sqrt[z]]	(* CRC z-transform rule 32 *)

iPowerSum[1/((2 n_)!) EulerE[2 n_], z_, n_] := Sech[Sqrt[z]]

iPowerSum[1/((n_)!) EulerE[n_], z_, n_] := Sech[z]

iPowerSum[(-1)^n2_ (-1 + 2^n1_) BernoulliB[n1_] / ((n1_)!), z_, n_] :=
  (
	(Tan[z/2] + I)/2
  ) /; Expand[n1] == n+1 && Expand[2 n2] == n-1

iPowerSum[(2^n1_ - 1) BernoulliB[n1_] / ((n1_)!), z_, n_] :=
  (
	(Tanh[z/2] - 1)/2
  ) /; Expand[n1] == n+1


    (* generating functions of sequences
      (here we assume that a[n] is defined for n >= 0) *)

iPowerSum[n_^m_. a_[n_ + d_.], z_, n_] :=
    Block[{k, sum},
         sum = Apply[Plus,
		Map[(StirlingS2[m, #] z^# *
            	     Derivative[#][(iPowerSum[a[k], z, k] -
                     Sum[a[k] z^k, {k, 0, d - 1}])/z^d ])&,
		    Range[0, m]]];
	 Factor[sum]
    ] /; IntegerQ[d] && TTQ[d >= 0] && IntegerQ[m] && TTQ[m >= 0] &&
	(Head[a] =!= Symbol || Context[a] =!= "System`")

iPowerSum[a_[k_. n_ + d_.], z_, n_] :=
    Block[{j, m},
        (z^(-d/k)/k Sum[Omega[k]^(-d j) (Gf[a][Omega[k]^j z^(1/k)] -
            Sum[Omega[k]^(m j) a[m] z^(m/k), {m, 0, d-1}]),
            {j, 0, k - 1} ] ) // Factor
    ] /; IntegerQ[d] && TTQ[d >= 0] && IntegerQ[k] && TTQ[k > 0] &&
	(Head[a] =!= Symbol || Context[a] =!= "System`")



    (* rational inhomogeneity *)

iPowerSum[n_^m_., z_, n_] :=
    Block[{sum},
	sum = Apply[Plus,
	       Map[( (StirlingS2[m, #] #! z^#)/(1 - z)^(# + 1) )&,
	           Range[0, m]]];
        Factor[sum]
    ] /; IntegerQ[m] && TTQ[m >= 0]


iPowerSum[(n_ + a_)^m_Integer?Negative, z_, n_] :=
    Block[{j},
        z^(-a) PolyLog[-m, z] - Sum[z^(j-a) j^m, {j, 1, a-1}]
    ] /;	IntegerQ[a] && TTQ[a > 0]


    (* convolutions *)

iPowerSum[Sum[expr_, {k_, lo_, hi_}], z_, n_] :=
    Block[{aa, bb, nfree, nfull, s, expand, ips, result, sum, if},
         (
          {nfree, nfull} = If[Head[expr] === Times,
           			{Select[expr,  FreeQ[#, n]&],
					Select[expr, !FreeQ[#, n]&]},
	        		If[FreeQ[expr, n],
					{expr, 1},
					{1, expr}
				]
			];
	  nfull = nfull /. {Sum :> sum, If :> if};
	  expand = Expand[nfull /. n :> (s + k)];
	  ips = iPowerSum[expand, z, s, -k];
	  result = sum[z^k nfree ips, {k, aa, bb - 1}];
	  result = result /. {sum :> Sum};
	  ips = iPowerSum[expand, z, s, -bb];
	  result + iPowerSum[nfree ips, z, k, Max[aa, bb] ]
    	 ) /; FreeQ[aa = Expand[lo], n] && FreeQ[bb = Expand[hi - n], n]
    ]

    (* conditionals *)

iPowerSum[If[Odd[n_], a_, 0], z_, n_] :=
  (
	Tanh[z]
  ) /; MatchQ[a, 2^n1_ (-1 + 2^n1_) BernoulliB[n1_] / ((n1_)!) /; n1 == n+1]

iPowerSum[If[Odd[n_], a_, 0], z_, n_] :=
  (
	Tan[z]
  ) /; MatchQ[a, (-1)^n2_ 2^n1_*(-1 + 2^n1_)* BernoulliB[n1_] / ((n1_)!) /;
			   n1 == n+1 && Expand[n2] == n/2 - 1/2]


    (* multisection of series *)

iPowerSum[If[SymbolicMod[n_ + d_., m_] == k_, a_, b_] c_., z_, n_] :=
    Block[{l = Mod[k - d, m], jj,
           fps = iPowerSum[Expand[(a - b)c], z, n]},
        iPowerSum[Expand[b c], z, n] +
           1/m Sum[Omega[m]^(jj (m - l)) fps /. z -> z Omega[m]^jj,
           {jj, 0, m - 1}] // Expand
    ]

(* NOTE: added new in V3.0 *)
iPowerSum[If[Even[n_], a_, b_] c_., z_, n_] :=
 Block[{jj, fps, gps, sum, result, summand, m},
  (
	result
  ) /; (
        If[FreeQ[fps = iPowerSum[Expand[(a - b)c], z, n], iPowerSum] &&
	   FreeQ[gps = iPowerSum[Expand[b c], z, n], iPowerSum] &&
	   FreeQ[sum =
	     Sum[Omega[2]^(2 jj) fps /. z -> z Omega[2]^jj, {jj, 0, 1}], Sum],
             result = Expand[gps + 1/2 sum],
	     (* use built-in Sum to implement PowerSum *)
	     summand = ((a c z^n) /. {n :> 2m}) +
		       ((b c z^n) /. {n :> (2m+1)});
	     If[FreeQ[sum = Sum[Evaluate[summand],
				 {m, 0, Infinity}], Sum],
		result = sum,
		   result = $Failed
	     ]
	];
	result =!= $Failed
       )
 ]

(* NOTE: added new in V3.0 *)
iPowerSum[If[Odd[n_], a_, b_] c_., z_, n_] :=
 Block[{jj, fps, gps, sum, result, summand, m},
  (
	result
  ) /; (
        If[FreeQ[fps = iPowerSum[Expand[(a - b)c], z, n], iPowerSum] &&
	   FreeQ[gps = iPowerSum[Expand[b c], z, n], iPowerSum] &&
	   FreeQ[sum =
		  Sum[Omega[2]^jj fps /. z -> z Omega[2]^jj, {jj, 0, 1}], Sum],
		result = Expand[gps + 1/2 sum],
		(* use built-in Sum to implement PowerSum *)
		summand = ((a c z^n) /. {n :> (2m+1)}) +
			  ((b c z^n) /. {n :> 2m});
		If[FreeQ[sum = Sum[Evaluate[summand],
				 {m, 0, Infinity}], Sum],
		   result = sum,
		   result = $Failed
		]
	];
	result =!= $Failed
       )
 ]


    (* other conditionals *)

iPowerSum[If[n_ == n0_Integer, a_, 0], z_, n_] :=
	( (a /. n:>n0) z^n0) /; n0 >= 0

iPowerSum[If[n_ == n0_Integer, a_, 0], z_, n_] := 0 /; n0 < 0

iPowerSum[If[cond_, a_, b_] c_., z_, n_] :=
    Block[{t, tt, if, aa = a, bb = b, result, s, k, sum},
      (
	aa = aa /. {If :> if};
	bb = bb /. {If :> if};
        result = Plus @@ (sum[Expand[aa c] z^n, {n, First[#], Last[#]}]& /@ t) +
                 Plus @@ (sum[Expand[bb c] z^n, {n, First[#], Last[#]}]& /@ tt);
	result = result  /.
               {sum[s_. z^n, {n, k_, Infinity}] :> PowerSum[s, {z, n, k}],
                sum[0, {__}] -> 0};
	result = result /. {sum -> Sum};
	result /. {if -> If}
     ) /; (t = IneqSolve[cond && n >= 0, n];
	   tt = IneqSolve[!cond && n >= 0, n];
	   (t === {} || !FreeQ[t, RSInterval]) &&
	   (tt === {} || !FreeQ[tt, RSInterval])
	  )
    ]

    (* iPowerSum under Series *)

iPowerSum /:
    Series[iPowerSum[a_, z_, n_], {z_, 0, m_}] :=
        Module[{k},
          Sum[(a /. n -> k) z^k, {k, 0, m}] + O[z]^(m+1)
	]

    (* using Sum and ZTransform *)

iPowerSum[expr_, z_, n_Symbol] :=
   Module[{result, w},
	(
	result
	) /; (
	      If[FreeQ[result = ZTransform[expr, n, w], ZTransform],
		 result = result /. {w :> 1/z};
		 True,
	         FreeQ[result = Sum[expr z^n, {n, 0, Infinity}], Sum]
	     ])
   ]



(* ======================== EXPONENTIAL POWER SUM ========================== *)
(* Note that ExponentialPowerSum[a[n], {z, n}] gives EGf[a][z],
	but Sum[a[n]/n! z^n, {n, 0, Infinity}] remains unevaluated.
   So ExponentialPowerSum is more useful than Sum for symbolic
	functions of n.   This is why internal iExponentialPowerSum rules
	are tried before trying Sum.
*)

    (* lists and equations *)

ExponentialPowerSum[expr_, {z_, n_Symbol, n0_:0}] :=
    (
    ExponentialPowerSum[#, {z, n, n0}]& /@ expr
    ) /; NumberQ[N[n0]] && n0 >= 0 &&
	 (Head[expr]===List || Head[expr]===Equal)


    (* apply internal iExponentialPowerSum rules;  these rules include
	application of Sum *)

(* NOTE: eliminated rules transforming Even or Odd to SymbolicMod expressions *)
ExponentialPowerSum[expr_, {z_, n_Symbol, n0_:0}] :=
    (issueObsoleteFunMessage[ExponentialPowerSum, "DiscreteMath`RSolve`"];
    Block[{e, e0, sum, if, result},
     (
	result
     ) /; (
	      e = expr //. {Sum :> sum, If :> if};
	      e0 = (e /. {n :> n + n0}) n! / (n+n0)!;
	      e0 = e0 /.
                      {a_Symbol?(Context[#] =!= "System`" && # =!= K &&
                         # =!= Even && # =!= Odd &)[m_] :> a[Expand[m]]};
	      e0 = e0 /. {sum :> Sum, if :> If};
	      e0 = e0 /. {Sum[a_, {k_, m_}] :> Sum[a, {k, 1, m}],
                         Mod :> SymbolicMod};
	      (* Apply iExponentialPowerSum rules. *)
	      FreeQ[result = z^n0 iExponentialPowerSum[e0, z, n],
				iExponentialPowerSum]
          )
    ] /; NumberQ[N[n0]] && n0 >= 0 &&
	!(Head[expr]===List || Head[expr]===Equal))


(* ===================== internal EXPONENTIAL POWER SUM =================== *)


    (* lists and equations *)

iExponentialPowerSum[expr_, z_, n_, n0_:0] :=
        (
        iExponentialPowerSum[#, z, n, n0]& /@ expr
        ) /; Head[expr]===List || Head[expr]===Equal


    (* non-zero starting point *)
    (* (This rule is needed because the "convolution" iExponentialPowerSum rule
		returns a 4-argument iPowerSum.) *)

iExponentialPowerSum[expr_, z_, n_, n0_] :=
 Module[{e, sum, if},
    (
    e = expr //. {Sum :> sum, If :> if};
    e = e /. {n :> n + n0};
    e = e /. {a_Symbol?(Context[#] =!= "System`" && # =!= K &&
                   # =!= Even && # =!= Odd &)[m_] :> a[Expand[m]]};
    e = e //. {sum :> Sum, if :> If};
    z^n0 iExponentialPowerSum[e, z, n]
    )
 ]  /; !(Head[expr]===List || Head[expr]===Equal) && NumberQ[N[n0]]

    (* the exponential series *)

iExponentialPowerSum[1, z_, n_] := E^z


    (* linearity *)

iExponentialPowerSum[c_ expr_., z_, n_] :=
    c iExponentialPowerSum[expr, z, n] /; FreeListQ[c, {n, Blank}]

iExponentialPowerSum[(c_.)(expr1_ + expr2_), rest__] :=
 (
    iExponentialPowerSum[c expr1, rest] +
    iExponentialPowerSum[c expr2, rest]
 )

iExponentialPowerSum[Sum[a_, {k_, lo_, hi_}] b_., z_, n_] :=
    Sum[iExponentialPowerSum[Expand[a b], z, n], {k, lo, hi}] /;
                                   FreeQ[{lo, hi}, n] && FreeQ[b, k]


    (* powers *)

iExponentialPowerSum[c_^((a_ + b_)d_), rest__] :=
    iExponentialPowerSum[c^(a d + b d), rest]

iExponentialPowerSum[c_^(a_ + b_), z_, n_] :=
    c^a iExponentialPowerSum[c^b, z, n] /;
                        FreeListQ[c, {n, Blank}] && FreeListQ[a, {n, Blank}]


    (* exponentials *)

iExponentialPowerSum[e_. c_^((a_. n_ + b_.)d_.), z_, n_] :=
    (c^(b d) iExponentialPowerSum[e, z, n] /. z -> c^(a d) z) /;
  (FreeListQ[#, {n, Blank}]& /@ And[a, b, c, d]) && FreeListQ[e, {z, Blank}]


    (* polynomial inhomogeneity *)

iExponentialPowerSum[n_^m_., z_, n_] :=
    Block[{sum},
	sum = Apply[Plus, Map[( StirlingS2[m, #] z^# )&,
		Range[0, m] ]];
        E^z Factor[sum]
    ] /; IntegerQ[m] && TTQ[m >= 0]


    (* factorial inhomogeneity *)

iExponentialPowerSum[(n_ + a_.)!, z_, n_] :=
  (
    PowerSum[Pochhammer[n + 1, a], {z, n}]
  ) /; IntegerQ[a]


    (* convolutions *)

iExponentialPowerSum[Sum[expr_ Binomial[n_ + cc_, k_], {k_, lo_, hi_}],
    z_, n_] :=
    Block[{aa, bb, e, sum, if},
     (
	e = expr /. {Sum :> sum, If :> if};
	e = e /. n :> (n-cc);
	e = e /. {sum :> Sum, if :> If};

        Derivative[cc][iExponentialPowerSum[
	  Sum[Evaluate[e Binomial[n, k]], {k, aa, n + bb - cc} ],
		z, n ] ]
     ) /; FreeQ[aa = Expand[lo], n] && FreeQ[bb = Expand[hi - n], n]
    ] /; IntegerQ[cc] && TTQ[cc > 0]

iExponentialPowerSum[Sum[expr_ Binomial[n_, k_], {k_, lo_, hi_}], z_, n_] :=
    Block[{aa, bb, nfree, nfull, s, expand, ieps, result, sum, if},
     (
       {nfree, nfull} = If[Head[expr] === Times,
           {Select[expr,  FreeQ[#, n]&], Select[expr, !FreeQ[#, n]&]},
	        If[FreeQ[expr, n], {expr, 1}, {1, expr}] ];
	nfull = nfull /. {Sum :> sum, If :> if};
	expand = Expand[nfull /. n :> (s + k)];
	ieps = iExponentialPowerSum[expand, z, s, -k];
	result = sum[z^k nfree/k! ieps, {k, aa, bb - 1}];
	result = result /. {sum :> Sum, if :> If};
	ieps = iExponentialPowerSum[expand, z, s, -bb];
	result + iExponentialPowerSum[nfree ieps, z, k, Max[aa, bb] ]
     ) /; FreeQ[aa = Expand[lo], n] && FreeQ[bb = Expand[hi - n], n]
    ]


    (* multisection of series *)

iExponentialPowerSum[If[SymbolicMod[n_ + d_., m_] == k_, a_, b_] c_., z_, n_] :=
    Block[{l = Mod[k - d, m], jj,
           fps = iExponentialPowerSum[Expand[(a - b)c], z, n]},
        iExponentialPowerSum[Expand[b c], z, n] +
           1/m Sum[Omega[m]^(jj (m - l)) fps /. z -> z Omega[m]^jj,
           {jj, 0, m - 1}] // Expand
    ]

(* NOTE: added new in V3.0 *)
iExponentialPowerSum[If[Even[n_], a_, b_] c_., z_, n_] :=
    Block[{jj, fps = iExponentialPowerSum[Expand[(a - b)c], z, n]},
        iExponentialPowerSum[Expand[b c], z, n] +
           1/2 Sum[Omega[2]^(2 jj) fps /. z -> z Omega[2]^jj, {jj, 0, 1}] //
        	Expand
    ]

(* NOTE: added new in V3.0 *)
iExponentialPowerSum[If[Odd[n_], a_, b_] c_., z_, n_] :=
    Block[{jj, fps = iExponentialPowerSum[Expand[(a - b)c], z, n]},
        iExponentialPowerSum[Expand[b c], z, n] +
           1/2 Sum[Omega[2]^jj fps /. z -> z Omega[2]^jj, {jj, 0, 1}] //
        	Expand
    ]


    (* other conditionals *)

iExponentialPowerSum[If[cond_, a_, b_] c_., z_, n_] :=
    Block[{t, tt, temp, s, k, sum},
     (
	temp = If[TrueQ[a==0], 0,
		  Plus @@
		  (Sum[Expand[a c] z^n / n!, {n, First[#], Last[#]}]& /@ t)] +
	       If[TrueQ[b==0], 0,
		  Plus @@
		  (Sum[Expand[b c] z^n / n!, {n, First[#], Last[#]}]& /@ tt)];
	If[!FreeQ[temp, Sum],
	   temp = temp /. {Sum :> sum};
	   temp = temp /. {sum[s_. z^n / n!, {n, k_, Infinity}] :>
				      ExponentialPowerSum[s, {z, n, k}]};
	   temp = temp /. {sum :> Sum}];
	temp
     ) /; (FreeQ[(t = IneqSolve[cond && n >= 0, n]), IneqSolve] &&
	   FreeQ[(tt = IneqSolve[!cond && n >= 0, n]), IneqSolve] &&
	   t =!= $Failed && tt =!= $Failed)
    ]


    (* generating functions of sequences
      (here we assume that a[n] is defined for n >= 0) *)

iExponentialPowerSum[a_[n_ + d_.], z_, n_] :=
  (
    Derivative[d][EGf[a][z]]
  )  /; IntegerQ[d] && TTQ[d >= 0] &&
	(Head[a] =!= Symbol || Context[a] =!= "System`")

iExponentialPowerSum[n_^m_. a_[n_ + d_.], z_, n_] :=
    Block[{k, sum},
        sum  = Apply[Plus, Map[( StirlingS2[m, #] z^# *
            			 Derivative[d + #][EGf[a][z]] )&,
			       Range[0, m] ]];
	Factor[sum]
    ] /; IntegerQ[d] && TTQ[d >= 0] && IntegerQ[m] && TTQ[m >= 0] &&
	(Head[a] =!= Symbol || Context[a] =!= "System`")

iExponentialPowerSum[n_^m_. a_[n_ - 1], z_, n_] :=
    Block[{k, sum},
        sum = Apply[Plus, Map[ (StirlingS2[m, #] z^# *
				Derivative[# - 1][EGf[a][z]] )&,
		Range[m] ]];
	Factor[sum]
    ] /; IntegerQ[m] && TTQ[m > 0] &&
	(Head[a] =!= Symbol || Context[a] =!= "System`")


	(* using Sum *)

iExponentialPowerSum[expr_, z_, n_Symbol] :=
   Module[{result},
	(
	result
	) /; (
	      FreeQ[result = Sum[expr/n! z^n, {n, 0, Infinity}], Sum]
	     )
   ]


(* ========================= GENERATING FUNCTION ========================= *)

GeneratingFunction[eqn_, fcn_, n_, z_, opts___Rule] :=
 (issueObsoleteFunMessage[GeneratingFunction, "DiscreteMath`RSolve`"];
 Block[{elist, flist, temp, recur, conds, unknowns, startValues,
           start, rec, con, gfc, i, prs},
  (
   temp /. aa_?(MemberQ[unknowns,#]&)[kk_] :> aa[Expand[kk + start[aa]]]
  ) /; (

	If[!FreeQ[{opts}, PrecisionHS],
		 Message[GeneratingFunction::obsoptprec]];
	elist = MakeList[eqn]; flist = MakeList[fcn];
	If[!Apply[And, Map[(Head[#] === Equal)&, elist]],
	   Message[GeneratingFunction::eqn, elist];  False,
	   prs = Parse[elist, flist, n];
	   If[Head[prs] === Fail,
	      Message[GeneratingFunction::recur, prs[[1]]];
	      False,
	      {recur, conds, unknowns, startValues} = prs;
	      {rec, con} = Reset[recur, conds, unknowns, startValues, 0];
	      (Evaluate[start /@ unknowns]) = startValues;
	      gfc = GeneratingFunctionConstants /. {opts} /.
		 Options[GeneratingFunction];
	      temp = FunctionSolve[rec, con, unknowns, n, PowerSum, z, gfc];
	      If[FreeQ[temp, $Failed],
		       (* FunctionSolve may return expressions in terms of
			  HypergeometricF. *)
		       temp = temp //. {HypergeometricF :> HypergeometricPFQ};
		       temp = Table[#[[i]] z^startValues[[i]],
                		{i, Length[unknowns]} ]& /@ temp;
		       True,
		       If[Length[temp] > 0,
		    	  Message[GeneratingFunction::intind, temp[[1]]] ];
	   	       False
	      ] (* end If FreeQ[temp, $Failed] *)
	   ] (* end If prs === $Failed *)
	])
    ] /; If[FreeQ[eqn, Fail] && FreeQ[eqn, Condition], True,
	    Message[GeneratingFunction::obssyn]; False])

GeneratingFunction::obsoptprec =
"Warning: PrecisionHS is an obsolete option.  The result of GeneratingFunction \
is always exact."

GeneratingFunction::obssyn =
"Warning:  It is obsolete to specify an equation eqn valid for condition cond \
using the syntax Condition[eqn, cond] or eqn /; cond."

GeneratingFunction::intind =
"The initial conditions `` must have integral indices."

GeneratingFunction::eqn = "`` is not an equation or a system of equations."

GeneratingFunction::range =
"Unable to solve for the valid range of `1` using assumption `2`."

GeneratingFunction::recur =
"Unable to shift recurrence equations to solve for `` in terms of the \
initial conditions."


(* ====================== EXPONENTIAL GENERATING FUNCTION =================== *)

ExponentialGeneratingFunction[eqn_, fcn_, n_, z_, opts___Rule] :=
 (issueObsoleteFunMessage[ExponentialGeneratingFunction, "DiscreteMath`RSolve`"];
 Block[{elist, flist, temp, recur, conds, unknowns, startValues,
           start, rec, con, egfc, prs},
  (
   temp /. aa_?(MemberQ[unknowns,#]&)[kk_] :> aa[Expand[kk + start[aa]]]
  ) /; (

	If[!FreeQ[{opts}, PrecisionHS],
		 Message[ExponentialGeneratingFunction::obsoptprec]];
	elist = MakeList[eqn];  flist = MakeList[fcn];
	If[!Apply[And, Map[(Head[#] === Equal)&, elist]],
	   Message[ExponentialGeneratingFunction::eqn, elist];  False,
	   prs = Parse[elist, flist, n];
           If[Head[prs] === Fail,
	      Message[ExponentialGeneratingFunction::recur, prs[[1]]];
	      False,
              {recur, conds, unknowns, startValues} = prs;
	      {rec, con} = Reset[recur, conds, unknowns, startValues, 0];
              (Evaluate[start /@ unknowns]) = startValues;
	      egfc = ExponentialGeneratingFunctionConstants /. {opts} /.
			Options[ExponentialGeneratingFunction];
              temp = FunctionSolve[rec, con, unknowns, n,
				ExponentialPowerSum, z, egfc];
              If[FreeQ[temp, $Failed],
		       (* FunctionSolve may return expressions in terms of
                        	HypergeometricF. *)
                       temp = temp //. {HypergeometricF :> HypergeometricPFQ};
		       True,
		       If[Length[temp] > 0,
	                  Message[ExponentialGeneratingFunction::intind,
			  temp[[1]]] ];
		       False
              ] (* end If FreeQ[temp, $Failed] *)
	   ] (* end If prs === $Failed *)
	])
    ] /; If[FreeQ[eqn, Fail] && FreeQ[fcn, Condition], True,
            Message[ExponentialGeneratingFunction::obssyn]; False])

ExponentialGeneratingFunction::obsoptprec =
"Warning: PrecisionHS is an obsolete option.  The result of \
ExponentialGeneratingFunction is always exact."

ExponentialGeneratingFunction::obssyn =
"Warning:  It is obsolete to specify an equation eqn valid for condition cond \
using the syntax Condition[eqn, cond] or eqn /; cond."

ExponentialGeneratingFunction::intind =
"The initial conditions `` must have integral indices."

ExponentialGeneratingFunction::eqn =
"`` is not an equation or a system of equations."

ExponentialGeneratingFunction::range =
"Unable to solve for the valid range of `1` using assumption `2`."

ExponentialGeneratingFunction::recur =
"Unable to shift recurrence equations to solve for `` in terms of the \
initial conditions."

(* ================================= HSOLVE =============================== *)

HSolve::obsoptprec =
"Warning: PrecisionHS is an obsolete option.  The result of HSolve is always \
exact as are the results of Solve and DSolve."

HSolve[eqn_, fcn_, x_Symbol, opts___] :=
    (issueObsoleteFunMessage[HSolve, "DiscreteMath`RSolve`"];
    Block[{hsolvec, hsol},
      (
       (* iHSolve may export results in terms of HypergeometricF *)
       hsol = hsol //. {HypergeometricF :> HypergeometricPFQ}
      ) /; (If[!FreeQ[{opts}, PrecisionHS], Message[HSolve::obsoptprec]];
	    hsolvec = HSolveConstants /. {opts} /. Options[HSolve];
	    hsol = iHSolve[eqn, fcn, x, hsolvec];
	    hsol =!= $Failed)
    ])

(* iHSolve is called by HSolve (an external function) and FunctionSolve
   (an internal function). *)
iHSolve[eq_, f_, x_, hsolvec_] :=

    Block[{eqn, y, terms, weights, minw, intnm, theta, inhom,
	   pm, lhs, rhs, llc, rlc,
	   rootList, aList, bList,
	   badB, nHS, n0, badB1, nneg, n1, cC, subst, k, temp, ll},

        If[Head[eq] === List,
            If[Length[eq] > 1, Return[$Failed]];
            eqn = First[eq],
            eqn = eq ];
        If[Head[f] === List,
            If[Length[f] > 1, Return[$Failed]];
            y = Head[First[f]],
            y = Head[f] ];
        lhs = Expand[Numerator[Together[First[eqn] - Last[eqn]]]];
        terms = Select[MakeList[lhs, Plus], !FreeQ[#, y]&];
        weights = Union[Weight[#, y[x], x]& /@ terms];
        minw = Min[weights];
        If[!MemberQ[{{0},{0,1}}, weights - minw],
		Return[$Failed]];
        intnm = (Head[StirlingS1::intnm] === $Off);
	Off[StirlingS1::intnm];
        lhs = Expand[lhs/x^minw] /. x^k_.Derivative[n_][y][x] ->
            x^(k - n) Sum[StirlingS1[n, j] theta^j y[x], {j,0,n}]
            // Expand;
	If[!intnum, On[StirlingS1::intnm]];
        inhom = (Plus @@ Select[MakeList[lhs, Plus], FreeQ[#, y]&]) // Expand;
        pm = PoleMultiplicity[inhom, {x, 0}];
        If[!(IntegerQ[pm] && pm >= 0), Return[$Failed]];
        If[!PolynomialQ[x^pm inhom, x], Return[$Failed]];
        lhs = (lhs - inhom) / y[x] // Expand;
        rhs = Plus @@ Select[MakeList[lhs, Plus], FreeQ[#, x]&];
        lhs = Expand[(rhs - lhs)/x];
        {llc, rlc} = {LeadingCoef[lhs, theta], LeadingCoef[rhs, theta]};
	rootList = {ToRules[Roots[lhs == 0, theta,
			Cubics -> False, Quartics -> False]]};
        aList = If[FreeQ[lhs, theta], {}, -#[[1,2]]& /@   rootList ];
	rootList = {ToRules[Roots[rhs == 0, theta,
                        Cubics -> False, Quartics -> False]]};
	bList = If[FreeQ[rhs, theta], {}, (1-#[[1,2]])& /@   rootList ];
        badB = Select[bList, (IntegerQ[#] && # <= 0)&];
        nHS = If[badB != {}, 1 - Min[badB], 0];
        n0 = Max[nHS, Exponent[inhom, x]];
        badB1 = Select[bList, (IntegerQ[#] && # > 0)&];
        nneg = If[badB1 != {}, Max[badB1] - 1, 0];
        n1 = Max[nneg, pm];
        cC[-n1 - 1] = 0;
        subst = {ToRules[Reduce[Table[If[n + 1 == 0,
            Coefficient[inhom, x, 0] /. x -> 1/x /. x -> 0,
            Coefficient[inhom, x^(n+1)]] +
            rlc (Times @@ (bList + n) cC[n+1])
             == llc (Times @@ (aList + n) cC[n]), {n, -n1 - 1, n0 - 1}],
            Table[cC[n], {n, n0, -n1, -1}] ]]};
        If[subst === {}, Return[{}] ];

        If[Times @@ (Pochhammer[# + nHS, n0 - nHS]& /@ aList) =!= 0 &&
           llc =!= 0,

            (* then *)

            {aList, bList} = {aList, bList} + nHS;
            If[!MemberQ[bList, 1], AppendTo[aList, 1];
                               AppendTo[bList, 1] ];
            bList = Drop[bList, {FirstPos[bList, 1]}];
            If[MemberQ[aList, #], aList = Drop[aList,
                                 {FirstPos[aList, #]}];
                              bList = Drop[bList,
                                 {FirstPos[bList, #]}]
              ]& /@ bList;
            aList = Sort[aList];
            bList = Sort[bList];

            temp = (  Sum[cC[k] x^k, {k, -n1, n0 - 1}] +
                      cC[n0] (llc/rlc)^(nHS - n0) *
                	(Times @@ (Pochhammer[#, n0 - nHS]& /@ bList)) /
                	(Times @@ (Pochhammer[#, n0 - nHS]& /@ aList)) *
			(n0 - nHS)! x^nHS *
                	( HypergeometricF[aList, bList, llc/rlc x] -
                          Sum[(Times @@ (Pochhammer[#, k]& /@ aList)) /
                              (Times @@ (Pochhammer[#, k]& /@ bList)) *
			      (llc/rlc x)^k / k!,
                    	      {k, 0, n0 - nHS - 1}]
			)
		   ) /. subst // Simplify,

            (* else *)

            {aList, bList} = {aList, bList} + n0;
            If[!MemberQ[bList, 1], AppendTo[aList, 1];
                               AppendTo[bList, 1] ];
            bList = Drop[bList, {FirstPos[bList, 1]}];
            If[MemberQ[aList, #], aList = Drop[aList,
                                 {FirstPos[aList, #]}];
                              bList = Drop[bList,
                                 {FirstPos[bList, #]}]
              ]& /@ bList;
            aList = Sort[aList];
            bList = Sort[bList];

            temp = (  Sum[cC[k] x^k, {k, -n1, n0 - 1}] +
                      cC[n0] x^n0 HypergeometricF[aList, bList, llc/rlc x]
		   ) /. subst // Expand

	];

        temp = Thread[y[x] -> #]& /@ {temp};

        ll = PList[temp, cC[_]];
        temp /. Thread[ll -> Table[hsolvec[k], {k, Length[ll]}]]
    ]	(* end iHSolve *)



(* ================================= WEIGHT =============================== *)

Weight[a_. x_^k_. Derivative[n_][y_][x_], y_[x_], x_] := k - n /;
                                                 FreeListQ[a, {x, y}]

Weight[a_. Derivative[n_][y_][x_], y_[x_], x_] := -n /; FreeListQ[a, {x, y}]

Weight[a_. x_^k_. y_[x_], y_[x_], x_] := k  /; FreeListQ[a, {x, y}]

Weight[a_. y_[x_], y_[x_], x_] := 0  /; FreeListQ[a, {x, y}]

Weight[a_, y_[x_], x_] := Infinity  /; FreeQ[a, y]


(* ============================ HYPERGEOMETRIC F ========================== *)
(*  In V2.2 and earlier, this pkge used to export expressions involving
    HypergeometricF.  For V3.0, replacing HypergeometricF entirely with
    HypergeometricPFQ was tried, but unfortunately, the automatic simplification
    of HypergeometricPFQ[{1, 2}, {}, z] causes
    Series[(1 + HypergeometricPFQ[{1, 2}, {}, z])/2, {z, 0, 0}]
    to fail.  Thus HypergeometricF is used internally to prevent the automatic
    simplification and allow
    Series[(1 + HypergeometricF[{1, 2}, {}, z])/2, {z, 0, 0}]
    (and probably other series expressions) to work.
*)

HypergeometricF /:
    Series[HypergeometricF[aList_List, bList_List, c_. z_],
    {z_, 0, n_}] :=
        Block[{kk},
            Sum[Times @@ (Pochhammer[#, kk]& /@ aList) /
                Times @@ (Pochhammer[#, kk]& /@ bList) / kk! (c z)^kk,
                {kk, 0, n}] + O[z]^(n + 1) ]


(* ============================== SERIES TERM ============================ *)
(* Note that SeriesTerm[a[z], {z, 0, n}] remains unevaluated for
	symbolic functions of z such as a[z].  So SeriesTerm is no
	better than InverseZTransform for symbolic functions of z.
	That is why InverseZTransform is tried before internal
	iSeriesTerm rules.
*)

SeriesTerm::badopt = "`` -> `` is not a valid option."

SeriesTerm::assmp = "Warning: Too many assumptions on ``.  Restricting \
assumptions to ``."

SeriesTerm::obsoptprec =
"Warning: PrecisionST is an obsolete option.  The result of SeriesTerm is \
always exact."

SeriesTerm::obsoptreal = "Warning: MakeReal is an obsolete option.  SeriesTerm \
always tries to replace pairs of conjugate complex quantities with double their \
real parts."

SeriesTerm::obsoptusea = "Warning: UseApart is an obsolete option used to \
control the application of Apart prior to power series expansion.  SeriesTerm \
automatically uses Apart with no second argument."

SeriesTerm::obsoptusem = "Warning: UseMod is an obsolete option, superseded by \
IntegerFunctions.  Using IntegerFunctions -> ``."

(* SeriesTerm of a list *)
SeriesTerm[expr_List, {z_, a_, n_}, opts___Rule] :=
	(
	SeriesTerm[#, {z, a, n}, opts]& /@ expr
	) /; Head[n] =!= Blank && Head[n] =!= Pattern

(* main entry point *)
SeriesTerm[f_, {z_, a_, n_}, opts___Rule] :=
    (issueObsoleteFunMessage[SeriesTerm, "DiscreteMath`RSolve`"];
    Block[{result, oList, assmp, if, sf},
	(
	 result
	) /; (
	       oList = {opts};
	       (* Issue obsolete messages if obsolete options are in use. *)
	       If[!FreeQ[oList, PrecisionST], Message[SeriesTerm::obsoptprec]];
               If[!FreeQ[oList, MakeReal], Message[SeriesTerm::obsoptreal] ];
	       If[!FreeQ[oList, UseApart], Message[SeriesTerm::obsoptusea] ];
	       If[!FreeQ[oList, UseMod],
		  oList = oList /. {(UseMod -> x_) :> (IntegerFunctions -> x)};
		  Message[SeriesTerm::obsoptusem,
			IntegerFunctions /. oList ] 	];
     	       {assmp, if, sf} =
			{Assumptions, IntegerFunctions, SpecialFunctions} /.
			 {opts} /. Options[SeriesTerm];
	       (* Note that if SeriesTerm is called from iMethodGF or
			iMethodEGF, then there will be no Assumptions option
			in oList.  RSolve does not support the Assumptions
			option, although SeriesTerm does. *)
	       (result = internalSeriesTerm[f, {z, a, n},
                                assmp, if, sf]) =!= $Failed &&
               FreeQ[result, internalSeriesTerm]
 	     )
    ] /; Head[n] =!= Blank && Head[n] =!= Pattern && !NumberQ[n])
	(* NOTE: Conditions on n added so that
			 SeriesTerm[1/(1+x), {x, 0, 1/2}]
		 and
			 SeriesTerm[1/((x-2)x^2), {x, 0, -2}]
		 do not evaluate using rules for symbolic n. *)

(* special case of numeric n where Series is applicable;
	from Dan Lichtblau's nthTerm.m *)
SeriesTerm[f_, {z_, a_, (n_Integer|n_Rational)}, opts___Rule] :=
        (issueObsoleteFunMessage[SeriesTerm, "DiscreteMath`RSolve`"];
        Module[{m0, m, head, ser, coef},
          (
                coef
          ) /;  (
	         m0 = Ceiling[n+1];
		 m = m0;
	         head = Head[ser = Series[f,{z,a,m}]];
		 If[head === SeriesData,
		        (* Note: Series doesn't always find the expansion to the
			   desired order, so m is incremented until the
			   desired order is achieved. *)
		        While[FreeQ[ser, Series] &&
		              ser[[5]]/ser[[6]] - 1 < m0 && m <= 2 m0,
		          (m++;
		           ser = Series[f,{z,a,m}];
		          )
		        ];
		        (* Make sure Series evaluated and that actual expansion
			   order (ser[[5]]/ser[[6]] - 1) is at least as large
			   as the requested expansion order m. *)
		        (FreeQ[ser, Series] && ser[[5]]/ser[[6]] - 1 >= m0) &&
 		        (coef = Coefficient[Normal[ser], z-a, n];
	                 Head[coef]=!=Coefficient &&
		         (FreeQ[coef, z] ||
	                  (n == 0 &&
		           (coef = Expand[coef, z];
			    If[Head[coef] === Plus,
			       coef = Apply[Plus, Select[Apply[List, coef],
					FreeQ[#, z]&]];
			       True,
		               False
		            ])
			))),
			False
		 ] (* end If *)
		)
        ])


(* ========================= internal SERIES TERM ======================= *)

(* internalSeriesTerm of a list *)
internalSeriesTerm[expr_List, {z_, a_, n_}, assmp_, if_, sf_] :=
	(
	internalSeriesTerm[#, {z, a, n}, assmp, if, sf]& /@ expr
	)


internalSeriesTerm[f_, {z_, a_, n_},  assmp_, if_, sf_] :=
  Block[{  assmp0, assmp1, info, plist, pvar, svars, solve, nST,
	   assmp2, varlist, assmp3,
	   $SumDepth, (* implicitly passed to iSeriesTerm *)
	   ff, ff1, n0, result00, result0, result, head, sum, den},

     (* Note: integerFunctions & specialFunctions are global variables,
		and do not belong in the variable list of Block. *)
     {integerFunctions, specialFunctions} = {if, sf};

     (* If Assumptions indicates Automatic or  {n >= 0} or (n >= 0),
	 try using kernel InverseZTransform. *)
     If[assmp === Automatic || assmp === {n >= 0} || assmp === (n >= 0),
	ff = f /. {z :> 1/z+a};
	If[FreeQ[result = InverseZTransform[ff, z, n], InverseZTransform],
     	        If[MatchQ[iInfo[n], n >= _],
		   (* In V4.0, InverseZTransform returns results in terms of
			UnitStep.  Try to simplify UnitStep expressions
			using assumptions on n. *)
                   n0 = iInfo[n][[2]];
		   If[!FreeQ[result, UnitStep],
              	      result0 = result //. {
				   UnitStep[n + cc_.] :> 1 /; -cc <= n0
					   };
              	      result00 = result0 /. n -> n0;
                      If[NumericQ[result00],
                      	result00 = FullSimplify[result00]];
                      If[result00 == (result /. n -> n0), result = result0]
                   ] (* If !FreeQ[result, UnitStep] *)
	        ]; (* If MatchQ[iInfo[n], n >= _] *)
		(* No need to reset iInfo[n]...  this is done by the code that
			set it to begin with (iMethodGF or iMethodEGF). *)
		Return[result]
	] (* If FreeQ[result, InverseZTransform] *)
     ]; (* If assmp === {} *)

     (*
	If
	(Assumptions -> Automatic OR Assumptions -> n >= 0) BUT
		 InverseZTransform failed
	OR
	Assumptions -> something else,
	   then try iSeriesTerm.
     *)
     ff = f /. {z :> z+a};
     ff = If[trigMatchQ[ff, z],
             ff,
             expandTrigsInVar[ff, z] ];
     assmp0 = Which[
	assmp === Automatic, {n >= 0},
	Head[assmp] =!= List, {assmp},
	True, assmp];
     IntegerQ[nST] ^= True;

     (* ================================================================ *)
     plist = PatternList[n, _Symbol?(Context[#] =!= "System`"&) ];
     If[plist === {},
	(* n is numeric *)
	   (* ================= set iInfo[nST] ==================== *)
	   nST /: iInfo[nST] = (nST == n),
	(* n includes a symbolic term *)
	   (* ========= set iInfo[pvar] according to Assumptions ========= *)
	   pvar = plist[[1]];
	   assmp1 = Select[assmp0, ((!FreeQ[#, pvar])&)];
           If[Length[assmp1] > 1,
		 (* Too many assumptions on pvar... restricting assumptions
			to the first one! *)
		 assmp1 = {assmp1[[1]]};
		 Message[SeriesTerm::assmp, pvar, assmp1[[1]] ]];
           If[Length[assmp1] == 1,
	 	Evaluate[pvar] /: iInfo[pvar] = assmp1[[1]] ];
           info = iInfo[pvar];
	   (* ================= set iInfo[nST] ==================== *)
	   svars = (Head[Solve::svars] === $Off);
	   Off[Solve::svars];
	   solve = Solve[n == nST, pvar];
	   If[!svars, On[Solve::svars]];
	   If[Length[solve] >= 1,
	   	nST /: iInfo[nST] = (info /. solve[[1]])		 ]
     ];
     (* ============================================================== *)
     (* === set iInfo for other variables mentioned in Assumptions === *)
	assmp2 = Select[assmp0, (FreeQ[#, n])&];
	varlist = Union[ assmp2 /. {g_[x_Symbol, y_] :> x} ];
	Scan[With[{m = #},
	      assmp3 = Select[assmp2, ((!FreeQ[#, m])&)];
	      If[Length[assmp3] > 1,
			assmp3 = {assmp3[[1]]};
		        Message[SeriesTerm::assmp, n, assmp3[[1]] ]];
	      If[Length[assmp3] == 1,
		 m /: iInfo[m] = assmp3[[1]]			 ]
	     ]&, varlist];
     (* =========================================================== *)
     $SumDepth = 1;
     (* Put the function ff into an "easily transformed" form. *)
     (* This simplification reduces
		(1 - z^2)^(1/2)*(-1/(2*(-1 + z)) + 1/(2*(1 + z)))
	   to
		(1 - z^2)^(-1/2)
	   recognized by a binomial rule.
     *)
     ff1 = Together[ff];
     If[PolynomialQ[(den = Denominator[ff1]), z] && den =!= 1,
	   (* denominator is a polynomial in z *)
	   If[MatchQ[(num = Numerator[ff1]),
                      kk_. Power[p_, Rational[_, _]] /;
                      FreeQ[kk, z] && PolynomialQ[p, z]],
	      (* numerator is a radical of a polynomial in z *)
	      ff = Simplify[ff1],
	      (* numerator is NOT a radical of a polynomial in z *)
	      If[!MatchQ[ff1, ggg_. (aaa_ + bbb_.*z^kkk_.)^mmm_Integer /;
		   FreeQ[{aaa,bbb}, z] && Positive[kkk] && IntegerQ[kkk] ],
		 (* Don't touch expressions of the form g[x]/(c^3 + x^3)^2. *)
                 ff1 = Apart[Factor[ff1], z]
	      ];
	      If[Head[ff1]===Plus,
		 ff = Map[If[PolynomialQ[(den = Denominator[#]),z],
			         If[LeafCount[r = Numerator[#]/Simplify[den]] <
				   LeafCount[rr = Numerator[#]/Expand[den]],
					r, rr],
				#]&, ff1],
		 den = Denominator[ff1];
		 If[LeafCount[r = Numerator[ff1]/Simplify[den]] <
		       LeafCount[rr = Numerator[ff1]/Expand[den]],
				ff = r,
				ff = rr
		 ]
	      ] (* end If Head[ff]===Plus *)
	   ] (* num is a radical of a polynomial in z *)
     ]; (* den is a polynomial in z *)
     (* the function ff should be in a form that will match with the
		"best" iSeriesTerm rule *)
     If[Head[ff] === Times,
		{constant, ff} = SeparateProduct[ff, FreeQ[#, z]&],
		constant = 1];
     head = Head[result = iSeriesTerm[ff, z, nST]];
     result *= constant;
     If[head === iSeriesTerm,
	   If[Head[n] === Symbol,
              UpValues[n] = {}  (* reset iInfo[n] *)
	   ];
	   Return[$Failed],
	   (* head =!= iSeriesTerm *)
	   result = result //. {nST :> n};
	   If[!FreeQ[result, iSeriesTerm],
	      If[Head[n] === Symbol,
	         UpValues[n] = {}  (* reset iInfo[n] *)
	      ];
	      Return[result /. {iSeriesTerm[g_, nu_, m_] :>
				SeriesTerm[g, {nu, 0, m}]}]
	   ]
     ];
     (* ======================= simplification ===================== *)
     If[FreeQ[result, If] && FreeQ[result, Root] && FreeQ[result, Factorial],
	   result = Simplify[result]
     ];
     If[FreeQ[result, Root],
	   (* Try to simplify using Marko's collection of rules. *)
	   result = iSimplify[result]
     ];
     If[!FreeQ[result, Sum],
	   (* Try to simplify Sums involving If; once the kernel
		does this automatically, this step will be unnecessary.  *)
           result = SimplifySum[result]
     ];
     If[MatchQ[iInfo[n], n >= _],
	   (* Try to simplify If expressions using assumptions on n. *)
           n0 = iInfo[n][[2]];
	   If[!FreeQ[result, If],
              result0 = result //. { If[n >= aa_, b_, _] :> b /; aa <= n0,
				     If[n >= aa_, b_, 0] :>
				 	If[n >= n0, b, 0] /; aa > n0 &&
				 	Scan[(
				      	   If[(b /. n:>#) =!= 0,
						 Return[$Failed]]
				             )&,
						Range[n0, aa-1]] =!= $Failed
			           };
	      result00 = result0 /. n -> n0;
	      If[NumericQ[result00],
		 result00 = FullSimplify[result00]];
              If[result00 == (result /. n -> n0), result = result0]
	   ] (* If !FreeQ[result, If] *)
     ]; (* If MatchQ[iInfo[n], n >= _] *)
     (* ============== Reset iInfo for various symbols ================= *)
     If[Head[n] === Symbol,
     		UpValues[n] = {}
     ];
     UpValues[nST] = {};
     If[Length[assmp1] == 1,
         UpValues[pvar] = {}
     ];
     Scan[(
		UpValues[#] = {})&,
		 varlist];
     (* ============================================================= *)
     result
  ] (* end of internalSeriesTerm *)



(* =========================== iSERIESTERM ============================ *)

    (* linearity *)

(* Apply iSeriesTerm to sum only if for each term
	iSeriesTerm evaluates OR iSeriesTerm yields iSeriesTerm[g[z], z, n-m]
	for some g and m. *)
(* NOTE: SeriesTerm[f[x] + g[x], {x, 0, n}] is allowed for abstract f[x]
	and g[x] (see MatchQ). *)
iSeriesTerm[f_Plus, z_Symbol, n_] :=
  Module[{result, scan, ist},
    (
	result
    ) /; (result = 0;
	  scan = Scan[(
		       If[FreeQ[ist = iSeriesTerm[#, z, n], iSeriesTerm] ||
			 MatchQ[ist, a_. iSeriesTerm[(g_)[z], z, m_] /;
						FreeQ[{a, g}, z]],
			 result += ist,
			 Return[$Failed]]
		      )&, Apply[List, f]];
	  scan =!= $Failed)
  ]

iSeriesTerm[c_ f_, z_Symbol, n_] :=
	(
	c iSeriesTerm[f, z, n]
	) /; 	 FreeQ[c, z] && !FreeQ[f, z]

    (* iSeriesTerm is the inverse of Gf and of iPowerSum *)

iSeriesTerm[Gf[a_][z_], z_Symbol, n_] := a[n]

iSeriesTerm[iPowerSum[a_, z_Symbol, n_], z_Symbol, m_] := (a /. n :> m)


    (* powers and constants *)

(* (1 + z^2) f *)
(* NOTE: SeriesTerm[x f[x], {x, 0, n}] is allowed for abstract f[x]
	(see MatchQ), with Context[f] usually "Global`". *)
iSeriesTerm[p_ f_, z_Symbol, n_] :=
  Module[{ist, nn},
    (
    (* NOTE: returning "ist" is OK if ist is naturally 0 for n < k.
	Otherwise should return When[n >= k, ist, 0] *)
    CoefficientList[p, z] . Map[(ist /. {nn->(n-#)})&,
		 Range[0, Exponent[p, z]] ]
    ) /; FreeQ[ist = iSeriesTerm[f, z, nn], iSeriesTerm] ||
	 MatchQ[ist, a_. iSeriesTerm[(g_)[z], z, m_] /;
                     ( FreeQ[{a, g}, z] && (Head[g] =!= Symbol ||
		       (Context[g] =!= "DiscreteMath`RSolve`Private`" &&
		        Context[g] =!= "System`") )
		     )
	 ]
  ] /; !FreeQ[f, z] && PolynomialQ[p, z] &&
	(!PolynomialQ[1/f, z] || Exponent[1/f, z] > Exponent[p, z])

(* NOTE: Don't evaluate SeriesTerm[z^k f, {z, 0, n}] for abstract k and
	 abstract n, because Series[z^k f, {z, 0, n}] does not evaluate for
	 abstract k and integer n. *)
(* z^(-1) f *)
iSeriesTerm[z1_ f_, z_Symbol, n_] :=
  Module[{k, ist},
    (
    (* NOTE: returning "ist" is OK if ist is naturally 0 for n < k.
	Otherwise should return When[n >= k, ist, 0] *)
    ist
    ) /; MatchQ[Collect[z1, z], z^k1_. /; (k = k1; IntegerQ[k1])] &&
	FreeQ[ist = iSeriesTerm[f, z, n-k], iSeriesTerm]
  ] /; !FreeQ[f, z]

(* z^3 *)
iSeriesTerm[z1_, z_Symbol, n_] :=
 Module[{k},
   (
     When[n == k, 1, 0]
   ) /; MatchQ[Collect[z1, z], z^k1_. /;
		(k = k1;
	         IntegerQ[k] (* Make no assumptions about k... result
				is different depending on whether k is
				integral or not. *)
		)
	]
 ]

iSeriesTerm[f_, z_Symbol, n_] :=
   (
	f When[n == 0, 1, 0]
   )  /; FreeQ[f, z]

    (* iSeriesTerm is close to the inverse of EGf and of
       iExponentialPowerSum *)

iSeriesTerm[EGf[a_][z_], z_Symbol, n_] := a[n] / n!

iSeriesTerm[iExponentialPowerSum[a_, z_Symbol, n_], z_Symbol, m_] :=
	(
	 (a / n!) /. {n:>m}
	)

(* binomials *)
(* first rule is superseded by the second, but the results are better
	simplified if both rules are present *)
(* NOTE: the integerQ pattern is designed to allow one to use assumptions
	about parameters. *)
iSeriesTerm[poly1_^(m_Integer|m_Rational|m_?(integerQ[#]&)), z_Symbol, n_] :=
    Block[{a, b, mepr, power2, result},
      (
       mepr = (Head[General::meprec] === $Off);
       Off[General::meprec];
       power1 = PowerExpand[(-a/b)^n];
       power2 = PowerExpand[(a/b)^n];
       If[!mepr, On[General::meprec]];
       result = If[ TrueQ[Entails[iInfo[n], n < 0]],
	(* when n is manifestly negative, then simplify expression to 0 *)
	0,
        b^m Which[
	  TrueQ[m == -1],
	    	When[n >= 0, power1, 0],
	  TrueQ[m < 0] || TrueQ[Entails[Info[m], m < 0]],
                When[n >= 0, power1 Binomial[n - m - 1, n], 0],
	  True,
                power2 Binomial[m, n]   ]
       ];
       result
      ) /; ({b, a} = CoefficientList[poly1, z]; !TrueQ[b == 0])
    ] /; PolynomialQ[poly1, z] && Exponent[poly1, z] == 1


(* NOTE: this rule needs a "If[n >= 0, etc, 0]" or "When[n >= 0, etc, 0]"
	 condition, because it is not otherwise zero for negative n.  *)
(* NOTE: the integerQ pattern is designed to allow one to use assumptions
        about parameters. *)
iSeriesTerm[(a_ + b_.*z_^k_.)^(m_Integer|m_?(integerQ[#]&)), z_, n_] :=
   Module[{q = n/k, c = -b/a, r},
	 r = If[TrueQ[m == -1], 1, Product[(j - m), {j, 0, q-1}]/q!];
	 result =
         If[NumberQ[c] && Negative[c] && MatchQ[q, s_Symbol i_Integer?Even],
	    (* do something special to make (-1)^(2 n) simplify *)
	    If[m > 0,
	       (* don't need constraint on n *)
	       a^m (c^2)^Simplify[q/2] r,
	       When[n >= 0,
	            a^m (c^2)^Simplify[q/2] r,
	            0]
	    ],
	    If[m > 0,
	       (* don't need constraint on n *)
	       When[SymbolicMod[n, k] == 0, a^m c^q r, 0],
	       When[n >= 0,
	            When[SymbolicMod[n, k] == 0, a^m c^q r, 0],
	            0]
	    ]
	 ];
	 result
   ] /; integerFunctions && FreeQ[{a,b,k}, z] && Positive[k] &&
                (integerQ[k] || Head[k] === Rational)


(* NOTE: the integerQ pattern is designed to allow one to use assumptions
        about parameters. *)
iSeriesTerm[(a1_. z_^m1_. + a2_. z_^m2_.)^(k_Integer|k_?(integerQ[#]&)),
		 z_Symbol, n_] :=
  Module[{iST},
   (
    Cancel[a2^k iST]
   ) /; ( FreeQ[iST = iSeriesTerm[z^(m2 k)(1 + a1/a2 z^(m1-m2))^k, z, n],
		 iSeriesTerm])
  ]  /;	(FreeQ[#, z]&) /@ (a1 && a2 && m1 && m2)


(* The next three rules are from Dan Lichtblau's nthTerm.m *)

(* NOTE: this needn't have a "If[n >= 0, etc, 0]" condition,
	 because 1/((n/k)!) is zero for negative n and positive k  *)
iSeriesTerm[base_^(a_.*z_^k_. + b_.), z_Symbol, n_] :=
   With[{q=n/k},
        When[SymbolicMod[n, k] == 0,
	   base^b * (Log[base]*a)^q / (q!),
	   0]
   ] /; FreeQ[{base,a,b,k}, z] && Positive[k] &&
		(integerQ[k] || Head[k] === Rational)

(* NOTE: the integerQ pattern is designed to allow one to use assumptions
        about parameters. *)
iSeriesTerm[p_^(r_Integer|r_?(integerQ[#]&)), z_, n_] :=
        Module[{newp},
          (
		iSeriesTerm[newp, z, n]
	  ) /; (
	        newp=PartialFractions[p^r, z];
		newp =!= p^r)
        ] /; (r<0 && PolynomialQ[p,z] && Exponent[p,z]>1)

iSeriesTerm[(z_^(r_Rational)) (a_ + b_.*z_^(r_Rational))^(-1), z_, n_] :=
        (
         When[n >= 1, 1/a * (-b/a)^(n/r - 1), 0]
        ) /; FreeQ[{a,b},z] && MatchQ[r, Rational[1, _]]

(* NOTE: added application of expandTrigsInVar before the iSeriesTerm *)
iSeriesTerm[Log[e_], z_, n_] :=
  Module[{f, f1, ist, s, coef0},
   (
	When[n == 0,
	     iFullSimplify[coef0],
	     ist/n
	]
   ) /; (
	 f = Together[z D[e,z]/e];
	 f1 = If[trigMatchQ[f, z],
                        f,
                        expandTrigsInVar[f, z] ];
	 ist=iSeriesTerm[f1, z, n];
	 FreeQ[ist, iSeriesTerm]) &&
	(
	 s = Series[Log[e], {z, 0, 0}];
	 FreeQ[s, Series] && (coef0 = Normal[s]) =!= Log[e] && FreeQ[coef0, z]
	)
  ] /; !FreeQ[e, z] && n=!=0


    (* Sin, Sinh, Cos, Cosh, new in V3.0 *)

iSeriesTerm[np_. / dp_, z_Symbol, n_] :=
  Module[{nlist, dlist, plist, k0, k1, expAlpha, beta, omega, sinn, hyperbolic},
  (
    {expAlpha, beta, omega, hyperbolic} = plist;
    If[Length[nlist] == 1, {k0, k1} = {nlist[[1]], 0},
        {k0, k1} = nlist];
   If[hyperbolic,
      sinn = Switch[omega,
		    I Pi + nu_, (-1)^n Sinh[n (omega - I Pi)],
		    -I Pi + nu_, (-1)^n Sinh[n (omega + I Pi)],
		    _, Sinh[omega n]];
      Which[TrueQ[k0 == 0], (* try to produce simple result *)
	     (PowerExpand[expAlpha^(-1 - n)]*k1*sinn*Csch[omega])/beta ,
	    TrueQ[k1 == 0],
	     k0 (PowerExpand[expAlpha^(-2 - n)]*
		(Cosh[n*omega] + sinn*Coth[omega]))/beta ,
	    True,
             (k0 Cosh[omega n] + (k1 expAlpha + k0 Cosh[omega])/
		Sinh[omega] sinn) / (PowerExpand[expAlpha^(2 + n)] beta)
      ],
      sinn = Switch[omega,
		    Pi + nu_, (-1)^n Sin[n (omega - Pi)],
		    -Pi + nu_, (-1)^n Sin[n (omega + Pi)],
		    _, Sin[omega n]];
      Which[TrueQ[k0 == 0], (* try to produce simple result *)
	     (PowerExpand[expAlpha^(-1 - n)]*k1*sinn*Csc[omega])/beta ,
	    TrueQ[k1 == 0],
	     k0 (PowerExpand[expAlpha^(-2 - n)]*
		(Cos[n*omega] + sinn*Cot[omega]))/beta ,
	    True,
             (k0 Cos[omega n] + (k1 expAlpha + k0 Cos[omega])/Sin[omega] sinn) /
     		(PowerExpand[expAlpha^(2 + n)] beta)
      ]
   ]
  ) /; Which[PolynomialQ[np, z] && PolynomialQ[dp, z],
                nlist = CoefficientList[np, z];
                dlist = CoefficientList[dp, z];
                Length[nlist] <= 2 && (plist = parm[dlist]) =!= $Failed,
             PolynomialQ[z np, z] && PolynomialQ[z dp, z],
                nlist = CoefficientList[z np, z];
                dlist = CoefficientList[z dp, z];
                Length[nlist] <= 2 && (plist = parm[dlist]) =!= $Failed,
             PolynomialQ[z^2 np, z] && PolynomialQ[z^2 dp, z],
                nlist = CoefficientList[z^2 np, z];
                dlist = CoefficientList[z^2 dp, z];
                Length[nlist] <= 2 && (plist = parm[dlist]) =!= $Failed,
             True,
                False
       ]
  ]

parm[dlist_] :=
  Module[{beta, sqrt, alpha, cos, omega, hyperbolic = True},
        If[Length[dlist] < 3,
		 Return[$Failed]];
        beta = dlist[[3]];
        If[beta == 0,
		 Return[$Failed]];
        sqrt = Sqrt[ dlist[[1]]/dlist[[3]] ];
	If[MatchQ[sqrt, Power[z_, 1/2]],
	   sqrt = PowerExpand[sqrt]];
	If[FreeQ[N[sqrt], Complex] && !TrueQ[N[sqrt] < 0],
	   expAlpha = sqrt,
	   Return[$Failed]
        ];
        sqrt = Sqrt[ dlist[[1]] dlist[[3]] ];
	If[MatchQ[sqrt, Power[z_, 1/2]],
	   sqrt = PowerExpand[sqrt]];
        cos = dlist[[2]]/(-2 dlist[[3]])/sqrt;
	If[cos - 1 == 0 || cos + 1 == 0,
		 Return[$Failed]];
        Which[MatchQ[cos, Cosh[z_]],
		omega = cos[[1]],
	      MatchQ[cos, -Cosh[z_]],
		omega = I Pi + (-cos)[[1]],
	      MatchQ[cos, Cos[z_]],
                omega = cos[[1]];  hyperbolic = False,
	      MatchQ[cos, -Cos[z_]],
		omega = Pi + (-cos)[[1]];  hyperbolic = False,
              True,
                Return[$Failed]
        ];
        {expAlpha, beta, omega, hyperbolic}
  ]

    (* hypergeometrics *)

(* NOTE: changed from HypergeometricF to HypergeometricPFQ in V3.0.
	Including rules for both HypergeometricPFQ and
	HypergeometricF because the latter is used internally. *)

iSeriesTerm[HypergeometricPFQ[aList_, bList_, c_. z_], z_Symbol, n_] :=
   (
     When[n >= 0,
       (Times @@ (If[IntegerQ[#], (# + n - 1)!/(# - 1)!,
          Pochhammer[#, n]]& /@ aList))/
       (Times @@ (If[IntegerQ[#], (# + n - 1)!/(# - 1)!,
          Pochhammer[#, n]]& /@ bList))/n! c^n,
       0]
   )  /; FreeQ[c, z]

iSeriesTerm[HypergeometricF[aList_, bList_, c_. z_], z_Symbol, n_] :=
   (
     When[n >= 0,
       (Times @@ (If[IntegerQ[#], (# + n - 1)!/(# - 1)!,
          Pochhammer[#, n]]& /@ aList))/
       (Times @@ (If[IntegerQ[#], (# + n - 1)!/(# - 1)!,
          Pochhammer[#, n]]& /@ bList))/n! c^n,
       0]
   )  /; FreeQ[c, z]

iSeriesTerm[Hypergeometric2F1[a_, b_, c_, d_. z_], z_Symbol, n_] :=
   (
     When[n >= 0,
       (Times @@ (If[IntegerQ[#], (# + n - 1)!/(# - 1)!,
           Pochhammer[#, n]]& /@ {a, b}))/
       (Times @@ (If[IntegerQ[#], (# + n - 1)!/(# - 1)!,
           Pochhammer[#, n]]& /@ {c, 1})) d^n,
       0]
   )  /; FreeQ[d, z]

iSeriesTerm[Hypergeometric1F1[a_, b_, c_. z_], z_Symbol, n_] :=
   (
     When[n >= 0,
     	If[IntegerQ[a],
	   (a + n - 1)!/(a - 1)!,
	   Pochhammer[a, n]
	] / (Times @@ (If[IntegerQ[#],
			  (# + n - 1)!/(# - 1)!,
        		  Pochhammer[#, n]
		       ]& /@ {b, 1})) c^n,
	0]
   )  /; FreeQ[c, z]

iSeriesTerm[Hypergeometric0F1[a_, b_. z_], z_Symbol, n_] :=
   (
     When[n >= 0,
        1/(Times @@ (If[IntegerQ[#], (# + n - 1)!/(# - 1)!,
           Pochhammer[#, n]]& /@ {a, 1})) b^n,
	0]
   )  /; FreeQ[b, z]

     (* exponentials *)

iSeriesTerm[E^p_, z_Symbol, n_] :=
  Module[{m, k},
	{m, k} = CoefficientList[p, z];
	(E^m k^n/n!)
  ] /; PolynomialQ[p, z] && Exponent[p, z] == 1


    (* trigonometrics *)

iSeriesTerm[Sin[p_], z_Symbol, n_] := (* ROC: all real values of z *)
  Module[{a, b, result},
    {b, a} = CoefficientList[p, z];
    result = Which[
	(* when n is manifestly negative, then simplify expression to 0 *)
	TrueQ[Entails[iInfo[n], n < 0]], 0,
	True, If[Evaluate[Odd[n]],
	   	     Evaluate[a^n Cos[b] (-1)^((n-1)/2) / n!],
	   	     Evaluate[a^n Sin[b] (-1)^(n/2) / n!]
		] ];
    result
  ] /; PolynomialQ[p, z] && Exponent[p, z] == 1		(* CRC *)

iSeriesTerm[Cos[p_], z_Symbol, n_] := (* ROC: all real values of z *)
  Module[{a, b, result},
    {b, a} = CoefficientList[p, z];
    result = Which[
	(* when n is manifestly negative, then simplify expression to 0 *)
	TrueQ[Entails[iInfo[n], n < 0]], 0,
	True, If[Evaluate[Even[n]],
		Evaluate[a^n Cos[b] (-1)^(n/2) / n!],
		Evaluate[- a^n Sin[b] (-1)^((n-1)/2) / n!]
		] ];
    result
  ] /; PolynomialQ[p, z] && Exponent[p, z] == 1		(* CRC *)

iSeriesTerm[Tan[a_. z_], z_Symbol, n_] := (* ROC: Abs[z] < Pi/2 *)
 Module[{result},
   result = Which[
	(* when n is manifestly negative, then simplify expression to 0 *)
	TrueQ[Entails[iInfo[n], n < 0]], 0,
	(* Note that the "If Odd[n]" is needed because otherwise the zeroth
		order term will be nonzero. *)
	True, If[Evaluate[Odd[n]],
		Evaluate[a^n (-1)^((n-1)/2) 2^(n+1) (2^(n+1) - 1) *
			 BernoulliB[n+1] / (n+1)!],
		0] ];
   result
 ] /; FreeQ[a, z] && FreeQ[a, n] (* CRC *)

iSeriesTerm[Sec[a_. z_], z_Symbol, n_] := (* ROC: Abs[z] < Pi/2 *)
 Module[{result},
   result = Which[
	(* when n is manifestly negative, then simplify expression to 0 *)
	TrueQ[Entails[iInfo[n], n < 0]], 0,
	(* the following takes advantage of the fact that EulerE[n] is zero
		for odd n *)
	True, a^n (-1)^(n/2) EulerE[n] / n! ];
   result
 ] /; FreeQ[a, z] && FreeQ[a, n] (* CRC *)

iSeriesTerm[Cos[a_. Sqrt[b_. z_]], z_Symbol, n_] :=
 Module[{result},
   result = Which[
	(* when n is manifestly negative, then simplify expression to 0 *)
	TrueQ[Entails[iInfo[n], n < 0]], 0,
	True, (a Sqrt[b])^(2 n) (-1)^n /(2 n)! ];
   result
  ] /; FreeQ[{a, b}, z] && FreeQ[{a, b}, n]

iSeriesTerm[Sec[a_. Sqrt[b_. z_]], z_Symbol, n_] :=
 Module[{result},
   result = Which[
	(* when n is manifestly negative, then simplify expression to 0 *)
	TrueQ[Entails[iInfo[n], n < 0]], 0,
        True, (a Sqrt[b])^(2 n) (-1)^n EulerE[2 n]/(2 n)! ];
   result
 ] /; FreeQ[{a, b}, z] && FreeQ[{a, b}, n]


    (* hyperbolics *)

iSeriesTerm[Sinh[p_], z_Symbol, n_] :=
  Module[{a, b, result},
     {b, a} = CoefficientList[p, z];
     result = Which[
	(* when n is manifestly negative, then simplify expression to 0 *)
        TrueQ[Entails[iInfo[n], n < 0]], 0,
	True, If[Evaluate[Odd[n]],
		Evaluate[a^n Cosh[b] / n!],
		Evaluate[a^n Sinh[b] / n!]
		] ];
     result
  ] /; PolynomialQ[p, z] && Exponent[p, z] == 1         (* CRC *)

iSeriesTerm[Cosh[p_], z_Symbol, n_] :=
  Module[{a, b, result},
     {b, a} = CoefficientList[p, z];
     result = Which[
	(* when n is manifestly negative, then simplify expression to 0 *)
	TrueQ[Entails[iInfo[n], n < 0]], 0,
	True, If[Evaluate[Even[n]],
		Evaluate[a^n Cosh[b] / n!],
		Evaluate[a^n Sinh[b] / n!]
		] ];
     result
  ] /; PolynomialQ[p, z] && Exponent[p, z] == 1         (* CRC *)

iSeriesTerm[Tanh[a_. z_], z_Symbol, n_] := (* ROC: Abs[z] < Pi/2 *)
 Module[{result},
   result = Which[
	(* when n is manifestly negative, then simplify expression to 0 *)
        TrueQ[Entails[iInfo[n], n < 0]], 0,
	(* Note that the "If Odd[n]" is necessary because w/out it the
		zeroth order term would be nonzero. *)
	True, If[Evaluate[Odd[n]],
		Evaluate[a^n 2^(n+1) (2^(n+1) - 1) BernoulliB[n+1] / (n+1)!],
		0] ];
   result
 ] /; FreeQ[a, z] && FreeQ[a, n] (* CRC *)

(* NOTE: no longer needed because expandTrigsInVar transforms Sech pattern *)
iSeriesTerm[Sech[a_. z_], z_Symbol, n_] := (* ROC: Abs[z] < Pi/2 *)
  Module[{result},
   result = Which[
	(* when n is manifestly negative, then simplify expression to 0 *)
        TrueQ[Entails[iInfo[n], n < 0]], 0,
        True, a^n EulerE[n] / n! ];
   result
  ] /; FreeQ[a, z] && FreeQ[a, n] (* CRC *)

iSeriesTerm[Cosh[a_. Sqrt[b_. z_]], z_Symbol, n_] :=
  Module[{result}, (* CRC rule 32, new in V3.0  *)
   result = Which[
	(* when n is manifestly negative, then simplify expression to 0 *)
        TrueQ[Entails[iInfo[n], n < 0]], 0,
	True, (a Sqrt[b])^(2 n)/(2 n)!
	];
   result
  ] /; FreeQ[{a, b}, z] && FreeQ[{a, b}, n]

iSeriesTerm[Sech[a_. Sqrt[b_. z_]], z_Symbol, n_] :=
 Module[{result},
   result = Which[
	(* when n is manifestly negative, then simplify expression to 0 *)
	TrueQ[Entails[iInfo[n], n < 0]], 0,
        True, (a Sqrt[b])^(2 n) EulerE[2 n]/(2 n)!
	];
   result
 ] /; FreeQ[{a, b}, z] && FreeQ[{a, b}, n]


    (* logarithms *)

iSeriesTerm[Log[poly_^k_.], z_Symbol, n_] :=
   Module[{a, b},
    (
    When[n == 0, k Log[a], -k(-b/a)^n / n]
    ) /; ({a, b} = CoefficientList[poly, z];  !TrueQ[a == 0])
   ] /; FreeQ[k, z] && PolynomialQ[poly, z] && Exponent[poly, z] == 1




    (* special functions *)

iSeriesTerm[1 / Sqrt[p2_], z_Symbol, n_] :=
   Module[{a, b, c},
    (
    When[n >= 0, 	(* When *)
	(* NOTE that the following expression is not zero for n < 0. *)
        Sqrt[c/a]^n / Sqrt[a] LegendreP[n, -Sgn[a] b /(2 Sqrt[a c])],
        0]
    ) /; ({a, b, c} = CoefficientList[p2, z]; a != 0)
   ] 	/; specialFunctions && PolynomialQ[p2, z] && Exponent[p2, z] == 2

 FreeQ[{a,b,c}, z]

iSeriesTerm[Power[p2_, Rational[k_, 2]], z_Symbol, n_] :=
  Module[{a, b, c},
   (
    iSeriesTerm[Expand[(a + b z + c z^2)^((k + 1)/2)] /
        MySqrt[a + b z + c z^2], z, n]
   )  /; ({a, b, c} = CoefficientList[p2, z]; a != 0)
  ] /; specialFunctions && k != -1 && PolynomialQ[p2, z] && Exponent[p2, z] == 2

iSeriesTerm[1 / MySqrt[a_], z_Symbol, n_] :=
    (
    iSeriesTerm[1 / Sqrt[a], z, n]
    )

iSeriesTerm[poly1_^m_ poly2_^m_, z_Symbol, n_] :=
  Module[{a, b, c, d, expand, expand0},
    (
    result
    ) /; ({a, b} = CoefficientList[poly1, z]; !TrueQ[a==0]) &&
	 ({c, d} = CoefficientList[poly2, z]; !TrueQ[c==0]) &&
	 (expand = Expand[a c]^m;
	  expand0 = Expand[a^m c^m];
	  result = Which[expand == expand0,
			iSeriesTerm[Expand[(a + b z) (c + d z)]^m, z, n],
		         expand == -expand0,
		 	-iSeriesTerm[Expand[(a + b z) (c + d z)]^m, z, n],
	                 True,
			$Failed];
	 result =!= $Failed)
  ] /; specialFunctions &&
	 PolynomialQ[poly1, z] && Exponent[poly1, z] == 1 &&
	 PolynomialQ[poly2, z] && Exponent[poly2, z] == 1

    (* products with a rational function *)

(* NOTE: this rule modifies $SumDepth *)
iSeriesTerm[expr_Times, z_Symbol, n_] :=
  Block[{a, b, bm, m, deg, temp, i, ist, sd = $SumDepth},
    (
         If[PolynomialQ[a, z],
             deg = Exponent[a, z];
             temp = CoefficientList[a, z] .
                 Table[bm /. m -> i, {i, n, n - deg, -1}] /. If -> When,
  	     aa = If[MatchQ[a, (ccc_ + ddd_.*z^kkk_.)^mmm_Integer /;
			FreeQ[{ccc,ddd}, z] && Positive[kkk] && IntegerQ[kkk] ],
		     (* Don't touch expressions of the form 1/(c^3 + x^3)^2. *)
		     a,
		     Apart[a, z]
	     ];
             ist = iSeriesTerm[aa, z, n - K[sd]];
	     pma = PoleMultiplicity[a, {z, 0}];
	     pmb = PoleMultiplicity[b, {z, 0}];
             temp = Sum[ Evaluate[ ist * bm /. m -> K[sd] ],
               		 Evaluate[{K[sd], -pmb, n + pma}] ]
         ];
         temp
    ) /; (
	  (* ``a'' is a ratio of polynomials in z, ``b'' is not *)
	  {a, b} = SeparateProduct[expr,
		(PolynomialQ[#, z] || PolynomialQ[#^(-1), z])& ];
          Increment[$SumDepth];
	  a =!= 1 && b =!= 1 && FreeQ[bm = iSeriesTerm[b, z, m], iSeriesTerm]
	 )
  ]

   (* trinomials *)

(* NOTE: this rule modifies $SumDepth *)
(* This rule fires for
  SeriesTerm[1/Sqrt[-1 + 2*z + 3 z^2], {z, 0, n}, SpecialFunctions -> False]
   but not for
  SeriesTerm[1/Sqrt[1 - 2*x*z + z^2], {z, 0, n}]
   or
  SeriesTerm[1/Sqrt[-1 + 2*z + 3 z^2], {z, 0, n}]
   or
  SeriesTerm[(-1 + z + z^2)^(-1), {z, 0, n}]
   or
  SeriesTerm[(-1 + z + x z^2)^(-1), {z, 0, n}]


  Other rules provide simpler results for these examples.
 *)
iSeriesTerm[poly2_^(m_Integer|m_Rational), z_Symbol, n_] :=
    Block[{a, b, c, result, sd = $SumDepth},
      (
         result = Sum[
	     Evaluate[  Binomial[m, K[sd]] Binomial[K[sd], n - K[sd]] *
                	a^(m - K[sd]) b^(2 K[sd] - n) c^(n - K[sd])  ],
             Evaluate[  {K[sd], 0, n}  ]];
         Increment[$SumDepth];
         result
      ) /; ({a, b, c} = CoefficientList[poly2, z];  FreeQ[{a, b, c}, 0])
    ] /; PolynomialQ[poly2, z] && Exponent[poly2, z] == 2 && !specialFunctions


   (* derivatives *)

iSeriesTerm[Derivative[k_][f_][z_], z_Symbol, n_] :=
   (
    Pochhammer[n + 1, k] iSeriesTerm[f[z], z, n + k]
   )


   (* The next six rules are from Dan Lichtblau's nthTerm.m *)

(* NOTE: don't need If[n >= -1, result, 0] because result is zero for n < -1.
     However, BernoulliB[n+1] gives a BernoulliB::intnm message for n < -1. *)
iSeriesTerm[1/(a_^(a2_.*z_) - 1), z_Symbol, n_] :=
        (
        (a2*Log[a])^n * BernoulliB[n+1] / Factorial[n+1]
        ) /; FreeQ[{a,a2}, z]

(* NOTE: don't need If[n >= -1, result, 0] because result is zero for n < -1.
     However, BernoulliB[n+1] gives a BernoulliB::intnm message for n < -1. *)
iSeriesTerm[a_^(a2_.*z_)/(b_^(b2_.*z_) - 1), z_, n_] :=
        (
        (b2*Log[b])^n * BernoulliB[n+1,a2*Log[a]/(b2*Log[b])] / Factorial[n+1]
        ) /; FreeQ[{a,a2,b,b2}, z]

(* NOTE: don't need If[n >= 0, result, 0] because result is zero for n < 0.
     However, EulerE[n] gives a EulerE::intnm message for n < 0. *)
iSeriesTerm[1/(b_^(b2_.*z_) + 1), z_, n_] :=
        (
        1/2 EulerE[n, 0] * (b2*Log[b])^n / Factorial[n]
        ) /; FreeQ[{b,b2}, z]

(* NOTE: don't need If[n >= 0, result, 0] because result is zero for n < 0.
     However, EulerE[n] gives a EulerE::intnm message for n < 0. *)
iSeriesTerm[a_^(a2_.*z_)/(b_^(b2_.*z_) + 1), z_, n_] :=
        (
	1/2 EulerE[n,a2*Log[a]/(b2*Log[b])] * (b2*Log[b])^n / Factorial[n]
        ) /; FreeQ[{a,a2,b,b2}, z]

(* NOTE: don't need If[n >= 0, result, 0] because result is composed of
	a Sum that is zero for n < 0. *)
iSeriesTerm[1/(c_.*a_^poly1_ + b_), z_, n_] :=
        Module[{d, m, sum, result, sd = $SumDepth},
	 (
	  {d, m} = CoefficientList[poly1, z];
	  result = If[rhp[c/b],
          	(2*b*n!)^(-1) * (m*Log[a])^n *
                  sum[EulerE[j,0] * (d*Log[a]+Log[c/b])^(j-n) / (j-n)!,
                        {j,n,Infinity}],
		(* lhp[c/b] *)
		(-1)^(n+1) * (1/b) *  (* BernoulliB[0] term !! *)
                (m*Log[a])^n * (1/(d*Log[a]+Log[-c/b]))^(n+1) +
                (-1/b) * 1/n! * (m*Log[a])^n *
                sum[BernoulliB[j] * 1/j *
                        (d*Log[a]+Log[-c/b])^(j-n-1) / (j-1-n)!,
                 {j,n+1,Infinity}]
	  ];
	  result = result /. j -> K[sd];
	  result /. sum->Sum
	 ) /; PolynomialQ[poly1, z] && Exponent[poly1, z] == 1
	] /; FreeQ[{a,b,c}, z] && NumberQ[c/b]

(* NOTE: don't need If[n >= 0, result, 0] because result is composed of
	a Sum that is zero for n < 0. *)
iSeriesTerm[r_^poly12_/(c_.*a_^poly11_ + b_), z_, n_] :=
     Module[{d, m, t, s, sum, result, sd = $SumDepth},
      (
	{d, m} = CoefficientList[poly11, z];
	{t, s} = CoefficientList[poly12, z];
        result = If[rhp[c/b],
		 r^(t-s*(d*Log[a]+Log[c/b])/(m*Log[a])) / (2*b*(n!)) *
		 	(m*Log[a])^n * sum[EulerE[j,s*Log[r]/(m*Log[a])] *
                	(d*Log[a]+Log[c/b])^(j-n) / (j-n)!, {j, n, Infinity}],
		 (-1)^(n+1) * (1/b) * (* BernoulliB[0] term !! *)
          		r^(t-s*(d*Log[a]+Log[-c/b])/(m*Log[a])) *
          		(m*Log[a])^n * (1/(d*Log[a]+Log[-c/b]))^(n+1) +
        		(-1/b) * 1/n! * (m*Log[a])^n *
          		r^(t-s*(d*Log[a]+Log[-c/b])/(m*Log[a])) *
          		sum[BernoulliB[j, (s*Log[a])/(m*Log[a])] * 1/j *
                		(d*Log[a]+Log[-c/b])^(j-n-1) / (j-1-n)!,
				 {j,n+1,Infinity}]
	];
	result = result /. j -> K[sd];
	result /. sum->Sum
      ) /; PolynomialQ[poly11, z] && Exponent[poly11, z] == 1 &&
	   PolynomialQ[poly12, z] && Exponent[poly12, z] == 1
     ] /; FreeQ[{r,c,a,b}, z] && NumberQ[c/b]



   (* The next two rules are from Dan Lichtblau's nthTerm.m *)
(* The purpose is to handle e.g. Sec[x]^(positive integer power), by reducing to
 case of power-minus-one. We do this by differentiating Exp[a*x]*(1+Exp[b*x])^k
(k a negative integer), getting something for Exp[a*x]*(1+Exp[x])^(k-1) in terms
 of similar things involving the exponent k rather than k-1, that is reducing
the exponent. The patterns below could be generalized to bases other than E. *)

iSeriesTerm[(a_.*Exp[m_.*z_ + t_.] + b_)^k_Integer, z_, n_] :=
  Module[{ist, nn},
        (
        a^k * 1/((k+1)*m) *
         (m * (ist //. {nn :> n}) + (n+1) * (ist //. {nn :> (n+1)}) )
        ) /; FreeQ[ist = iSeriesTerm[Exp[-m*z]*(Exp[m*z+t]+(b/a))^(k+1),z,nn],
			iSeriesTerm]
  ] /; k<-1 && FreeQ[{a,m,t,b},z]

iSeriesTerm[Exp[p_.*z_ + q_.]*(a_.*Exp[m_.*z_ + t_.] + b_)^k_Integer, z_, n_] :=
  Module[{ist, nn},
        (
         a^k * 1/((k+1)*m) *
           ((n+1) * (ist //. {nn :> (n+1)}) + (m-p) * (ist //. {nn :> n})  )
        ) /; FreeQ[ist =
		 iSeriesTerm[Exp[(p-m)*z+(q-t)]*(Exp[m*z+t]+(b/a))^(k+1),z,nn],
		iSeriesTerm]
  ] /; k<-1 && FreeQ[{p,q,a,m,t,b},z]



    (* rational functions *)

iSeriesTerm[rat:(_Times | _Power), z_Symbol, n_] :=
    Block[{ff = Apart[Factor[rat], z]},
      (
        If[Head[ff] === Plus,
            SeriesRat[#, z, n]& /@ ff,
            SeriesRat[ff, z, n]
        ]
      ) /; (f = Numerator[rat]; PolynomialQ[f, z]) &&
           (g = Denominator[rat]; PolynomialQ[g, z] && Exponent[g, z] >= 1)
    ]

	(* general products *)

(* NOTE: this rule modifies $SumDepth *)
iSeriesTerm[expr_Times, z_Symbol, n_] :=
  Block[{a, b, a1, b1, k = Unique[], f, pma, pmb, ilist, sd = $SumDepth,
	   xx1, xx2, yy0, yy1, temp, if, sum},
    (
      Increment[$SumDepth];
      f = a1 b1;
      f = f //. {HoldPattern[If[c_, d_, e_]] :> if[c, d, e],
		 HoldPattern[Sum[e___]] :> sum[e],
		 k :> K[sd]};
      pma = PoleMultiplicity[a, {z, 0}];
      pmb = PoleMultiplicity[b, {z, 0}];
      ilist = {K[sd], -pmb, n + pma};

      (* The following is necessary until Sum learns to
		do something with If. *)
      If[MatchQ[f, x1_. (x2_. + if[K[sd] == y0_, y1_, 0]) /;
		( {xx1, xx2, yy0, yy1} = {x1, x2, y0, y1};
		  FreeQ[y1, K[sd]] &&
		  (TrueQ[y0 == ilist[[2]]] || TrueQ[y0 == ilist[[3]]] ||
		   TrueQ[ilist[[2]] <= y0 <= ilist[[3]]])
		) ],
	    temp = sum[ xx1 xx2, ilist ] + yy1 (xx1 /. {K[sd]->yy0}),
	    temp = sum[ f, ilist ]
      ];
      temp //. {if[c_, d_, e_] :> If[c, d, e], sum[e___] :> Sum[e]}
    ) /; (
	  Module[{length = Length[expr], set, subsets, scan, a1OK, b1OK},
		set = Range[length];
                subsets = Map[
                              Module[{p = Position[IntegerDigits[#, 2], 1]},
                                        Map[#[[1]]&, p]
                              ]&, 2 Range[2^length/2-1]-1];
		scan = Scan[
		   (a = expr[[#]];
		    b = expr[[Complement[set, #]]];
      		    Increment[$SumDepth];
		    a1OK = FreeQ[a1 = iSeriesTerm[a, z, n-k], iSeriesTerm] &&
				!FreeQ[a1, n];
		    If[a1OK,
		       Increment[$SumDepth];
		       b1OK = FreeQ[b1 = iSeriesTerm[b, z, k], iSeriesTerm] &&
				!FreeQ[b1, k];
		       If[b1OK,
			  Return[True]]
		    ]
		   )&, subsets];
		TrueQ[scan]
	  ])
  ] /; (
       (* Restrict rule to 3 or fewer factors, because otherwise there is
		the potential for some SeriesTerm expressions to take
		unreasonably long to return unevaluated.  For example,
		SeriesTerm[Sin[ArcCot[a*x]]/(ArcCot[a*x]^2*(Cos[x] + Sin[x])^2),
			{x, 0, n}] .
       *)
       Length[expr] <= 3 &&
       Module[{a, b},
	  (* Make sure that this expression isn't better handled by
		"products with a rational function" rule or a rule for
		handling a ratio of polynomials in z.  *)
	  (* ``a'' is a ratio of polynomials in z, ``b'' is not *)
	  {a, b} = SeparateProduct[expr,
		(PolynomialQ[#, z] || PolynomialQ[#^(-1), z])& ];
	  a === 1
       ]
       )

(* NOTE: this rule modifies $SumDepth *)
iSeriesTerm[a_^k_Integer?Positive, z_Symbol, n_] :=
    Block[{sd, result, a1, b1, pma, pmb},
      (
      pma = PoleMultiplicity[a, {z, 0}];
      pmb = PoleMultiplicity[a^(k - 1), {z, 0}];
      result = Sum[Evaluate[ a1 b1 ],
            	    Evaluate[{K[sd], -pmb, n + pma}] ];
      result
      ) /; Block[{a1OK, b1OK},
	      sd = $SumDepth;
	      Increment[$SumDepth];
	      a1OK = FreeQ[a1 = iSeriesTerm[a, z, n - K[sd]], iSeriesTerm];
              If[a1OK,
		 Increment[$SumDepth];
		 b1OK =
	   	   FreeQ[b1 = iSeriesTerm[a^(k - 1), z, K[sd]], iSeriesTerm];
		 b1OK,
	         False
	      ]
	   ]
    ] /; !FreeQ[a, z]


(* ============================== SERIES RAT ============================ *)

SeriesRat[f_, z_Symbol, n_] :=
        (
        iSeriesTerm[f, z, n])  /; PolynomialQ[f, z]

SeriesRat[c_. z_^k_Integer?Negative, z_Symbol, n_] :=
        (
         c iSeriesTerm[z^k, z, n])  /; FreeQ[c, z]

SeriesRat[f_. g_^k_Integer?Negative, z_Symbol, n_] :=

    Block[{ff = Expand[f], gg = Expand[g], deg, l, mod, modList, displace,
           denom = Expand[g^(-k)], rootList, poleList, poleSet,
           mp, inverseRoot, poleUse, temp, temp1, i, j},

         deg = Exponent[gg, z];

         If[integerFunctions,
            l = CoefficientList[ff, z];
            mod = Exponent[gg, z, GCD];
            modList = Union[Mod[#, mod]& /@ (Select[Range[Length[l]],
                 (l[[#]] =!= 0)&] - 1)];
            If[Length[modList] == 1,
                 displace = First[modList],
                {displace, mod} = {0, 1}
            ],
            {displace, mod} = {0, 1}
         ];

         {ff, gg, denom} = {Expand[ff/z^displace], gg, denom} /.
             z -> z^(1/mod);
         deg = deg/mod;
         rootList = {ToRules[Roots[gg == 0, z,
                 Cubics -> False, Quartics -> False]]};
         (* Note: don't numericalize unless !FreeQ[rootList, Root] *)

         (* an infinite precision exact solution is the only option in V3.0 *)
         If[ FreeQ[rootList, Root],

               poleList = Cancel[#[[1,2]]& /@ rootList];
               poleSet = Union[poleList];
               mp = -k Count[poleList, #]& /@ poleSet;
               If[ deg <= 2,

                  inverseRoot = Expand[(1 - gg/(gg /. z -> 0))/z];
                  poleUse = ArgPi[Apart[
                        inverseRoot /. z -> #]]^(-1)& /@ poleSet;
                  temp = iSeriesTerm[
                   Plus @@ Table[
                    SeriesDivide[
                     Horner[ff, {z, poleSet[[i]], mp[[i]]}],
                     Drop[Horner[denom, {z, poleSet[[i]], 2 mp[[i]]}], mp[[i]]],
                     mp[[i]] ] .
                    Table[(z - poleUse[[i]])^(-j), {j, mp[[i]], 1, -1}],
                    {i, Length[poleSet]}],
                                     z, Expand[(n-displace)/mod]  ],

                  (* deg > 2 *)
                  (* poleUse is important in putting roots in a simplified
                        form so that the final result from iSeriesTerm will
                        be trigonometric *)
                  poleUse = Complex[Together[ReP[#]//.SimplifyCubic],
                           Together[ImP[#]//.SimplifyCubic]]& /@ poleSet;
                  poleUse = poleUse /. Complex[a_, 0] -> a;
                  temp = iSeriesTerm[
                   Plus @@ Table[
                    SeriesDivide[
                     Horner[ff, {z, poleSet[[i]], mp[[i]]}],
                     Drop[Horner[denom, {z, poleSet[[i]], 2 mp[[i]]}], mp[[i]]],
                     mp[[i]] ] .
                    Table[(z - poleUse[[i]])^(-j), {j, mp[[i]], 1, -1}],
                    {i, Length[poleSet]}],
                                     z, Expand[(n-displace)/mod]  ]

               ] (* end If deg <= 2 *),

               (* use PartialFractions *)
               temp = iSeriesTerm[PartialFractions[f g^k, z],
                          z, Expand[(n-displace)/mod] ]

         ]; (* end If FreeQ[rootList, Root] *)

         temp = temp //. a_ c_If + b_ c_ -> (a + b) c;
         temp = temp /. SimplifyComplexE1;
         Switch[deg, 2, temp = temp /. SimplifyComplex2,
                     3, temp = temp /. SimplifyComplex3,
                     4, temp = temp //. SimplifyComplex4];
         temp = temp /. SimplifyComplexE2;
         If[deg == 2,
           (* simplify algebraic expressions after temp is in terms of
                trig functions *)
            temp1 = Simplify[temp];
            If[LeafCount[temp1] < LeafCount[temp] &&
               Length[Position[temp1, n]] <= Length[Position[temp, n]],
                        temp = temp1            ]];
         temp = temp /. Complex[a_, b_] :> a + b I;
         If[Evaluate[SymbolicMod[n, mod] == Mod[displace, mod]],
                Evaluate[temp],
		0]
    ] /; PolynomialQ[f, z] && PolynomialQ[g, z] && !FreeQ[g, z]


(* =============================== HORNER ============================= *)

Horner[p_, {z_, alpha_, m_}] :=
    Block[{qh = p, ih, kh, nh, ah},
        {nh, ah} = {Exponent[qh, z], CoefficientList[qh, z]};
        Do[ah[[ih]] = alpha ah[[ih + 1]] + ah[[ih]], {kh, 1, Min[nh, m]},
            {ih, nh, kh, -1}];
        If[m > nh + 1,
                Do[AppendTo[ah, 0], {ih, m - nh - 1}]
        ];
        Take[ah, m]
    ]

(* ============================== SERIES DIVIDE ========================== *)

SeriesDivide[a_, b_, n_] :=
    Block[{is, cs = Range[n], bRR},
        bRR = Reverse[Rest[b]];
        Do[(cs[[is]] = (a[[is]] - Take[cs, is-1].Take[bRR, is-1]) /
                      b[[1]]), {is, 1, n}];
        cs
    ]


(* ============================== SYMBOLIC MOD =========================== *)

(*
SymbolicMod[n, k] is equal to Mod[n, k] when n is a number, and is left
unevaluated otherwise.
*)

SymbolicMod[n_, 1] = 0

SymbolicMod[n_?NumberQ, k_] := Mod[n, k]

SymbolicMod /: (SymbolicMod[n_ + a_., 2] == b_) := Even[n] /;
               EvenQ[b - a] && (SameQ[Head[n],Symbol] || MatchQ[n,K[_Integer]])

SymbolicMod /: (SymbolicMod[n_ + a_., 2] == b_) := Odd[n] /;
               OddQ[b - a] && (SameQ[Head[n],Symbol] || MatchQ[n,K[_Integer]])

SymbolicMod /: (SymbolicMod[n_, 2] == 0) := Even[n]

SymbolicMod /: (SymbolicMod[n_, 2] == 1) := Odd[n]

SymbolicMod /: (SymbolicMod[n_, k_Rational] == 0) := True

(* ============================ PARTIAL FRACTIONS ========================= *)
PartialFractions[expr_, x_] :=
    (issueObsoleteFunMessage[PartialFractions, "DiscreteMath`RSolve`"];
    Module[{apart = Apart[expr], apartlist, parfraclist, num, den, result},
      apartlist = If[Head[apart] === Plus,
                        Apply[List, apart], {apart}];
      parfraclist = Map[({num, den} = {Numerator[#], Denominator[#]};
                         If[(PolynomialQ[num, x] && PolynomialQ[den, x] &&
                             Max[1, Exponent[num, x]] < Exponent[den, x] &&
                             Exponent[den, x] > 2) &&
			    !MatchQ[den, c_. ((a_ + b_.*x^k_.)^m_.) /;
				FreeQ[{a, b, c, k, m}, x] ],
                       	    temp = pfexpand[num, den, x];
			    If[FreeQ[temp, $Failed],
			       temp,
			       num/den],
			    #] )&,
		        apartlist];
      result = Apply[Plus, parfraclist];
    (* necessary to verify that the expression changed; there is a
       possibility for uncontrolled loops if it didn't really expand,
       but changed form slightly *)
      If[Head[result] === Plus, result, expr]
    ])

pfexpand[num_, den:(densqfree_^mult_.), x_] :=
   Module[{n=Exponent[densqfree,x], roots = {}, root, scan,
		 cn, terms, xx, result},
        cn = Coefficient[densqfree, x, n];
	scan = Scan[Function[{k},
		      root = Root[Function[densqfree /. x->#], k];
		      If[FreeQ[root, $Failed],
                        AppendTo[roots, root],
                        Return[$Failed]
		      ]
		    ],      Range[n]];
        If[scan === $Failed, Return[$Failed]];
        terms = (x-#)^(-mult)*Normal[
          Series[(num)*((densqfree-(densqfree/.x->#))/(x-#))^(-mult),
          {x,#,mult-1}]];
        terms = Collect[terms, (x-#)];
        result = Sum[Evaluate[terms]& [roots[[k]]], {k,1,n}];
	result
   ]

(* ================================= REALQ ============================== *)

RealQ[x_] := True /; IntegerQ[x]
RealQ[x_Rational] = True
RealQ[x_Real] = True
RealQ[Pi] = True
RealQ[E] = True
RealQ[n_!] := True /; RealQ[n]

(* ============================== REP and IMP =========================== *)

(*
ReP[x] gives the real part of x.  ImP[x] gives the imaginary part of x.
*)

ReP[x_] := x /; RealQ[x]
ImP[x_] := 0 /; RealQ[x]

ReP[k_Integer x_] := k ReP[x]
ReP[k_Rational x_] := k ReP[x]
ReP[k_Real x_] := k ReP[x]

ImP[k_Integer x_] := k ImP[x]
ImP[k_Rational x_] := k ImP[x]
ImP[k_Real x_] := k ImP[x]

ReP[Complex[x_, y_]] := x
ImP[Complex[x_, y_]] := y

ReP[x_ + y_] := ReP[x] + ReP[y]
ImP[x_ + y_] := ImP[x] + ImP[y]

ReP[x_ Sqrt[y_?Positive]] :=  ReP[x] Sqrt[y]
ImP[x_ Sqrt[y_?Positive]] :=  ImP[x] Sqrt[y]
ReP[x_ Sqrt[y_?Negative]] := -ImP[x] Sqrt[-y]
ImP[x_ Sqrt[y_?Negative]] :=  ReP[x] Sqrt[-y]

ReP[x_ y_] := ReP[x] ReP[y] - ImP[x] ImP[y]
ImP[x_ y_] := ReP[x] ImP[y] + ImP[x] ReP[y]

ReP[x_?Positive ^n_] := x^n /; ImP[n] == 0
ImP[x_?Positive ^n_] := 0   /; ImP[n] == 0

ReP[x_?Negative ^n_Rational] := 0 /; IntegerQ[2n]
ImP[x_?Negative ^n_Rational] := (-x)^n (-1)^(n-1/2) /;
                                          IntegerQ[2n]

ReP[1/x_] := ReP[x] / (ReP[x]^2 + ImP[x]^2)
ImP[1/x_] := -ImP[x] / (ReP[x]^2 + ImP[x]^2)

ReP[x_^Rational[p_,q_]] :=
  Module[{u = x^Quotient[p,q], v = x^(Mod[p,q]/q)},
    ReP[u] ReP[v] - ImP[u] ImP[v]
  ]	/; 	AbsV[p] >= AbsV[q]

ImP[x_^Rational[p_,q_]] :=
  Module[{u = x^Quotient[p,q], v = x^(Mod[p,q]/q)},
    ReP[u] ImP[v] + ImP[u] ReP[v]
  ]	/; 	AbsV[p] >= AbsV[q]



ReP[x_^q_Rational] := AbsV[x]^q Cos[q Angle[x]] /;
                                      AbsV[q] <= 1
ImP[x_^q_Rational] := AbsV[x]^q Sin[q Angle[x]] /;
                                      AbsV[q] <= 1

ReP[E^x_] := Cos[ImP[x]] Exp[ReP[x]]
ImP[E^x_] := Sin[ImP[x]] Exp[ReP[x]]

ReP[x_^2] := ReP[x]^2 - ImP[x]^2
ImP[x_^2] := 2 ReP[x] ImP[x]

ReP[x_^3] := ReP[x]^3 - 3 ReP[x] ImP[x]^2
ImP[x_^3] := 3 ReP[x]^2 ImP[x] - ImP[x]^3

ReP[x_^n_Integer] := Block[{a, b},
	a = Round[n/2];
	b = n-a;
	ReP[x^a] ReP[x^b] - ImP[x^a] ImP[x^b]
	] /; n != -1

ImP[x_^n_Integer] := Block[{a, b},
	a = Round[n/2];
	b = n-a;
	ReP[x^a] ImP[x^b] + ImP[x^a] ReP[x^b]
	] /; n != -1

(* ================================== ANGLE ================================= *)

Angle[z_] :=
    Block[{angle =
	     Block[{x = ReP[z], y = ImP[z]},
                 Which[
		   TrueQ[Expand[x] == 0], Pi/2 Sgn[y],
                   TrueQ[Expand[y] == 0], Pi/2 (1 - Sgn[x]),
                   True, ArcTan[y/x] - Pi/2 (Sgn[x]-1) Sgn[y]
	         ]
	     ]},
	  angle /; FreeQ[angle,Sgn]
    ] /;  FreeQ[{ReP[z],ImP[z]},ReP] && FreeQ[{ReP[z],ImP[z]},ImP]

(* =================================== SGN ================================== *)

Sgn[0] = 0
Sgn[a_?Positive] = 1
Sgn[a_?Negative] = -1
Sgn[a_ b_] := Sgn[a] Sgn[b]
Sgn[a_^b_] := Sgn[a]^b
Sgn[Sqrt[a_]] = 1
Sgn[a_ + b_] := Sgn[Chop[N[a + b]]] /; !SameQ[a + b, Chop[N[a + b]]]

(* ================================= ABSV ================================= *)

AbsV[0] = 0
AbsV[x_?Positive] := x
AbsV[x_?Negative] := -x
AbsV[x_ y_] :=
   Block[{a = AbsV[x], b = AbsV[y]},
	a b /; FreeQ[{a,b},AbsV]
   ]
AbsV[x_^n_] :=
   Block[{a = AbsV[x]},
	a^n /; FreeQ[a,AbsV]
   ] /; Head[n] =!= Complex
AbsV[x_] :=
   Block[{absv = Block[{a = ReP[x], b = ImP[x]},
			Which[
			  TrueQ[Expand[b] == 0], Expand[a Sgn[a]],
			  TrueQ[Expand[a] == 0], Expand[b Sgn[b]],
			  True, Sqrt[Expand[a^2 + b^2]]
                        ]
                 ]},
         absv  /; FreeQ[absv, Sgn]
  ] /; FreeQ[{ReP[x],ImP[x]}, ReP] && FreeQ[{ReP[x],ImP[x]}, ImP]

(* =============================== CONJUGATEQ ============================ *)

(*
ConjugateQ[x, y] tests whether x and y are complex conjugates.
*)
(* NOTE: it is possible for this to return unevaluated, 7/94 *)
ConjugateQ[a_, b_] :=
    Block[{l = PatternList[{a, b},
               _Symbol?(Context[#] =!= "System`"&) ],
	   repa, repb, impa, impb},
      (
        Which[
            l === {},
                Chop[ N[repa - repb] ] == 0 &&
                Chop[ N[PolynomialMod[impa + impb, 2 Pi]] ] == 0,
            l =!= {},
                Expand[repa - repb] == 0 &&
                Expand[PolynomialMod[impa + impb, 2 Pi]] == 0
	]
      ) /; (
	    FreeQ[repa = ReP[a], ReP] && FreeQ[repb = ReP[b], ReP] &&
	    FreeQ[impa = ImP[a], ImP] && FreeQ[impb = ImP[b], ImP])
    ]


(* =============================== iSIMPLIFY =========================== *)

(* NOTE: iSimplify is at best an ad hoc collection of rules for
	simplifying a final result of SeriesTerm.  They could bear pruning. *)
iSimplify[expr_] :=
  Module[{if, sum, result},
	result = expr //. {If :> if, Sum :> sum};
	result = result /. {SymbolicMod :> Mod};
        result = result //. SimplifyTrig;
        result = result /. SimplifyBinomial;
        result = result /.
                {Power[aa_, b_] :> Power[aa, Expand[b]],
                 Binomial[aa_, b_] :> Binomial[Simplify[Expand[aa]],
			Simplify[Expand[b]]],
                 if[aa_, b_, c_] :> Module[{isolve = ISolve[aa]},
				     (
					if[isolve, b, c]
				     ) /; isolve =!= $Failed
				    ] /; FreeListQ[aa, {Odd, Even, Mod}]
		};
        result = result /.
                {if[Odd[m_], if[m_ >= aa_, b_, c_] d_., e_] :>
                     if[Odd[m], Expand[b d], e] /;
                     Entails[iInfo[m], m >= aa - 1] && OddQ[aa],
                 if[Even[m_], if[m_ >= aa_, b_, c_] d_., e_] :>
                     if[Even[m], Evaluate[Expand[b d]], e] /;
                     Entails[iInfo[m], m >= aa - 1] && EvenQ[aa] };
	(* NOTE: iFullSimplify used instead of Simplify, so that
		Log[1 - I] - Log[1 + I] will simplify to -I Pi/2 *)
	result = result //.
		{a_. if[cond_, b_, c_] + d_ if[cond_, e_, f_] :>
		   a if[cond,
			 iFullSimplify[b-e], iFullSimplify[c-f]] /; a + d == 0,
		 a_. if[cond_, b_, c_] + d_ if[cond_, e_, f_] :>
		   if[cond,
			 iFullSimplify[a b + d e], iFullSimplify[a c + d f]],
		 a_. if[cond_, b_, c_] + a_. if[cond_, e_, f_] :>
		   a if[cond,
		 	 iFullSimplify[b+e], iFullSimplify[c+f]],
		 a_. if[m_ >= 0, if[Even[m_], b_, 0], 0] +
		   e_. if[m_ >= 1, if[Even[m_ - 1], f_, 0], 0] :>
			if[m >= 0, if[Even[m], a b, e f], 0],
		 c1_. if[m_ >= m1_, a1_, 0] + c2_. if[m_ >= m2_, a2_, 0] :>
			if[m >= m1, Simplify[c1 a1 + c2 a2], 0]  /; (m2 > m1 &&
			Scan[If[(
			      !TrueQ[iFullSimplify[a2 /. {m :> #}] == 0]),
			     Return[False]]&, Range[m1, m2-1]] =!= False)

		 };
	result = result //. {sum :> Sum, if :> If};
	result

  ]

iFullSimplify[expr_] :=
	(
	FullSimplify[expr]) /; NumberQ[N[expr]]
(* NOTE: FreeQ[expr, Complex] because
	Simplify[
   (2^(1 + n)*(-(1 - I*Sqrt[3])^(-1))^n)/(1 - I*Sqrt[3]) -
   (2^(1 + n)*(-(1 + I*Sqrt[3])^(-1))^n)/(1 + I*Sqrt[3])]
	as in SeriesTerm[(1 + x + x^2)^(-3), {x, 0, n}] takes too long.
*)
iFullSimplify[expr_] :=
	(
	 Simplify[expr]) /; FreeQ[expr, Complex]
iFullSimplify[expr_] := expr

(*
SimplifyTrig is a list of rules that put trigonometric expressions into
canonical form. *)

SimplifyTrig = {
    Sin[x_. + r_Rational Pi] :> Cos[x] /; EvenQ[r-1/2],
    Sin[x_. + n_Integer?OddQ Pi] :> -Sin[x],
    Sin[x_. + r_Rational Pi] :> -Cos[x] /; EvenQ[r-3/2],
    Sin[x_. + n_Integer?EvenQ Pi] :> Sin[x],
    Cos[x_. + r_Rational Pi] :> -Sin[x] /; EvenQ[r-1/2],
    Cos[x_. + n_Integer?OddQ Pi] :> -Cos[x],
    Cos[x_. + r_Rational Pi] :> Sin[x] /; EvenQ[r-3/2],
    Cos[x_. + n_Integer?EvenQ Pi] :> Cos[x],
    Sin[a_?Negative b_.] :> -Sin[-a b],
    Cos[a_?Negative b_.] :>  Cos[-a b],
    Sin[a_Plus] :> -Sin[Expand[-a]] /; MatchQ[a[[1]], _?Negative _.],
    Cos[a_Plus] :>  Cos[Expand[-a]] /; MatchQ[a[[1]], _?Negative _.],
    ArcTan[a_?Negative b_.] :> -ArcTan[-a b],
    ArcTan[a_Plus] :> -ArcTan[Expand[-a]] /;
                                       MatchQ[a[[1]], _?Negative _.],
    ArcTan[Sqrt[3]] -> Pi/3,
    ArcTan[Sqrt[3]/3] -> Pi/6,
    ArcTan[1/Sqrt[3]] -> Pi/6}


SimplifySqrt = {a_^Rational[b_,2] :> a^((b-1)/2) Sqrt[a]}

SimplifyCubic = Join[SimplifyTrig, SimplifySqrt]

SimplifyBinomial = {
    Binomial[1/2, k_ + 1] :>
	(
	   (-4)^(-k) Binomial[2 k, k]/(2(k + 1))  ) /;
        		Entails[iInfo[k], k != -1],
    Binomial[1/2, k_] -> (-4)^(-k) Binomial[2 k, k]/(1 - 2 k),
    Binomial[-1/2, k_] -> (-4)^(-k) Binomial[2 k, k],
    Binomial[k_ - 1/2, k_] -> 4^(-k) Binomial[2 k, k]}


(* ============================= SIMPLIFYSUM ============================= *)

SimplifySum[expr_] :=
  Module[{simplifySumRules, if, sum, result},

        simplifySumRules = {
    sum[1, {k_, a_, b_}] :> When[a <= b, b - a + 1, 0],
    sum[a_. b_, {k_, c__}] :>
	(
	 b sum[a, {k, c}]
	) /; FreeQ[b, k],
    sum[if[k_ == m_, a_, b_] c_., {k_, m_, n_}] :>
       ((a c) /. k -> m) When[n >= m, 1, 0] +
	  (
        	sum[b c, {k, m + 1, n}]
	  ),
    sum[(if[-k_ + n_ >= 0, a_, b_] c_. + d_.) e_., {k_, m_, n_}] :>
	(
         sum[Expand[a c e + d e], {k, m, n}]
	),
    sum[(if[k_ >= m_, a_, b_] c_. + d_.) e_., {k_, m_, n_}] :>
	(
         sum[Expand[a c e + d e], {k, m, n}]
	),
    sum[if[Even[p_], a_, 0] if[Even[k_], b_, 0], {k_, m_, n_}] :>
	 (
	 if[Even[n], sum[if[Even[k], Simplify[a b], 0],
		 {k, m, n}], 0]
         ) /; n === p+k,
    sum[if[Odd[p_], a_, 0] if[Odd[k_], b_, 0], {k_, m_, n_}] :>
 	 (
	 if[Even[n], sum[if[Odd[k], Simplify[a b], 0],
		{k, m, n}], 0]
         ) /; n === p+k,
    sum[if[Even[p_], a_, 0] if[Odd[k_], b_, 0], {k_, m_, n_}] :>
         (
	 if[Odd[n], sum[if[Odd[k], Simplify[a b], 0],
		{k, m, n}], 0]
	 ) /; n === p+k,
    sum[if[Odd[p_], a_, 0] if[Even[k_], b_, 0], {k_, m_, n_}] :>
         (
	 if[Odd[n], sum[if[Even[k], Simplify[a b], 0],
		{k, m, n}], 0]
	 ) /; n === p+k
	 };

	result = expr //. {If :> if, Sum :> sum};
	result = result //. simplifySumRules;
	result //. {sum :> Sum, if :> If}
  ] (* end SimplifySum *)



(* NOTE:
SimplifyComplexE1, SimplifyComplexE2, SimplifyComplex3, and SimplifyComplex4
are lists of rules used to replace
conjugate complex quantities by double their real parts.
*)

SimplifyComplexE1 = {
    a_. I^c_ + d_. (-I)^c_ :>
		 (
		   ArgPi[a] E^Expand[I Pi c/2] +
        		ArgPi[d] E^Expand[-I Pi c/2] ) /;
	  (ConjugateQ[a, d] && FreeQ[{ArgPi[a],ArgPi[d]},ArgPi]),
    a_. E^b_ + d_. E^e_ :>
		 (
		   2 a Cos[ImP[b]] ) /;
       (ImP[a] == a - d == ReP[b] == PolynomialMod[b + e, 2 Pi I] == 0),
    a_. E^b_ + d_. E^e_ :>
		   (
		   -2 ImP[a] Sin[ImP[b]] ) /;
       (ReP[a] == a + d == ReP[b] == PolynomialMod[b + e, 2 Pi I] == 0),
    a_. E^b_ + d_. E^e_ :>
			 (
			 ArgPi[a] E^b + ArgPi[d] E^e ) /;
       FreeQ[{ArgPi[a],ArgPi[d]},ArgPi]
		    }


SimplifyComplex2 = {
    a_. b_^c_ + d_. e_^c_ :>
	Module[{conjad, conjbe},
	 (
          2 AbsV[a]AbsV[b]^c SimplifyCos[Cos[Angle[a] + c Angle[b]]]
         ) /;  (FreeQ[conjad = ConjugateQ[a, d], ConjugateQ] && conjad) &&
	       (FreeQ[conjbe = ConjugateQ[b, e], ConjugateQ] && conjbe)	&&
		FreeQ[{AbsV[a],AbsV[b]}, AbsV] &&
		FreeQ[{Angle[a],Angle[b]}, Angle]
	] /; ImP[c] == 0
	           }

SimplifyComplex3 = {
    a_. b_Complex^c_ + d_. e_Complex^c_ :>
	Module[{conjad, conjbe},
	 (
          2 AbsV[a]AbsV[b]^c Cos[Angle[a] + c Angle[b]]
	 ) /; (FreeQ[conjad = ConjugateQ[a, d], ConjugateQ] && conjad) &&
	      (FreeQ[conjbe = ConjugateQ[b, e], ConjugateQ] && conjbe) &&
              FreeQ[{AbsV[a],AbsV[b]}, AbsV] &&
              FreeQ[{Angle[a],Angle[b]}, Angle]
        ] /; ImP[c] == 0
		   }

SimplifyComplex4 = {
    a_. b_Complex^c_ + d_. e_Complex^c_ :>
	Module[{conjad, conjbe},
	 (
          2 AbsV[a]AbsV[b]^c SimplifyCos[Cos[Angle[a] + c Angle[b]]]
	 ) /; (FreeQ[conjad = ConjugateQ[a, d], ConjugateQ] && conjad) &&
              (FreeQ[conjbe = ConjugateQ[b, e], ConjugateQ] && conjbe) &&
	      FreeQ[{AbsV[a],AbsV[b]}, AbsV] &&
              FreeQ[{Angle[a],Angle[b]}, Angle]
	] /; ImP[c] == 0
		   }

SimplifyComplexE2 = {
    a_. E^b_ + d_. E^e_ :>
    	Block[{c = ImP[b]},
	  (
        	2 ReP[a] Cos[c] - 2 ImP[a] Sin[c]
	  ) /; (FreeQ[conjad = ConjugateQ[a, d], ConjugateQ] && conjad) &&
               (FreeQ[conjbe = ConjugateQ[b, e], ConjugateQ] && conjbe) (* &&
	       FreeQ[{ReP[a],ImP[a]},ReP] && FreeQ[{ReP[a],ImP[a]},ImP] *)
		(* NOTE: the above is not needed as long as
		   ConjugateQ[a, d] checks whether ReP[a] is free of ReP and
		   ImP[a] is free of ImP *)
    	] /; ReP[b] == 0
		    }

SimplifyCos[x_] :=
    Block[{x1 = (x /. Cos[a_] :> Cos[Expand[a]]) //. SimplifyTrig,
           x2 =  x //. SimplifyTrig },
        If[LeafCount[x1] <= LeafCount[x2], x1, x2 ]
    ]





(* ================================ ARGPI ================================= *)

(*
When possible, ArgPi[z] expresses the complex number z in the form
Abs[z] E^(r Pi I) where r is a rational number.
*)

ArgPi[z_] :=
    Block[{fraction = Rationalize[N[Arg[z]/Pi]]},
        If[Head[fraction] === Rational,
            AbsV[z] E^(fraction Pi I),
            z
	]
    ]


(* =========================== FIRST POS and FREE L ======================= *)

(*
FirstPos[l, pattern] returns the position of the first argument of l that
matches pattern, or Null if there is no such element.
*)

FirstPos[l_, pattern_] :=
    SafeFirst[Select[Range[Length[l]], MatchQ[l[[#]], pattern]& ]]

(*
FreeListQ[expr, list] returns True if none of the elements of list appears
in expr, False otherwise.
*)

FreeListQ[expr_, list_] :=
	(
	 Apply[And, Map[FreeQ[expr, #]&, list]]
	)

(* ============================== INEQSOLVE ============================== *)

IneqSolve[x_ + a_ > b_, x_] := IneqSolve[x > b - a, x]

IneqSolve[x_ + a_ >= b_, x_] := IneqSolve[x >= b - a, x]

IneqSolve[x_ + a_ < b_, x_] := IneqSolve[x < b - a, x]

IneqSolve[x_ + a_ <= b_, x_] := IneqSolve[x <= b - a, x]

IneqSolve[x_ > n_Integer, x_] := {RSInterval[n+1, Infinity]}

IneqSolve[x_ >= n_Integer, x_] := {RSInterval[n, Infinity]}

IneqSolve[x_ < n_Integer, x_] := {RSInterval[-Infinity, n-1]}

IneqSolve[x_ <= n_Integer, x_] := {RSInterval[-Infinity, n]}

IneqSolve[x_ > K[k_Integer] + n_., x_] :=
	 {RSInterval[K[k] + n + 1, Infinity]} /; IntegerQ[n]

IneqSolve[x_ >= K[k_Integer] + n_., x_] :=
	 {RSInterval[K[k] + n, Infinity]}  /; IntegerQ[n]

IneqSolve[x_ < K[k_Integer] + n_., x_] :=
	 {RSInterval[-Infinity, K[k] + n - 1]} /; IntegerQ[n]

IneqSolve[x_ <= K[k_Integer] + n_., x_] :=
	 {RSInterval[-Infinity, K[k] + n]}  /; IntegerQ[n]

IneqSolve[a_, x_] := a /; (
			Length[Union[PatternList[a,
                     		_Symbol?(Context[#] =!= "System`"&) ]]] > 1
			  )

IneqSolve[a_ && b_, x_] :=
    Block[{aa = IneqSolve[a, x],
           bb = IneqSolve[b, x],
           merge, accum, begin, nn},
      If[FreeQ[aa, IneqSolve] && FreeQ[bb, IneqSolve],
	If[aa === $Failed || bb === $Failed, Return[$Failed] ];
        aa = Append[aa, {}];
        bb = Append[bb, {}];
        merge = Join[
            Thread[{Join @@ (aa /. RSInterval -> List),
                    Join @@ (aa /. RSInterval[_,_] -> {-1,1}) }],
            Thread[{Join @@ (bb /. RSInterval -> List),
                    Join @@ (bb /. RSInterval[_,_] -> {-1,1}) }] ];
	(* Catch instances where LEQ fails to evaluate to True or False. *)
	merge = Catch[Sort[merge, LEQ]];
	If[merge === $Failed,
	   Return[$Failed] ];
        accum = Thread[{First /@ merge,
                        FoldList[Plus, First[Last /@ merge],
				Drop[Last /@ merge, 1]]}];
        begin = Join @@ Append[Position[accum, {_,-2}], {}];
        (RSInterval @@ # & /@ Thread[{begin, begin + 1}]) /.
                             nn_Integer :> First[accum[[nn]]],
        aa && bb
      ]
    ]

IneqSolve[!a_, x_] :=
    Block[{aa = IneqSolve[a, x], merge, c, d},
        If[FreeQ[aa, IneqSolve],
	    If[aa === $Failed, Return[$Failed]];
            aa = Append[aa, {}];
            merge = Join @@ (aa /. RSInterval[c_, d_] -> {c-1, d+1});
            If[merge === {},
                merge = {-Infinity, Infinity},
                If[First[merge] === -Infinity,
                    merge = Rest[merge],
                    PrependTo[merge, -Infinity] ];
                If[Last[merge] === Infinity,
                    merge = Drop[merge,-1],
                    AppendTo[merge, Infinity] ]
	    ];
            RSInterval @@ # & /@ Partition[merge, 2],
            !aa
	]
    ]

IneqSolve[a_ || b_, x_] :=
    Block[{ccc = IneqSolve[!(!a && !b), x]},
        If[FreeQ[ccc, IneqSolve] && ccc =!= $Failed,
	   ccc,
	   bbb = IneqSolve[b, x];
	   If[FreeQ[(aaa = IneqSolve[a, x]), IneqSolve] && aaa =!= $Failed &&
	      FreeQ[(bbb = IneqSolve[b, x]), IneqSolve] && bbb =!= $Failed,
	      aaa || bbb,
	      $Failed
	   ]
	]
    ]

IneqSolve[Inequality[a_, b_, c_, d_, e__], x_] :=
  (
    IneqSolve[And @@ (Inequality @@ # & /@
        Partition[{a,b,c,d,e},3,2]), x]
  )


	(* 3 or more arguments *)
IneqSolve[h_[a_, b_, c__], x_] :=
  (
    IneqSolve[And @@ (h @@ # & /@ Partition[{a,b,c},2,1]), x]
  ) /; MemberQ[{Less, LessEqual, Greater, GreaterEqual, Equal, Unequal}, h]

	(* main procedure; 2 arguments *)
IneqSolve[ineq: h_[a_, b_], x_] :=
    Block[{lhs = Together[a - b],
           num, den, zeroList, poleList, rootlist,
	   points, merge, c, d, args,
		result},

        {num, den} = {Numerator[lhs], Denominator[lhs]};
        zeroList = If[FreeQ[num, x], {},
		      rootlist = {ToRules[Roots[num == 0, x]]};
		      If[!ListQ[rootlist], {},
		         #[[1,2]]& /@ rootlist
		   ]];
        poleList = If[FreeQ[den, x], {},
		      rootlist = {ToRules[Roots[den == 0, x]]};
                      If[!ListQ[rootlist], {},
                         #[[1,2]]& /@ rootlist
                   ]];
	(* Assume that Im[K[n]] is zero for any n, because K is a default
		generic name for the summation index. *)
        zeroList = Select[zeroList, MatchQ[Im[#], 0 | Im[K[_]]]&] // Union;
        poleList = Select[poleList, MatchQ[Im[#], 0 | Im[K[_]]]&] // Union;
        points = Union[zeroList, poleList];
        merge = Partition[Append[Prepend[points,-Infinity],Infinity],
            2, 1];
        merge = Select[merge, (ineq /. x :>
                  Which[# === {-Infinity, Infinity}, 0,
                        #[[1]] === -Infinity, #[[2]] - 1,
                        #[[2]] ===  Infinity, #[[1]] + 1,
                        True, (Plus @@ #)/2 ] )&];
        merge = (RSInterval @@ # & /@ merge) /.
            RSInterval[c_, d_] :> RSInterval[Ceiling[c], Floor[d]];
	(* Assume that K[n] is integer valued so that Ceiling[K[1] + 1] and
		Floor[K[1] + 1] both simplify to K[1] + 1 . *)
        merge = merge //. {Ceiling[-Infinity] :> -Infinity,
                          Floor[Infinity] :> Infinity,
			  Ceiling[K[k_] + i_.] :> K[k] + i /; IntegerQ[i],
			  Floor[K[k_] + i_.] :> K[k] + i /; IntegerQ[i]};
	(* Allow intervals of the types RSInterval[K[1], K[1]] and
		RSInterval[-Infinity, K[1]] and
		RSInterval[K[1], Infinity] to be selected. *)
        merge = Select[merge,
	   (#[[1]] <= #[[2]] ||
	    MatchQ[#, RSInterval[K[k_] + i_., K[k_] + i_.] /; IntegerQ[i]] ||
	    MatchQ[#, RSInterval[-Infinity, K[k_] + i_.] /; IntegerQ[i]] ||
	    MatchQ[#, RSInterval[K[k_] + i_., Infinity] /; IntegerQ[i]])&];
        zeroList = Join[Floor /@ zeroList, Ceiling /@ zeroList];
	(* Assume that anything of the form K[n] + i is integer valued, so
	   Floor[K[1] + 3] -> K[1] + 3 and Ceiling[K[1] - 2] -> K[1] - 2. *)
	zeroList = zeroList //.
		 {Floor[K[k_] + i_.] :> K[k] + i /; IntegerQ[i],
		  Ceiling[K[k_] + i_.] :> K[k] + i /; IntegerQ[i]};
	zeroList = Sort[zeroList];
        zeroList = Select[zeroList, ((den /. x->#) != 0)&];
        zeroList = Select[zeroList, (ineq /. x->#)&];
        merge = Join[merge, Thread[RSInterval[zeroList, zeroList]]];
	(* Catch instances where LEQ fails to evaluate to True or False. *)
	merge = Catch[Sort[merge, LEQ]];
	If[merge === $Failed,
	   Return[$Failed] ];
        merge = merge //.
            {RSInterval[c_?((NumberQ[#] && ((den /. x:>#) == 0 ||
                !ineq/.x->#))&), d_] :> RSInterval[c+1, d]};
        merge = merge //.
            {RSInterval[c_, d_?((NumberQ[#] && ((den /. x:>#) == 0 ||
                !ineq/.x->#))&)] :> RSInterval[c, d-1]};
	merge = Map[Switch[#,
		RSInterval[K[k_] + i_., Infinity] /; IntegerQ[i],
			RSInterval[#[[1]] + 1, Infinity],
		RSInterval[-Infinity, K[k_] + i_.] /; IntegerQ[i],
			RSInterval[-Infinity, #[[2]] - 1],
		_,
			#]&, merge];
	(* Allow intervals of the types RSInterval[K[1], K[1]] and
		RSInterval[-Infinity, K[1]-1] and
		RSInterval[K[1]+1, Infinity] to be selected. *)
        merge = Select[merge,
	   (#[[1]] <= #[[2]] ||
	    MatchQ[#, RSInterval[K[k_] + i_., K[k_] + i_.] /; IntegerQ[i]] ||
	    MatchQ[#, RSInterval[-Infinity, K[k_] + i_.] /; IntegerQ[i]] ||
	    MatchQ[#, RSInterval[K[k_] + i_., Infinity] /; IntegerQ[i]])&];
        args = List @@ (Join @@ Append[merge, RSInterval[]]);
        result = If[args === {},
	   {},
           {first, last} = {First[args], Last[args]};
            RSInterval @@ # & /@ Partition[Append[Prepend[Join @@
                Append[Select[Partition[Drop[Rest[args], -1], 2],
                (#[[2]] - #[[1]] > 1)&], {}], first], last], 2]
	   ];
	result
    ]  /; MemberQ[{Less, LessEqual, Greater, GreaterEqual, Equal, Unequal}, h]


(* ============================== ISOLVE =============================== *)

ISolve[expr_] :=
    (issueObsoleteFunMessage[ISolve, "DiscreteMath`RSolve`"];
    Block[{l = UserSymbols[expr], n, temp, p, nn},
        If[Length[l] == 1,
            n = First[l];
            If[(temp = IneqSolve[expr /. {n :> nn}, nn]) === $Failed,
		Return[$Failed] ];
	    temp = temp /. {nn :> n};
            temp = temp /. List[p___RSInterval] :> Or[p];
            temp = temp /.
              		{RSInterval[-Infinity, Infinity] :> True,
               		RSInterval[-Infinity, b_] :> n <= b,
               		RSInterval[a_, Infinity] :> n >= a,
               		RSInterval[a_, a_] :> n == a,
               		RSInterval[a_, b_] :> a <= n <= b};
	    If[!FreeQ[temp, IneqSolve],
                  temp /. {IneqSolve[aaa_, nnn_] :> aaa},
	          temp],
            expr
        ]
    ])

(* ================================ LEQ ================================= *)
LEQ[h_[a_, b_], h_[c_, d_]] :=
  Module[{leq, sa = Sign[a], sc = Sign[c]},
	leq = sa-sc < 0 || (sa-sc === 0 && b-d <= 0);
	leq
  ] /; Head[a] === Head[c] === DirectedInfinity


LEQ[h_[a_,b_], h_[c_,d_]] :=
   Module[{leq},
	 (* Assume that -Infinity < K[1] + i < Infinity for integer i. *)
	 leq = (a-c < 0 ||
		 MatchQ[{a, c}, {-Infinity, K[k_] + i_. /; IntegerQ[i]} |
				{K[k_] + i_. /; IntegerQ[i], Infinity}]
	       ) ||
	       (a-c === 0 && b-d <= 0);
	 leq = If[leq === True || leq === False,
		leq,
		Throw[$Failed] ];
	 leq
   ]


(* =============================== IINFO ================================== *)

iInfo[_?NumberQ] = True
iInfo[_Symbol] = True
iInfo[f_[a___]] :=
	(
	And @@ (iInfo /@ {a})
	)


(* ============================= LEADING COEF ============================ *)

(*
LeadingCoef[poly, x] returns the leading coefficient of the polynomial
poly[x].
*)

LeadingCoef[poly_, x_] :=
  (
   If[poly === 0, 0,
    Coefficient[poly, x, Exponent[poly, x]] ]
  )


(* ============================= MAKE LIST =============================== *)

(*
MakeList[expr, head:List] returns the list of arguments of expr if the head
of expr matches the given one, otherwise, it returns {expr}.
*)

MakeList[expr_, head_:List] :=
    If[Head[expr] === head, List @@ expr, List @ expr]


(* =========================== MAKE TRINOMIAL ============================ *)

(*
MakeTrinomial[expr_] tries to write expr in the form Multinomial[a, b, c].
*)

MakeTrinomial[a: Binomial[b_, c_] Binomial[d_, e_]] :=
    Block[{l1 = {b, d},
           l2 = {c, b - c, e, d - e}, l},
        l = Intersection[l1, l2];
        If[Length[l] != 1, Return[a]];
        l1 = ListComplement[l1, l];
        l2 = ListComplement[l2, l];
        If[{Plus @@ l2} != l1, a,
           Sort[Multinomial @@ l2]
	]
    ]

MakeTrinomial[a_] := a


(* =========================== LIST COMPLEMENT =========================== *)

(*
ListComplement[list1, list2] returns the multiset-difference of l1 and l2.
*)

ListComplement[l1_List, l2_List] :=
    Block[{l = l1},
        If[MemberQ[l, #],
            l = Drop[l, {FirstPos[l, #]}] ]& /@ l2;
         l
    ]


(* =================================== PARSE ============================== *)


Parse[l1_, l2_, n_] :=
    Block[{unknowns, recur, initconds, recur0,
	   nlist, sum, maxindices0, scan, tempRecur,
	   OK, k, m, range, startValues, aa, kk, lo, hi},

        unknowns = Head /@ l2;

        (* separate recurrences from initial conditions *)

        recur =  Select[l1, !FreeQ[#, n]&];
        initconds = Select[l1,  FreeQ[#, n]&];

	(* a recurrence like a[n] == 1 + n a[-1 + n] implies
		initial condition a[0] == 1 *)
	If[initconds === {},
	   recur0 = recur /. n:>0;
	   If[Apply[And,
	       Map[Module[{eq = #},
		    Apply[Plus, Map[
			 Length[Position[eq, #[_]]]&, unknowns]] == 1
		   		]&,		recur0]],
	      initconds = recur0
	   ]
	];

	(* Find n1, the leftmost point of the domain of the equations;
		 this was previously specifiable by the user.
	   The specification of the domain is an artifact of Marko's original
		 design and should be eliminated. *)
	If[initconds =!= {},
           nlist = Map[Cases[initconds, #[x_] -> x, Infinity]&,
		 unknowns];
	   nlist = Map[Max, nlist] + 1;

	   recur = recur /. {Sum :> sum};
	   maxindices0 = MaxIndices[recur, n, unknowns];

	   diff = nlist-maxindices0;

           If[ maxindices0 =!= nlist,
	      (* need to shift equations *)
	      scan = Scan[Function[{i},
		           (tempRecur = ( # /. {n -> n + i} )& /@ recur;
		            If[MaxIndices[tempRecur, n, unknowns] === nlist,
		               recur = tempRecur;
			       Return[OK]]
			   )], Union[diff]	];
	      If[scan =!= OK,
                 (* could not find a single shift appropriate for shifting
			entire system of equations *)
		 If[Length[recur] != Length[diff],
		         (* returning Fail[list] because list gives
				info on the nature of the failure *)
			 Return[Fail[
			   Map[#[[1]][#[[2]]]&,
				 Transpose[{unknowns, nlist}] ]
				]]
	         ];
		 scan = Scan[Function[{p},
			        (tempRecur = MapIndexed[
					( #1 /. {n -> n + p[[ #2[[1]] ]]} )& ,
					recur];
				 If[MaxIndices[tempRecur, n, unknowns]===nlist,
                                    recur = tempRecur;
                                    Return[OK]]
			        )], Permutations[diff]];
	         If[scan =!= OK,
		    (* could not find a set of shifts appropriate for
			  shifting entire set of equations *)
		    (* returning Fail[list] because list gives
                                info on the nature of the failure *)
	            Return[Fail[
			Map[#[[1]][#[[2]]]&,
                                 Transpose[{unknowns, nlist}] ]
                                ]]
                 ]; (* end If scan =!= OK *)
	      ] (* end If scan =!= OK *)

	   ]; (* end If maxindices0 =!= nlist *)

           recur = recur /. {sum :> Sum};
	]; (* end If initconds =!= {} *)

        (* determine starting values of n for all sequences *)

        {recur, initconds} = {recur, initconds} //.
            Sum[a_, {k_, m_}] :> Sum[a, {k, 1, m}];
        {recur, initconds} = {recur, initconds} /.
            {HoldPattern[Even[a_]] :> SymbolicMod[a, 2] == 0,
             HoldPattern[Odd[a_]]  :> SymbolicMod[a, 2] == 1,
		Mod -> SymbolicMod};
        range = {initconds, recur /. (Sum[aa_, {kk_, lo_, hi_}] :>
            {aa /. kk -> lo, aa /. kk -> hi} )};
	range = (First /@ (PatternList[range, #[_]] /. n -> 0))& /@ unknowns;
        startValues = Map[Min, range];

	(* Try to translate boundary conditions into initial conditions. *)
	Module[{recur0, minmax, initconds1, ilist, min, max, rlist, n1, n2,
		vars, eqs, soln},
	  recur0 = recur /. n->0;
	  minmax = {};
	  initconds1 = Map[(
	     ilist = Sort[Map[First, PatternList[initconds, #[_]]]];
	     {min, max} = {Min[ilist], Max[ilist]};
	     AppendTo[minmax, {min, max}];
	     Switch[ilist,
		{}, {},
		Range[min, max], {},
		_,
		   rlist = Map[First, PatternList[recur0, #[_]]];
		   n1 = min-Min[rlist];
		   n2 = max-Max[rlist];
		   Table[recur /. n->n0, {n0, n1, n2}]
	     ] )&, unknowns];

	  If[FreeQ[initconds1, DirectedInfinity] &&
	     FreeQ[initconds1, Indeterminate],
	  	initconds1 = Union[Flatten[initconds1]];
	  	initconds1 = Select[initconds1,
		 	validICQ[#, unknowns, minmax]&];

	  	If[initconds1 =!= {},
	   		vars = Apply[Join, Table[({min, max} = minmax[[i]];
			          Map[unknowns[[i]], Range[min, max]]),
					{i, Length[unknowns]}] ];
	   		eqs = Union[initconds, initconds1];
	   		soln = Solve[eqs, vars];
	   		If[FreeQ[soln, Solve] && Length[soln]>=1,
	   			initconds1 = (soln[[1]]) /. {Rule->Equal};
	   			(* add new conditions to initconds *)
	   			initconds = Union[initconds, initconds1]
			]
		]
	  ]
	]; (* end Module *)


        {recur, initconds, unknowns, startValues}
    ] (* END Parse *)

validICQ[eq_, unknowns_, minmax_] :=
	Module[{l = Length[unknowns], ilist, i, min, max},
		(* Check that equation eq does not contain u[k],
			 k < kmin or k > kmax, where
			u is an unknown and kmin and kmax are indices for
			that unknown. *)
		Apply[And, Table[
			   (
			    ilist = Map[First,
					PatternList[eq, (unknowns[[i]])[_]] ];
			    {min, max} = minmax[[i]];
			    Apply[And, Map[(# >= min && # <= max)&,
					ilist]]
			   ), {i, l}]]
	]

MaxIndices[recur_, n_, unknowns_] :=
   Module[{temp = recur /.
		{sum[expr_, {k_, m_}] :> {expr /. k->1, expr /. k->m},
	         sum[expr_, {k_, m1_, m2_}] :> {expr /. k->m1, expr /. k->m2}}
	  },
	Map[Function[{unknown},
	        Max[Map[Max[Cases[{#}, unknown[x_] :> (x /. n:>0) /;
				!FreeQ[x, n], Infinity]]&,
		        temp
	        ]]
	    ], unknowns]
   ]

(* =============================== PATTERN LIST ========================== *)

(*
PatternList[expr, pattern] returns the list of all subexpressions of expr
that match the given pattern, provided that expr does not contain Rule[].
*)

PatternList[expr_?(FreeQ[#, Rule]&), pattern_] :=
    (
     Apply[Part, Prepend[#, expr]]& /@ Position[expr, pattern]
    )

PList[expr_, pattern_] :=
    Block[{xpr = expr /. ((a_ -> b_) -> {a,b})},
        Apply[Part, Prepend[#, xpr]]& /@ Position[xpr, pattern]
    ]

(* =========================== POLE MULTIPLICITY ======================== *)

(*
PoleMultiplicity[f, {z, a}] computes the multiplicity of the pole of f at
z = a.
*)

PoleMultiplicity[f_, {z_, a_}] :=
    Block[{pom, result},
	pom = SafeSeries[f /. z -> z + a, {z, 0, 0}];
        result =
	  If[!FreeListQ[pom, {$Failed, Series}],
	     Infinity,
             pom = Normal[pom];
             If[TrueQ[pom == 0], 0,
                Max[0, Exponent[Expand[pom /. z -> 1/z], z]]
	     ]
	  ];
	result
    ]

(* new in V3.0 *)
PoleMultiplicity[z_^(-k_Symbol), {z_, 0}] := k


(* ================================ RESET ================================= *)

(*
Reset[recur, conds, unknowns, startValues, s0] returns the list {recur, conds}
with starting values of the index reset so that the list starts with s0 rather
than with startValues, for all unknown sequences.
*)

Reset[recur_, conds_, unknowns_, startValues_, s0_] :=
    Block[{ruleDelayed, result, k, sum},
       ruleDelayed = Map[((#[[1]])[k_] :>
		         Evaluate[(#[[1]])[k + s0 - #[[2]]]])&,
		         Transpose[{unknowns, startValues}]];
       result = result //. {Sum :> sum};
       result = {recur, conds} /. ruleDelayed;
       result //. {sum :> Sum}
    ]


(* =============================== SAFE FIRST ============================= *)

(*
SafeFirst[l] returns the first argument of l, or Null if the length of l is
zero.
*)

SafeFirst[l_] :=
    If[Length[l] == 0, Null, First[l]]

(* ============================== SAFE SERIES ============================ *)

(*
SafeSeries[f, {z, a, n}] overcomes a bug in Series[] that fails when n is
less than the degree of zero of the denominator of f at a.
*)

SafeSeries[f_, {z_, a_, n_}] :=
    Block[{lhs, infy, indet, tdep, ifun, poles, ord, esss, sspec, csa, tem},
	lhs = Denominator[Together[f]];
	{infy, indet, tdep, ifun} = {(Head[Power::infy] === $Off),
			             (Head[Infinity::indet] === $Off),
			             (Head[Solve::tdep] === $Off),
				     (Head[InverseFunction::ifun] === $Off)};
	Off[Power::infy]; Off[Infinity::indet];
	Off[Solve::tdep]; Off[InverseFunction::ifun];
        If[!FreeQ[solve = Solve[lhs == 0, z, InverseFunctions -> True],
		Solve],
	   solve = {}
        ];
        poles = z /. solve;
	If[!infy, On[Power::infy]]; If[!indet, On[Infinity::indet]];
	If[!tdep, On[Solve::tdep]]; If[!ifun, On[InverseFunction::ifun]];
        ord = Max[n, Count[poles, a]];
	{esss, sspec, csa} = {(Head[Series::esss] === $Off),
	  (Head[Series::sspec] === $Off), (Head[SeriesData::csa] === $Off)};
        Off[Series::esss]; Off[Series::sspec]; Off[SeriesData::csa];
	tem = Series[f, {z, a, ord}];
	If[!FreeQ[tem, Series], tem = $Failed];
        If[!esss, On[Series::esss]]; If[!sspec, On[Series::sspec]];
		If[!csa, On[SeriesData::csa]];
        If[tem =!= $Failed && n =!= ord,
	  tem = Series[tem, {z, a, n}]
	];
        tem
    ]


(* ========================== SEPARATE PRODUCT =========================== *)

SeparateProduct[p_Times, predicate_] :=
   {Select[p, predicate], Select[p, !predicate[#]&]}


(* ============================ USER SYMBOLS ============================= *)

(*
UserSymbols[expr] returns a list of maximal subexpressions of expr.  Each
subexpression either has head K or is a symbol defined in a context other
than System`.
*)

(* NOTE: updated UserSymbols to accept K[1], K[2], etc as UserSymbols
	following post-V2.2 addition of K to the System` symbols *)
UserSymbols[expr_] :=
    Block[{h = If[Length[expr] == 0, expr, Head[expr]]},
         Which[!FreeQ[h, _Symbol?(Context[#] =!= "System`"&)] ||
	       h === K,
                  {expr},
               Length[expr] == 0,
                  {},
               True,
                  Union @@ (UserSymbols /@ (List @@ expr))
	 ]
    ]


(* ========================== ENTAILS, TTQ, WHEN =========================== *)

(*
Entails[a, b] determines whether a implies b, where a and b are Boolean
combinations of equalities and/or inequalities among rational functions of a
single integer variable.
*)
Entails[a_, a_] := True
Entails[a___ && b_ && c___, b_] := True
Entails[False, a_] := True
Entails[a_, b_] :=
  Block[{l, var, isolve},
    (
    var = First[l];
    isolve = IneqSolve[LogicalExpand[Implies[a, b]], var   ];
    (* apparently, Entails[a, b] is True when IneqSolve[b || !a, var]
	yields {RSInterval[-Infinity, Infinity]} *)
    isolve  ===  {RSInterval[-Infinity, Infinity]}
    )  /; (l = Union[PatternList[{a, b}, _Symbol?(Context[#] =!= "System`"&) ]];
	   Length[l] == 1)
  ]

(*
TTQ[cond] returns True if it determines that the information given about the
variables implies that cond is true, and False otherwise.
*)
TTQ[True] := True
TTQ[a_] := Entails[iInfo[a], a]

When[cond_, a_, b_] :=
 (
   Which[
	 TrueQ[Entails[iInfo[cond],  cond]], a,
	 TrueQ[Entails[iInfo[cond], !cond]], b,
         True, Evaluate[If[Evaluate[cond], Evaluate[a], Evaluate[b]]]
	 ]
 )

(* =============================== EVEN, ODD ============================ *)

Even[n_]:=(issueObsoleteFunMessage[Even, "DiscreteMath`RSolve`"];EvenQ[n] /; NumberQ[n])
Odd[n_]:=(issueObsoleteFunMessage[Odd, "DiscreteMath`RSolve`"];OddQ[n] /; NumberQ[n])


(* =============================== RHP, LHP ============================== *)
(* Determine, for branch-cut purposes, whether a non-zero number is in
	 the right-half-plane or the left-half-plane. *)
rhp[x_] := Re[x]>0 || (Re[x]==0 && Im[x]>0)
lhp[x_] := Re[x]<0 || (Re[x]==0 && Im[x]<0)


(* =============================== integerQ ============================== *)

integerQ[a_] := IntegerQ[a] /; (NumberQ[N[a]] || Head[a]===Symbol)

integerQ[a:(_Times | _Plus)] :=
  With[{list = Map[integerQ, Apply[List, a]]},
	(
       Apply[And, list]
	) /; FreeQ[list, integerQ]
  ]


(* =========================== expandTrigsInVar ========================= *)
(* From Dan Lichtblau's nthTerm.m *)

expandTrigsInVar[e_, x_] := e /; FreeQ[e, x]

expandTrigsInVar[f_*e_, x_] :=
  (
       	f * expandTrigsInVar[e, x]
  ) /; FreeQ[f, x]

expandTrigsInVar[e_Plus, x_] :=
	(
	 Map[expandTrigsInVar[#, x]&, e])

expandTrigsInVar[e_, x_] :=
  (
	e
  ) /; MatchQ[e, a_.*x^k_. /; FreeQ[{a,k}, x]] || !FreeQ[e, Root]

expandTrigsInVar[e_, x_] :=
  Module[{head=Head[e], ee, rules, revrules, newvars, newvar,
		 deninv, res, numer, den},
        res = Which[
                     (* head===Plus || *) head===Times || head===Power,
                        Map[expandTrigsInVar[#, x]&, e],
                     arcTrigHeadQ[head],
			TrigToExp[e],
                     hasTrigQ[e],
			With[{trigexp = TrigExpand[e]},
			  If[ Head[trigexp] === Plus,
                           	Return[expandTrigsInVar[trigexp, x]]
			  ]; (* end If *)
			  Expand[Together[TrigToExp[trigexp]]]
			], (* end With *)
                     True,
			e]; (* end Which *)
        rules = {Exp[a_Integer*b_.*x + c_.] -> Exp[c]*ee[b*x]^a,
          	 Exp[x+c_.] -> Exp[c]*ee[x],
          	 Exp[b_?(!NumberQ[#]&)*x + c_.] -> Exp[c]*ee[b*x],
          	 Exp[Complex[0,a_Integer]*b_.*x + c_.] -> Exp[c]*ee[b*I*x]^a};
        revrules = HoldPattern[ee[a_.*x]]^b_. -> Exp[a*b*x];
        res = MapAll[CollectPowers[#,x]&, res];
        res = res //. rules;
	(* NOTE: do not want to do Together[Sqrt[4 + x^2] +
			(2*E^x)/(-1 + E^(2*x))] because the resulting
			expression is much worse than the original. *)
        res = Together[res];
        den = Denominator[res];
        newvars = Intersection[Cases[den, ee[a_], Infinity]];
        If [Length[newvars]>0,
		newvar = newvars[[1]];
		If[MatchQ[den, (cc_.*(aa_ + bb_.*newvar^kk_.)^mm_.) /;
					FreeQ[{bb,cc,aa,kk,mm}, newvar]] ||
		   MatchQ[den, (aa_.*newvar_^n_Integer) /; FreeQ[aa, newvar]],
		   deninv = 1/den,
                   deninv = PartialFractions[1/den, newvar];
		   (* Note: usually cannot do anything useful with Root,
			i.e., SeriesTerm[1/(1 + Sin[x]^4),{x,0,n}] *)
		   If[!FreeQ[deninv, Root],
			deninv = 1/den];
		];
                numer = Expand[Numerator[res]];
                res = numer*deninv;
                If[Head[numer]===Plus,
		   res = Distribute[res]
		];
                If[Head[res]===Plus,
		   res = Distribute[res]
		];
        ];
        res = res /. revrules;
        (* Don't expand a rational polynomial like
		(-x - 2*x^3)/((-1 + x)^2*(-1 + 4*x^2))
	   because the same series term will have to be found more than once. *)
        If[!(Head[res] === Times && PolynomialQ[Numerator[res], x] &&
		PolynomialQ[Denominator[res], x]),
           res = Expand[res]
	 ];
        If[FreeQ[res, Root],
	   res = MapAll[CollectPowers[#,x]&, res]
	];
	res
  ]   (* end expandTrigsInVar *)

trigheads := {Sin, Cos, Tan, Csc, Sec, Cot,
	Sinh, Cosh, Tanh, Csch, Sech, Coth}
arctrigheads := {ArcSin, ArcCos, ArcTan, ArcCsc, ArcSec, ArcCot,
        ArcSinh, ArcCosh, ArcTanh, ArcCsch, ArcSech, ArcCoth}

arcTrigHeadQ[e_] := MemberQ[arctrigheads, e]
hasTrigQ[e_] := Apply[Or, Map[!FreeQ[e, #]&, trigheads]]

CollectPowers[e_^p_, x_] := e^Collect[p,x]
CollectPowers[e_, _] := e

(* This tests whether the expr will be matched by existing iSeriesTerm
	rules w/out transformation by expandTrigsInVar. *)
trigMatchQ[expr_, z_] :=
 MatchQ[expr, Sin[p_] /; PolynomialQ[p, z] && Exponent[p, z] == 1] ||
 MatchQ[expr, Cos[p_] /; PolynomialQ[p, z] && Exponent[p, z] == 1] ||
 MatchQ[expr, Tan[a_. z] /; FreeQ[a, z]] ||
 MatchQ[expr, Sec[a_. z] /; FreeQ[a, z]] ||
 MatchQ[expr, Cos[a_. Sqrt[b_. z]] /; FreeQ[{a, b}, z]] ||
 MatchQ[expr, Sinh[p_] /; PolynomialQ[p, z] && Exponent[p, z] == 1] ||
 MatchQ[expr, Cosh[p_] /; PolynomialQ[p, z] && Exponent[p, z] == 1] ||
 MatchQ[expr, Tanh[a_. z] /; FreeQ[a, z]] ||
 MatchQ[expr, Cosh[a_. Sqrt[b_. z]] /; FreeQ[{a, b}, z]] ||
 MatchQ[expr, Sech[a_. Sqrt[b_. z]] /; FreeQ[{a, b}, z]]

(* ====================================================================== *)
End[]

Protect[PowerSum, ExponentialPowerSum, GeneratingFunction,
	ExponentialGeneratingFunction, Gf, EGf, HSolve, HypergeometricF,
	SeriesTerm, RealQ, ISolve, PartialFractions, Even, Odd]

EndPackage[]
