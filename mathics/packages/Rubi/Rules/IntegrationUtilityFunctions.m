(* ::Package:: *)

(* ::Title:: *)
(*Integration Utility Functions*)


$TimeLimit::usage = "$TimeLimit is the time constraint in seconds on some potentially expensive routines.";
If[Not[NumberQ[$TimeLimit]], $TimeLimit=5.0];


(* ::Section::Closed:: *)
(*IntHide[u,x]*)


IntHide::usage = "IntHide[u,x] suppresses the display of steps while integrating u wrt x.";
IntHide[u_,x_Symbol] :=
  Block[{$ShowSteps=False,$StepCounter=Null}, Int[u,x]]


(* ::Section::Closed:: *)
(*Mapping functions*)


(* ::Subsection::Closed:: *)
(*EveryQ[func,lst]*)


EveryQ::usage = "EveryQ[func,lst] applies func to the elements of lst until False is returned and EveryQ returns False; else it returns True.";
EveryQ[func_,lst_] :=
  Catch[Scan[Function[If[func[#],Null,Throw[False]]],lst];True]


(* ::Subsection::Closed:: *)
(*Map2[func,lst1,lst2]*)


Map2[func_,lst1_,lst2_] :=
  Module[{ii},
    ReapList[Do[Sow[func[lst1[[ii]],lst2[[ii]]]],{ii,Length[lst1]}]]]


(* ::Subsection::Closed:: *)
(*ReapList[u]*)


ReapList[u_] :=
  With[{lst=Reap[u][[2]]},
  If[lst==={}, lst, lst[[1]]]]

SetAttributes[ReapList,HoldFirst]


(* ::Section::Closed:: *)
(*Numerical type predicates*)


(* ::Subsection::Closed:: *)
(*FalseQ[u]*)


FalseQ::usage = "If u is False, FalseQ[u] returns True; else it returns False.";
FalseQ[False] = True;
FalseQ[_] = False;


(* ::Subsection::Closed:: *)
(*IntegersQ[u]*)


IntegersQ::usage = "If m, n, ... are explicit integers, IntegersQ[m,n,...] returns True; else it returns False.";
IntegersQ[__Integer] = True;
IntegersQ[__] = False;


(* ::Subsection::Closed:: *)
(*HalfIntegerQ[u]*)


HalfIntegerQ::usage = "If m, n, ... are explicit half-integers, HalfIntegerQ[m,n,...] returns True; else it returns False.";
HalfIntegerQ[u__] := Scan[Function[If[Head[#]===Rational && Denominator[#]==2,Null,Return[False]]],{u}]===Null


(* ::Subsection::Closed:: *)
(*FractionQ[u]*)


FractionQ::usage = "If m, n, ... are explicit fractions, FractionQ[m,n,...] returns True; else it returns False.";
FractionQ[__Rational] = True;
FractionQ[__] = False;


(* ::Subsection::Closed:: *)
(*RationalQ[u]*)


RationalQ::usage = "If m, n, ... are explicit integers or fractions, RationalQ[m,n,...] returns True; else it returns False.";
RationalQ[u__] := Scan[Function[If[IntegerQ[#] || Head[#]===Rational,Null,Return[False]]],{u}]===Null


(* ::Subsection::Closed:: *)
(*ComplexNumberQ[u]*)


ComplexNumberQ::usage = "If u an explicit complex number, ComplexNumberQ[u] returns True; else it returns False.";
ComplexNumberQ[_Complex] = True;
ComplexNumberQ[_] = False;


(* ::Subsection::Closed:: *)
(*FractionOrNegativeQ[u]*)


FractionOrNegativeQ::usage = "If u a fraction or negative number, FractionOrNegativeQ[u] returns True; else it returns False.";
FractionOrNegativeQ[u__] := Scan[Function[If[FractionQ[#] || IntegerQ[#] && #<0,Null,Return[False]]],{u}]===Null


(* ::Subsection::Closed:: *)
(*SqrtNumberQ[u]*)


SqrtNumberQ::usage = "If u is equivalent to the square-root of a rational number, SqrtNumberQ[u] returns True; else it returns False.";
SqrtNumberQ[m_^n_] :=
  IntegerQ[n] && SqrtNumberQ[m] || IntegerQ[n-1/2] && RationalQ[m]


SqrtNumberQ[u_*v_] :=
  SqrtNumberQ[u] && SqrtNumberQ[v]


SqrtNumberQ[u_] :=
  RationalQ[u] || u===I


(* ::Subsection::Closed:: *)
(*SqrtNumberSumQ[u]*)


SqrtNumberSumQ::usage = "If u is equivalent to the sum of square-root numbers, SqrtNumberSumQ[u] returns True; else it returns False.";
SqrtNumberSumQ[u_] :=
  SumQ[u] && SqrtNumberQ[First[u]] && SqrtNumberQ[Rest[u]] ||
  ProductQ[u] && SqrtNumberQ[First[u]] && SqrtNumberSumQ[Rest[u]]


(* ::Subsection::Closed:: *)
(*ConstantQ[u]*)


ConstantQ::usage = "If u is a constant (i.e. an expression free of variables), ConstantQ[u] returns True; else it returns False.";
ConstantQ[u_] :=
  If[AtomQ[u],
    NumberQ[u] || MemberQ[{Pi,E,I,GoldenRatio,EulerGamma,Catalan},u],
  If[Head[u]===DirectedInfinity,
    False,
  Scan[Function[If[ConstantQ[#],Null,Return[False]]],u]===Null]]


(* ::Section::Closed:: *)
(*Expression type predicates*)


(* ::Subsection::Closed:: *)
(*PowerQ[u]*)


PowerQ::usage = "If u is a power, PowerQ[u] returns True; else it returns False.";
PowerQ[_Power] = True;
PowerQ[_] = False;


(* ::Subsection::Closed:: *)
(*ProductQ[u]*)


ProductQ::usage = "If u is a product, ProductQ[u] returns True; else it returns False.";
ProductQ[_Times] = True;
ProductQ[_] = False;


(* ::Subsection::Closed:: *)
(*SumQ[u]*)


SumQ::usage = "If u is a sum, SumQ[u] returns True; else it returns False.";
SumQ[_Plus] = True;
SumQ[_] = False;


(* ::Subsection::Closed:: *)
(*NonsumQ[u]*)


NonsumQ::usage = "If u is not a sum, NonsumQ[u] returns True; else it returns False.";
NonsumQ[_Plus] = False;
NonsumQ[_] = True;


(* ::Subsection::Closed:: *)
(*IntegerPowerQ[u]*)


IntegerPowerQ::usage = "If u is an integer power, IntegerPowerQ[u] returns True; else it returns False.";
IntegerPowerQ = Function[Head[#]===Power && IntegerQ[#[[2]]]];


(* ::Subsection::Closed:: *)
(*FractionalPowerQ[u]*)


FractionalPowerQ::usage = "If u is an fractional power, FractionalPowerQ[u] returns True; else it returns False.";
FractionalPowerQ = Function[Head[#]===Power && Head[#[[2]]]===Rational];


(* ::Subsection::Closed:: *)
(*FractionalPowerFreeQ[u]*)


FractionalPowerFreeQ::usage = "If u is free of fractional powers whose bases are not atoms, FractionalPowerFreeQ[u] returns True; else it returns False.";
FractionalPowerFreeQ[u_] :=
  If[AtomQ[u],
    True,
  If[FractionalPowerQ[u] && Not[AtomQ[u[[1]]]],
    False,
  Catch[Scan[Function[If[FractionalPowerFreeQ[#],Null,Throw[False]]],u];True]]]


(* ::Subsection::Closed:: *)
(*ComplexFreeQ[u]*)


ComplexFreeQ::usage = "If u is free of complex numbers, ComplexFreeQ[u] returns True; else it returns False.";
ComplexFreeQ[u_] :=
  If[AtomQ[u],
    Not[ComplexNumberQ[u]],
  Scan[Function[If[ComplexFreeQ[#],Null,Return[False]]],u]===Null]


(* ::Subsection::Closed:: *)
(*LogQ[u]*)


LogQ::usage = "If u is an expression of the form Log[v], LogQ[u] returns True; else it returns False.";
LogQ[u_] := Head[u]===Log


(* ::Subsection::Closed:: *)
(*TrigQ[u]*)


TrigQ::usage = "If u is an expression of the form F[v] where F is a circular trig function, TrigQ[u] returns True; else it returns False.";
$TrigFunctions = {Sin, Cos, Tan, Cot, Sec, Csc};
TrigQ[u_] := MemberQ[$TrigFunctions, If[AtomQ[u],u,Head[u]]]


(* ::Subsection::Closed:: *)
(*HyperbolicQ[u]*)


HyperbolicQ::usage = "If u is an expression of the form F[v] where F is a hyperbolic trig function, HyperbolicQ[u] returns True; else it returns False.";
$HyperbolicFunctions = {Sinh, Cosh, Tanh, Coth, Sech, Csch};
HyperbolicQ[u_] := MemberQ[$HyperbolicFunctions, If[AtomQ[u],u,Head[u]]]


(* ::Subsection::Closed:: *)
(*InverseTrigQ[u]*)


InverseTrigQ::usage = "If u is an expression of the form F[v] where F is an inverse circular trig function, InverseTrigQ[u] returns True; else it returns False.";
$InverseTrigFunctions = {ArcSin, ArcCos, ArcTan, ArcCot, ArcSec, ArcCsc};
InverseTrigQ[u_] := MemberQ[$InverseTrigFunctions, If[AtomQ[u],u,Head[u]]]


(* ::Subsection::Closed:: *)
(*InverseHyperbolicQ[u]*)


InverseHyperbolicQ::usage = "If u is an expression of the form F[v] where F is an inverse hyperbolic trig function, InverseHyperbolicQ[u] returns True; else it returns False.";
$InverseHyperbolicFunctions = {ArcSinh, ArcCosh, ArcTanh, ArcCoth, ArcSech, ArcCsch};
InverseHyperbolicQ[u_] := MemberQ[$InverseHyperbolicFunctions, If[AtomQ[u],u,Head[u]]]


(* ::Subsection::Closed:: *)
(*CalculusQ[u]*)


CalculusQ::usage = "If u involves a calculus function (e.g. D, Integrate or Sum), CalculusQ[u] returns True; else it returns False.";
$CalculusFunctions = {D, Integrate, Sum, Product, Int, Unintegrable, CannotIntegrate, Dif, Subst};
CalculusQ[u_] := MemberQ[$CalculusFunctions, If[AtomQ[u],u,Head[u]]]


HeldFormQ::usage = "If u is a held form (e.g. Hold, Defer or Pattern), HeldFormQ[u] returns True; else it returns False.";
$HeldFunctions = {Hold, HoldForm, Defer, Pattern};
HeldFormQ[u_] :=
  If[AtomQ[Head[u]],
    MemberQ[$HeldFunctions,Head[u]],
  HeldFormQ[Head[u]]]


StopFunctionQ::usage = "If u is a held form or unevaluated integral (e.g. Unintegrable), StopFunctionQ[u] returns True; else it returns False.";
$StopFunctions = {Hold, HoldForm, Defer, Pattern, If, Int, Unintegrable, CannotIntegrate};
StopFunctionQ[u_] :=
  If[AtomQ[Head[u]],
    MemberQ[$StopFunctions,Head[u]],
  StopFunctionQ[Head[u]]]


InverseFunctionQ::usage = "If u is a call on an inverse function, InverseFunctionQ[u] returns True; else it returns False.";
InverseFunctionQ[u_] :=
  LogQ[u] || InverseTrigQ[u] && Length[u]<=1 || InverseHyperbolicQ[u] || Head[u]===Mods  || Head[u]===PolyLog


TrigHyperbolicFreeQ::usage = "If u is free of trig, hyperbolic and calculus functions involving x, TrigHyperbolicFreeQ[u,x] returns true; else it returns False.";
TrigHyperbolicFreeQ[u_,x_Symbol] :=
  If[AtomQ[u],
    True,
  If[TrigQ[u] || HyperbolicQ[u] || CalculusQ[u],
    FreeQ[u,x],
  Catch[Scan[Function[If[TrigHyperbolicFreeQ[#,x],Null,Throw[False]]],u];True]]]


InverseFunctionFreeQ::usage = "If u is free of inverse, calculus and hypergeometric functions involving x, InverseFunctionFreeQ[u,x] returns true; else it returns False.";
InverseFunctionFreeQ[u_,x_Symbol] :=
  If[AtomQ[u],
    True,
  If[InverseFunctionQ[u] || CalculusQ[u] || Head[u]===Hypergeometric2F1 || Head[u]===AppellF1,
    FreeQ[u,x],
  Catch[Scan[Function[If[InverseFunctionFreeQ[#,x],Null,Throw[False]]],u];True]]]


(* ElementaryExpressionQ::usage = "ElementaryExpressionQ[u] returns True if u is a sum, product, or power and all the operands are elementary expressions; or if u is a call on a trig, hyperbolic, or inverse function and all the arguments are elementary expressions; else it returns False."; *)
(* ElementaryFunctionQ[u_] :=
  If[AtomQ[u],
    True,
  If[SumQ[u] || ProductQ[u] || PowerQ[u] || TrigQ[u] || HyperbolicQ[u] || InverseFunctionQ[u],
    Catch[Scan[Function[If[ElementaryFunctionQ[#],Null,Throw[False]]],u];True],
  False]] *)


(* ::Subsection::Closed:: *)
(*IndependentQ[u,x]*)


IndependentQ::usage = "If u is independent of x, IndependentQ[u,x] returns True; else it returns False.";
IndependentQ[u_,x_] :=
  FreeQ[Together[u],x]


(* ::Subsection::Closed:: *)
(*CalculusFreeQ[u,x]*)


CalculusFreeQ::usage = "If u is free of calculus functions whose variable is x, CalculusFreeQ[u,x] returns True; else it returns False.";
CalculusFreeQ[u_,x_] :=
  If[AtomQ[u],
    True,
  If[CalculusQ[u] && u[[2]]===x || HeldFormQ[u],
    False,
  Catch[Scan[Function[If[CalculusFreeQ[#,x],Null,Throw[False]]],u]; True]]]


(* ::Subsection::Closed:: *)
(*IntegralFreeQ[u]*)


IntegralFreeQ::usage = "If u is free of integrals, IntegralFreeQ[u] returns True; else it returns False.";
IntegralFreeQ[u_] :=
  FreeQ[u,Int] && FreeQ[u,Integral] && FreeQ[u,Unintegrable] && FreeQ[u,CannotIntegrate]


(* ::Section::Closed:: *)
(*Equality and inequality predicates*)


(* ::Subsection::Closed:: *)
(*Equality predicates*)


EqQ::usage = "If u-v equals 0, EqQ[u,v] returns True; else it returns False.";
EqQ[u_,v_] := Quiet[PossibleZeroQ[u-v]] || TrueQ[Refine[u==v]]


NeQ::usage = "If u-v equals 0, NeQ[u,v] returns False; else it returns True.";
NeQ[u_,v_] := Not[Quiet[PossibleZeroQ[u-v]] || TrueQ[Refine[u==v]]]


(* ::Subsection::Closed:: *)
(*Integer inequality predicates*)


IGtQ::usage = "n must be a rational number.  If u is an integer and u>n, IGtQ[u,n] returns True; else it returns False.";
IGtQ[u_,n_] := IntegerQ[u] && u>n


ILtQ::usage = "n must be a rational number.  If u is an integer and u<n, ILtQ[u,n] returns True; else it returns False.";
ILtQ[u_,n_] := IntegerQ[u] && u<n


IGeQ::usage = "n must be a rational number.  If u is an integer and u>=n, IGeQ[u,n] returns True; else it returns False.";
IGeQ[u_,n_] := IntegerQ[u] && u>=n


ILeQ::usage = "n must be a rational number.  If u is an integer and u<=n, ILeQ[u,n] returns True; else it returns False.";
ILeQ[u_,n_] := IntegerQ[u] && u<=n


(* ::Subsection::Closed:: *)
(*Numeric inequality predicates*)


GtQ::usage = "If u>v, GtQ[u,v] returns True; else it returns False.  If u>v and v>w, GtQ[u,v,w] returns True; else it returns False.";
GtQ[u_,v_] :=
  If[RealNumberQ[u],
    If[RealNumberQ[v],
      u>v,
    With[{vn=N[Together[v]]},
    Head[vn]===Real && u>vn]],
  With[{un=N[Together[u]]},
  If[Head[un]===Real,
    If[RealNumberQ[v],
      un>v,
    With[{vn=N[Together[v]]},
    Head[vn]===Real && un>vn]],
  False]]]

GtQ[u_,v_,w_] := GtQ[u,v] && GtQ[v,w]


LtQ::usage = "If u>v, LtQ[u,v] returns True; else it returns False.  If u<v and v<w, LtQ[u,v,w] returns True; else it returns False.";
LtQ[u_,v_] :=
  If[RealNumberQ[u],
    If[RealNumberQ[v],
      u<v,
    With[{vn=N[Together[v]]},
    Head[vn]===Real && u<vn]],
  With[{un=N[Together[u]]},
  If[Head[un]===Real,
    If[RealNumberQ[v],
      un<v,
    With[{vn=N[Together[v]]},
    Head[vn]===Real && un<vn]],
  False]]]

LtQ[u_,v_,w_] := LtQ[u,v] && LtQ[v,w]


GeQ::usage = "If u>v, GeQ[u,v] returns True; else it returns False.  If u>=v and v>=w, GeQ[u,v,w] returns True; else it returns False.";
GeQ[u_,v_] :=
  If[RealNumberQ[u],
    If[RealNumberQ[v],
      u>=v,
    With[{vn=N[Together[v]]},
    Head[vn]===Real && u>=vn]],
  With[{un=N[Together[u]]},
  If[Head[un]===Real,
    If[RealNumberQ[v],
      un>=v,
    With[{vn=N[Together[v]]},
    Head[vn]===Real && un>=vn]],
  False]]]

GeQ[u_,v_,w_] := GeQ[u,v] && GeQ[v,w]


LeQ::usage = "If u>v, LeQ[u,v] returns True; else it returns False.  If u<=v and v<=w, LeQ[u,v,w] returns True; else it returns False.";
LeQ[u_,v_] :=
  If[RealNumberQ[u],
    If[RealNumberQ[v],
      u<=v,
    With[{vn=N[Together[v]]},
    Head[vn]===Real && u<=vn]],
  With[{un=N[Together[u]]},
  If[Head[un]===Real,
    If[RealNumberQ[v],
      un<=v,
    With[{vn=N[Together[v]]},
    Head[vn]===Real && un<=vn]],
  False]]]

LeQ[u_,v_,w_] := LeQ[u,v] && LeQ[v,w]


Unprotect[RealNumberQ];
RealNumberQ::usage = "If u is an explicit non-complex number, RealNumberQ[u] returns True; else it returns False.";
RealNumberQ[u_] := NumberQ[u] && Head[u]=!=Complex


(* ::Section::Closed:: *)
(*Multinomial predicates*)


(* ::Subsection::Closed:: *)
(*PolyQ[F[x],x,n]*)


(* Mathematica's built-in PolynomialQ, Exponent and Coefficient functions can return erroneous results because they do not *)
(* cancel the gcd in pseudo-rational functions that are actually polynomial. *)
(* PolynomialQ[F[x],x] returns false, but PolynomialQ[F[x],x^2] returns True! *)
(* For some unfully expanded polynomials, the built-in Mathematica function Exponent sometimes returns erronously large degrees. *)
(* For example, Exponent[3*(1+a)*x^4 + 3*x^5 + x^6 - (a+x+x^2)^3, x] incorrectly returns 4 instead of 3. *)
(* Despite what the online help says, PolynomialQ[u,x^v] returns an error message if v is a sum. *)


PolyQ::usage =
"If F[x] is a polynomial in x, PolyQ[F[x],x] returns True; else it returns False.
If F[x] is a polynomial in x of degree n, PolyQ[F[x],x,n] returns True; else it returns False.
If C is free of x and F[x] is a polynomial in x^C, PolyQ[F[x],x^C] returns True; else it returns False.
If C is free of x and F[x] is a polynomial in x^C of degree n, PolyQ[F[x],x^C,n] returns True; else it returns False.";


PolyQ[Fx_,x_Symbol] :=
  PolynomialQ[Fx,x] || PolynomialQ[Together[Fx],x]


PolyQ[Fx_,x_Symbol,n_] :=
  If[PolynomialQ[Fx,x],
    EqQ[Exponent[Fx,x],n] && NeQ[Coefficient[Fx,x,n],0],
  With[{Gx=Together[Fx]},
  PolynomialQ[Gx,x] && EqQ[Exponent[Gx,x],n] && NeQ[Coefficient[Gx,x,n],0]]]


PolyQ[Fx_,x_Symbol^n_Integer] :=
  If[n<=0,
    False,
  If[PolynomialQ[Fx,x],
    PolynomialQ[Fx,x^n],
  With[{Gx=Together[Fx]}, PolynomialQ[Gx,x] && PolynomialQ[Gx,x^n]]]]


(*PolyQ[Fx_,x_Symbol^(n_*C_Symbol)] :=
  PolyQ[ReplaceAll[Fx,C->C/n],x^C] /;
RationalQ[n] && NeQ[C,x]*)

(*PolyQ[Fx_,x_Symbol^C_] :=
  If[TrueQ[Quiet[PolynomialQ[Fx,x^C]]],
    FreeQ[CoefficientList[Fx,x^C],x],
  With[{Gx=Together[Fx]}, TrueQ[Quiet[PolynomialQ[Gx,x^C]]] && FreeQ[CoefficientList[Gx,x^C],x]]] /;
NonsumQ[C] && FreeQ[C,x]*)


PolyQ[Fx_,x_Symbol^C_] :=
  If[AtomQ[Fx],
    Fx=!=x,
  If[PowerQ[Fx],
    If[Fx[[1]]===x,
      IGtQ[Simplify[Fx[[2]]/C],0],
    IGtQ[Fx[[2]],0] && PolyQ[Fx[[1]],x^C]],
  If[ProductQ[Fx] || SumQ[Fx],
    PolyQ[First[Fx],x^C] && PolyQ[Rest[Fx],x^C],
  False]]]


PolyQ[Fx_,x_Symbol^C_,n_] :=
  PolyQ[Fx,x^C] && EqQ[Expon[Fx,x^C],n] && NeQ[Coeff[Fx,x^C,n],0]


(* ::Subsection::Closed:: *)
(*ProperPolyQ[u,x]*)


ProperPolyQ::usage = "If u is a polynomial in x and the constant term is nonzero, ProperPolyQ[u,x] returns True; else it returns False.";
ProperPolyQ[u_,x_Symbol] :=
  PolyQ[u,x] && NeQ[Coeff[u,x,0],0]


(* ::Subsection::Closed:: *)
(*BinomialQ[u,x,n]*)


BinomialQ::usage =
"If u is equivalent to an expression of the form a+b*x^n where n and b are not 0, BinomialQ[u,x] returns True; else it returns False.
If u is equivalent to an expression of the form a+b*x^n, BinomialQ[u,x,n] returns True; else it returns False.";
BinomialQ[u_,x_Symbol] :=
  If[ListQ[u],
    Catch[Scan[Function[If[Not[BinomialQ[#,x]],Throw[False]]],u]; True],
  ListQ[BinomialParts[u,x]]]

BinomialQ[u_,x_Symbol,n_] :=
  If[ListQ[u],
    Catch[Scan[Function[If[Not[BinomialQ[#,x,n]],Throw[False]]],u]; True],
  Function[ListQ[#] && #[[3]]===n][BinomialParts[u,x]]]


(* ::Subsection::Closed:: *)
(*TrinomialQ[u,x]*)


TrinomialQ::usage = "If u is equivalent to an expression of the form a+b*x^n+c*x^(2*n) where n, b and c are not 0, TrinomialQ[u,x] returns True; else it returns False.";
TrinomialQ[u_,x_Symbol] :=
  If[ListQ[u],
    Catch[Scan[Function[If[Not[TrinomialQ[#,x]],Throw[False]]],u]; True],
  ListQ[TrinomialParts[u,x]] && Not[QuadraticQ[u,x]] && Not[MatchQ[u,w_^2 /; BinomialQ[w,x]]]]


(* ::Subsection::Closed:: *)
(*GeneralizedBinomialQ[u,x]*)


GeneralizedBinomialQ::usage = "If u is equivalent to an expression of the form a*x^q+b*x^n where n, q and b are not 0, GeneralizedBinomialQ[u,x] returns True; else it returns False.";
GeneralizedBinomialQ[u_,x_Symbol] :=
  If[ListQ[u],
    Catch[Scan[Function[If[Not[GeneralizedBinomialQ[#,x]],Throw[False]]],u]; True],
  ListQ[GeneralizedBinomialParts[u,x]]]


(* ::Subsection::Closed:: *)
(*GeneralizedTrinomialQ[u,x]*)


GeneralizedTrinomialQ::usage = "If u is equivalent to an expression of the form a*x^q+b*x^n+c*x^(2*n-q) where n, q, b and c are not 0, GeneralizedTrinomialQ[u,x] returns True; else it returns False.";
GeneralizedTrinomialQ[u_,x_Symbol] :=
  If[ListQ[u],
    Catch[Scan[Function[If[Not[GeneralizedTrinomialQ[#,x]],Throw[False]]],u]; True],
  ListQ[GeneralizedTrinomialParts[u,x]]]


(* ::Section::Closed:: *)
(*Expression form predicates*)


(* ::Subsection::Closed:: *)
(*PosQ[u]*)


PosQ::usage = "If u is not 0 and has a positive form, PosQ[u] returns True, else it returns False.";
PosQ[u_] :=
  PosAux[TogetherSimplify[u]]

PosAux[u_] :=
  If[NumberQ[u],
    If[Head[u]===Complex,
      If[EqQ[Re[u],0],
        PosAux[Im[u]],
      PosAux[Re[u]]],
    u>0],
  If[NumericQ[u],
    With[{v=Simplify[Re[u]]},
      If[NumberQ[v],
        If[EqQ[v,0],
          PosAux[Simplify[Im[u]]],
        v>0],
      With[{w=N[u]}, NumberQ[w] && PosAux[w]]]],
  With[{v=Refine[u>0]}, If[v===True || v===False,
    v,
  If[PowerQ[u],
    If[IntegerQ[u[[2]]],
      EvenQ[u[[2]]] || PosAux[u[[1]]],
    True],
  If[ProductQ[u],
    If[PosAux[First[u]],
      PosAux[Rest[u]],
    Not[PosAux[Rest[u]]]],
  If[SumQ[u],
    PosAux[First[u]],
  True]]]]]]]


(* ::Subsection::Closed:: *)
(*NegQ[u]*)


NegQ::usage = "If u is not 0 and has a negative form, NegQ[u] returns True, else it returns False.";
NegQ[u_] :=
  Not[PosQ[u]] && NeQ[u,0]


(* ::Subsection::Closed:: *)
(*NiceSqrtQ[u]*)


NiceSqrtQ::usage = "If u has a nice squareroot (e.g. a positive number or none of the degrees of the factors of the squareroot of u are fractions), NiceSqrtQ[u] returns True, else it returns False.";
NiceSqrtQ[u_] :=
  If[RationalQ[u],
    u>0,
  Not[FractionalPowerFactorQ[Rt[u,2]]]]


(* ::Subsection::Closed:: *)
(*FractionalPowerFactorQ[u]*)


FractionalPowerFactorQ::usage = "If a factor of u is a complex constant or a fractional power, FractionalPowerFactorQ[u] returns True; else it returns False.";
FractionalPowerFactorQ[u_] :=
  If[AtomQ[u],
    Head[u]===Complex,
  If[PowerQ[u],
    FractionQ[u[[2]]],
  If[ProductQ[u],
    FractionalPowerFactorQ[First[u]] || FractionalPowerFactorQ[Rest[u]],
  False]]]


(* ::Section::Closed:: *)
(*Simpler functions*)


(* ::Subsection::Closed:: *)
(*SimplerQ[u,v]*)


SimplerQ::usage = "If u is simpler than v, SimplerQ[u,v] returns True, else it returns False.  SimplerQ[u,u] returns False.";
SimplerQ[u_,v_] :=
  If[IntegerQ[u],
    If[IntegerQ[v],
      If[u==v,
        False,
      If[u==-v,
        v<0,
      Abs[u]<Abs[v]]],
    True],
  If[IntegerQ[v],
    False,
  If[FractionQ[u],
    If[FractionQ[v],
      If[Denominator[u]==Denominator[v],
        SimplerQ[Numerator[u],Numerator[v]],
      Denominator[u]<Denominator[v]],
    True],
  If[FractionQ[v],
    False,
  If[(Re[u]===0 || Re[u]===0.0) && (Re[v]===0 || Re[v]===0.0),
    SimplerQ[Im[u],Im[v]],
  If[ComplexNumberQ[u],
    If[ComplexNumberQ[v],
      If[Re[u]==Re[v],
        SimplerQ[Im[u],Im[v]],
      SimplerQ[Re[u],Re[v]]],
    False],
  If[NumberQ[u],
    If[NumberQ[v],
      OrderedQ[{u,v}],
    True],
  If[NumberQ[v],
    False,
  If[AtomQ[u],
    If[AtomQ[v],
      OrderedQ[{u,v}],
    True],
  If[AtomQ[v],
    False,
  If[Head[u]===Head[v],
    If[Length[u]==Length[v],
      If[LeafCount[u]==LeafCount[v],
        Catch[Do[If[u[[ii]]===v[[ii]],Null,Throw[SimplerQ[u[[ii]],v[[ii]]]]],{ii,Length[u]}]; False],
      LeafCount[u]<LeafCount[v]],
    Length[u]<Length[v]],
  If[LeafCount[u]==LeafCount[v],
    Not[OrderedQ[{v,u}]],
  LeafCount[u]<LeafCount[v]]]]]]]]]]]]]


(* ::Subsection::Closed:: *)
(*SimplerSqrtQ[u,v]*)


SimplerSqrtQ::usage = "If Rt[u,2] is simpler than Rt[v,2], SimplerSqrtQ[u,v] returns True, else it returns False.  SimplerSqrtQ[u,u] returns False.";
SimplerSqrtQ[u_,v_] :=
  If[LtQ[v,0] && Not[LtQ[u,0]],
    True,
  If[LtQ[u,0] && Not[LtQ[v,0]],
    False,
  With[{sqrtu=Rt[u,2],sqrtv=Rt[v,2]},
  If[IntegerQ[sqrtu],
    If[IntegerQ[sqrtv],
      sqrtu<sqrtv,
    True],
  If[IntegerQ[sqrtv],
    False,
  If[RationalQ[sqrtu],
    If[RationalQ[sqrtv],
      sqrtu<sqrtv,
    True],
  If[RationalQ[sqrtv],
    False,
  If[PosQ[u],
    If[PosQ[v],
      LeafCount[sqrtu]<LeafCount[sqrtv],
    True],
  If[PosQ[v],
    False,
  If[LeafCount[sqrtu]<LeafCount[sqrtv],
    True,
  If[LeafCount[sqrtv]<LeafCount[sqrtu],
    False,
  Not[OrderedQ[{v,u}]]]]]]]]]]]]]


(* ::Subsection::Closed:: *)
(*SumSimplerQ[u,v]*)


SumSimplerQ::usage =
"If u+v is simpler than u, SumSimplerQ[u,v] returns True, else it returns False.
If for every term w of v there is a term of u equal to n*w where n<-1/2, u+v will be simpler than u.";
SumSimplerQ[u_,v_] :=
  If[RationalQ[u,v],
    If[v==0,
      False,
    If[v>0,
      u<-1,
    u>=-v]],
  SumSimplerAuxQ[Expand[u],Expand[v]]]


SumSimplerAuxQ[u_,v_] :=
  (RationalQ[First[v]] || SumSimplerAuxQ[u,First[v]]) &&
  (RationalQ[Rest[v]] || SumSimplerAuxQ[u,Rest[v]]) /;
SumQ[v]

SumSimplerAuxQ[u_,v_] :=
  SumSimplerAuxQ[First[u],v] || SumSimplerAuxQ[Rest[u],v] /;
SumQ[u]

SumSimplerAuxQ[u_,v_] :=
  v=!=0 &&
  NonnumericFactors[u]===NonnumericFactors[v] &&
  (NumericFactor[u]/NumericFactor[v]<-1/2 || NumericFactor[u]/NumericFactor[v]==-1/2 && NumericFactor[u]<0)


(* ::Subsection::Closed:: *)
(*SimplerIntegrandQ[u,v,x]*)


SimplerIntegrandQ::usage = "If u is simpler to integrate wrt x than v, SimplerIntegrandQ[u,v,x] returns True; else it returns False.";
SimplerIntegrandQ[u_,v_,x_Symbol] :=
  Module[{lst=CancelCommonFactors[u,v],u1,v1},
  u1=lst[[1]];
  v1=lst[[2]];
(*If[Head[u1]===Head[v1] && Length[u1]==Length[v1]==1,
    SimplerIntegrandQ[u1[[1]],v1[[1]],x], *)
  If[LeafCount[u1]<6/10*LeafCount[v1],
(*If[LeafCount[u1]<3/4*LeafCount[v1], *)
    True,
  If[RationalFunctionQ[u1,x],
    If[RationalFunctionQ[v1,x],
      Apply[Plus,RationalFunctionExponents[u1,x]]<Apply[Plus,RationalFunctionExponents[v1,x]],
    True],
  False]]]


CancelCommonFactors::usage = "CancelCommonFactors[u,v] returns the list {u',v'} where u' and v' are the noncommon factors of u and v respectively.";
CancelCommonFactors[u_,v_] :=
  If[ProductQ[u],
    If[ProductQ[v],
      If[MemberQ[v,First[u]],
        CancelCommonFactors[Rest[u],DeleteCases[v,First[u],1,1]],
      Function[{First[u]*#[[1]],#[[2]]}][CancelCommonFactors[Rest[u],v]]],
    If[MemberQ[u,v],
      {DeleteCases[u,v,1,1],1},
    {u,v}]],
  If[ProductQ[v],
    If[MemberQ[v,u],
      {1,DeleteCases[v,u,1,1]},
    {u,v}],
  {u,v}]]


(* ::Section::Closed:: *)
(*Parts functions*)


(* ::Subsection::Closed:: *)
(*BinomialParts[u,x]*)


BinomialDegree::usage = "u is a binomial. BinomialDegree[u,x] returns the degree of x in u.";
BinomialDegree[u_,x_Symbol] :=
  BinomialParts[u,x][[3]]


BinomialDegree::usage = "If u[x] is equivalent to an expression of the form a+b*x^n where n!=0 and b!=0, BinomialParts[u,x] returns the list {a,b,n}; else it returns False.";
BinomialParts[u_,x_Symbol] :=
  If[PolynomialQ[u,x],
    If[Exponent[u,x]>0,
      With[{lst=Exponent[u,x,List]},
        If[Length[lst]==1,
          {0, Coefficient[u,x,Exponent[u,x]], Exponent[u,x]},
        If[Length[lst]==2 && lst[[1]]==0,
          {Coefficient[u,x,0], Coefficient[u,x,Exponent[u,x]], Exponent[u,x]},
        False]]],
    False],
  If[PowerQ[u],
    If[u[[1]]===x && FreeQ[u[[2]],x],
      {0,1,u[[2]]},
    False],
  If[ProductQ[u],
    If[FreeQ[First[u],x],
      With[{lst2=BinomialParts[Rest[u],x]},
      If[AtomQ[lst2],
        False,
      {First[u]*lst2[[1]],First[u]*lst2[[2]],lst2[[3]]}]],
    If[FreeQ[Rest[u],x],
      With[{lst1=BinomialParts[First[u],x]},
      If[AtomQ[lst1],
        False,
      {Rest[u]*lst1[[1]],Rest[u]*lst1[[2]],lst1[[3]]}]],
    With[{lst1=BinomialParts[First[u],x]},
    If[AtomQ[lst1],
      False,
    With[{lst2=BinomialParts[Rest[u],x]},
    If[AtomQ[lst2],
      False,
    With[{a=lst1[[1]],b=lst1[[2]],m=lst1[[3]], c=lst2[[1]],d=lst2[[2]],n=lst2[[3]]},
    If[EqQ[a,0],
      If[EqQ[c,0],
        {0,b*d,m+n},
      If[EqQ[m+n,0],
        {b*d,b*c,m},
      False]],
    If[EqQ[c,0],
      If[EqQ[m+n,0],
        {b*d,a*d,n},
      False],
    If[EqQ[m,n] && EqQ[a*d+b*c,0],
      {a*c,b*d,2*m},
    False]]]]]]]]]],
  If[SumQ[u],
    If[FreeQ[First[u],x],
     With[{lst2=BinomialParts[Rest[u],x]},
      If[AtomQ[lst2],
        False,
      {First[u]+lst2[[1]],lst2[[2]],lst2[[3]]}]],
    If[FreeQ[Rest[u],x],
      With[{lst1=BinomialParts[First[u],x]},
      If[AtomQ[lst1],
        False,
      {Rest[u]+lst1[[1]],lst1[[2]],lst1[[3]]}]],
    With[{lst1=BinomialParts[First[u],x]},
    If[AtomQ[lst1],
      False,
    With[{lst2=BinomialParts[Rest[u],x]},
    If[AtomQ[lst2],
      False,
    If[EqQ[lst1[[3]],lst2[[3]]],
      {lst1[[1]]+lst2[[1]],lst1[[2]]+lst2[[2]],lst1[[3]]},
    False]]]]]]],
  False]]]]


(* ::Subsection::Closed:: *)
(*TrinomialParts[u,x]*)


TrinomialDegree::usage = "If u is equivalent to a trinomial of the form a + b*x^n + c*x^(2*n) where n!=0, b!=0 and c!=0, TrinomialDegree[u,x] returns n.";
TrinomialDegree[u_,x_Symbol] :=
  TrinomialParts[u,x][[4]]


TrinomialParts::usage = "If u is equivalent to a trinomial of the form a + b*x^n + c*x^(2*n) where n!=0, b!=0 and c!=0, TrinomialParts[u,x] returns the list {a,b,c,n}; else it returns False.";
TrinomialParts[u_,x_Symbol] :=
  If[PolynomialQ[u,x],
    With[{lst=CoefficientList[u,x]},
    If[Length[lst]<3 || EvenQ[Length[lst]] || EqQ[lst[[(Length[lst]+1)/2]],0],
      False,
    Catch[
      Scan[Function[If[EqQ[#,0],Null,Throw[False]]],Drop[Drop[Drop[lst,{(Length[lst]+1)/2}],1],-1]];
      {First[lst],lst[[(Length[lst]+1)/2]],Last[lst],(Length[lst]-1)/2}]]],
  If[PowerQ[u],
    If[EqQ[u[[2]],2],
      With[{lst=BinomialParts[u[[1]],x]},
      If[AtomQ[lst] || EqQ[lst[[1]],0],
        False,
      {lst[[1]]^2,2*lst[[1]]*lst[[2]],lst[[2]]^2,lst[[3]]}]],
    False],
  If[ProductQ[u],
    If[FreeQ[First[u],x],
      With[{lst2=TrinomialParts[Rest[u],x]},
      If[AtomQ[lst2],
        False,
      {First[u]*lst2[[1]],First[u]*lst2[[2]],First[u]*lst2[[3]],lst2[[4]]}]],
    If[FreeQ[Rest[u],x],
      With[{lst1=TrinomialParts[First[u],x]},
      If[AtomQ[lst1],
        False,
      {Rest[u]*lst1[[1]],Rest[u]*lst1[[2]],Rest[u]*lst1[[3]],lst1[[4]]}]],
    With[{lst1=BinomialParts[First[u],x]},
    If[AtomQ[lst1],
      False,
    With[{lst2=BinomialParts[Rest[u],x]},
    If[AtomQ[lst2],
      False,
    With[{a=lst1[[1]],b=lst1[[2]],m=lst1[[3]], c=lst2[[1]],d=lst2[[2]],n=lst2[[3]]},
    If[EqQ[m,n] && NeQ[a*d+b*c,0],
      {a*c,a*d+b*c,b*d,m},
    False]]]]]]]],
  If[SumQ[u],
    If[FreeQ[First[u],x],
      With[{lst2=TrinomialParts[Rest[u],x]},
      If[AtomQ[lst2],
        False,
      {First[u]+lst2[[1]],lst2[[2]],lst2[[3]],lst2[[4]]}]],
    If[FreeQ[Rest[u],x],
      With[{lst1=TrinomialParts[First[u],x]},
      If[AtomQ[lst1],
        False,
      {Rest[u]+lst1[[1]],lst1[[2]],lst1[[3]],lst1[[4]]}]],
    With[{lst1=TrinomialParts[First[u],x]},
    If[AtomQ[lst1],
      With[{lst3=BinomialParts[First[u],x]},
      If[AtomQ[lst3],
        False,
      With[{lst2=TrinomialParts[Rest[u],x]},
      If[AtomQ[lst2],
        With[{lst4=BinomialParts[Rest[u],x]},
        If[AtomQ[lst4],
          False,
        If[EqQ[lst3[[3]],2*lst4[[3]]],
          {lst3[[1]]+lst4[[1]],lst4[[2]],lst3[[2]],lst4[[3]]},
        If[EqQ[lst4[[3]],2*lst3[[3]]],
          {lst3[[1]]+lst4[[1]],lst3[[2]],lst4[[2]],lst3[[3]]},
        False]]]],
      If[EqQ[lst3[[3]],lst2[[4]]] && NeQ[lst3[[2]]+lst2[[2]],0],
        {lst3[[1]]+lst2[[1]],lst3[[2]]+lst2[[2]],lst2[[3]],lst2[[4]]},
      If[EqQ[lst3[[3]],2*lst2[[4]]] && NeQ[lst3[[2]]+lst2[[3]],0],
        {lst3[[1]]+lst2[[1]],lst2[[2]],lst3[[2]]+lst2[[3]],lst2[[4]]},
      False]]]]]],
    With[{lst2=TrinomialParts[Rest[u],x]},
    If[AtomQ[lst2],
      With[{lst4=BinomialParts[Rest[u],x]},
      If[AtomQ[lst4],
        False,
      If[EqQ[lst4[[3]],lst1[[4]]] && NeQ[lst1[[2]]+lst4[[2]],0],
        {lst1[[1]]+lst4[[1]],lst1[[2]]+lst4[[2]],lst1[[3]],lst1[[4]]},
      If[EqQ[lst4[[3]],2*lst1[[4]]] && NeQ[lst1[[3]]+lst4[[2]],0],
        {lst1[[1]]+lst4[[1]],lst1[[2]],lst1[[3]]+lst4[[2]],lst1[[4]]},
      False]]]],
    If[EqQ[lst1[[4]],lst2[[4]]] && NeQ[lst1[[2]]+lst2[[2]],0] && NeQ[lst1[[3]]+lst2[[3]],0],
      {lst1[[1]]+lst2[[1]],lst1[[2]]+lst2[[2]],lst1[[3]]+lst2[[3]],lst1[[4]]},
    False]]]]]]],
  False]]]]


(* ::Subsection::Closed:: *)
(*GeneralizedBinomialParts[u,x]*)


GeneralizedBinomialDegree::usage = "If u is equivalent to a generalized binomial of the form a*x^q + b*x^n where a, b, n, and q not equal 0, GeneralizedBinomialDegree[u,x] returns n-q.";
GeneralizedBinomialDegree[u_,x_Symbol] :=
  Function[#[[3]]-#[[4]]][GeneralizedBinomialParts[u,x]]


GeneralizedBinomialParts::usage = "If u is equivalent to a generalized binomial of the form a*x^q + b*x^n where a, b, n, and q not equal 0, GeneralizedBinomialParts[u,x] returns the list {a,b,n,q}; else it returns False.";
GeneralizedBinomialParts[a_.*x_^q_.+b_.*x_^n_.,x_Symbol] :=
  {a,b,n,q} /;
FreeQ[{a,b,n,q},x] && PosQ[n-q]

GeneralizedBinomialParts[a_*u_,x_Symbol] :=
  With[{lst=GeneralizedBinomialParts[u,x]},
  {a*lst[[1]], a*lst[[2]], lst[[3]], lst[[4]]} /;
 ListQ[lst]] /;
FreeQ[a,x]

GeneralizedBinomialParts[x_^m_.*u_,x_Symbol] :=
  With[{lst=GeneralizedBinomialParts[u,x]},
  {lst[[1]], lst[[2]], m+lst[[3]], m+lst[[4]]} /;
 ListQ[lst] && NeQ[m+lst[[3]],0] && NeQ[m+lst[[4]],0]] /;
FreeQ[m,x]

GeneralizedBinomialParts[x_^m_.*u_,x_Symbol] :=
  With[{lst=BinomialParts[u,x]},
  {lst[[1]], lst[[2]], m+lst[[3]], m} /;
 ListQ[lst] && NeQ[m+lst[[3]],0]] /;
FreeQ[m,x]

GeneralizedBinomialParts[u_,x_Symbol] :=
  False


(* ::Subsection::Closed:: *)
(*GeneralizedTrinomialParts[u,x]*)


GeneralizedTrinomialDegree::usage = "If u is equivalent to a generalized trinomial of the form a*x^q + b*x^n + c*x^(2*n-q) where n!=0, q!=0, b!=0 and c!=0, GeneralizedTrinomialDegree[u,x] returns n-q.";
GeneralizedTrinomialDegree[u_,x_Symbol] :=
  Function[#[[4]]-#[[5]]][GeneralizedTrinomialParts[u,x]]


GeneralizedTrinomialParts::usage = "If u is equivalent to a generalized trinomial of the form a*x^q + b*x^n + c*x^(2*n-q) where n!=0, q!=0, b!=0 and c!=0, GeneralizedTrinomialParts[u,x] returns the list {a,b,c,n,q}; else it returns False.";
GeneralizedTrinomialParts[a_.*x_^q_.+b_.*x_^n_.+c_.*x_^r_.,x_Symbol] :=
  {a,b,c,n,q} /;
FreeQ[{a,b,c,n,q},x] && EqQ[r,2*n-q]

GeneralizedTrinomialParts[a_*u_,x_Symbol] :=
  With[{lst=GeneralizedTrinomialParts[u,x]},
  {a*lst[[1]], a*lst[[2]], a*lst[[3]], lst[[4]], lst[[5]]} /;
 ListQ[lst]] /;
FreeQ[a,x]

GeneralizedTrinomialParts[u_,x_Symbol] :=
  With[{lst=Expon[u,x,List]},
  If[Length[lst]==3 && NeQ[lst[[1]],0] && EqQ[lst[[3]],2*lst[[2]]-lst[[1]]],
    {Coeff[u,x,lst[[1]]],Coeff[u,x,lst[[2]]],Coeff[u,x,lst[[3]]],lst[[2]],lst[[1]]},
  False]] /;
PolyQ[u,x]

GeneralizedTrinomialParts[x_^m_.*u_,x_Symbol] :=
  With[{lst=GeneralizedTrinomialParts[u,x]},
  {lst[[1]], lst[[2]], lst[[3]], m+lst[[4]], m+lst[[5]]} /;
 ListQ[lst] && NeQ[m+lst[[4]],0] && NeQ[m+lst[[5]],0]] /;
FreeQ[m,x]

GeneralizedTrinomialParts[x_^m_.*u_,x_Symbol] :=
  With[{lst=TrinomialParts[u,x]},
  {lst[[1]], lst[[2]], lst[[3]], m+lst[[4]], m} /;
 ListQ[lst] && NeQ[m+lst[[4]],0]] /;
FreeQ[m,x]

GeneralizedTrinomialParts[u_,x_Symbol] :=
  False


(* ::Section::Closed:: *)
(*Selection functions*)


(* ::Subsection::Closed:: *)
(*NumericFactor[u] *)


NumericFactor::usage = "NumericFactor[u] returns the real numeric factor of u.";
NumericFactor[u_] :=
  If[NumberQ[u],
    If[EqQ[Im[u],0],
      u,
    If[EqQ[Re[u],0],
      Im[u],
    1]],
  If[PowerQ[u],
    If[RationalQ[u[[1]]] && FractionQ[u[[2]]],
      If[u[[2]]>0,
        1/Denominator[u[[1]]],
      1/Denominator[1/u[[1]]]],
    1],
  If[ProductQ[u],
    Map[NumericFactor,u],
  If[SumQ[u],
    If[LeafCount[u]<50,             (* Eliminate this kludge! *)
      Function[If[SumQ[#], 1, NumericFactor[#]]][ContentFactor[u]],
    With[{m=NumericFactor[First[u]],n=NumericFactor[Rest[u]]},
    If[m<0 && n<0,
      -GCD[-m,-n],
    GCD[m,n]]]],
  1]]]]


NumericFactor::usage = "NonnumericFactors[u] returns the product of the factors of u that are not real numbers.";
NonnumericFactors[u_] :=
  If[NumberQ[u],
    If[EqQ[Im[u],0],
      1,
    If[EqQ[Re[u],0],
      I,
    u]],
  If[PowerQ[u],
    If[RationalQ[u[[1]]] && FractionQ[u[[2]]],
      u/NumericFactor[u],
    u],
  If[ProductQ[u],
    Map[NonnumericFactors,u],
  If[SumQ[u],
    If[LeafCount[u]<50,             (* Eliminate this kludge! *)
      Function[If[SumQ[#], u, NonnumericFactors[#]]][ContentFactor[u]],
    With[{n=NumericFactor[u]},
    Map[Function[#/n],u]]],
  u]]]]


(* ::Subsection::Closed:: *)
(*RemoveContent[u,x]*)


RemoveContent::usage = "RemoveContent[u,x] returns u with the content free of x removed.";
RemoveContent[u_,x_Symbol] :=
  With[{v=NonfreeFactors[u,x]},
  With[{w=Together[v]},
  If[EqQ[FreeFactors[w,x],1],
    RemoveContentAux[v,x],
  RemoveContentAux[NonfreeFactors[w,x],x]]]]


RemoveContentAux[a_^m_*u_.+b_*v_.,x_Symbol] :=
  If[m>1,
    RemoveContentAux[a^(m-1)*u-v,x],
  RemoveContentAux[u-a^(1-m)*v,x]] /;
IntegersQ[a,b] && a+b==0 && RationalQ[m]


RemoveContentAux[a_^m_.*u_.+a_^n_.*v_.,x_Symbol] :=
  RemoveContentAux[u+a^(n-m)*v,x] /;
FreeQ[a,x] && RationalQ[m,n] && n-m>=0


RemoveContentAux[a_^m_.*u_.+a_^n_.*v_.+a_^p_.*w_.,x_Symbol] :=
  RemoveContentAux[u+a^(n-m)*v+a^(p-m)*w,x] /;
FreeQ[a,x] && RationalQ[m,n,p] && n-m>=0 && p-m>=0


RemoveContentAux[u_,x_Symbol] :=
  If[SumQ[u] && NegQ[First[u]],
    -u,
  u]


(* ::Subsection::Closed:: *)
(*FreeFactors[u,x]*)


FreeFactors::usage = "FreeFactors[u,x] returns the product of the factors of u free of x.";
FreeFactors[u_,x_] :=
  If[ProductQ[u],
    Map[Function[If[FreeQ[#,x],#,1]],u],
  If[FreeQ[u,x],
    u,
  1]]


(* ::Subsection::Closed:: *)
(*NonfreeFactors[u,x]*)


NonfreeFactors::usage = "NonfreeFactors[u,x] returns the product of the factors of u not free of x.";
NonfreeFactors[u_,x_] :=
  If[ProductQ[u],
    Map[Function[If[FreeQ[#,x],1,#]],u],
  If[FreeQ[u,x],
    1,
  u]]


(* ::Subsection::Closed:: *)
(*FreeTerms[u,x]*)


FreeTerms::usage = "FreeTerms[u,x] returns the sum of the terms of u free of x.";
FreeTerms[u_,x_] :=
  If[SumQ[u],
    Map[Function[If[FreeQ[#,x],#,0]],u],
  If[FreeQ[u,x],
    u,
  0]]


(* ::Subsection::Closed:: *)
(*NonfreeTerms[u,x]*)


NonfreeTerms::usage = "NonfreeTerms[u,x] returns the sum of the terms of u not free of x.";
NonfreeTerms[u_,x_] :=
  If[SumQ[u],
    Map[Function[If[FreeQ[#,x],0,#]],u],
  If[FreeQ[u,x],
    0,
  u]]


(* ::Subsection::Closed:: *)
(*Polynomial functions*)


Expon[expr_,form_] :=
  Exponent[Together[expr],form]

Expon[expr_,form_,h_] :=
  Exponent[Together[expr],form,h]


Coeff[expr_,form_] :=
  Coefficient[Together[expr],form]

Coeff[expr_,form_,n_] :=
  With[{coef1=Coefficient[expr,form,n],coef2=Coefficient[Together[expr],form,n]},
  If[Simplify[coef1-coef2]===0,
    coef1,
  coef2]]


(* ::Subsection::Closed:: *)
(*LeadTerm[u]*)


LeadTerm[u_] :=
  If[SumQ[u],
    First[u],
  u]


(* ::Subsection::Closed:: *)
(*RemainingTerms[u]*)


RemainingTerms[u_] :=
  If[SumQ[u],
    Rest[u],
  0]


(* ::Subsection::Closed:: *)
(*LeadFactor[u]*)


LeadFactor::usage = "LeadFactor[u] returns the leading factor of u.";
LeadFactor[u_] :=
  If[ProductQ[u],
    LeadFactor[First[u]],
  If[ComplexNumberQ[u] && Re[u]===0,
    If[Im[u]===1,
      u,
    LeadFactor[Im[u]]],
  u]]


(* ::Subsection::Closed:: *)
(*RemainingFactors[u]*)


RemainingFactors::usage = "RemainingFactors[u] returns the remaining factors of u.";
RemainingFactors[u_] :=
  If[ProductQ[u],
    RemainingFactors[First[u]]*Rest[u],
  If[ComplexNumberQ[u] && Re[u]===0,
    If[Im[u]===1,
      1,
    I*RemainingFactors[Im[u]]],
  1]]


(* ::Subsection::Closed:: *)
(*LeadBase[u]*)


LeadBase::usage = "LeadBase[u] returns the base of the leading factor of u.";
LeadBase[u_] :=
  With[{v=LeadFactor[u]},
  If[PowerQ[v],
    v[[1]],
  v]]


(* ::Subsection::Closed:: *)
(*LeadDegree[u]*)


LeadDegree::usage = "LeadDegree[u] returns the degree of the leading factor of u.";
LeadDegree[u_] :=
  With[{v=LeadFactor[u]},
  If[PowerQ[v],
    v[[2]],
  1]]


(* ::Subsection::Closed:: *)
(*Numer[u]*)


Numer::usage = "Numer[u] returns the numerator of u.";
Numer[m_Integer^n_Rational] :=
  1 /;
n<0


Numer[u_*v_] :=
  Numer[u]*Numer[v]


Numer[u_] := Numerator[u]


(* ::Subsection::Closed:: *)
(*Denom[u]*)


Denom::usage = "Denom[u] returns the denominator of u.";
Denom[m_Integer^n_Rational] :=
  m^-n /;
n<0


Denom[u_*v_] :=
  Denom[u]*Denom[v]


Denom[u_] := Denominator[u]


(* ::Section::Closed:: *)
(*Multinomial functions*)


(* ::Subsection::Closed:: *)
(*Multinomial Recognizer functions*)


LinearQ::usage = "LinearQ[u,x] returns True iff u is a polynomial of degree 1.";
LinearQ[u_,x_Symbol] :=
  If[ListQ[u],
    Catch[Scan[Function[If[PolyQ[#,x,1],Null,Throw[False]]],u]; True],
  PolyQ[u,x,1]]


QuadraticQ::usage = "QuadraticQ[u,x] returns True iff u is a polynomial of degree 2 and not a monomial of the form a x^2.";
QuadraticQ[u_,x_Symbol] :=
  If[ListQ[u],
    Catch[Scan[Function[If[Not[QuadraticQ[#,x]],Throw[False]]],u]; True],
  PolyQ[u,x,2] && Not[Coefficient[u,x,0]===0 && Coefficient[u,x,1]===0]]


LinearPairQ::usage = "LinearPairQ[u,v,x] returns True iff u and v are linear not equal x but u/v is a constant wrt x.";
LinearPairQ[u_,v_,x_Symbol] :=
  LinearQ[u,x] && LinearQ[v,x] && NeQ[u,x] && EqQ[Coefficient[u,x,0]*Coefficient[v,x,1]-Coefficient[u,x,1]*Coefficient[v,x,0],0]


MonomialQ::usage = "If u is of the form a*x^n where n!=0 and a!=0, MonomialQ[u,x] returns True; else False.";
MonomialQ[u_,x_Symbol] :=
  If[ListQ[u],
    Catch[Scan[Function[If[MonomialQ[#,x],Null,Throw[False]]],u]; True],
  MatchQ[u, a_.*x^n_. /; FreeQ[{a,n},x]]]


MinimumMonomialExponent::usage = "u is sum whose terms are monomials.  MinimumExponent[u,x] returns the exponent of the term having the smallest exponent.";
MinimumMonomialExponent[u_,x_Symbol] :=
  Module[{n=MonomialExponent[First[u],x]},
  Scan[Function[If[PosQ[n-MonomialExponent[#,x]],n=MonomialExponent[#,x]]],u];
  n]


MonomialExponent::usage = "u is a monomial. MonomialExponent[u,x] returns the exponent of x in u.";
MonomialExponent[a_,x_Symbol] :=
  0 /;
FreeQ[a,x]

MonomialExponent[a_.*x_^n_.,x_Symbol] :=
  n /;
FreeQ[{a,n},x]


(* ::Subsection::Closed:: *)
(*Multinomial Match functions*)


LinearMatchQ::usage = "LinearMatchQ[u,x] returns True iff u matches patterns of the form a+b x where a and b are free of x.";
LinearMatchQ[u_,x_Symbol] :=
  If[ListQ[u],
    Catch[Scan[Function[If[Not[LinearMatchQ[#,x]],Throw[False]]],u]; True],
  MatchQ[u, a_.+b_.*x /; FreeQ[{a,b},x]]]


QuadraticMatchQ::usage = "QuadraticMatchQ[u,x] returns True iff u matches patterns of the form a+b x+c x^2 or a+c x^2 where a, b and c are free of x.";
QuadraticMatchQ[u_,x_Symbol] :=
  If[ListQ[u],
    Catch[Scan[Function[If[Not[QuadraticMatchQ[#,x]],Throw[False]]],u]; True],
  MatchQ[u, a_.+b_.*x+c_.*x^2 /; FreeQ[{a,b,c},x]] || MatchQ[u, a_.+c_.*x^2 /; FreeQ[{a,c},x]]]


CubicMatchQ::usage = "CubicMatchQ[u,x] returns True iff u matches patterns of the form a+b x+c x^2+d x^3, a+b x+d x^3, a+c x^2+d x^3 or a+d x^3 where a, b, c and d are free of x.";
CubicMatchQ[u_,x_Symbol] :=
  If[ListQ[u],
    Catch[Scan[Function[If[Not[CubicMatchQ[#,x]],Throw[False]]],u]; True],
  MatchQ[u, a_.+b_.*x+c_.*x^2+d_.*x^3 /; FreeQ[{a,b,c,d},x]] ||
  MatchQ[u, a_.+b_.*x+d_.*x^3 /; FreeQ[{a,b,d},x]] ||
  MatchQ[u, a_.+c_.*x^2+d_.*x^3 /; FreeQ[{a,c,d},x]] ||
  MatchQ[u, a_.+d_.*x^3 /; FreeQ[{a,d},x]]]


BinomialMatchQ::usage = "BinomialMatchQ[u,x] returns True iff u matches patterns of the form a+b x^n where a, b and n are free of x.";
BinomialMatchQ[u_,x_Symbol] :=
  If[ListQ[u],
    Catch[Scan[Function[If[Not[BinomialMatchQ[#,x]],Throw[False]]],u]; True],
  MatchQ[u, a_.+b_.*x^n_. /; FreeQ[{a,b,n},x]]]


GeneralizedBinomialMatchQ::usage = "GeneralizedBinomialMatchQ[u,x] returns True iff u matches patterns of the form a x^q+b x^n where a, b, n and q are free of x.";
GeneralizedBinomialMatchQ[u_,x_Symbol] :=
  If[ListQ[u],
    Catch[Scan[Function[If[Not[GeneralizedBinomialMatchQ[#,x]],Throw[False]]],u]; True],
  MatchQ[u, a_.*x^q_.+b_.*x^n_. /; FreeQ[{a,b,n,q},x]]]


TrinomialMatchQ::usage = "TrinomialMatchQ[u,x] returns True iff u matches patterns of the form a+b x+c x^n or a+c x^(2 n) where a, b, c and n are free of x.";
TrinomialMatchQ[u_,x_Symbol] :=
  If[ListQ[u],
    Catch[Scan[Function[If[Not[TrinomialMatchQ[#,x]],Throw[False]]],u]; True],
  MatchQ[u, a_.+b_.*x^n_.+c_.*x^j_. /; FreeQ[{a,b,c,n},x] && EqQ[j-2*n,0]]]


GeneralizedTrinomialMatchQ::usage = "GeneralizedTrinomialMatchQ[u,x] returns True iff u matches patterns of the form a x^q+b x^n+c x^(2 n-q) where a, b, c, n and q are free of x.";
GeneralizedTrinomialMatchQ[u_,x_Symbol] :=
  If[ListQ[u],
    Catch[Scan[Function[If[Not[GeneralizedTrinomialMatchQ[#,x]],Throw[False]]],u]; True],
  MatchQ[u, a_.*x^q_.+b_.*x^n_.+c_.*x^r_. /; FreeQ[{a,b,c,n,q},x] && EqQ[r-(2*n-q),0]]]


QuotientOfLinearsMatchQ::usage = "QuotientOfLinearsMatchQ[u,x] returns True iff u matches patterns of the form e (a+b x)/(c+d x) where a, b, c and d are free of x.";
QuotientOfLinearsMatchQ[u_,x_Symbol] :=
  If[ListQ[u],
    Catch[Scan[Function[If[Not[QuotientOfLinearsMatchQ[#,x]],Throw[False]]],u]; True],
  MatchQ[u, e_.*(a_.+b_.*x)/(c_.+d_.*x) /; FreeQ[{a,b,c,d,e},x]]]


(* ::Subsection::Closed:: *)
(*Polynomial Terms functions*)


PolynomialTermQ::usage = "If u(x) is an expression of the form a*x^n where n is zero or a positive integer, PolynomialTermQ[u,x] returns True; else it returns False.";
PolynomialTermQ[u_,x_Symbol] :=
  FreeQ[u,x] || MatchQ[u,a_.*x^n_. /; FreeQ[a,x] && IntegerQ[n] && n>0]


PolynomialTerms::usage = "u(x) is a sum.  PolynomialTerms[u,x] returns the sum of the polynomial terms of u(x).";
PolynomialTerms[u_,x_Symbol] :=
  Map[Function[If[PolynomialTermQ[#,x],#,0]],u]


NonpolynomialTerms::usage = "u(x) is a sum.  NonpolynomialTerms[u,x] returns the sum of the nonpolynomial terms of u(x).";
NonpolynomialTerms[u_,x_Symbol] :=
  Map[Function[If[PolynomialTermQ[#,x],0,#]],u]


(* ::Subsection::Closed:: *)
(*PseudoBinomial Routines*)


PseudoBinomialQ::usage = "If u is equivalent to a polynomial of the form a+b*(c+d*x)^n where n\[Element]\[DoubleStruckCapitalZ] and n>2, PseudoBinomialQ[u,x] returns True; else it returns False.";
PseudoBinomialQ[u_,x_Symbol] :=
  ListQ[PseudoBinomialParts[u,x]]


PseudoBinomialPairQ::usage = "If u is equivalent to a polynomial of the form a+b*(e+f*x)^n and v to a polynomial of the form c+d*(e+f*x)^n where n\[Element]\[DoubleStruckCapitalZ] and n>2, PseudoBinomialPairQ[u,v,x] returns True; else it returns False.";
PseudoBinomialPairQ[u_,v_,x_Symbol] :=
  With[{lst1=PseudoBinomialParts[u,x]},
  If[AtomQ[lst1],
    False,
  With[{lst2=PseudoBinomialParts[v,x]},
  If[AtomQ[lst2],
    False,
  Drop[lst1,2]===Drop[lst2,2]]]]]


NormalizePseudoBinomial::usage = "u is pseudo-binomial in x, NormalizePseudoBinomial[u,x] returns u in the form a+b*(c+d*x)^n.";
NormalizePseudoBinomial[u_,x_Symbol] :=
  With[{lst=PseudoBinomialParts[u,x]},
  lst[[1]]+lst[[2]]*(lst[[3]]+lst[[4]]*x)^lst[[5]]]


PseudoBinomialParts::usage = "If u is equivalent to a polynomial of the form a+b*(c+d*x)^n where n\[Element]\[DoubleStruckCapitalZ] and n>2, PseudoBinomialParts[u,x] returns the list {a,b,c,d,n}; else it returns False.";
PseudoBinomialParts[u_,x_Symbol] :=
  If[PolynomialQ[u,x] && Expon[u,x]>2,
    Module[{a,c,d,n},
    n=Expon[u,x];
    d=Rt[Coefficient[u,x,n],n];
    c=Coefficient[u,x,n-1]/(n*d^(n-1));
    a=Simplify[u-(c+d*x)^n];
    If[NeQ[a,0] && FreeQ[a,x],
      {a,1,c,d,n},
    False]],
  False]


(* ::Subsection::Closed:: *)
(*Perfect Power Test Function*)


PerfectPowerTest::usage = "If u (x) is equivalent to a polynomial raised to an integer power greater than 1, PerfectPowerTest[u,x] returns u (x) as an expanded polynomial raised to the power; else it returns False.";
PerfectPowerTest[u_,x_Symbol] :=
  If[PolynomialQ[u,x],
    Module[{lst=FactorSquareFreeList[u],gcd=0,v=1},
    If[lst[[1]]==={1,1},
      lst=Rest[lst]];
    Scan[Function[gcd=GCD[gcd,#[[2]]]],lst];
    If[gcd>1,
      Scan[Function[v=v*#[[1]]^(#[[2]]/gcd)],lst];
      Expand[v]^gcd,
    False]],
  False]


(* ::Subsection::Closed:: *)
(*Square Free Factor Test Function*)


(* SquareFreeFactorTest::usage = "If u (x) can be square free factored, SquareFreeFactorTest[u,x] returns u (x) in factored form; else it returns False."; *)
(* SquareFreeFactorTest[u_,x_Symbol] :=
  If[PolynomialQ[u,x],
    With[{v=FactorSquareFree[u]},
    If[PowerQ[v] || ProductQ[v],
      v,
    False]],
  False] *)


(* ::Section::Closed:: *)
(*Rational function functions*)


(* ::Subsection::Closed:: *)
(*RationalFunctionQ[u,x]*)


RationalFunctionQ::usage = "If u is a rational function of x, RationalFunctionQ[u,x] returns True; else it returns False.";
RationalFunctionQ[u_,x_Symbol] :=
  If[AtomQ[u] || FreeQ[u,x],
    True,
  If[IntegerPowerQ[u],
    RationalFunctionQ[u[[1]],x],
  If[ProductQ[u] || SumQ[u],
    Catch[Scan[Function[If[Not[RationalFunctionQ[#,x]],Throw[False]]],u];True],
  False]]]


(* ::Subsection::Closed:: *)
(*RationalFunctionFactors[u,x]*)


RationalFunctionFactors::usage = "RationalFunctionFactors[u,x] returns the product of the factors of u that are rational functions of x.";
RationalFunctionFactors[u_,x_Symbol] :=
  If[ProductQ[u],
    Map[Function[If[RationalFunctionQ[#,x],#,1]],u],
  If[RationalFunctionQ[u,x],
    u,
  1]]


(* ::Subsection::Closed:: *)
(*NonrationalFunctionFactors[u,x]*)


NonrationalFunctionFactors::usage = "NonrationalFunctionFactors[u,x] returns the product of the factors of u that are not rational functions of x.";
NonrationalFunctionFactors[u_,x_Symbol] :=
  If[ProductQ[u],
    Map[Function[If[RationalFunctionQ[#,x],1,#]],u],
  If[RationalFunctionQ[u,x],
    1,
  u]]


(* ::Subsection::Closed:: *)
(*RationalFunctionExponents[u,x]*)


RationalFunctionExponents::usage = "u is a polynomial or rational function of x.  RationalFunctionExponents[u,x] returns a list of the exponent of the numerator of u and the exponent of the denominator of u.";
RationalFunctionExponents[u_,x_Symbol] :=
  If[PolynomialQ[u,x],
    {Exponent[u,x],0},
  If[IntegerPowerQ[u],
    If[u[[2]]>0,
      u[[2]]*RationalFunctionExponents[u[[1]],x],
    (-u[[2]])*Reverse[RationalFunctionExponents[u[[1]],x]]],
  If[ProductQ[u],
    RationalFunctionExponents[First[u],x]+RationalFunctionExponents[Rest[u],x],
  If[SumQ[u],
    With[{v=Together[u]},
    If[SumQ[v],
      Module[{lst1,lst2},
      lst1=RationalFunctionExponents[First[u],x];
      lst2=RationalFunctionExponents[Rest[u],x];
      {Max[lst1[[1]]+lst2[[2]],lst2[[1]]+lst1[[2]]],lst1[[2]]+lst2[[2]]}],
    RationalFunctionExponents[v,x]]],
  {0,0}]]]]


(* ::Subsection::Closed:: *)
(*RationalFunctionExpand[u,x]*)


RationalFunctionExpand[u_,v_,x_Symbol] :=
  DistributeOverTerms[u,RationalFunctionExpand[v,x],x]


RationalFunctionExpand::usage = "u is a polynomial or rational function of x.  RationalFunctionExpand[u,x] returns the expansion of the factors of u that are rational functions times the other factors.";
RationalFunctionExpand[u_*v_^n_,x_Symbol] :=
  With[{w=RationalFunctionExpand[u,x]},
  If[SumQ[w],
    Map[Function[#*v^n],w],
  w*v^n]] /;
FractionQ[n] && v=!=x

RationalFunctionExpand[u_,x_Symbol] :=
  Module[{v,w},
  v=ExpandIntegrand[u,x];
  If[v=!=u && Not[MatchQ[u, x^m_.*(c_+d_.*x)^p_/(a_+b_.*x^n_) /; FreeQ[{a,b,c,d,p},x] && IntegersQ[m,n] && m==n-1]],
    v,
  v=ExpandIntegrand[RationalFunctionFactors[u,x],x];
  w=NonrationalFunctionFactors[u,x];
  If[SumQ[v],
    Map[Function[#*w],v],
  v*w]]]


(* ::Subsection::Closed:: *)
(*PolyGCD[u,v,x]*)


PolyGCD::usage = "u and v are polynomials in x.  PolyGCD[u,v,x] returns the factors of the gcd of u and v dependent on x.";
PolyGCD[u_,v_,x_Symbol] :=
  NonfreeFactors[PolynomialGCD[u,v],x]


(* ::Section::Closed:: *)
(*Algebraic function functions*)


(* ::Subsection::Closed:: *)
(*AlgebraicFunctionQ*)


AlgebraicFunctionQ::usage = "AlgebraicFunctionQ[u,x] returns True iff u is an algebraic function of x. If flag is True, exponents can be nonnumeric.";
AlgebraicFunctionQ[u_,x_Symbol,flag_:False] :=
  If[AtomQ[u] || FreeQ[u,x],
    True,
  If[PowerQ[u] && (RationalQ[u[[2]]] || flag && FreeQ[u[[2]],x]),
    AlgebraicFunctionQ[u[[1]],x,flag],
  If[ProductQ[u] || SumQ[u],
    Catch[Scan[Function[If[Not[AlgebraicFunctionQ[#,x,flag]],Throw[False]]],u];True],
  If[ListQ[u],
    If[u==={},
      True,
    If[AlgebraicFunctionQ[First[u],x,flag],
      AlgebraicFunctionQ[Rest[u],x,flag],
    False]],
  False]]]]


(* ::Subsection::Closed:: *)
(*AlgebraicFunctionFactors*)


(* AlgebraicFunctionFactors::usage = "AlgebraicFunctionFactors[u,x] returns the product of the factors of u that are algebraic functions of x."; *)
(* AlgebraicFunctionFactors[u_,x_Symbol,flag_:False] :=
  If[ProductQ[u],
    Map[Function[If[AlgebraicFunctionQ[#,x,flag],#,1]],u],
  If[AlgebraicFunctionQ[u,x,flag],u,1]] *)


(* ::Subsection::Closed:: *)
(*NonalgebraicFunctionFactors*)


(* NonalgebraicFunctionFactors::usage = "NonalgebraicFunctionFactors[u,x] returns the product of the factors of u that are not algebraic functions of x."; *)
(* NonalgebraicFunctionFactors[u_,x_Symbol,flag_:False] :=
  If[ProductQ[u],
    Map[Function[If[AlgebraicFunctionQ[#,x,flag],1,#]],u],
  If[AlgebraicFunctionQ[u,x,flag],1,u]] *)


(* ::Section::Closed:: *)
(*Quotient of linears functions*)


(* ::Subsection::Closed:: *)
(*QuotientOfLinearsQ*)


(* ::Item:: *)
(*QuotientOfLinearsQ[u,x] returns True iff u is equivalent to an expression of the form (a+b x)/(c+d x) where b!=0 and d!=0.*)


QuotientOfLinearsQ[u_,x_Symbol] :=
  If[ListQ[u],
    Catch[Scan[Function[If[Not[QuotientOfLinearsQ[#,x]],Throw[False]]],u]; True],
  QuotientOfLinearsP[u,x] && Function[NeQ[#[[2]],0] && NeQ[#[[4]],0]][QuotientOfLinearsParts[u,x]]]


QuotientOfLinearsP[a_*u_,x_] :=
  QuotientOfLinearsP[u,x] /;
FreeQ[a,x]

QuotientOfLinearsP[a_+u_,x_] :=
  QuotientOfLinearsP[u,x] /;
FreeQ[a,x]

QuotientOfLinearsP[1/u_,x_] :=
  QuotientOfLinearsP[u,x]

QuotientOfLinearsP[u_,x_] :=
  True /;
LinearQ[u,x]

QuotientOfLinearsP[u_/v_,x_] :=
  True /;
LinearQ[u,x] && LinearQ[v,x]

QuotientOfLinearsP[u_,x_] :=
  u===x || FreeQ[u,x]


(* ::Subsection::Closed:: *)
(*QuotientOfLinearsParts*)


QuotientOfLinearsParts::usage = "If u is equivalent to an expression of the form (a+b*x)/(c+d*x), QuotientOfLinearsParts[u,x] returns the list {a, b, c, d}.";
QuotientOfLinearsParts[a_*u_,x_] :=
  Apply[Function[{a*#1, a*#2, #3, #4}], QuotientOfLinearsParts[u,x]] /;
FreeQ[a,x]

QuotientOfLinearsParts[a_+u_,x_] :=
  Apply[Function[{#1+a*#3, #2+a*#4, #3, #4}], QuotientOfLinearsParts[u,x]] /;
FreeQ[a,x]

QuotientOfLinearsParts[1/u_,x_] :=
  Apply[Function[{#3, #4, #1, #2}], QuotientOfLinearsParts[u,x]]

QuotientOfLinearsParts[u_,x_] :=
  {Coefficient[u,x,0], Coefficient[u,x,1], 1, 0} /;
LinearQ[u,x]

QuotientOfLinearsParts[u_/v_,x_] :=
  {Coefficient[u,x,0], Coefficient[u,x,1], Coefficient[v,x,0], Coefficient[v,x,1]} /;
LinearQ[u,x] && LinearQ[v,x]

QuotientOfLinearsParts[u_,x_] :=
  If[u===x,
    {0, 1, 1, 0},
  If[FreeQ[u,x],
    {u, 0, 1, 0},
  Print["QuotientOfLinearsParts error!"];
  {u, 0, 1, 0}]]


(* ::Subsection::Closed:: *)
(*SubstForFractionalPowerOfQuotientOfLinears*)


SubstForFractionalPowerOfQuotientOfLinears::usage = "If u has a subexpression of the form ((a+b*x)/(c+d*x))^(m/n) where m and n>1 are integers, SubstForFractionalPowerOfQuotientOfLinears[u,x] returns the list {v,n,(a+b*x)/(c+d*x),b*c-a*d} where v is u with subexpressions of the form ((a+b*x)/(c+d*x))^(m/n) replaced by x^m and x replaced by (-a+c*x^n)/(b-d*x^n), and all times x^(n-1)/(b-d*x^n)^2; else it returns False.";
SubstForFractionalPowerOfQuotientOfLinears[u_,x_Symbol] :=
  Module[{lst=FractionalPowerOfQuotientOfLinears[u,1,False,x]},
  If[AtomQ[lst] || AtomQ[lst[[2]]],
    False,
  With[{n=lst[[1]],tmp=lst[[2]]},
  lst=QuotientOfLinearsParts[tmp,x];
  With[{a=lst[[1]],b=lst[[2]],c=lst[[3]],d=lst[[4]]},
  If[EqQ[d,0],
    False,
  lst=Simplify[x^(n-1)*SubstForFractionalPower[u,tmp,n,(-a+c*x^n)/(b-d*x^n),x]/(b-d*x^n)^2];
  {NonfreeFactors[lst,x],n,tmp,FreeFactors[lst,x]*(b*c-a*d)}]]]]]


SubstForFractionalPowerQ::usage = "If the substitution x=v^(1/n) will not complicate algebraic subexpressions of u, SubstForFractionalPowerQ[u,v,x] returns True; else it returns False.";
SubstForFractionalPowerQ[u_,v_,x_Symbol] :=
  If[AtomQ[u] || FreeQ[u,x],
    True,
  If[FractionalPowerQ[u],
    SubstForFractionalPowerAuxQ[u,v,x],
  Catch[Scan[Function[If[Not[SubstForFractionalPowerQ[#,v,x]],Throw[False]]],u];True]]]

SubstForFractionalPowerAuxQ[u_,v_,x_] :=
  If[AtomQ[u],
    False,
  If[FractionalPowerQ[u] && EqQ[u[[1]],v],
    True,
  Catch[Scan[Function[If[SubstForFractionalPowerAuxQ[#,v,x],Throw[True]]],u];False]]]


FractionalPowerOfQuotientOfLinears::usage = "If u has a subexpression of the form ((a+b*x)/(c+d*x))^(m/n), FractionalPowerOfQuotientOfLinears[u,1,False,x] returns {n,(a+b*x)/(c+d*x)}; else it returns False.";
FractionalPowerOfQuotientOfLinears[u_,n_,v_,x_] :=
  If[AtomQ[u] || FreeQ[u,x],
    {n,v},
  If[CalculusQ[u],
    False,
  If[FractionalPowerQ[u] && QuotientOfLinearsQ[u[[1]],x] && Not[LinearQ[u[[1]],x]] && (FalseQ[v] || EqQ[u[[1]],v]),
    {LCM[Denominator[u[[2]]],n],u[[1]]},
  Catch[Module[{lst={n,v}},
    Scan[Function[If[AtomQ[lst=FractionalPowerOfQuotientOfLinears[#,lst[[1]],lst[[2]],x]],Throw[False]]],u];
    lst]]]]]


(* ::Subsection::Closed:: *)
(*SubstForInverseFunctionOfQuotientOfLinears*)


SubstForInverseFunctionOfQuotientOfLinears::usage = "If u has a subexpression of the form g[(a+b*x)/(c+d*x)] where g is the inverse of function h and f[x,g[(a+b*x)/(c+d*x)]] equals u, SubstForInverseFunctionOfQuotientOfLinears[u,x] returns the list {f[(-a+c*h[x])/(b-d*h[x]),x]*h'[x]/(b-d*h[x])^2, g[(a+b*x)/(c+d*x)], b*c-a*d}.";
SubstForInverseFunctionOfQuotientOfLinears[u_,x_Symbol] :=
  With[{tmp=InverseFunctionOfQuotientOfLinears[u,x]},
  If[AtomQ[tmp],
    False,
  With[{h=InverseFunction[Head[tmp]],lst=QuotientOfLinearsParts[tmp[[1]],x]},
  With[{a=lst[[1]],b=lst[[2]],c=lst[[3]],d=lst[[4]]},
  {SubstForInverseFunction[u,tmp,(-a+c*h[x])/(b-d*h[x]),x]*D[h[x],x]/(b-d*h[x])^2, tmp, b*c-a*d}]]]]


InverseFunctionOfQuotientOfLinears::usage = "If u has a subexpression of the form g[(a+b*x)/(c+d*x)] where g is an inverse function, InverseFunctionOfQuotientOfLinears[u,x] returns g[(a+b*x)/(c+d*x)]; else it returns False.";
InverseFunctionOfQuotientOfLinears[u_,x_Symbol] :=
  If[AtomQ[u] || CalculusQ[u] || FreeQ[u,x],
    False,
  If[InverseFunctionQ[u] && QuotientOfLinearsQ[u[[1]],x],
    u,
  Module[{tmp},
  Catch[
    Scan[Function[If[Not[AtomQ[tmp=InverseFunctionOfQuotientOfLinears[#,x]]],Throw[tmp]]],u];
    False]]]]


(* ::Subsubsection::Closed:: *)
(*Substitution for inverse functions*)


SubstForFractionalPower::usage = "SubstForFractionalPower[u,v,n,w,x] returns u with subexpressions equal to v^(m/n) replaced by x^m and x replaced by w.";
SubstForFractionalPower[u_,v_,n_,w_,x_Symbol] :=
  If[AtomQ[u],
    If[u===x,
      w,
    u],
  If[FractionalPowerQ[u] && EqQ[u[[1]],v],
    x^(n*u[[2]]),
  Map[Function[SubstForFractionalPower[#,v,n,w,x]],u]]]


SubstForInverseFunction::usage = "SubstForInverseFunction[u,v,w,x] returns u with subexpressions equal to v replaced by x and x replaced by w.";
SubstForInverseFunction[u_,v_,x_Symbol] :=
(*  Module[{a=Coefficient[v[[1]],0],b=Coefficient[v[[1]],1]},
  SubstForInverseFunction[u,v,-a/b+InverseFunction[Head[v]]/b,x]] *)
  SubstForInverseFunction[u,v,
		(-Coefficient[v[[1]],x,0]+InverseFunction[Head[v]][x])/Coefficient[v[[1]],x,1],x]

SubstForInverseFunction[u_,v_,w_,x_Symbol] :=
  If[AtomQ[u],
    If[u===x,
      w,
    u],
  If[Head[u]===Head[v] && EqQ[u[[1]],v[[1]]],
    x,
  Map[Function[SubstForInverseFunction[#,v,w,x]],u]]]


(* ::Section::Closed:: *)
(*Absurd number factors*)


AbsurdNumberQ::usage = "AbsurdNumberQ[u] returns True if u is an absurd number, else it returns False.  A number is absurd if it is a rational number, a positive rational number raised to a fractional power, or a product of absurd numbers.";
(*  *)
AbsurdNumberQ[u_] :=
  RationalQ[u]

AbsurdNumberQ[u_^v_] :=
  RationalQ[u] && u>0 && FractionQ[v]

AbsurdNumberQ[u_*v_] :=
  AbsurdNumberQ[u] && AbsurdNumberQ[v]


AbsurdNumberFactors::usage = "AbsurdNumberFactors[u] returns the product of the factors of u that are absurd numbers.";
AbsurdNumberFactors[u_] :=
  If[AbsurdNumberQ[u],
    u,
  If[ProductQ[u],
    Map[AbsurdNumberFactors,u],
  NumericFactor[u]]]


NonabsurdNumberFactors::usage = "NonabsurdNumberFactors[u] returns the product of the factors of u that are not absurd numbers.";
NonabsurdNumberFactors[u_] :=
  If[AbsurdNumberQ[u],
    1,
  If[ProductQ[u],
    Map[NonabsurdNumberFactors,u],
  NonnumericFactors[u]]]


FactorAbsurdNumber::usage = "m must be an absurd number.  FactorAbsurdNumber[m] returns the prime factorization of m as list of base-degree pairs where the bases are prime numbers and the degrees are rational.";
FactorAbsurdNumber[m_] :=
  If[RationalQ[m],
    FactorInteger[m],
  If[PowerQ[m],
    Map[Function[{#[[1]], #[[2]]*m[[2]]}],FactorInteger[m[[1]]]],
  CombineExponents[Sort[Flatten[Map[FactorAbsurdNumber,Apply[List,m]],1], Function[#1[[1]]<#2[[1]]]]]]]


CombineExponents[lst_] :=
  If[Length[lst]<2,
    lst,
  If[lst[[1,1]]==lst[[2,1]],
    CombineExponents[Prepend[Drop[lst,2],{lst[[1,1]],lst[[1,2]]+lst[[2,2]]}]],
  Prepend[CombineExponents[Rest[lst]],First[lst]]]]


AbsurdNumberGCD::usage = "m, n, ... must be absurd numbers.  AbsurdNumberGCD[m,n,...] returns the gcd of m, n, ... .";
AbsurdNumberGCD[seq__] :=
  With[{lst={seq}},
  If[Length[lst]==1,
    First[lst],
  AbsurdNumberGCDList[FactorAbsurdNumber[First[lst]],FactorAbsurdNumber[Apply[AbsurdNumberGCD,Rest[lst]]]]]]


AbsurdNumberGCDList::usage = "lst1 and lst2 must be absurd number prime factorization lists.  AbsurdNumberGCDList[lst1,lst2] returns the gcd of the absurd numbers represented by lst1 and lst2.";
AbsurdNumberGCDList[lst1_,lst2_] :=
  If[lst1==={},
    Apply[Times,Map[Function[#[[1]]^Min[#[[2]],0]],lst2]],
  If[lst2==={},
    Apply[Times,Map[Function[#[[1]]^Min[#[[2]],0]],lst1]],
  If[lst1[[1,1]]==lst2[[1,1]],
    If[lst1[[1,2]]<=lst2[[1,2]],
      lst1[[1,1]]^lst1[[1,2]]*AbsurdNumberGCDList[Rest[lst1],Rest[lst2]],
    lst1[[1,1]]^lst2[[1,2]]*AbsurdNumberGCDList[Rest[lst1],Rest[lst2]]],
  If[lst1[[1,1]]<lst2[[1,1]],
    If[lst1[[1,2]]<0,
      lst1[[1,1]]^lst1[[1,2]]*AbsurdNumberGCDList[Rest[lst1],lst2],
    AbsurdNumberGCDList[Rest[lst1],lst2]],
  If[lst2[[1,2]]<0,
    lst2[[1,1]]^lst2[[1,2]]*AbsurdNumberGCDList[lst1,Rest[lst2]],
  AbsurdNumberGCDList[lst1,Rest[lst2]]]]]]]


(* ::Section::Closed:: *)
(*Normalization functions*)


(* ::Subsection::Closed:: *)
(*NormalizeIntegrand*)


NormalizeIntegrand::usage = "NormalizeIntegrand[u,x] returns u in a standard form recognizable by integration rules.";
NormalizeIntegrand[u_,x_Symbol] :=
  With[{v=NormalizeLeadTermSigns[NormalizeIntegrandAux[u,x]]},
  If[v===NormalizeLeadTermSigns[u],
    u,
  v]]


(* ::Subsection::Closed:: *)
(*NormalizeIntegrandAux*)


NormalizeIntegrandAux[u_,x_Symbol] :=
  If[SumQ[u],
    Map[Function[NormalizeIntegrandAux[#,x]],u],
  If[ProductQ[MergeMonomials[u,x]],
    Map[Function[NormalizeIntegrandFactor[#,x]],MergeMonomials[u,x]],
  NormalizeIntegrandFactor[MergeMonomials[u,x],x]]]


NormalizeIntegrandFactor[u_,x_Symbol] :=
  Module[{bas,deg,min},
  If[PowerQ[u] && FreeQ[u[[2]],x],
    bas=NormalizeIntegrandFactorBase[u[[1]],x];
    deg=u[[2]];
    If[IntegerQ[deg] && SumQ[bas] && EveryQ[Function[MonomialQ[#,x]],bas],
      min=MinimumMonomialExponent[bas,x];
      x^(min*deg)*Map[Function[Simplify[#/x^min]],bas]^deg,
    bas^deg],
  If[PowerQ[u] && FreeQ[u[[1]],x],
    u[[1]]^NormalizeIntegrandFactorBase[u[[2]],x],
  bas=NormalizeIntegrandFactorBase[u,x];
  If[SumQ[bas] && EveryQ[Function[MonomialQ[#,x]],bas],
    min=MinimumMonomialExponent[bas,x];
    x^min*Map[Function[#/x^min],bas],
  bas]]]]


NormalizeIntegrandFactorBase[x_^m_.*u_,x_Symbol] :=
  NormalizeIntegrandFactorBase[Map[Function[x^m*#],u],x] /;
FreeQ[m,x] && SumQ[u]


NormalizeIntegrandFactorBase[u_,x_Symbol] :=
  If[BinomialQ[u,x],
    If[BinomialMatchQ[u,x],
      u,
    ExpandToSum[u,x]],
  If[TrinomialQ[u,x],
    If[TrinomialMatchQ[u,x],
      u,
    ExpandToSum[u,x]],
  If[ProductQ[u],
    Map[Function[NormalizeIntegrandFactor[#,x]],u],
  If[PolynomialQ[u,x] && Exponent[u,x]<=4,
    ExpandToSum[u,x],
  If[SumQ[u],
    With[{v=TogetherSimplify[u]},
    If[SumQ[v] || MatchQ[v, x^m_.*w_ /; FreeQ[m,x] && SumQ[w]] || LeafCount[v]>LeafCount[u]+2,
      UnifySum[u,x],
    NormalizeIntegrandFactorBase[v,x]]],
  Map[Function[NormalizeIntegrandFactor[#,x]],u]]]]]]


(* ::Subsection::Closed:: *)
(*NormalizeLeadTermSigns*)


NormalizeTogether[u_] :=
  NormalizeLeadTermSigns[Together[u]]


NormalizeLeadTermSigns::usage = "NormalizeLeadTermSigns[u] returns an expression equal u but with not more than one sum factor raised to a integer degree having a lead term with a negative coefficient.";
NormalizeLeadTermSigns[u_] :=
  With[{lst=If[ProductQ[u], Map[SignOfFactor,u], SignOfFactor[u]]},
  If[lst[[1]]==1,
    lst[[2]],
  AbsorbMinusSign[lst[[2]]]]]


AbsorbMinusSign::usage = "AbsorbMinusSign[u] returns an expression equal to -u.  If there is a factor of u of the form v^m where v is a sum and m is an odd power, the minus sign is distributed over v; otherwise -u is returned.";
AbsorbMinusSign[u_.*v_Plus] :=
  u*(-v)

AbsorbMinusSign[u_.*v_Plus^m_] :=
  u*(-v)^m /;
OddQ[m]

AbsorbMinusSign[u_] :=
  -u


(* ::Subsection::Closed:: *)
(*NormalizeSumFactors*)


NormalizeSumFactors::usage = "NormalizeSumFactors[u] returns an expression equal u but with the numeric coefficient of the lead term of sum factors made positive where possible.";
NormalizeSumFactors[u_] :=
  If[AtomQ[u] || StopFunctionQ[u],
    u,
  If[ProductQ[u],
    Function[#[[1]]*#[[2]]][SignOfFactor[Map[NormalizeSumFactors,u]]],
  Map[NormalizeSumFactors,u]]]


SignOfFactor::usage = "SignOfFactor[u] returns the list {n,v} where n*v equals u, n^2 equals 1, and the lead term of the sum factors of v raised to integer degrees all have positive coefficients.";
SignOfFactor[u_] :=
  If[RationalQ[u] && u<0 || SumQ[u] && NumericFactor[First[u]]<0,
    {-1, -u},
  If[IntegerPowerQ[u] && SumQ[u[[1]]] && NumericFactor[First[u[[1]]]]<0,
    {(-1)^u[[2]], (-u[[1]])^u[[2]]},
  If[ProductQ[u],
    Map[SignOfFactor,u],
  {1, u}]]]


(* ::Subsection::Closed:: *)
(*MergeMonomials*)


(* ::Subsubsection::Closed:: *)
(*Basis: If  m\[Element]\[DoubleStruckCapitalZ] \[And] b c-a d==0, then (a+b z)^m==b^m/d^m (c+d z)^m*)


MergeMonomials[u_.*(a_.+b_.*x_)^m_.*(c_.+d_.*x_)^n_.,x_Symbol] :=
  u*b^m/d^m*(c+d*x)^(m+n) /;
FreeQ[{a,b,c,d},x] && IntegerQ[m] && EqQ[b*c-a*d,0]


(* ::Subsubsection::Closed:: *)
(*Basis: If  m/n\[Element]\[DoubleStruckCapitalZ], then z^m (c z^n)^p==(c z^n)^(m/n+p)/c^(m/n)*)


MergeMonomials[u_.*(a_.+b_.*x_)^m_.*(c_.*(a_.+b_.*x_)^n_.)^p_,x_Symbol] :=
  u*(c*(a+b*x)^n)^(m/n+p)/c^(m/n) /;
FreeQ[{a,b,c,m,n,p},x] && IntegerQ[m/n]


(* ::Subsubsection::Closed:: *)
(*Miscellaneous simplification*)


MergeMonomials[a_.*u_^m_,x_Symbol] :=
  a*u^Simplify[m] /;
FreeQ[{a,m},x]

MergeMonomials[u_,x_Symbol] :=
  If[LinearQ[u,x],
    Cancel[u],
  u]


(* ::Section::Closed:: *)
(*Simplification functions*)


(* ::Subsection::Closed:: *)
(*SimplifyIntegrand*)


SimplifyIntegrand::usage = "SimplifyIntegrand[u,x] simplifies u and returns the result in a standard form recognizable by integration rules.";
SimplifyIntegrand[u_,x_Symbol] :=
  Module[{v},
  v=NormalizeLeadTermSigns[NormalizeIntegrandAux[Simplify[u],x]];
  If[LeafCount[v]<4/5*LeafCount[u],
    v,
  If[v=!=NormalizeLeadTermSigns[u],
    v,
  u]]]


(* ::Subsection::Closed:: *)
(*SimplifyTerm        ????*)


(* SimplifyTerm[u_,x_Symbol] :=
  Module[{v=Simplify[u],w},
  w=Together[v];
  NormalizeIntegrand[If[LeafCount[v]<LeafCount[w],v,w],x]] *)


SimplifyTerm[u_,x_Symbol] :=
  Module[{v=Simplify[u],w},
  w=Together[v];
  NormalizeIntegrand[If[LeafCount[v]<LeafCount[w],w,w],x]]


(* ::Subsection::Closed:: *)
(*TogetherSimplify*)


TogetherSimplify[u_] :=
  TimeConstrained[
    With[{v=Together[Simplify[Together[u]]]},
    TimeConstrained[FixSimplify[v],$TimeLimit/3,v]],
  $TimeLimit,u]


(* TogetherSimplify could replace SmartSimplify, but results in more complicated antiderivatives and would require thousands of changes to test suite. *)
SmartSimplify[u_] :=
  TimeConstrained[
    Module[{v,w},
    v=Simplify[u];
    w=Factor[v];
    v=If[LeafCount[w]<LeafCount[v] (* -1 *),w,v];
    v=If[Not[FalseQ[w=FractionalPowerOfSquareQ[v]]] && FractionalPowerSubexpressionQ[u,w,Expand[w]],SubstForExpn[v,w,Expand[w]],v];
    v=FactorNumericGcd[v];
    TimeConstrained[FixSimplify[v],$TimeLimit/3,v]],
  $TimeLimit,u]


SubstForExpn[u_,v_,w_] :=
  If[u===v,
    w,
  If[AtomQ[u],
    u,
  Map[Function[SubstForExpn[#,v,w]],u]]]


(* ::Subsection::Closed:: *)
(*Simp[expn]*)


Simp::usage = "Simp[u] and Simp[u,x] simplifies and returns u.";


Simp[(e_.*(a_+b_)^r_.)^p_.*(c_+d_)^q_.] :=
  With[{u=Simplify[(a+b)/(c+d)]},
  If[IntegerQ[p] || GtQ[e,0] && GtQ[u,0] && GtQ[r,0],
    e^p*u^(p*r),
  e^IntPart[p]*u^(r*IntPart[p])*(e*(a+b)^r)^FracPart[p]/(c+d)^(r*FracPart[p])]] /;
IntegerQ[r] && EqQ[p*r+q,0]


Simp[(g_.*(a_+b_)^s_.)^p_.*(c_+d_)^q_.*(e_+f_)^r_.] :=
  With[{u=Simplify[(a+b)/((c+d)*(e+f))]},
  If[IntegerQ[p],
    g^p*u^(p*s),
  If[GtQ[g,0] && GtQ[u,0] && (NeQ[g,1] || NeQ[u,1]),
    g^p*u^(p*s)*Simp[((c+d)^s*(e+f)^s)^p/((c+d)^(p*s)*(e+f)^(p*s))],
  If[GtQ[g,0] && EqQ[a,c^2] && EqQ[b,-d^2] && GtQ[c,0],
    g^p*Simp[(c-d)^(p*s)/(e+f)^(p*s)],
  If[GtQ[g,0] && EqQ[a,e^2] && EqQ[b,-f^2] && GtQ[e,0],
    g^p*Simp[(e-f)^(p*s)/(c+d)^(p*s)],
  g^IntPart[p]*u^(s*IntPart[p])*(g*(a+b)^s)^FracPart[p]/((c+d)^(s*FracPart[p])*(e+f)^(s*FracPart[p]))]]]]] /;
IntegerQ[s] && EqQ[p*s+q,0] && EqQ[p*s+r,0]


Simp[(u_^q_.*v_^r_.)^p_.*u_^pq_.*v_^pr_.] :=
  If[IntegerQ[p],
    1,
  (u^q*v^r)^FracPart[p]/(u^(q*FracPart[p])*v^(r*FracPart[p]))] /;
IntegersQ[q,r] && EqQ[pq+p*q,0] && EqQ[pr+p*r,0]


Simp[(u_*v_)^p_.] :=
  If[IntegerQ[p] || GtQ[u,0] || GtQ[v,0],
    Simp[u^p]*Simp[v^p],
  (u*v)^p]


Simp[(a_+b_)^p_.*(c_+d_)^q_*(e_+f_)^q_] :=
  (a/c^2)^p*(c+d)^(p+q)*(e+f)^(p+q) /;
IntegerQ[p] && EqQ[e,c] && EqQ[f,-d] && EqQ[b*c^2+a*d^2,0]


Simp[(a_+b_)^p_*(c_+d_)^p_] :=
  (a^2-b^2)^p /;
 EqQ[a,c] && EqQ[b,-d] && GtQ[a,0]


Simp[u_] := Simplify[u]


(* ::Subsection::Closed:: *)
(*Simp[expn,var]*)


Simp[u_,x_] :=
  TimeConstrained[NormalizeSumFactors[SimpHelp[u,x]],$TimeLimit,u]


SimpHelp[E^(u_.*(v_.*Log[a_]+w_)),x_] :=
  a^(u*v)*SimpHelp[E^(u*w),x]

SimpHelp[u_,x_] :=
  If[AtomQ[u] || StopFunctionQ[u],
    u,
  If[FreeQ[u,x],
    With[{v=SmartSimplify[u]},
    If[LeafCount[v]<=LeafCount[u],
      v,
    u]],
  If[ProductQ[u],
    If[EqQ[First[u],1/2] && MatchQ[Rest[u],a_.+n_*Pi+b_.*v_ /; FreeQ[{a,b},x] && Not[FreeQ[v,x]] && EqQ[n^2,1/4]],
      If[MatchQ[Rest[u],n_*Pi+b_.*v_ /; FreeQ[b,x] && Not[FreeQ[v,x]] && EqQ[n^2,1/4]],
        Map[Function[1/2*#],Rest[u]],
      If[MatchQ[Rest[u],m_*a_.+n_*Pi+p_*b_.*v_ /; FreeQ[{a,b},x] && Not[FreeQ[v,x]] && IntegersQ[m/2,p/2]],
        Map[Function[1/2*#],Rest[u]],
      u]],
    Module[{v=FreeFactors[u,x],w=NonfreeFactors[u,x]},
    v=NumericFactor[v]*SmartSimplify[NonnumericFactors[v]*x^2]/x^2;
    w=If[ProductQ[w], Map[Function[SimpHelp[#,x]],w], SimpHelp[w,x]];
    w=FactorNumericGcd[w];
    v=MergeFactors[v,w];
    If[ProductQ[v],
      Map[Function[SimpFixFactor[#,x]],v],
    v]]],
  If[SumQ[u],
    If[MatchQ[u,a_.+n_*Pi+b_.*x /; FreeQ[{a,b},x] && EqQ[n^2,1/16]],
      u,
    If[PolynomialQ[u,x] && Exponent[u,x]<=0,
      SimpHelp[Coefficient[u,x,0],x],
    If[PolynomialQ[u,x] && Exponent[u,x]==1 && Coefficient[u,x,0]===0,
      SimpHelp[Coefficient[u,x,1],x]*x,
    Module[{v=0,w=0},
    Scan[Function[If[FreeQ[#,x],v=#+v,w=#+w]],u];
    v=SmartSimplify[v];
    w=If[SumQ[w], Map[Function[SimpHelp[#,x]],w], SimpHelp[w,x]];
    v+w]]]],
  If[TrigQ[u],
    With[{v=SimpHelp[u[[1]],x]},
    If[LinearQ[v,x] && MatchQ[Coefficient[v,x,0],m_.*(n_.*Pi+r_.)+s_. /; RationalQ[m,n]],
      NormalizeTrig[Head[u],Coefficient[v,x,0],Coefficient[v,x,1],x],
    Head[u][v]]],
  If[HyperbolicQ[u],
    With[{v=SimpHelp[u[[1]],x]},
    If[LinearQ[v,x] && MatchQ[Coefficient[v,x,0],m_.*(n_.*Complex[0,nz_]*Pi+r_.)+s_. /; RationalQ[m,n,nz]],
      NormalizeHyperbolic[Head[u],Coefficient[v,x,0],Coefficient[v,x,1],x],
    Head[u][v]]],
  Map[Function[SimpHelp[#,x]],u]]]]]]]


NormalizeTrig[func_,m_.*(n_.*Pi+r_.)+s_.,b_,x_] :=
  If[m*n==1/4 && NegQ[b],
    Switch[func,
	  Sin, Cos[Pi/4-m*r-s-b*x],        (* Sin[Pi/4-z] == Cos[Pi/4+z] *)
	  Cos, Sin[Pi/4-m*r-s-b*x],        (* Cos[Pi/4-z] == Sin[Pi/4+z] *)
	  Tan, Cot[Pi/4-m*r-s-b*x],        (* Tan[Pi/4-z] == Cot[Pi/4+z] *)
	  Cot, Tan[Pi/4-m*r-s-b*x],        (* Cot[Pi/4-z] == Tan[Pi/4+z] *)
	  Sec, Csc[Pi/4-m*r-s-b*x],        (* Sec[Pi/4-z] == Csc[Pi/4+z] *)
	  Csc, Sec[Pi/4-m*r-s-b*x]],       (* Csc[Pi/4-z] == Sec[Pi/4+z] *)
  If[m*n==-1/4,
    If[PosQ[b],
      Switch[func,
	    Sin, -Cos[Pi/4+m*r+s+b*x],     (* Sin[-Pi/4+z] == -Cos[Pi/4+z] *)
	    Cos, Sin[Pi/4+m*r+s+b*x],      (* Cos[-Pi/4+z] == Sin[Pi/4+z] *)
	    Tan, -Cot[Pi/4+m*r+s+b*x],     (* Tan[-Pi/4+z] == -Cot[Pi/4+z] *)
	    Cot, -Tan[Pi/4+m*r+s+b*x],     (* Cot[-Pi/4+z] == -Tan[Pi/4+z] *)
	    Sec, Csc[Pi/4+m*r+s+b*x],      (* Sec[-Pi/4+z] == Csc[Pi/4+z] *)
	    Csc, -Sec[Pi/4+m*r+s+b*x]],    (* Csc[-Pi/4+z] == -Sec[Pi/4+z] *)
    Switch[func,
	  Sin, -Sin[Pi/4-m*r-s-b*x],       (* Sin[-Pi/4-z] == -Sin[Pi/4+z] *)
	  Cos, Cos[Pi/4-m*r-s-b*x],        (* Cos[-Pi/4-z] == Cos[Pi/4+z] *)
	  Tan, -Tan[Pi/4-m*r-s-b*x],       (* Tan[-Pi/4-z] == -Tan[Pi/4+z] *)
	  Cot, -Cot[Pi/4-m*r-s-b*x],       (* Cot[-Pi/4-z] == -Cot[Pi/4+z] *)
	  Sec, Sec[Pi/4-m*r-s-b*x],        (* Sec[-Pi/4-z] == Sec[Pi/4+z] *)
	  Csc, -Csc[Pi/4-m*r-s-b*x]]],     (* Csc[-Pi/4-z] == -Csc[Pi/4+z] *)
  func[m*n*Pi+m*r+s+b*x]]] /;
RationalQ[m,n]


NormalizeHyperbolic[func_,m_.*(n_.*Complex[0,nz_]*Pi+r_.)+s_.,b_,x_] :=
  If[m*n*nz==1/4 && NegQ[b],
    Switch[func,
	  Sinh, I*Cosh[I*Pi/4-m*r-s-b*x],        (* Sinh[I*Pi/4-z] == I*Cosh[I*Pi/4+z] *)
	  Cosh, -I*Sinh[I*Pi/4-m*r-s-b*x],       (* Cosh[I*Pi/4-z] == -I*Sinh[I*Pi/4+z] *)
	  Tanh, -Coth[I*Pi/4-m*r-s-b*x],         (* Tanh[I*Pi/4-z] == -Coth[I*Pi/4+z] *)
	  Coth, -Tanh[I*Pi/4-m*r-s-b*x],         (* Coth[I*Pi/4-z] == -Tanh[I*Pi/4+z] *)
	  Sech, I*Csch[I*Pi/4-m*r-s-b*x],        (* Sech[I*Pi/4-z] == I*Csch[I*Pi/4+z] *)
	  Csch, -I*Sech[I*Pi/4-m*r-s-b*x]],      (* Csch[I*Pi/4-z] == -I*Sech[I*Pi/4+z] *)
  If[m*n*nz==-1/4,
    If[PosQ[b],
      Switch[func,
	    Sinh, -I*Cosh[I*Pi/4+m*r+s+b*x],     (* Sinh[-I*Pi/4+z] == -I*Cosh[I*Pi/4+z] *)
	    Cosh, -I*Sinh[I*Pi/4+m*r+s+b*x],     (* Cosh[-I*Pi/4+z] == -I*Sinh[I*Pi/4+z] *)
	    Tanh, Coth[I*Pi/4+m*r+s+b*x],        (* Tanh[-I*Pi/4+z] == Coth[I*Pi/4+z] *)
	    Coth, Tanh[I*Pi/4+m*r+s+b*x],        (* Coth[-I*Pi/4+z] == Tanh[I*Pi/4+z] *)
	    Sech, I*Csch[I*Pi/4+m*r+s+b*x],      (* Sech[-I*Pi/4+z] == I*Csch[I*Pi/4+z] *)
	    Csch, I*Sech[I*Pi/4+m*r+s+b*x]],     (* Csch[-I*Pi/4+z] == I*Sech[I*Pi/4+z] *)
    Switch[func,
	  Sinh, -Sinh[I*Pi/4-m*r-s-b*x],         (* Sinh[-I*Pi/4-z] == -Sinh[I*Pi/4+z] *)
	  Cosh, Cosh[I*Pi/4-m*r-s-b*x],          (* Cosh[-I*Pi/4-z] == Cosh[I*Pi/4+z] *)
	  Tanh, -Tanh[I*Pi/4-m*r-s-b*x],         (* Tanh[-I*Pi/4-z] == -Tanh[I*Pi/4+z] *)
	  Coth, -Coth[I*Pi/4-m*r-s-b*x],         (* Coth[-I*Pi/4-z] == -Coth[I*Pi/4+z] *)
	  Sech, Sech[I*Pi/4-m*r-s-b*x],          (* Sech[-I*Pi/4-z] == Sech[I*Pi/4+z] *)
	  Csch, -Csch[I*Pi/4-m*r-s-b*x]]],       (* Csch[-I*Pi/4-z] == -Csch[I*Pi/4+z] *)
  func[m*n*nz*I*Pi+m*r+s+b*x]]] /;
RationalQ[m,n,nz]


(* ::Subsection::Closed:: *)
(*FractionalPowerOfSquareQ*)


FractionalPowerOfSquareQ::usage = "If a subexpression of u is of the form ((v+w)^2)^n where n is a fraction, FractionalPowerOfSquareQ[u] returns (v+w)^2; else it returns False.";
FractionalPowerOfSquareQ[u_] :=
  If[AtomQ[u],
    False,
  If[FractionalPowerQ[u] && MatchQ[u[[1]], a_.*(b_+c_)^2 /; NonsumQ[a]],
    u[[1]],
  Module[{tmp},
  Catch[
    Scan[Function[If[Not[FalseQ[tmp=FractionalPowerOfSquareQ[#]]],Throw[tmp]]],u];
    False]]]]


FractionalPowerSubexpressionQ::usage = "If a subexpression of u is of the form w^n where n is a fraction but not equal to v, FractionalPowerSubexpressionQ[u,v,w] returns True; else it returns False.";
FractionalPowerSubexpressionQ[u_,v_,w_] :=
  If[AtomQ[u],
    False,
  If[FractionalPowerQ[u] && GtQ[u[[1]]/w,0],
    Not[u[[1]]===v] && LeafCount[w]<3*LeafCount[v],
  Catch[Scan[Function[If[FractionalPowerSubexpressionQ[#,v,w],Throw[True]]],u]; False]]]


(* ::Subsection::Closed:: *)
(*FixSimplify*)


Clear[FixSimplify]


(* ::Item:: *)
(*Basis: If  n\[Element]\[DoubleStruckCapitalZ], then u I (v I+w)^n==(-1)^((n+1)/2) u (v-I w)^n*)


FixSimplify[u_.*Complex[0,a_]*(v_.*Complex[0,b_]+w_)^n_.] :=
  (-1)^((n+1)/2)*a*u*FixSimplify[(b*v-Complex[0,1]*w)^n] /;
OddQ[n]


(* ::Item:: *)
(*Basis: If  u>0 \[And] v>0, let g=GCD[m,n], then u^m v^n==((u^(m/g) v^(n/g)))^g*)


FixSimplify[w_.*u_^m_.*v_^n_] :=
  With[{z=Simplify[u^(m/GCD[m,n])*v^(n/GCD[m,n])]},
  FixSimplify[w*z^GCD[m,n]]/;
 AbsurdNumberQ[z] || SqrtNumberSumQ[z]] /;
RationalQ[m] && FractionQ[n] && SqrtNumberSumQ[u] && SqrtNumberSumQ[v] && GtQ[u,0] && GtQ[v,0]


FixSimplify[w_.*u_^m_.*v_^n_] :=
  With[{z=Simplify[u^(m/GCD[m,-n])*v^(n/GCD[m,-n])]},
  FixSimplify[w*z^GCD[m,-n]]/;
 AbsurdNumberQ[z] || SqrtNumberSumQ[z]] /;
RationalQ[m] && FractionQ[n] && SqrtNumberSumQ[u] && SqrtNumberSumQ[1/v] && GtQ[u,0] && GtQ[v,0]


(* ::Item:: *)
(*Basis: If  m\[Element]\[DoubleStruckCapitalZ] \[And] u<0 \[And] v>0, then u^m v^n==-(((-u))^(m/n) v)^n*)


FixSimplify[w_.*u_^m_.*v_^n_] :=
  With[{z=Simplify[(-u)^(m/GCD[m,n])*v^(n/GCD[m,n])]},
  FixSimplify[-w*z^GCD[m,n]]/;
 AbsurdNumberQ[z] || SqrtNumberSumQ[z]] /;
IntegerQ[m] && FractionQ[n] && SqrtNumberSumQ[u] && SqrtNumberSumQ[v] && LtQ[u,0] && GtQ[v,0]


FixSimplify[w_.*u_^m_.*v_^n_] :=
  With[{z=Simplify[(-u)^(m/GCD[m,-n])*v^(n/GCD[m,-n])]},
  FixSimplify[-w*z^GCD[m,-n]]/;
 AbsurdNumberQ[z] || SqrtNumberSumQ[z]] /;
IntegerQ[m] && FractionQ[n] && SqrtNumberSumQ[u] && SqrtNumberSumQ[1/v] && LtQ[u,0] && GtQ[v,0]


(* ::Item:: *)
(*Basis: If  p\[Element]\[DoubleStruckCapitalZ] \[Or] a>0, then a^m (u+v)^p==(a^(m/p) u+a^(m/p) v)^p*)


FixSimplify[w_.*a_^m_*(u_+b_^n_*v_.)^p_.] :=
  With[{c=Simplify[a^(m/p)*b^n]},
  FixSimplify[w*(a^(m/p)*u+c*v)^p] /;
 RationalQ[c]] /;
RationalQ[a,b,m,n] && a>0 && b>0 && IGtQ[p,0]


(* ::Item:: *)
(*Basis: If  p\[Element]\[DoubleStruckCapitalZ], then a^m (a^n u+(-a)^p v)==a^(m+n) (u+(-1)^p a^(p-n) v)*)


FixSimplify[w_.*a_^m_.*(a_^n_*u_.+b_^p_.*v_.)] :=
  With[{z=w*a^(m+n)*(u+(-1)^p*a^(p-n)*v)},
  FixSimplify[z] /;
 Not[MatchQ[z, ww_.*aa_^mm_*(uu_+bb_^nn_*vv_.) /;
   RationalQ[aa,bb,mm,nn] && aa>0 && bb>0 && RationalQ[Simplify[aa^mm*bb^nn]]]]] /;
RationalQ[m] && FractionQ[n] && IntegerQ[p] && p-n>0 && a+b===0


(* ::Item:: *)
(*Basis: If  m\[Element]\[DoubleStruckCapitalZ] \[And] b c-a d==0, then (a+b)^m==(b/d)^m (c+d)^m*)


FixSimplify[w_.*(a_+b_)^m_.*(c_+d_)^n_] :=
  With[{q=Simplify[b/d]},
  FixSimplify[w*q^m*(c+d)^(m+n)] /;
 FreeQ[q,Plus]] /;
IntegerQ[m] && Not[IntegerQ[n]] && EqQ[b*c-a*d,0]


(* ::Item:: *)
(*Basis: If  t\[Element]\[DoubleStruckCapitalZ], then (a^m u+a^n v+...)^t==a^(m t) (u+a^(n-m) v+...)^t*)


FixSimplify[w_.*(a_^m_.*u_.+a_^n_.*v_.)^t_.] :=
  FixSimplify[a^(m*t)*w*(u+a^(n-m)*v)^t] /;
Not[RationalQ[a]] && IntegerQ[t] && RationalQ[m,n] && 0<m<=n

FixSimplify[w_.*(a_^m_.*u_.+a_^n_.*v_.+a_^p_.*z_.)^t_.] :=
  FixSimplify[a^(m*t)*w*(u+a^(n-m)*v+a^(p-m)*z)^t] /;
Not[RationalQ[a]] && IntegerQ[t] && RationalQ[m,n,p] && 0<m<=n<=p

FixSimplify[w_.*(a_^m_.*u_.+a_^n_.*v_.+a_^p_.*z_.+a_^q_.*y_.)^t_.] :=
  FixSimplify[a^(m*t)*w*(u+a^(n-m)*v+a^(p-m)*z+a^(q-m)*y)^t] /;
Not[RationalQ[a]] && IntegerQ[t] && RationalQ[m,n,p] && 0<m<=n<=p<=q


(* ::Item:: *)
(*Basis: a Sqrt[v]+b Sqrt[v]+...==(a+b+...)Sqrt[v]*)


FixSimplify[w_.*(u_.+a_.*Sqrt[v_Plus]+b_.*Sqrt[v_]+c_.*Sqrt[v_]+d_.*Sqrt[v_])] :=
  FixSimplify[w*(u+FixSimplify[a+b+c+d]*Sqrt[v])]

FixSimplify[w_.*(u_.+a_.*Sqrt[v_Plus]+b_.*Sqrt[v_]+c_.*Sqrt[v_])] :=
  FixSimplify[w*(u+FixSimplify[a+b+c]*Sqrt[v])]

FixSimplify[w_.*(u_.+a_.*Sqrt[v_Plus]+b_.*Sqrt[v_])] :=
  FixSimplify[w*(u+FixSimplify[a+b]*Sqrt[v])]


(* ::Item:: *)
(*Basis: If  a>0, then a^(m/4) Sqrt[b(c+d Sqrt[a])]==Sqrt[b(c a^(m/2)+d a^((m+1)/2))]*)


(* FixSimplify[u_.*a_^m_*Sqrt[b_.*(c_+d_.*Sqrt[a_])]] :=
  Sqrt[Together[b*(c*a^(2*m)+d*a^(2*m+1/2))]]*FixSimplify[u] /;
RationalQ[a,b,c,d,m] && a>0 && Denominator[m]==4

FixSimplify[u_.*a_^m_/Sqrt[b_.*(c_+d_.*Sqrt[a_])]] :=
  FixSimplify[u]/Sqrt[Together[b*(c/a^(2*m)+d/a^(2*m-1/2))]] /;
RationalQ[a,b,c,d,m] && a>0 && Denominator[m]==4 *)


(* ::Item:: *)
(*Basis: If  w^-n==v, then v^m w^n==v^(m-1)*)


FixSimplify[u_.*v_^m_*w_^n_] :=
  -FixSimplify[u*v^(m-1)] /;
RationalQ[m] && Not[RationalQ[w]] && FractionQ[n] && n<0 && EqQ[v+w^(-n),0]


(* ::Item:: *)
(*Basis: If  n\[Element]\[DoubleStruckCapitalZ], then v^m (-v)^n==(-1)^n v^(m+n)*)


FixSimplify[u_.*v_^m_*w_^n_.] :=
  (-1)^(n)*FixSimplify[u*v^(m+n)] /;
RationalQ[m] && Not[RationalQ[w]] && IntegerQ[n] && EqQ[v+w,0]


(* ::Item:: *)
(*Basis: If  n/p\[Element]\[DoubleStruckCapitalZ], then (-v^p)^m v^n==(-1)^(n/p) (-v^p)^(m+n/p)*)


FixSimplify[u_.*(-v_^p_.)^m_*w_^n_.] :=
  (-1)^(n/p)*FixSimplify[u*(-v^p)^(m+n/p)] /;
RationalQ[m] && Not[RationalQ[w]] && IntegerQ[n/p] && EqQ[v-w,0]


(* ::Item:: *)
(*Basis: If  (n|n/p)\[Element]\[DoubleStruckCapitalZ], then (-v^p)^m (-v)^n==(-1)^(n+n/p) (-v^p)^(m+n/p)*)


FixSimplify[u_.*(-v_^p_.)^m_*w_^n_.] :=
  (-1)^(n+n/p)*FixSimplify[u*(-v^p)^(m+n/p)] /;
RationalQ[m] && Not[RationalQ[w]] && IntegersQ[n,n/p] && EqQ[v+w,0]


(* ::Item:: *)
(*Basis: (a-b) (a+b) == a^2-b^2*)


FixSimplify[u_.*(a_-b_)^m_.*(a_+b_)^m_.] :=
  u*(a^2-b^2)^m /;
IntegerQ[m] && AtomQ[a] && AtomQ[b]


FixSimplify[u_.*(c_Symbol*d_Symbol^2-e_Symbol*(b_Symbol*d_Symbol-a_Symbol*e_Symbol))^m_.] :=
  u*(c*d^2-b*d*e+a*e^2)^m /;
RationalQ[m] && OrderedQ[{a,b,c,d,e}]

FixSimplify[u_.*(c_Symbol*d_Symbol^2+e_Symbol*(-b_Symbol*d_Symbol+a_Symbol*e_Symbol))^m_.] :=
  u*(c*d^2-b*d*e+a*e^2)^m /;
RationalQ[m] && OrderedQ[{a,b,c,d,e}]

FixSimplify[u_.*(-c_Symbol*d_Symbol^2+e_Symbol*(b_Symbol*d_Symbol-a_Symbol*e_Symbol))^m_.] :=
  (-1)^m*u*(c*d^2-b*d*e+a*e^2)^m /;
IntegerQ[m] && OrderedQ[{a,b,c,d,e}]

FixSimplify[u_.*(-c_Symbol*d_Symbol^2-e_Symbol*(-b_Symbol*d_Symbol+a_Symbol*e_Symbol))^m_.] :=
  (-1)^m*u*(c*d^2-b*d*e+a*e^2)^m /;
RationalQ[m] && OrderedQ[{a,b,c,d,e}]


FixSimplify[u_] := u


(* ::Subsection::Closed:: *)
(*SimpFixFactor*)


(* SimpFixFactor[(a_.*c_ + b_.*c_)^p_.,x_] :=
  c^p*SimpFixFactor[(a+b)^p,x] /;
FreeQ[c,x] && IntegerQ[p] && c^p=!=-1 *)

SimpFixFactor[(a_.*Complex[0,c_] + b_.*Complex[0,d_])^p_.,x_] :=
  I^p*SimpFixFactor[(a*c+b*d)^p,x] /;
IntegerQ[p]

SimpFixFactor[(a_.*Complex[0,d_] + b_.*Complex[0,e_]+ c_.*Complex[0,f_])^p_.,x_] :=
  I^p*SimpFixFactor[(a*d+b*e+c*f)^p,x] /;
IntegerQ[p]


SimpFixFactor[(a_.*c_^r_ + b_.*x_^n_.)^p_.,x_] :=
  c^(p*r)*SimpFixFactor[(a+b/c^r*x^n)^p,x] /;
FreeQ[{a,b,c},x] && IntegersQ[n,p] && AtomQ[c] && RationalQ[r] && r<0

SimpFixFactor[(a_. + b_.*c_^r_*x_^n_.)^p_.,x_] :=
  c^(p*r)*SimpFixFactor[(a/c^r+b*x^n)^p,x] /;
FreeQ[{a,b,c},x] && IntegersQ[n,p] && AtomQ[c] && RationalQ[r] && r<0


SimpFixFactor[(a_.*c_^s_. + b_.*c_^r_.*x_^n_.)^p_.,x_] :=
  c^(p*s)*SimpFixFactor[(a+b*c^(r-s)*x^n)^p,x] /;
FreeQ[{a,b,c},x] && IntegersQ[n,p] && RationalQ[s,r] && 0<s<=r && c^(p*s)=!=-1

SimpFixFactor[(a_.*c_^s_. + b_.*c_^r_.*x_^n_.)^p_.,x_] :=
  c^(p*r)*SimpFixFactor[(a*c^(s-r)+b*x^n)^p,x] /;
FreeQ[{a,b,c},x] && IntegersQ[n,p] && RationalQ[s,r] && 0<r<s && c^(p*r)=!=-1

SimpFixFactor[u_,x_] := u


(* ::Subsection::Closed:: *)
(*FactorNumericGcd*)


FactorNumericGcd::usage = "FactorNumericGcd[u] returns u with the gcd of the numeric coefficients of terms of sums factored out.";
FactorNumericGcd[u_] :=
  If[PowerQ[u] && RationalQ[u[[2]]],
    FactorNumericGcd[u[[1]]]^u[[2]],
  If[ProductQ[u],
    Map[FactorNumericGcd,u],
  If[SumQ[u],
    With[{g=Apply[GCD,Map[NumericFactor,Apply[List,u]]]},
    g*Map[Function[#/g],u]],
  u]]]


(* ::Subsection::Closed:: *)
(*MergeFactors*)


MergeFactors::usage = "MergeFactors[u,v] returns the product of u and v, but with the mergeable factors of u merged into v.";
MergeFactors[u_,v_] :=
  If[ProductQ[u],
    MergeFactors[Rest[u],MergeFactors[First[u],v]],
  If[PowerQ[u],
    If[MergeableFactorQ[u[[1]],u[[2]],v],
      MergeFactor[u[[1]],u[[2]],v],
    If[RationalQ[u[[2]]] && u[[2]]<-1 && MergeableFactorQ[u[[1]],-1,v],
      MergeFactors[u[[1]]^(u[[2]]+1),MergeFactor[u[[1]],-1,v]],
    u*v]],
  If[MergeableFactorQ[u,1,v],
    MergeFactor[u,1,v],
  u*v]]]


MergeFactor::usage = "If MergeableFactorQ[bas,deg,v], MergeFactor[bas,deg,v] return the product of bas^deg and v, but with bas^deg merged into the factor of v whose base equals bas.";
MergeFactor[bas_,deg_,v_] :=
  If[bas===v,
    bas^(deg+1),
  If[PowerQ[v],
    If[bas===v[[1]],
      bas^(deg+v[[2]]),
    MergeFactor[bas,deg/v[[2]],v[[1]]]^v[[2]]],
  If[ProductQ[v],
    If[MergeableFactorQ[bas,deg,First[v]],
      MergeFactor[bas,deg,First[v]]*Rest[v],
    First[v]*MergeFactor[bas,deg,Rest[v]]],
  MergeFactor[bas,deg,First[v]] + MergeFactor[bas,deg,Rest[v]]]]]


MergeableFactorQ::usage = "MergeableFactorQ[bas,deg,v] returns True iff bas equals the base of a factor of v or bas is a factor of every term of v.";
MergeableFactorQ[bas_,deg_,v_] :=
  If[bas===v,
    RationalQ[deg+1] && (deg+1>=0 || RationalQ[deg] && deg>0),
  If[PowerQ[v],
    If[bas===v[[1]],
      RationalQ[deg+v[[2]]] && (deg+v[[2]]>=0 || RationalQ[deg] && deg>0),
    SumQ[v[[1]]] && IntegerQ[v[[2]]] && (Not[IntegerQ[deg]] || IntegerQ[deg/v[[2]]]) && MergeableFactorQ[bas,deg/v[[2]],v[[1]]]],
  If[ProductQ[v],
    MergeableFactorQ[bas,deg,First[v]] || MergeableFactorQ[bas,deg,Rest[v]],
  SumQ[v] && MergeableFactorQ[bas,deg,First[v]] && MergeableFactorQ[bas,deg,Rest[v]]]]]


(* ::Subsection::Closed:: *)
(*TrigSimplify*)


TrigSimplifyQ::usage = "TrigSimplifyQ[u] returns True if TrigSimplify[u] actually simplifies u; else False.";
TrigSimplifyQ[u_] :=
  ActivateTrig[u]=!=TrigSimplify[u]


TrigSimplify::usage = "TrigSimplify[u] returns a bottom-up trig simplification of u.";
TrigSimplify[u_] :=
  ActivateTrig[TrigSimplifyRecur[u]]


TrigSimplifyRecur[u_] :=
  If[AtomQ[u],
    u,
  If[Head[u]===If,
    u,
  TrigSimplifyAux[Map[TrigSimplifyRecur,u]]]]

Clear[TrigSimplifyAux]


(* Basis: a*z^m+b*z^n == z^m*(a+b*z^(n-m)) *)
TrigSimplifyAux[u_.*(a_.*v_^m_.+b_.*v_^n_.)^p_] :=
  u*v^(m*p)*TrigSimplifyAux[a+b*v^(n-m)]^p /;
InertTrigQ[v] && IntegerQ[p] && RationalQ[m,n] && m<n


(* ::Subsubsection:: *)
(*Basis: Sin[z]^2+Cos[z]^2==1*)


(* ::Subsubsection:: *)
(*Basis: Sec[z]^2-Tan[z]^2==1*)


(* ::Subsubsection::Closed:: *)
(*Basis: Csc[z]^2-Cot[z]^2==1*)


TrigSimplifyAux[a_.*cos[u_]^2+b_.*sin[u_]^2+v_.] := a+v /; a===b

TrigSimplifyAux[a_.*sec[u_]^2+b_.*tan[u_]^2+v_.] := a+v /; a===-b

TrigSimplifyAux[a_.*csc[u_]^2+b_.*cot[u_]^2+v_.] := a+v /; a===-b


(* ::Subsubsection::Closed:: *)
(*Basis: a Cos[z]^2==a-a Sin[z]^2*)


TrigSimplifyAux[(a_.*cos[u_]^2+b_.*sin[u_]^2+v_.)^n_] :=
  ((b-a)*Sin[u]^2+a+v)^n


(* Basis: 1-Sin[z]^2 == Cos[z]^2 *)
TrigSimplifyAux[u_+v_.*sin[z_]^2+w_.] := u*Cos[z]^2+w /; u===-v

(* Basis: 1-Cos[z]^2 == Sin[z]^2 *)
TrigSimplifyAux[u_+v_.*cos[z_]^2+w_.] := u*Sin[z]^2+w /; u===-v

(* Basis: 1+Tan[z]^2 == Sec[z]^2 *)
TrigSimplifyAux[u_+v_.*tan[z_]^2+w_.] := u*Sec[z]^2+w /; u===v

(* Basis: 1+Cot[z]^2 == Csc[z]^2 *)
TrigSimplifyAux[u_+v_.*cot[z_]^2+w_.] := u*Csc[z]^2+w /; u===v

(* Basis: -1+Sec[z]^2 == Tan[z]^2 *)
TrigSimplifyAux[u_+v_.*sec[z_]^2+w_.] := v*Tan[z]^2+w /; u===-v

(* Basis: -1+Csc[z]^2 == Cot[z]^2 *)
TrigSimplifyAux[u_+v_.*csc[z_]^2+w_.] := v*Cot[z]^2+w /; u===-v


(* Basis: If a^2-b^2==0, Sin[z]^2/(a+b*Cos[z]) == 1/a-Cos[z]/b *)
TrigSimplifyAux[u_.*sin[v_]^2/(a_+b_.*cos[v_])] := u*(1/a - Cos[v]/b) /; EqQ[a^2-b^2,0]

(* Basis: If a^2-b^2==0, Cos[z]^2/(a+b*Sin[z]) == 1/a-Sin[z]/b *)
TrigSimplifyAux[u_.*cos[v_]^2/(a_+b_.*sin[v_])] := u*(1/a - Sin[v]/b) /; EqQ[a^2-b^2,0]


(* Basis: If n is an integer, Tan[z]^n/(a+b*Tan[z]^n) == 1/(b+a*Cot[z]^n) *)
TrigSimplifyAux[u_.*tan[v_]^n_./(a_+b_.*tan[v_]^n_.)] := u/(b+a*Cot[v]^n) /; IGtQ[n,0] && NonsumQ[a]

(* Basis: If n is an integer, Cot[z]^n/(a+b*Cot[z]^n) == 1/(b+a*Tan[z]^n) *)
TrigSimplifyAux[u_.*cot[v_]^n_./(a_+b_.*cot[v_]^n_.)] := u/(b+a*Tan[v]^n) /; IGtQ[n,0] && NonsumQ[a]

(* Basis: If n is an integer, Sec[z]^n/(a+b*Sec[z]^n) == 1/(b+a*Cos[z]^n) *)
TrigSimplifyAux[u_.*sec[v_]^n_./(a_+b_.*sec[v_]^n_.)] := u/(b+a*Cos[v]^n) /; IGtQ[n,0] && NonsumQ[a]

(* Basis: If n is an integer, Csc[z]^n/(a+b*Csc[z]^n) == 1/(b+a*Sin[z]^n) *)
TrigSimplifyAux[u_.*csc[v_]^n_./(a_+b_.*csc[v_]^n_.)] := u/(b+a*Sin[v]^n) /; IGtQ[n,0] && NonsumQ[a]


(* Basis: If n is an integer, Tan[z]^n/(a+b*Sec[z]^n) == Sin[z]^n/(b+a*Cos[z]^n) *)
TrigSimplifyAux[u_.*tan[v_]^n_./(a_+b_.*sec[v_]^n_.)] := u*Sin[v]^n/(b+a*Cos[v]^n) /; IGtQ[n,0] && NonsumQ[a]

(* Basis: If n is an integer, Cot[z]^n/(a+b*Csc[z]^n) == Cos[z]^n/(b+a*Sin[z]^n) *)
TrigSimplifyAux[u_.*cot[v_]^n_./(a_+b_.*csc[v_]^n_.)] := u*Cos[v]^n/(b+a*Sin[v]^n) /; IGtQ[n,0] && NonsumQ[a]


(* ::Subsubsection::Closed:: *)
(*Basis: a Sec[z]+b Tan[z]==Sec[z](a+b Sin[z])*)


TrigSimplifyAux[u_.*(a_.*sec[v_]^n_.+b_.*tan[v_]^n_.)^p_.] :=
  u*Sec[v]^(n*p)*(a+b*Sin[v]^n)^p /;
IntegersQ[n,p]

TrigSimplifyAux[u_.*(a_.*csc[v_]^n_.+b_.*cot[v_]^n_.)^p_.] :=
  u*Csc[v]^(n*p)*(a+b*Cos[v]^n)^p /;
IntegersQ[n,p]


(* ::Subsubsection::Closed:: *)
(*Basis: a Tan[z]+b Sin[z]==Tan[z](a+b Cos[z])*)


TrigSimplifyAux[u_.*(a_.*tan[v_]^n_.+b_.*sin[v_]^n_.)^p_.] :=
  u*Tan[v]^(n*p)*(a+b*Cos[v]^n)^p /;
IntegersQ[n,p]

TrigSimplifyAux[u_.*(a_.*cot[v_]^n_.+b_.*cos[v_]^n_.)^p_.] :=
  u*Cot[v]^(n*p)*(a+b*Sin[v]^n)^p /;
IntegersQ[n,p]


(* ::Subsubsection:: *)
(*Basis: a+b Tan[z]+c Sec[z]==(c+b Sin[z]+a Cos[z])/Cos[z]*)


(* ::Subsubsection:: *)
(*Basis: a+b Tan[z]+c Sec[z]==Sec[z] (c+b Sin[z]+a Cos[z])*)


(* ::Subsubsection:: *)
(*Basis: a+b Cot[z]+c Csc[z]==(c+b Cos[z]+a Sin[z])/Sin[z]*)


(* ::Subsubsection::Closed:: *)
(*Basis: a+b Cot[z]+c Csc[z]==Csc[z] (c+b Cos[z]+a Sin[z])*)


TrigSimplifyAux[u_.*cos[v_]^m_.*(a_.+b_.*tan[v_]^n_.+c_.*sec[v_]^n_.)^p_.] :=
  u*Cos[v]^(m-n*p)*(c+b*Sin[v]^n+a*Cos[v]^n)^p /;
IntegersQ[m,n,p]

TrigSimplifyAux[u_.*sec[v_]^m_.*(a_.+b_.*tan[v_]^n_.+c_.*sec[v_]^n_.)^p_.] :=
  u*Sec[v]^(m+n*p)*(c+b*Sin[v]^n+a*Cos[v]^n)^p /;
IntegersQ[m,n,p]

TrigSimplifyAux[u_.*sin[v_]^m_.*(a_.+b_.*cot[v_]^n_.+c_.*csc[v_]^n_.)^p_.] :=
  u*Sin[v]^(m-n*p)*(c+b*Cos[v]^n+a*Sin[v]^n)^p /;
IntegersQ[m,n,p]

TrigSimplifyAux[u_.*csc[v_]^m_.*(a_.+b_.*cot[v_]^n_.+c_.*csc[v_]^n_.)^p_.] :=
  u*Csc[v]^(m+n*p)*(c+b*Cos[v]^n+a*Sin[v]^n)^p /;
IntegersQ[m,n,p]


(* ::Subsubsection:: *)
(*Basis: If n\[Element]\[DoubleStruckCapitalZ], then  a Csc[z]^m+b Sin[z]^n==(a+b Sin[z]^(m+n))/Sin[z]^m*)


(* ::Subsubsection::Closed:: *)
(*Basis: If n\[Element]\[DoubleStruckCapitalZ], then  a Sec[z]^m+b Cos[z]^n==(a+b Cos[z]^(m+n))/Cos[z]^m*)


TrigSimplifyAux[u_.*(a_.*csc[v_]^m_.+b_.*sin[v_]^n_.)^p_.] :=
  If[EqQ[m+n-2,0] && EqQ[a+b,0],
    u*(a*Cos[v]^2/Sin[v]^m)^p,
  u*((a+b*Sin[v]^(m+n))/Sin[v]^m)^p] /;
IntegersQ[m,n]

TrigSimplifyAux[u_.*(a_.*sec[v_]^m_.+b_.*cos[v_]^n_.)^p_.] :=
  If[EqQ[m+n-2,0] && EqQ[a+b,0],
    u*(a*Sin[v]^2/Cos[v]^m)^p,
  u*((a+b*Cos[v]^(m+n))/Cos[v]^m)^p] /;
IntegersQ[m,n]


(* (* Basis: Csc[z]+Cot[z] == Cot[z/2] *)
TrigSimplifyAux[(a_.*csc[v_]+b_.*cot[v_])^n_] := a^n*Cot[v/2]^n /; EvenQ[n] && EqQ[a-b,0]

(* Basis: Csc[z]-Cot[z] == Tan[z/2] *)
TrigSimplifyAux[(a_.*csc[v_]+b_.*cot[v_])^n_] := a^n*Tan[v/2]^n /; EvenQ[n] && EqQ[a+b,0] *)


(* (* Basis: Sin[z]*(a+b*Cot[z]) == a*Sin[z] + b*Cos[z] *)
(* TrigSimplifyAux[u_*sin[v_]^m_.*(a_.+b_.*cot[v_]^2)^p_.] :=
  u*(b*Cos[v]^2+a*Sin[v]^2)^p /;
IntegersQ[m,p] && m==2*p *)

(* Basis: a+b*Cot[z] == (b*Cos[z]+a*Sin[z])/Sin[z] *)
TrigSimplifyAux[u_.*sin[v_]^m_.*(a_.+b_.*cot[v_]^n_.)^p_.] :=
  u*Sin[v]^(m-n*p)*(b*Cos[v]^n+a*Sin[v]^n)^p /;
IntegersQ[m,n,p]

(* Basis: Cos[z]*(a+b*Tan[z]) == a*Cos[z] + b*Sin[z] *)
(* TrigSimplifyAux[u_*cos[v_]^m_.*(a_.+b_.*tan[v_]^2)^p_.] :=
  u*(b*Sin[v]^2+a*Cos[v]^2)^p /;
IntegersQ[m,p] && m==2*p *)

(* Basis: a+b*Tan[z] == (b*Sin[z]+a*Cos[z])/Cos[z] *)
TrigSimplifyAux[u_.*cos[v_]^m_.*(a_.+b_.*tan[v_]^n_.)^p_.] :=
  u*Cos[v]^(m-n*p)*(b*Sin[v]^n+a*Cos[v]^n)^p /;
IntegersQ[m,n,p]

(* Basis: (a+b*Tan[z])/Sec[z] == a*Cos[z] + b*Sin[z] *)
TrigSimplifyAux[u_*sec[v_]^m_.*(a_.+b_.*tan[v_]^2)^p_.] :=
  u*(b*Sin[v]^2+a*Cos[v]^2)^p /;
IntegersQ[m,p] && m+2*p==0

(* Basis: (a+b*Cot[z])/Csc[z] == a*Sin[z] + b*Cos[z] *)
TrigSimplifyAux[u_*csc[v_]^m_.*(a_.+b_.*cot[v_]^2)^p_.] :=
  u*(b*Cos[v]^2+a*Sin[v]^2)^p /;
IntegersQ[m,p] && m+2*p==0 *)


(* (* Basis: If n is an integer, Sin[z]^(-n)*(a*Cos[z]^n+b*Sin[z]^n) == b+a*Cot[z]^n *)
TrigSimplifyAux[sin[v_]^m_.*(a_.*cos[v_]^n_.+b_.*sin[v_]^n_.)^p_] :=
  (b+a*Cot[v]^n)^p /;
IntegersQ[m,n,p] && n>0 && p<0 && m==-n*p

(* Basis: If n is an integer, Cos[z]^(-n)*(a*Cos[z]^n+b*Sin[z]^n) == a+b*Tan[z]^n *)
TrigSimplifyAux[cos[v_]^m_.*(a_.*cos[v_]^n_.+b_.*sin[v_]^n_.)^p_] :=
  (a+b*Tan[v]^n)^p /;
IntegersQ[m,n,p] && n>0 && p<0 && m==-n*p *)


(* (* Basis: If a^2+b^2=0, 1/(a*Cos[z] + b*Sin[z]) == Cos[z]/a + Sin[z]/b *)
TrigSimplifyAux[(a_.*cos[v_]+b_.*sin[v_])^n_] :=
  (Cos[v]/a + Sin[v]/b)^(-n) /;
IntegerQ[n] && n<0 && EqQ[a^2+b^2,0] *)


TrigSimplifyAux[u_] := u


(* ::Subsubsection::Closed:: *)
(*Basis: Tan[z] Tan[2 z]==-1+Sec[2 z]*)


TrigSimplifyAux[u_.*(c_.*tan[v_]^n_.*tan[w_]^n_.)^p_.] :=
  u*((-c+c*sec[w])^n)^p /;
IntegerQ[n] && EqQ[w,2*v]


(* ::Subsection::Closed:: *)
(*IntPart[u]*)


IntPart::usage = "IntPart[u] returns the sum of the integer terms of u.";
IntPart[m_*u_,n_:1] :=
  IntPart[u,m*n] /;
RationalQ[m]

IntPart[u_,n_:1] :=
  If[RationalQ[u],
    IntegerPart[n*u],
  If[SumQ[u],
    Map[Function[IntPart[#,n]],u],
  0]]


(* ::Subsection::Closed:: *)
(*FracPart[u]*)


FracPart::usage = "FracPart[u] returns the sum of the non-integer terms of u.";
FracPart[m_*u_,n_:1] :=
  FracPart[u,m*n] /;
RationalQ[m]

FracPart[u_,n_:1] :=
  If[RationalQ[u],
    FractionalPart[n*u],
  If[SumQ[u],
    Map[Function[FracPart[#,n]],u],
  n*u]]


(* ::Section::Closed:: *)
(*Factoring functions*)


(* ::Subsection::Closed:: *)
(*ContentFactor*)


(* Basis: a*b+a*c == a*(b+c) *)
ContentFactor::usage = "ContentFactor[expn] returns expn with the content of sum factors factored out.";
ContentFactor[expn_] :=
  TimeConstrained[ContentFactorAux[expn],$TimeLimit,expn];

ContentFactorAux[expn_] :=
  If[AtomQ[expn],
    expn,
  If[IntegerPowerQ[expn],
    If[SumQ[expn[[1]]] && NumericFactor[expn[[1,1]]]<0,
      (-1)^expn[[2]] * ContentFactorAux[-expn[[1]]]^expn[[2]],
    ContentFactorAux[expn[[1]]]^expn[[2]]],
  If[ProductQ[expn],
    Module[{num=1,tmp},
    tmp=Map[Function[If[SumQ[#] && NumericFactor[#[[1]]]<0, num=-num; ContentFactorAux[-#], ContentFactorAux[#]]], expn];
    num*UnifyNegativeBaseFactors[tmp]],
  If[SumQ[expn],
    With[{lst=CommonFactors[Apply[List,expn]]},
    If[lst[[1]]===1 || lst[[1]]===-1,
      expn,
    lst[[1]]*Apply[Plus,Rest[lst]]]],
  expn]]]]


(* FWIW: This simplification should be done automatically by the host CAS! *)
UnifyNegativeBaseFactors::usage = "UnifyNegativeBaseFactors[u] returns u with factors of the form (-v)^m and v^n where n is an integer replaced with (-1)^n*(-v)^(m+n).";
UnifyNegativeBaseFactors[u_.*(-v_)^m_*v_^n_.] :=
  UnifyNegativeBaseFactors[(-1)^n*u*(-v)^(m+n)] /;
IntegerQ[n]

UnifyNegativeBaseFactors[u_] :=
  u


CommonFactors::usage = "If lst is a list of n terms, CommonFactors[lst] returns a n+1-element list whose first element is the product of the factors common to all terms of lst, and whose remaining elements are quotients of each term divided by the common factor.";
CommonFactors[lst_] :=
  Module[{lst1,lst2,lst3,lst4,common,base,num},
  lst1=Map[NonabsurdNumberFactors,lst];
  lst2=Map[AbsurdNumberFactors,lst];
  num=Apply[AbsurdNumberGCD,lst2];
  common=num;
  lst2=Map[Function[#/num],lst2];
  While[True,
    lst3=Map[LeadFactor,lst1];
    ( If[Apply[SameQ,lst3],
        common=common*lst3[[1]];
        lst1=Map[RemainingFactors,lst1],
      If[EveryQ[Function[LogQ[#] && IntegerQ[First[#]] && First[#]>0],lst3] &&
           EveryQ[RationalQ,lst4=Map[Function[FullSimplify[#/First[lst3]]],lst3]],
        num=Apply[GCD,lst4];
        common=common*Log[(First[lst3][[1]])^num];
        lst2=Map2[Function[#1*#2/num],lst2,lst4];
        lst1=Map[RemainingFactors,lst1],
      lst4=Map[LeadDegree,lst1];
      If[Apply[SameQ,Map[LeadBase,lst1]] && EveryQ[RationalQ,lst4],
        num=Smallest[lst4];
        base=LeadBase[lst1[[1]]];
        ( If[num!=0,
            common=common*base^num] );
        lst2=Map2[Function[#1*base^(#2-num)],lst2,lst4];
        lst1=Map[RemainingFactors,lst1],
      If[Length[lst1]==2 && EqQ[LeadBase[lst1[[1]]]+LeadBase[lst1[[2]]],0] &&
         NeQ[lst1[[1]],1] && IntegerQ[lst4[[1]]] && FractionQ[lst4[[2]]],
        num=Min[lst4];
        base=LeadBase[lst1[[2]]];
        ( If[num!=0,
            common=common*base^num] );
        lst2={lst2[[1]]*(-1)^lst4[[1]],lst2[[2]]};
        lst2=Map2[Function[#1*base^(#2-num)],lst2,lst4];
        lst1=Map[RemainingFactors,lst1],
      If[Length[lst1]==2 && EqQ[LeadBase[lst1[[1]]]+LeadBase[lst1[[2]]],0] &&
         NeQ[lst1[[2]],1] && IntegerQ[lst4[[2]]] && FractionQ[lst4[[1]]],
        num=Min[lst4];
        base=LeadBase[lst1[[1]]];
        ( If[num!=0,
            common=common*base^num] );
        lst2={lst2[[1]],lst2[[2]]*(-1)^lst4[[2]]};
        lst2=Map2[Function[#1*base^(#2-num)],lst2,lst4];
        lst1=Map[RemainingFactors,lst1],
      num=MostMainFactorPosition[lst3];
      lst2=ReplacePart[lst2,lst3[[num]]*lst2[[num]],num];
      lst1=ReplacePart[lst1,RemainingFactors[lst1[[num]]],num]]]]]] );
    If[EveryQ[Function[#===1],lst1],
      Return[Prepend[lst2,common]]]]]


MostMainFactorPosition[lst_List] :=
  Module[{factor=1,num=1,ii},
  Do[If[FactorOrder[lst[[ii]],factor]>0,factor=lst[[ii]];num=ii],{ii,Length[lst]}];
  num]


FactorOrder[u_,v_] :=
  If[u===1,
    If[v===1,
      0,
    -1],
  If[v===1,
    1,
  Order[u,v]]]


Smallest[num1_,num2_] :=
  If[num1>0,
    If[num2>0,
      Min[num1,num2],
    0],
  If[num2>0,
    0,
  Max[num1,num2]]]

Smallest[lst_List] :=
  Module[{num=lst[[1]]},
  Scan[Function[num=Smallest[num,#]],Rest[lst]];
  num]


(* ::Subsection::Closed:: *)
(*MonomialFactor*)


MonomialFactor::usage = "MonomialFactor[u,x] returns the list {n,v} where x^n*v==u and n is free of x.";
MonomialFactor[u_,x_Symbol] :=
  If[AtomQ[u],
    If[u===x,
      {1,1},
    {0,u}],
  If[PowerQ[u],
    If[IntegerQ[u[[2]]],
      With[{lst=MonomialFactor[u[[1]],x]},
      {lst[[1]]*u[[2]],lst[[2]]^u[[2]]}],
    If[u[[1]]===x && FreeQ[u[[2]],x],
      {u[[2]],1},
    {0,u}]],
  If[ProductQ[u],
    With[{lst1=MonomialFactor[First[u],x],lst2=MonomialFactor[Rest[u],x]},
    {lst1[[1]]+lst2[[1]],lst1[[2]]*lst2[[2]]}],
  If[SumQ[u],
    Module[{lst,deg},
    lst=Map[Function[MonomialFactor[#,x]],Apply[List,u]];
    deg=lst[[1,1]];
    Scan[Function[deg=MinimumDegree[deg,#[[1]]]],Rest[lst]];
    If[EqQ[deg,0] || RationalQ[deg] && deg<0,
      {0,u},
    {deg,Apply[Plus,Map[Function[x^(#[[1]]-deg)*#[[2]]],lst]]}]],
  {0,u}]]]]


MinimumDegree[deg1_,deg2_] :=
  If[RationalQ[deg1],
    If[RationalQ[deg2],
      Min[deg1,deg2],
    deg1],
  If[RationalQ[deg2],
    deg2,
  With[{deg=Simplify[deg1-deg2]},
  If[RationalQ[deg],
    If[deg>0,
      deg2,
    deg1],
  If[OrderedQ[{deg1,deg2}],
    deg1,
  deg2]]]]]


(* ::Subsection::Closed:: *)
(*ConstantFactor*)


ConstantFactor::usage = "ConstantFactor[u,x] returns a 2-element list of the factors of u[x] free of x and the factors not free of u[x].  Common constant factors of the terms of sums are also collected.";
ConstantFactor[u_,x_Symbol] :=
  If[FreeQ[u,x],
    {u,1},
  If[AtomQ[u],
    {1,u},
  If[PowerQ[u] && FreeQ[u[[2]],x],
    Module[{lst=ConstantFactor[u[[1]],x],tmp},
    If[IntegerQ[u[[2]]],
      {lst[[1]]^u[[2]],lst[[2]]^u[[2]]},
    tmp=PositiveFactors[lst[[1]]];
    If[tmp===1,
      {1,u},
    {tmp^u[[2]],(NonpositiveFactors[lst[[1]]]*lst[[2]])^u[[2]]}]]],
  If[ProductQ[u],
    With[{lst=Map[Function[ConstantFactor[#,x]],Apply[List,u]]},
    {Apply[Times,Map[First,lst]],Apply[Times,Map[Function[#[[2]]],lst]]}],
  If[SumQ[u],
    With[{lst1=Map[Function[ConstantFactor[#,x]],Apply[List,u]]},
    If[Apply[SameQ,Map[Function[#[[2]]],lst1]],
      {Apply[Plus,Map[First,lst1]],lst1[[1,2]]},
    With[{lst2=CommonFactors[Map[First,lst1]]},
    {First[lst2],Apply[Plus,Map2[Times,Rest[lst2],Map[Function[#[[2]]],lst1]]]}]]],
  {1,u}]]]]]


PositiveFactors::usage = "PositiveFactors[u] returns the positive factors of u.";
PositiveFactors[u_] :=
  If[EqQ[u,0],
    1,
  If[RationalQ[u],
    Abs[u],
  If[GtQ[u,0],
    u,
  If[ProductQ[u],
    Map[PositiveFactors,u],
  1]]]]


NonpositiveFactors::usage = "NonpositiveFactors[u] returns the nonpositive factors of u.";
NonpositiveFactors[u_] :=
  If[EqQ[u,0],
    u,
  If[RationalQ[u],
    Sign[u],
  If[GtQ[u,0],
    1,
  If[ProductQ[u],
    Map[NonpositiveFactors,u],
  u]]]]


(* ::Section::Closed:: *)
(*Generalized polynomial functions*)


(* ::Subsection::Closed:: *)
(*PolynomialInQ*)


(* ::Item:: *)
(*If u is a polynomial in v[x], PolynomialInQ[u,v,x] returns True; else it returns False.*)


PolynomialInQ[u_,v_,x_Symbol] :=
  PolynomialInAuxQ[u,NonfreeFactors[NonfreeTerms[v,x],x],x]


PolynomialInAuxQ[u_,v_,x_] :=
  If[u===v,
    True,
  If[AtomQ[u],
    u=!=x,
  If[PowerQ[u],
    If[PowerQ[v] && u[[1]]===v[[1]],
      IGtQ[u[[2]]/v[[2]],0],
    IGtQ[u[[2]],0] && PolynomialInAuxQ[u[[1]],v,x]],
  If[SumQ[u] || ProductQ[u],
    Catch[Scan[Function[If[Not[PolynomialInAuxQ[#,v,x]],Throw[False]]],u];True],
  False]]]]


(* ::Subsection::Closed:: *)
(*ExponentIn*)


(* ::Item:: *)
(*If u[v] is a polynomial in w, GeneralizedExponent[u,v,x] returns the degree of the u[x].*)


ExponentIn[u_,v_,x_Symbol] :=
  ExponentInAux[u,NonfreeFactors[NonfreeTerms[v,x],x],x]


ExponentInAux[u_,v_,x_] :=
  If[u===v,
    1,
  If[AtomQ[u],
    0,
  If[PowerQ[u],
    If[PowerQ[v] && u[[1]]===v[[1]],
      u[[2]]/v[[2]],
    u[[2]]*ExponentInAux[u[[1]],v,x]],
  If[ProductQ[u],
    Apply[Plus,Map[Function[ExponentInAux[#,v,x]],Apply[List,u]]],
  Apply[Max,Map[Function[ExponentInAux[#,v,x]],Apply[List,u]]]]]]]


(* ::Subsection::Closed:: *)
(*PolynomialInSubst*)


(* ::Item:: *)
(*If u is a polynomial in v[x], PolynomialInSubst[u,v,x] returns the polynomial u in x.*)


PolynomialInSubst[u_,v_,x_Symbol] :=
  With[{w=NonfreeTerms[v,x]},
  ReplaceAll[PolynomialInSubstAux[u,NonfreeFactors[w,x],x],{x->(x-FreeTerms[v,x])/FreeFactors[w,x]}]]


PolynomialInSubstAux[u_,v_,x_] :=
  If[u===v,
    x,
  If[AtomQ[u],
    u,
  If[PowerQ[u],
    If[PowerQ[v] && u[[1]]===v[[1]],
      x^(u[[2]]/v[[2]]),
    PolynomialInSubstAux[u[[1]],v,x]^u[[2]]],
  Map[Function[PolynomialInSubstAux[#,v,x]],u]]]]


(* ::Subsection::Closed:: *)
(*PolynomialDivide*)


PolynomialDivide::usage = "If u and v are polynomials in x, PolynomialDivide[u,v,x] returns the polynomial quotient of u and v plus the polynomial remainder divided by v.
If u[w] and v[w] are polynomials in w, PolynomialDivide[u,v,w,x] returns the polynomial quotient of u[x] and v[x] plus the polynomial remainder divided by v[x] with x replaced by w.";
PolynomialDivide[u_,v_,x_Symbol] :=
  Module[{quo=PolynomialQuotient[u,v,x],rem=PolynomialRemainder[u,v,x],free,monomial},
  quo=Apply[Plus,Map[Function[Simp[Together[Coefficient[quo,x,#]*x^#],x]],Exponent[quo,x,List]]];
  rem=Together[rem];
  free=FreeFactors[rem,x];
  rem=NonfreeFactors[rem,x];
  monomial=x^Exponent[rem,x,Min];
  If[NegQ[Coefficient[rem,x,0]], monomial=-monomial];
  rem=Apply[Plus,Map[Function[Simp[Together[Coefficient[rem,x,#]*x^#/monomial],x]],Exponent[rem,x,List]]];
(*rem=Simplify[rem]; *)
  If[BinomialQ[v,x],
    quo+free*monomial*rem/ExpandToSum[v,x],
  quo+free*monomial*rem/v]]


PolynomialDivide[u_,v_,w_,x_Symbol] :=
  ReplaceAll[PolynomialDivide[PolynomialInSubst[u,w,x],PolynomialInSubst[v,w,x],x],{x->w}]


(* ::Section::Closed:: *)
(*Expansion functions*)


(* ::Subsection::Closed:: *)
(*ExpandToSum[u,x]*)


(* ::Item:: *)
(*ExpandToSum[u,v,x] returns v expanded into a sum of monomials of x and distributes u over v.*)


ExpandToSum[u_,v_,x_Symbol] :=
  Module[{w=ExpandToSum[v,x],r},
  r=NonfreeTerms[w,x];
  If[SumQ[r],
    u*FreeTerms[w,x]+Map[Function[MergeMonomials[u*#,x]],r],
  u*FreeTerms[w,x]+MergeMonomials[u*r,x]]]


(* ::Item:: *)
(*ExpandToSum[u,x] returns u expanded into a sum of monomials of x.*)


ExpandToSum[u_,x_Symbol] :=
  If[PolyQ[u,x],
    Simp[Apply[Plus,Map[Function[Coeff[u,x,#]*x^#], Expon[u,x,List]]],x],
  If[BinomialQ[u,x],
    Function[#[[1]] + #[[2]]*x^#[[3]]][BinomialParts[u,x]],
  If[TrinomialQ[u,x],
    Function[#[[1]] + #[[2]]*x^#[[4]] + #[[3]]*x^(2*#[[4]])][TrinomialParts[u,x]],
  If[GeneralizedBinomialQ[u,x],
    Function[#[[1]]*x^#[[4]] + #[[2]]*x^#[[3]]][GeneralizedBinomialParts[u,x]],
  If[GeneralizedTrinomialQ[u,x],
    Function[#[[1]]*x^#[[5]] + #[[2]]*x^#[[4]] + #[[3]]*x^(2*#[[4]]-#[[5]])][GeneralizedTrinomialParts[u,x]],
  Print["Warning: Unrecognized expression for expansion ",u];
  Expand[u,x]]]]]]


(* ::Subsection::Closed:: *)
(*ExpandTrig[u,x]*)


ExpandTrig[u_,x_Symbol] :=
  ActivateTrig[ExpandIntegrand[u,x]]

ExpandTrig[u_,v_,x_Symbol] :=
  With[{w=ExpandTrig[v,x],z=ActivateTrig[u]},
  If[SumQ[w],
    Map[Function[z*#],w],
  z*w]]


(* ::Subsection::Closed:: *)
(*ExpandIntegrand[u,v,x]*)


Clear[ExpandIntegrand];


ExpandIntegrand[(a_.+b_.*Log[c_.*x_^n_.])^p_.,x_^m_.*(d_+e_.*x_^r_.)^q_.,x_Symbol] :=
  DistributeOverTerms[(a+b*Log[c*x^n])^p,CollectRecipTerms[ExpandIntegrand[x^m*(d+e*x^r)^q,x],x],x] /;
FreeQ[{a,b,c,d,e,n},x] && IGtQ[p,0] && IGtQ[r,0] && ILtQ[m,0] && ILtQ[q,0]   && EqQ[r,1]


ExpandIntegrand[u_,v_,x_Symbol] :=
  DistributeOverTerms[u,ExpandIntegrand[v,x],x]


DistributeOverTerms[u_,v_,x_Symbol] :=
  With[{w=NonfreeTerms[v,x]},
  u*FreeTerms[v,x] + If[SumQ[w],
    Map[Function[MergeMonomials[u*#,x]],w],
    MergeMonomials[u*w,x]]]


CollectRecipTerms[u_.+A_./x_+B_./(d_+e_.*x_),x_Symbol] :=
  u+A*d/(x*(d+e*x)) /;
FreeQ[{A,B,d,e},x] && EqQ[B+A*e,0]

CollectRecipTerms[u_,x_Symbol] := u


(* ::Subsection::Closed:: *)
(*ExpandIntegrand[u,x]*)


(* ExpandIntegrand[u_,x_Symbol] :=
  With[{nn=FunctionOfPower[u,x]},
  ReplaceAll[ExpandIntegrand[SubstFor[x^nn,u,x],x],x->x^nn] /;
 nn!=1] *)


ExpandIntegrand[u_^p_.,x_Symbol] :=
  If[EqQ[p,1],
    ExpandCleanup[u,x],
  ExpandCleanup[Expand[u^p,x],x]] /;
SumQ[u] && IGtQ[p,0]


ExpandIntegrand[(a_+b_.*x_^n_)^p_.,x_Symbol] :=
  ExpandIntegrand[x^(n*p)*(b+a*x^(-n))^p,x] /;
IntegerQ[p] && ILtQ[n,0]

ExpandIntegrand[x_^m_.*(a_+b_.*x_^n_)^p_.,x_Symbol] :=
  ExpandIntegrand[x^(m+n*p)*(b+a*x^(-n))^p,x] /;
IntegerQ[p] && ILtQ[n,0]


ExpandIntegrand[Px_.*x_^m_*(b_.*x_^n_.+c_.*x_^n1_)^p_.,x_Symbol] :=
  ExpandIntegrand[Px*x^(m+n*p)*(b+c*x)^p,x] /;
FreeQ[{b,c,m},x] && PolyQ[Px,x] && IGtQ[n,0] && EqQ[n1,n+1] && IntegerQ[p]

ExpandIntegrand[Px_.*(b_.*x_^n_.+c_.*x_^n1_)^p_.,x_Symbol] :=
  ExpandIntegrand[Px*x^(n*p)*(b+c*x)^p,x] /;
FreeQ[{b,c},x] && PolyQ[Px,x] && IGtQ[n,0] && EqQ[n1,n+1] && IntegerQ[p]


ExpandIntegrand[(a_.+b_.*F_^u_)^p_.*(c_.+d_.*F_^v_)^q_.,x_Symbol] :=
  With[{k=Simplify[u/v]},
  ReplaceAll[ExpandIntegrand[(a+b*x^Numerator[k])^p*(c+d*x^Denominator[k])^q,x],x->F^(v/Denominator[k])] /;
 RationalQ[k]] /;
FreeQ[{F,a,b,c,d},x] && IntegersQ[p,q]


(* ::Subsubsection::Closed:: *)
(*Basis: (a+b x)^m/(c+d x)==(b (a+b x)^(m-1))/d+((a d-b c) (a+b x)^(m-1))/(d (c+d x))*)


ExpandIntegrand[(a_.+b_.*x_)^m_.*F_^(e_.*(c_.+d_.*x_)^n_.)/(g_.+h_.*x_),x_Symbol] :=
  With[{tmp=a*h-b*g},
  Module[{k},
  SimplifyTerm[tmp^m/h^m,x]*F^(e*(c+d*x)^n)/(g+h*x) +
	Sum[SimplifyTerm[b*tmp^(k-1)/h^k,x]*F^(e*(c+d*x)^n)*(a+b*x)^(m-k),{k,1,m}]]] /;
FreeQ[{F,a,b,c,d,e,g,h},x] && IGtQ[m,0] && EqQ[b*c-a*d,0]


ExpandIntegrand[x_^m_.*(e_+f_.*x_)^p_.*F_^(b_.*(c_.+d_.*x_)^n_.),x_Symbol] :=
  If[IGtQ[m,0] && IGtQ[p,0] && m<=p && (EqQ[n,1] || EqQ[d*e-c*f,0]),
    ExpandLinearProduct[(e+f*x)^p*F^(b*(c+d*x)^n),x^m,e,f,x],
  If[IGtQ[p,0],
    Distribute[x^m*F^(b*(c+d*x)^n)*Expand[(e+f*x)^p,x],Plus,Times],
  ExpandIntegrand[F^(b*(c+d*x)^n),x^m*(e+f*x)^p,x]]] /;
FreeQ[{F,b,c,d,e,f,m,n,p},x]


ExpandIntegrand[x_^m_.*(e_+f_.*x_)^p_.*F_^(a_.+b_.*(c_.+d_.*x_)^n_.),x_Symbol] :=
  If[IGtQ[m,0] && IGtQ[p,0] && m<=p && (EqQ[n,1] || EqQ[d*e-c*f,0]),
    ExpandLinearProduct[(e+f*x)^p*F^(a+b*(c+d*x)^n),x^m,e,f,x],
  If[IGtQ[p,0],
    Distribute[x^m*F^(a+b*(c+d*x)^n)*Expand[(e+f*x)^p,x],Plus,Times],
  ExpandIntegrand[F^(a+b*(c+d*x)^n),x^m*(e+f*x)^p,x]]] /;
FreeQ[{F,a,b,c,d,e,f,m,n,p},x]


ExpandIntegrand[u_.*(a_+b_.*F_^v_)^m_.*(c_+d_.*F_^v_)^n_,x_Symbol] :=
  With[{w=ReplaceAll[ExpandIntegrand[(a+b*x)^m*(c+d*x)^n,x],x->F^v]},
  Map[Function[u*#],w] /;
 SumQ[w]] /;
FreeQ[{F,a,b,c,d},x] && IntegersQ[m,n] && n<0


ExpandIntegrand[u_*(a_.+b_.*x_)^m_.*F_^(e_.*(c_.+d_.*x_)^n_.),x_Symbol] :=
  With[{v=ExpandIntegrand[u*(a+b*x)^m,x]},
  Distribute[F^(e*(c+d*x)^n)*v,Plus,Times] /;
 SumQ[v]] /;
FreeQ[{F,a,b,c,d,e,m,n},x] && PolynomialQ[u,x]


ExpandIntegrand[u_*(a_.+b_.*x_)^m_.*Log[c_.*(d_.+e_.*x_^n_.)^p_.],x_Symbol] :=
  ExpandIntegrand[Log[c*(d+e*x^n)^p],u*(a+b*x)^m,x] /;
FreeQ[{a,b,c,d,e,m,n,p},x] && PolynomialQ[u,x]


ExpandIntegrand[u_*F_^(e_.*(c_.+d_.*x_)^n_.),x_Symbol] :=
  If[EqQ[n,1],
    ExpandIntegrand[F^(e*(c+d*x)^n),u,x],
  ExpandLinearProduct[F^(e*(c+d*x)^n),u,c,d,x]] /;
FreeQ[{F,c,d,e,n},x] && PolynomialQ[u,x]


ExpandIntegrand[F_[u_]^m_.*(a_+b_.*G_[u_])^n_.,x_Symbol] :=
  ReplaceAll[ExpandIntegrand[(a+b*x)^n/x^m,x],x->G[u]] /;
FreeQ[{a,b},x] && IntegersQ[m,n] && F[u]*G[u]===1


ExpandIntegrand[u_*(a_.+b_.*Log[c_.*(d_.*(e_.+f_.*x_)^p_.)^q_.])^n_,x_Symbol] :=
  ExpandLinearProduct[(a+b*Log[c*(d*(e+f*x)^p)^q])^n,u,e,f,x] /;
FreeQ[{a,b,c,d,e,f,n,p,q},x] && PolynomialQ[u,x]


ExpandIntegrand[u_*(a_.+b_.*F_[c_.+d_.*x_])^n_,x_Symbol] :=
  ExpandLinearProduct[(a+b*F[c+d*x])^n,u,c,d,x] /;
FreeQ[{a,b,c,d,n},x] && PolynomialQ[u,x] && MemberQ[{ArcSin,ArcCos,ArcSinh,ArcCosh},F]


(* ::Subsubsection::Closed:: *)
(*Basis: 1/(a x^n+b Sqrt[c+d x^(2 n)])==(a x^n-b Sqrt[c+d x^(2 n)])/(-b^2 c+(a^2-b^2 d) x^(2 n))*)


ExpandIntegrand[u_./(a_.*x_^n_+b_.*Sqrt[c_+d_.*x_^j_]),x_Symbol] :=
  ExpandIntegrand[u*(a*x^n-b*Sqrt[c+d*x^(2*n)])/(-b^2*c+(a^2-b^2*d)*x^(2*n)),x] /;
FreeQ[{a,b,c,d,n},x] && EqQ[j,2*n]


(* ::Subsubsection::Closed:: *)
(*Basis: (a+b x)^m/(c+d x)==(b (a+b x)^(m-1))/d+((a d-b c) (a+b x)^(m-1))/(d (c+d x))*)


ExpandIntegrand[(a_+b_.*x_)^m_/(c_+d_.*x_),x_Symbol] :=
  If[RationalQ[a,b,c,d],
    ExpandExpression[(a+b*x)^m/(c+d*x),x],
  With[{tmp=a*d-b*c},
  Module[{k},
  SimplifyTerm[tmp^m/d^m,x]/(c+d*x) + Sum[SimplifyTerm[b*tmp^(k-1)/d^k,x]*(a+b*x)^(m-k),{k,1,m}]]]] /;
FreeQ[{a,b,c,d},x] && IGtQ[m,0]


(* ::Subsubsection::Closed:: *)
(*Basis: ((a+b x)^m (A+B x))/(c+d x)==(B (a+b x)^m)/d+((A d-B c) (a+b x)^m)/(d (c+d x))*)


ExpandIntegrand[(a_+b_.*x_)^m_.*(A_+B_.*x_)/(c_+d_.*x_),x_Symbol] :=
  If[RationalQ[a,b,c,d,A,B],
    ExpandExpression[(a+b*x)^m*(A+B*x)/(c+d*x),x],
(*  PolynomialDivide[(a+b*x)^m*(A+B*x),c+d*x,x], *)
  Module[{tmp1,tmp2},
  tmp1=(A*d-B*c)/d;
  tmp2=ExpandIntegrand[(a+b*x)^m/(c+d*x),x];
  tmp2=If[SumQ[tmp2], Map[Function[SimplifyTerm[tmp1*#,x]],tmp2], SimplifyTerm[tmp1*tmp2,x]];
  SimplifyTerm[B/d,x]*(a+b*x)^m + tmp2]] /;
FreeQ[{a,b,c,d,A,B},x] && IGtQ[m,0]


ExpandIntegrand[u_*(a_+b_.*x_)^m_.*(c_+d_.*x_)^n_.,x_Symbol] :=
  ExpandIntegrand[(c+d*x)^n,u*(a+b*x)^m,x] /;
FreeQ[{a,b,c,d,m,n},x] && PolynomialQ[u,x] && Not[IntegerQ[m]] && IGtQ[n-m,0]


ExpandIntegrand[u_*(a_+b_.*x_)^m_.,x_Symbol] :=
  With[{sum1=ExpandLinearProduct[(a+b*x)^m,u,a,b,x]},
  If[Not[IntegerQ[m]] || m>2 && LinearQ[u,x],
    sum1,
  With[{sum2=ExpandExpression[u*(a+b*x)^m,x]},
(* With[{sum2=If[EqQ[m,-1], PolynomialDivide[u,a+b*x,x], ExpandExpression[u*(a+b*x)^m,x]]}, *)
  If[SumQ[sum2],
    If[m>0,
      If[Length[sum2]<=Exponent[u,x]+2 || LeafCount[sum2]<=2/3*LeafCount[sum1],
        sum2,
      sum1],
    If[LeafCount[sum2]<=LeafCount[sum1]+2,
      sum2,
    sum1]],
  sum1]]]] /;
FreeQ[{a,b,m},x] && PolynomialQ[u,x] &&
  Not[IGtQ[m,0] && MatchQ[u,w_.*(c_+d_.*x)^p_ /; FreeQ[{c,d},x] && IntegerQ[p] && p>m]]


ExpandIntegrand[u_*v_^n_*(a_+b_.*x_)^m_,x_Symbol] :=
  Function[ExpandIntegrand[#[[1]]*(a+b*x)^FractionalPart[m],x] + ExpandIntegrand[#[[2]]*v^n*(a+b*x)^m,x]][
    PolynomialQuotientRemainder[u,v^(-n)*(a+b*x)^(-IntegerPart[m]),x]]/;
FreeQ[{a,b,m},x] && ILtQ[n,0] && Not[IntegerQ[m]] && PolynomialQ[u,x] && PolynomialQ[v,x] &&
  RationalQ[m] && m<-1 && Exponent[u,x]>=-(n+IntegerPart[m])*Exponent[v,x]


ExpandIntegrand[u_*v_^n_*(a_+b_.*x_)^m_,x_Symbol] :=
  Function[ExpandIntegrand[#[[1]]*(a+b*x)^m,x] + ExpandIntegrand[#[[2]]*v^n*(a+b*x)^m,x]][PolynomialQuotientRemainder[u,v^(-n),x]]/;
FreeQ[{a,b,m},x] && ILtQ[n,0] && Not[IntegerQ[m]] && PolynomialQ[u,x] && PolynomialQ[v,x] &&
  Exponent[u,x]>=-n*Exponent[v,x]


(* ::Subsubsection::Closed:: *)
(*Basis: Let r/s=(-a/b)^(1/3), then  1/(a+b z^3)==r/(3a(r-s z))+(r(2 r+s z))/(3a(r^2+r s z+s^2 z^2))*)


(* ExpandIntegrand[1/(a_+b_.*u_^3),x_Symbol] :=
  With[{r=Numerator[Rt[-a/b,3]],s=Denominator[Rt[-a/b,3]]},
  r/(3*a*(r-s*u)) + r*(2*r+s*u)/(3*a*(r^2+r*s*u+s^2*u^2))] /;
FreeQ[{a,b},x] *)


(* ::Subsubsection::Closed:: *)
(*Basis: Let r/s=Sqrt[-a/b], then  1/(a+b z^2)==r/(2a(r-s z))+r/(2a(r+s z))*)


ExpandIntegrand[1/(a_+b_.*u_^n_),x_Symbol] :=
  With[{r=Numerator[Rt[-a/b,2]],s=Denominator[Rt[-a/b,2]]},
  r/(2*a*(r-s*u^(n/2))) + r/(2*a*(r+s*u^(n/2)))] /;
FreeQ[{a,b},x] && IGtQ[n/2,0]


(* ::Subsubsection::Closed:: *)
(*Basis: Let r/s=Sqrt[-a/b], then  (c+d z)/(a+b z^2) == -s (d r+c s)/(2 b r (r-s z))+s (d r-c s)/(2 b r (r+s z))*)


ExpandIntegrand[(c_+d_.*u_^n_)/(a_+b_.*u_^n2_),x_Symbol] :=
  With[{r=Numerator[Rt[-a/b,2]],s=Denominator[Rt[-a/b,2]]},
  -s*(d*r+c*s)/(2*b*r*(r-s*u^n)) + s*(d*r-c*s)/(2*b*r*(r+s*u^n))] /;
FreeQ[{a,b,c,d},x] && IGtQ[n,0] && EqQ[n2,2*n]


(* ::Subsubsection::Closed:: *)
(*Basis: (a+b z)^m (c+d z) == d/b (a+b z)^(m+1)+(b c-a d)/b (a+b z)^m*)


ExpandIntegrand[(a_+b_.*u_)^m_*(c_.+d_.*u_),x_Symbol] :=
  d/b*(a+b*u)^(m+1) + (b*c-a*d)/b*(a+b*u)^m /;
FreeQ[{a,b,c,d},x] && ILtQ[m,0]


(* ::Subsubsection::Closed:: *)
(*Basis: If  n\[Element]\[DoubleStruckCapitalZ]+, let r/s=(-a/b)^(1/n), then  1/(a+b*z^n) == (r*Sum[1/(r - (-1)^(2*(k/n))*s*z), {k, 1, n}])/(a*n)*)


ExpandIntegrand[1/(a_+b_.*u_^n_),x_Symbol] :=
  With[{r=Numerator[Rt[-a/b,n]],s=Denominator[Rt[-a/b,n]]},
  Module[{k},
  Sum[r/(a*n*(r-(-1)^(2*k/n)*s*u)),{k,1,n}]]] /;
FreeQ[{a,b},x] && IGtQ[n,1]


(* ::Subsubsection::Closed:: *)
(*Basis: If (m|n)\[Element]\[DoubleStruckCapitalZ] \[And] 0<=m<n, let r/s=(-a/b)^(1/n), then  (c + d*z^m)/(a + b*z^n) == (r*Sum[(c + (d*(r/s)^m)/(-1)^(2*k*(m/n)))/(r - (-1)^(2*(k/n))*s*z), {k, 1, n}])/(a*n)*)


ExpandIntegrand[(c_+d_.*u_^m_.)/(a_+b_.*u_^n_),x_Symbol] :=
  With[{r=Numerator[Rt[-a/b,n]],s=Denominator[Rt[-a/b,n]]},
  Module[{k},
  Sum[(r*c+r*d*(r/s)^m*(-1)^(-2*k*m/n))/(a*n*(r-(-1)^(2*k/n)*s*u)),{k,1,n}]]] /;
FreeQ[{a,b,c,d},x] && IntegersQ[m,n] && 0<m<n


(* ::Subsubsection::Closed:: *)
(*Basis: If (m|n,p)\[Element]\[DoubleStruckCapitalZ] \[And] 0<=m<p<n, let r/s=(-a/b)^(1/n), then  (c + d*z^m + e*z^p)/(a + b*z^n) == (r*Sum[(c + (d*(r/s)^m)/(-1)^(2*k*(m/n)) + (e*(r/s)^p)/(-1)^(2*k*(p/n)))/(r - (-1)^(2*(k/n))*s*z), {k, 1, n}])/(a*n)*)


ExpandIntegrand[(c_.+d_.*u_^m_.+e_.*u_^p_)/(a_+b_.*u_^n_),x_Symbol] :=
  With[{r=Numerator[Rt[-a/b,n]],s=Denominator[Rt[-a/b,n]]},
  Module[{k},
  Sum[(r*c+r*d*(r/s)^m*(-1)^(-2*k*m/n)+r*e*(r/s)^p*(-1)^(-2*k*p/n))/(a*n*(r-(-1)^(2*k/n)*s*u)),{k,1,n}]]] /;
FreeQ[{a,b,c,d,e},x] && IntegersQ[m,n,p] && 0<m<p<n


(* ::Subsubsection::Closed:: *)
(*Basis: If (m|n,p,q)\[Element]\[DoubleStruckCapitalZ] \[And] 0<=m<p<q<n, let r/s=(-a/b)^(1/n), then  (c + d*z^m + e*z^p + f*z^q)/(a + b*z^n) == (r*Sum[(c + (d*(r/s)^m)/(-1)^(2*k*(m/n)) + (e*(r/s)^p)/(-1)^(2*k*(p/n)) + (f*(r/s)^q)/(-1)^(2*k*(q/n)))/(r - (-1)^(2*(k/n))*s*z), {k, 1, n}])/(a*n)*)


ExpandIntegrand[(c_.+d_.*u_^m_.+e_.*u_^p_+f_.*u_^q_)/(a_+b_.*u_^n_),x_Symbol] :=
  With[{r=Numerator[Rt[-a/b,n]],s=Denominator[Rt[-a/b,n]]},
  Module[{k},
  Sum[(r*c+r*d*(r/s)^m*(-1)^(-2*k*m/n)+r*e*(r/s)^p*(-1)^(-2*k*p/n)+r*f*(r/s)^q*(-1)^(-2*k*q/n))/(a*n*(r-(-1)^(2*k/n)*s*u)),{k,1,n}]]] /;
FreeQ[{a,b,c,d,e,f},x] && IntegersQ[m,n,p,q] && 0<m<p<q<n


(* ::Subsubsection::Closed:: *)
(*Basis: If  q=Sqrt[-a c], then a+c z^2==((-q+c z)(q+c z))/c*)


ExpandIntegrand[(a_+c_.*u_^n_)^p_,x_Symbol] :=
  Module[{q},
  ReplaceAll[ExpandIntegrand[1/c^p,(-q+c*x)^p*(q+c*x)^p,x],{q->Rt[-a*c,2],x->u^(n/2)}]] /;
FreeQ[{a,c},x] && EvenQ[n] && ILtQ[p,0]


ExpandIntegrand[u_^m_.*(a_.+c_.*u_^n_)^p_,x_Symbol] :=
  Module[{q},
  ReplaceAll[ExpandIntegrand[1/c^p,x^m*(-q+c*x^(n/2))^p*(q+c*x^(n/2))^p,x],{q->Rt[-a*c,2],x->u}]] /;
FreeQ[{a,c},x] && IntegersQ[m,n/2] && ILtQ[p,0] && 0<m<n && m!=n/2


(* ::Subsubsection::Closed:: *)
(*Basis: If  n\[Element]\[DoubleStruckCapitalZ]+, then  a+b*z^n == -b*Product[(-a/b)^(1/n)-(-1)^((2*k)/n)*z, {k, 1, n}]*)


ExpandIntegrand[(a_+b_.*x_^n_)^p_,x_Symbol] :=
  With[{q=Rt[-a/b,n]},
  Module[{ii},
  ExpandIntegrand[(-b)^p,Product[(q-(-1)^((2*ii)/n)*x)^p,{ii,1,n}],x]]] /;
FreeQ[{a,b},x] && IGtQ[n,1] && ILtQ[p,-1]


(* ::Subsubsection::Closed:: *)
(*Basis: If  q=Sqrt[b^2-4 a c], then a+b z+c z^2 == (b-q+2 c z)(b+q+2 c z)/(4 c)*)


ExpandIntegrand[(a_.+b_.*u_^n_.+c_.*u_^n2_.)^p_,x_Symbol] :=
  Module[{q},
  ReplaceAll[ExpandIntegrand[1/(4^p*c^p),(b-q+2*c*x)^p*(b+q+2*c*x)^p,x],{q->Rt[b^2-4*a*c,2],x->u^n}]] /;
FreeQ[{a,b,c},x] && IntegerQ[n] && EqQ[n2,2*n] && ILtQ[p,0] && NeQ[b^2-4*a*c,0]


ExpandIntegrand[u_^m_.*(a_.+b_.*u_^n_.+c_.*u_^n2_.)^p_,x_Symbol] :=
  Module[{q},
  ReplaceAll[ExpandIntegrand[1/(4^p*c^p),x^m*(b-q+2*c*x^n)^p*(b+q+2*c*x^n)^p,x],{q->Rt[b^2-4*a*c,2],x->u}]] /;
FreeQ[{a,b,c},x] && IntegersQ[m,n,n2] && EqQ[n2,2*n] && ILtQ[p,0] && 0<m<2*n && Not[m==n && p==-1] && NeQ[b^2-4*a*c,0]


(* ::Subsubsection::Closed:: *)
(*Basis: If  q=Sqrt[-a/b], then (c+d z)/(a+b z^2)==-((c-d q)/(2 b q(q+z)))-(c+d q)/(2 b q(q-z))*)


ExpandIntegrand[(c_+d_.*u_^n_.)/(a_+b_.*u_^n2_.),x_Symbol] :=
  With[{q=Rt[-a/b,2]},
  -(c-d*q)/(2*b*q*(q+u^n)) - (c+d*q)/(2*b*q*(q-u^n))] /;
FreeQ[{a,b,c,d,n},x] && EqQ[n2,2*n]


(* ::Subsubsection::Closed:: *)
(*Basis: If  q=Sqrt[b^2-4a c] and r=(2 c d-b e)/q, then (d+e z)/(a+b z+c z^2)==(e+r)/(b-q+2 c z)+(e-r)/(b+q+2 c z)*)


ExpandIntegrand[(d_.+e_.*(f_.+g_.*u_^n_.))/(a_.+b_.*u_^n_.+c_.*u_^n2_.),x_Symbol] :=
  With[{q=Rt[b^2-4*a*c,2]}, With[{r=TogetherSimplify[(2*c*(d+e*f)-b*e*g)/q]},
  (e*g+r)/(b-q+2*c*u^n) + (e*g-r)/(b+q+2*c*u^n)]] /;
FreeQ[{a,b,c,d,e,f,g,n},x] && EqQ[n2,2*n] && NeQ[b^2-4*a*c,0]


(* ::Subsubsection::Closed:: *)
(*Basis: Miscellaneous*)


(* ExpandIntegrand[u_/v_,x_Symbol] :=
  With[{lst=CoefficientList[u,x]},
  Module[{i},
  lst[[-1]]*x^Exponent[u,x]/v + Sum[lst[[i]]*x^(i-1),{i,1,Exponent[u,x]}]/v]] /;
PolynomialQ[u,x] && PolynomialQ[v,x] && BinomialQ[v,x] && Exponent[u,x]==Exponent[v,x]-1>=2 *)


ExpandIntegrand[u_/v_,x_Symbol] :=
  PolynomialDivide[u,v,x] /;
PolynomialQ[u,x] && PolynomialQ[v,x] && Exponent[u,x]>=Exponent[v,x]


ExpandIntegrand[u_*(a_.*x_)^p_,x_Symbol] :=
  ExpandToSum[(a*x)^p,u,x] /;
Not[IntegerQ[p]] && PolynomialQ[u,x]


ExpandIntegrand[u_.*v_^p_,x_Symbol] :=
  ExpandIntegrand[NormalizeIntegrand[v^p,x],u,x] /;
Not[IntegerQ[p]]


ExpandIntegrand[u_,x_Symbol] :=
  With[{v=ExpandExpression[u,x]},
    v /;
 SumQ[v]]


ExpandIntegrand[u_^m_./(a_+b_.*u_^n_),x_Symbol] :=
  ExpandBinomial[a,b,m,n,u,x] /;
FreeQ[{a,b},x] && IntegersQ[m,n] && 0<m<n


ExpandIntegrand[u_,x_Symbol] := u


(* ::Subsection::Closed:: *)
(*ExpandExpression[u,x]*)


ExpandExpression[u_,x_Symbol] :=
  Module[{v,w},
  v=If[AlgebraicFunctionQ[u,x] && Not[RationalFunctionQ[u,x]], ExpandAlgebraicFunction[u,x], 0];
  If[SumQ[v],
    ExpandCleanup[v,x],
  v=SmartApart[u,x];
  If[SumQ[v],
    ExpandCleanup[v,x],
  v=SmartApart[RationalFunctionFactors[u,x],x,x];
  If[SumQ[v],
    w=NonrationalFunctionFactors[u,x];
    ExpandCleanup[Map[Function[#*w],v],x],
  v=Expand[u,x];
  If[SumQ[v],
    ExpandCleanup[v,x],
  v=Expand[u];
  If[SumQ[v],
    ExpandCleanup[v,x],
  SimplifyTerm[u,x]]]]]]]


(* ::Subsection::Closed:: *)
(*ExpandCleanup[u,x]*)


ExpandCleanup[u_+v_/(a_+b_.*x_)+w_/(c_+d_.*x_),x_Symbol] :=
  ExpandCleanup[u+(c*v+a*w)/(a*c+b*d*x^2),x] /;
FreeQ[{a,b,c,d},x] && EqQ[b*c+a*d,0] && EqQ[d*v+b*w,0]


ExpandCleanup[u_,x_Symbol] :=
  Module[{v=CollectReciprocals[u,x]},
  If[SumQ[v],
    v=Map[Function[SimplifyTerm[#,x]],v];
    If[SumQ[v],
      UnifySum[v,x],
    v],
  v]]


(* ::Subsubsection::Closed:: *)
(*Basis: e/(a+b x)+f/(c+d x)==(c e+a f+(d e+b f) x)/(a c+(b c+a d) x+b d x^2)*)


CollectReciprocals[u_+e_/(a_+b_.*x_)+f_/(c_+d_.*x_),x_Symbol] :=
  CollectReciprocals[u+(c*e+a*f)/(a*c+b*d*x^2),x] /;
FreeQ[{a,b,c,d,e,f},x] && EqQ[b*c+a*d,0] && EqQ[d*e+b*f,0]

CollectReciprocals[u_+e_/(a_+b_.*x_)+f_/(c_+d_.*x_),x_Symbol] :=
  CollectReciprocals[u+(d*e+b*f)*x/(a*c+b*d*x^2),x] /;
FreeQ[{a,b,c,d,e,f},x] && EqQ[b*c+a*d,0] && EqQ[c*e+a*f,0]

CollectReciprocals[u_,x_Symbol] := u


(* ::Subsection::Closed:: *)
(*ExpandBinomial[u,x]*)


(* ::Subsubsection:: *)
(*Basis: If  (m|(n-1)/2)\[Element]\[DoubleStruckCapitalZ] \[And] 0<=m<n, let r/s=(a/b)^(1/n), then z^m/(a + b*z^n) == (r*(-(r/s))^m*Sum[1/((-1)^(2*k*(m/n))*(r + (-1)^(2*(k/n))*s*z)), {k, 1, n}])/(a*n) == (r*(-(r/s))^m*Sum[(-1)^(2*k*((m + 1)/n))/((-1)^(2*(k/n))*r + s*z), {k, 1, n}])/(a*n)*)


(* ::Subsubsection:: *)
(*Basis: If  r/s=(-a/b)^(1/2), then z/(a+b z^2)==s/(2b(r+s z))-s/(2b(r-s z))==r^2/(2 a s (r-s z))-r^2/(2 a s (r+s z))*)


(* ::Subsubsection:: *)
(*Basis: If (m|n)\[Element]\[DoubleStruckCapitalZ] \[And] 0<=m<n, let r/s=(-a/b)^(1/n), then  z^m/(a + b*z^n) == (r*(r/s)^m*Sum[1/((-1)^(2*k*(m/n))*(r - (-1)^(2*(k/n))*s*z)), {k, 1, n}])/(a*n) == (r*(r/s)^m*Sum[(-1)^(2*k*((m + 1)/n))/((-1)^(2*(k/n))*r - s*z), {k, 1, n}])/(a*n)*)


(* ::Subsubsection:: *)
(*Note: If  m+1 and n are not coprime, the second summation is used since the coefficient (-1)^(2 k (m+1)/n) will be simpler.*)


(* ::Subsubsection::Closed:: *)
(*Note: The code reduces the exponents by their gcd.*)


ExpandBinomial::usage = "Assumes m and n are integers and 0<m<n. ExpandBinomial[a,b,m,n,u,x] expands u^m/(a+b*u^n) into a sum of terms of the form 1/(r+s*u).";
ExpandBinomial[a_,b_,m_,n_,u_,x_Symbol] :=
  If[OddQ[n/GCD[m,n]] && PosQ[a/b],
    With[{g=GCD[m,n],r=Numerator[Rt[a/b,n/GCD[m,n]]],s=Denominator[Rt[a/b,n/GCD[m,n]]]},
    Module[{k},
    If[CoprimeQ[m+g,n],
      Sum[r*(-r/s)^(m/g)*(-1)^(-2*k*m/n)/(a*n*(r+(-1)^(2*k*g/n)*s*u^g)),{k,1,n/g}],
    Sum[r*(-r/s)^(m/g)*(-1)^(2*k*(m+g)/n)/(a*n*((-1)^(2*k*g/n)*r+s*u^g)),{k,1,n/g}]]]],
  With[{g=GCD[m,n],r=Numerator[Rt[-a/b,n/GCD[m,n]]],s=Denominator[Rt[-a/b,n/GCD[m,n]]]},
  If[n/g==2,
    s/(2*b*(r+s*u^g)) - s/(2*b*(r-s*u^g)),
  Module[{k},
  If[CoprimeQ[m+g,n],
    Sum[r*(r/s)^(m/g)*(-1)^(-2*k*m/n)/(a*n*(r-(-1)^(2*k*g/n)*s*u^g)),{k,1,n/g}],
  Sum[r*(r/s)^(m/g)*(-1)^(2*k*(m+g)/n)/(a*n*((-1)^(2*k*g/n)*r-s*u^g)),{k,1,n/g}]]]]]]


(* ::Subsection::Closed:: *)
(*SmartApart[u,x]*)


SmartApart::usage = "SmartApart[u,x] returns the partial fraction expansion of u wrt x, avoiding the undesired side-effects of Apart[u,x]."


(* Apart[u,x] has the side-effect of factoring out -1 from terms having negative integer powers of linears when the coefficient of x is negative. *)
(* SmartApart[Px_.*(a_.+b_.*x_)^m_,x_Symbol] :=
  Module[{$a,$b},
  ReplaceAll[Apart[Px*($a+$b*x)^m,x],{$a->a,$b->b}]] /;
FreeQ[{a,b},x] && ILtQ[m,0] && PolynomialQ[Px,x] *)


(* SmartApart[Px_.*(a_.+b_.*x_)^m_*(c_.+d_.*x_)^n_,x_Symbol] :=
  Module[{$a,$b,$c,$d},
  ReplaceAll[Apart[Px*($a+$b*x)^m*($c+$d*x)^n,x],{$a->a,$b->b,$c->c,$d->d}]] /;
FreeQ[{a,b,c,d},x] && ILtQ[m,0] && ILtQ[n,0] && PolynomialQ[Px,x] *)


(* Apart[u,x] rationalizes denominators involving fractional powers resulting in hard to integrate expressions. *)
SmartApart[u_,x_Symbol] :=
  With[{alst=MakeAssocList[u,x]},
  With[{tmp=KernelSubst[Apart[GensymSubst[u,x,alst]],x,alst]},
  If[tmp===Indeterminate, u, tmp]]]

SmartApart[u_,v_,x_Symbol] :=
  With[{alst=MakeAssocList[u,x]},
  With[{tmp=KernelSubst[Apart[GensymSubst[u,x,alst],v],x,alst]},
  If[tmp===Indeterminate, u, tmp]]]


MakeAssocList::usage = "MakeAssocList[u,x,alst] returns an association list of gensymed symbols with the nonatomic parameters of a u that are not integer powers, products or sums.";
MakeAssocList[u_,x_Symbol,alst_List:{}] :=
  If[AtomQ[u],
    alst,
  If[IntegerPowerQ[u],
    MakeAssocList[u[[1]],x,alst],
  If[ProductQ[u] || SumQ[u],
    MakeAssocList[Rest[u],x,MakeAssocList[First[u],x,alst]],
  If[FreeQ[u,x],
    With[{tmp=Select[alst,Function[#[[2]]===u],1]},
    If[tmp==={},
      Append[alst,{Unique["Rubi"],u}],
    alst]],
  alst]]]]


GensymSubst::usage = "GensymSubst[u,x,alst] returns u with the kernels in alst free of x replaced by gensymed names.";
GensymSubst[u_,x_Symbol,alst_List] :=
  If[AtomQ[u],
    u,
  If[IntegerPowerQ[u],
    GensymSubst[u[[1]],x,alst]^u[[2]],
  If[ProductQ[u] || SumQ[u],
    Map[Function[GensymSubst[#,x,alst]],u],
  If[FreeQ[u,x],
    With[{tmp=Select[alst,Function[#[[2]]===u],1]},
    If[tmp==={},
      u,
    tmp[[1,1]]]],
  u]]]]


KernelSubst::usage = "KernelSubst[u,x,alst] returns u with the gensymed names in alst replaced by kernels free of x.";
KernelSubst[u_,x_Symbol,alst_List] :=
  If[AtomQ[u],
    With[{tmp=Select[alst,Function[#[[1]]===u],1]},
    If[tmp==={},
      u,
    tmp[[1,2]]]],
  If[IntegerPowerQ[u],
    With[{tmp=KernelSubst[u[[1]],x,alst]},
    If[u[[2]]<0 && EqQ[tmp,0],
      Indeterminate,
    tmp^u[[2]]]],
  If[ProductQ[u] || SumQ[u],
    Map[Function[KernelSubst[#,x,alst]],u],
  u]]]


(* ::Subsection::Closed:: *)
(*IncrementalExpand[u,x]*)


IncrementalExpand[u_,v_,x_Symbol] :=
  DistributeOverTerms[u,IncrementalExpand[v,x],x]


IncrementalExpand[u_,x_Symbol] :=
  ApartCollect[SmartApart[u,x],x]


ApartCollect[u_+v_.*Px_^m_Integer+w_.*Px_^n_Integer,x_Symbol] :=
  ApartCollect[u+Together[v*Px^m+w*Px^n],x] /;
PolyQ[Px,x] && GtQ[Expon[Px,x],0] && m<0 && n<0

ApartCollect[u_,x_Symbol] := u


(* ::Subsection::Closed:: *)
(*ExpandAlgebraicFunction*)


ExpandAlgebraicFunction[u_Plus*v_,x_Symbol] :=
  Map[Function[#*v],u] /;
Not[FreeQ[u,x]]

ExpandAlgebraicFunction[u_Plus^n_*v_.,x_Symbol] :=
  With[{w=Expand[u^n,x]},
  Map[Function[#*v],w] /;
 SumQ[w]] /;
IGtQ[n,0] && Not[FreeQ[u,x]]


(* ::Subsection::Closed:: *)
(*UnifySum[u,x]*)


UnifySum::usage = "UnifySum[u,x] returns u with terms having indentical nonfree factors of x collected into a single term.";
UnifySum[u_,x_Symbol] :=
  If[SumQ[u],
    Apply[Plus,UnifyTerms[Apply[List,u],x]],
  SimplifyTerm[u,x]]


UnifyTerms::usage = "lst is a list of terms.  UnifyTerms[lst,x] returns lst with terms collected into a single element.";
UnifyTerms[lst_,x_] :=
  If[lst==={},
    lst,
  UnifyTerm[First[lst],UnifyTerms[Rest[lst],x],x]]


UnifyTerm[term_,lst_,x_] :=
  If[lst==={},
    {term},
  With[{tmp=Simplify[First[lst]/term]},
  If[FreeQ[tmp,x],
    Prepend[Rest[lst],(1+tmp)*term],
  Prepend[UnifyTerm[term,Rest[lst],x],First[lst]]]]]


(* ::Subsection::Closed:: *)
(*ExpandLinearProduct[v,u,a,b,x]*)


ExpandLinearProduct::usage = "If u is a polynomial in x, ExpandLinearProduct[v,u,a,b,x] expands v*u into a sum of terms of the form c*v*(a+b*x)^n where n is a positive integer.";
ExpandLinearProduct[v_,u_,a_,b_,x_Symbol] :=
  Module[{lst},
  lst=CoefficientList[ReplaceAll[u,x->(x-a)/b],x];
  lst=Map[Function[SimplifyTerm[#,x]],lst];
  Module[{ii},
  Sum[v*lst[[ii]]*(a+b*x)^(ii-1),{ii,1,Length[lst]}]]] /;
FreeQ[{a,b},x] && PolynomialQ[u,x]


(* ::Subsection::Closed:: *)
(*ExpandTrigExpand[u,F,v,m,n,x]*)


ExpandTrigExpand[u_,F_,v_,m_,n_,x_Symbol] :=
  With[{w=ReplaceAll[Expand[TrigExpand[F[n*x]]^m,x],x->v]},
  If[SumQ[w],
    Map[Function[u*#],w],
  u*w]]


(* ::Subsection::Closed:: *)
(*ExpandTrigReduce[u,v,x]*)


ExpandTrigReduce[u_,v_,x_Symbol] :=
  With[{w=ExpandTrigReduce[v,x]},
  If[SumQ[w],
    Map[Function[u*#],w],
  u*w]]


(* This is necessary, because TrigReduce expands Sinh[n+v] and Cosh[n+v] to exponential form if n is a number. *)
ExpandTrigReduce[u_.*F_[n_+v_.]^m_.,x_Symbol] :=
  Module[{nn},
  ExpandTrigReduce[u*F[nn+v]^m,x] /. nn->n]/;
MemberQ[{Sinh,Cosh},F] && IntegerQ[m] && RationalQ[n]

ExpandTrigReduce[u_,x_Symbol] :=
  ExpandTrigReduceAux[u,x]


ExpandTrigReduceAux[u_,x_Symbol] :=
  With[{v=Expand[TrigReduce[u]]},
  If[SumQ[v],
    Map[Function[NormalizeTrigReduce[#,x]],v],
  NormalizeTrigReduce[v,x]]]


NormalizeTrigReduce[a_.*F_[u_]^n_.,x_Symbol] :=
  a*F[ExpandToSum[u,x]]^n /;
FreeQ[{F,a,n},x] && PolynomialQ[u,x] && Exponent[u,x]>0

NormalizeTrigReduce[u_,x_Symbol] := u


(* ::Subsection::Closed:: *)
(*ExpandTrigToExp[u,v,x]*)


Clear[ExpandTrigToExp];

ExpandTrigToExp[u_,x_Symbol] := ExpandTrigToExp[1,u,x]


ExpandTrigToExp[u_,v_,x_Symbol] :=
  Module[{w=TrigToExp[v]},
  w=If[SumQ[w], Map[Function[SimplifyIntegrand[u*#,x]],w], SimplifyIntegrand[u*w,x]];
  ExpandIntegrand[FreeFactors[w,x],NonfreeFactors[w,x],x]]


(* ::Section::Closed:: *)
(*Distribution functions*)


(* ::Subsection::Closed:: *)
(*Star[u,v]*)


Star::usage = "Star[u,v] displays as u*v, and returns the product of u and v with u distributed over the terms of v.";


DownValues[Star]={};


Star[u_,v_] := (
  Print["*** Warning ***:  0*", v, "]"];
  0 ) /;
EqQ[u,0]


Star[u_,v_] :=
  Map[Function[Star[u,#]],v] /;
SumQ[v]


Star[u_,Star[v_,w_]] :=
  Star[u*v,w]


Star[u_,v_] :=
  -Star[-u,v] /;
NumericFactor[u]<0 && NumericFactor[-u]>0


Star[u_,v_] :=
  u*v /;
Not[TrueQ[$ShowSteps]] || EqQ[u^2,1] || IntegralFreeQ[v]


Star[u_,v_*w_] :=
  Star[u*v,w] /;
IntegralFreeQ[v]


(* ::Section::Closed:: *)
(*Function of functions*)


(* ::Subsection::Closed:: *)
(*FunctionOfPower*)


(* FunctionOfPower::usage = "FunctionOfPower[u,x] returns the gcd of the integer degrees of x in u."; *)
(* FunctionOfPower[u_,x_Symbol] :=
  FunctionOfPower[u,Null,x]

FunctionOfPower[u_,n_,x_] :=
  If[FreeQ[u,x],
    n,
  If[u===x,
    1,
  If[PowerQ[u] && u[[1]]===x && IntegerQ[u[[2]]],
    If[n===Null,
      u[[2]],
    GCD[n,u[[2]]]],
  Module[{tmp=n},
    Scan[Function[tmp=FunctionOfPower[#,tmp,x]],u];
    tmp]]]] *)


(* ::Subsection::Closed:: *)
(*FunctionOfLinear*)


FunctionOfLinear::usage = "If u (x) is equivalent to an expression of the form f (a+b*x) and not the case that a==0 and b==1, FunctionOfLinear[u,x] returns the list {f (x),a,b}; else it returns False.";
FunctionOfLinear[u_,x_Symbol] :=
  With[{lst=FunctionOfLinear[u,False,False,x,False]},
  If[AtomQ[lst] || FalseQ[lst[[1]]] || lst[[1]]===0 && lst[[2]]===1,
    False,
  {FunctionOfLinearSubst[u,lst[[1]],lst[[2]],x],lst[[1]],lst[[2]]}]]


FunctionOfLinear[u_,a_,b_,x_,flag_] :=
  If[FreeQ[u,x],
    {a,b},
  If[CalculusQ[u],
    False,
  If[LinearQ[u,x],
    If[FalseQ[a],
      {Coefficient[u,x,0],Coefficient[u,x,1]},
    With[{lst=CommonFactors[{b,Coefficient[u,x,1]}]},
    If[EqQ[Coefficient[u,x,0],0] && Not[flag],
      {0,lst[[1]]},
    If[EqQ[b*Coefficient[u,x,0]-a*Coefficient[u,x,1],0],
      {a/lst[[2]],lst[[1]]},
    {0,1}]]]],
  If[PowerQ[u] && FreeQ[u[[1]],x],
    FunctionOfLinear[Log[u[[1]]]*u[[2]],a,b,x,False],
  Module[{lst},
  If[ProductQ[u] && NeQ[(lst=MonomialFactor[u,x])[[1]],0],
    If[False && IntegerQ[lst[[1]]] && lst[[1]]!=-1 && FreeQ[lst[[2]],x],
      If[RationalQ[LeadFactor[lst[[2]]]] && LeadFactor[lst[[2]]]<0,
        FunctionOfLinear[DivideDegreesOfFactors[-lst[[2]],lst[[1]]]*x,a,b,x,False],
      FunctionOfLinear[DivideDegreesOfFactors[lst[[2]],lst[[1]]]*x,a,b,x,False]],
    False],
  lst={a,b};
  Catch[
  Scan[Function[lst=FunctionOfLinear[#,lst[[1]],lst[[2]],x,SumQ[u]];
			If[AtomQ[lst],Throw[False]]],u];
  lst]]]]]]]


FunctionOfLinearSubst[u_,a_,b_,x_] :=
  If[FreeQ[u,x],
    u,
  If[LinearQ[u,x],
    Module[{tmp=Coefficient[u,x,1]},
    tmp=If[tmp===b, 1, tmp/b];
    Coefficient[u,x,0]-a*tmp+tmp*x],
  If[PowerQ[u] && FreeQ[u[[1]],x],
    E^FullSimplify[FunctionOfLinearSubst[Log[u[[1]]]*u[[2]],a,b,x]],
  Module[{lst},
  If[ProductQ[u] && NeQ[(lst=MonomialFactor[u,x])[[1]],0],
    If[RationalQ[LeadFactor[lst[[2]]]] && LeadFactor[lst[[2]]]<0,
      -FunctionOfLinearSubst[DivideDegreesOfFactors[-lst[[2]],lst[[1]]]*x,a,b,x]^lst[[1]],
    FunctionOfLinearSubst[DivideDegreesOfFactors[lst[[2]],lst[[1]]]*x,a,b,x]^lst[[1]]],
  Map[Function[FunctionOfLinearSubst[#,a,b,x]],u]]]]]]


DivideDegreesOfFactors::usage = "DivideDegreesOfFactors[u,n] returns the product of the base of the factors of u raised to the degree of the factors divided by n.";
DivideDegreesOfFactors[u_,n_] :=
  If[ProductQ[u],
    Map[Function[LeadBase[#]^(LeadDegree[#]/n)],u],
  LeadBase[u]^(LeadDegree[u]/n)]


(* ::Subsection::Closed:: *)
(*FunctionOfInverseLinear*)


FunctionOfInverseLinear::usage = "If u is a function of an inverse linear binomial of the form 1/(a+b*x), FunctionOfInverseLinear[u,x] returns the list {a,b}; else it returns False.";
FunctionOfInverseLinear[u_,x_Symbol] :=
  FunctionOfInverseLinear[u,Null,x]

FunctionOfInverseLinear[u_,lst_,x_] :=
  If[FreeQ[u,x],
    lst,
  If[u===x,
    False,
  If[QuotientOfLinearsQ[u,x],
    With[{tmp=Drop[QuotientOfLinearsParts[u,x],2]},
    If[tmp[[2]]===0,
      False,
    If[lst===Null,
      tmp,
    If[EqQ[lst[[1]]*tmp[[2]]-lst[[2]]*tmp[[1]],0],
      lst,
    False]]]],
  If[CalculusQ[u],
    False,
  Module[{tmp=lst},Catch[
  Scan[Function[If[AtomQ[tmp=FunctionOfInverseLinear[#,tmp,x]],Throw[False]]],u];
  tmp]]]]]]


(* ::Subsection::Closed:: *)
(*FunctionOfExponential*)


FunctionOfExponentialQ::usage = "FunctionOfExponentialQ[u,x] returns True iff u is a function of F^v where F is a constant and v is linear in x, and such an exponential explicitly occurs in u (i.e. not just implicitly in hyperbolic functions).";
FunctionOfExponentialQ[u_,x_Symbol] :=
  Block[{$base$=Null,$expon$=Null,$exponFlag$=False},
    FunctionOfExponentialTest[u,x] && $exponFlag$]


FunctionOfExponential::usage = "u is a function of F^v where v is linear in x.  FunctionOfExponential[u,x] returns F^v.";
FunctionOfExponential[u_,x_Symbol] :=
  Block[{$base$=Null,$expon$=Null,$exponFlag$=False},
    FunctionOfExponentialTest[u,x];
    $base$^$expon$]


FunctionOfExponentialFunction::usage = "u is a function of F^v where v is linear in x.  FunctionOfExponentialFunction[u,x] returns u with F^v replaced by x.";
FunctionOfExponentialFunction[u_,x_Symbol] :=
  Block[{$base$=Null,$expon$=Null,$exponFlag$=False},
    FunctionOfExponentialTest[u,x];
    SimplifyIntegrand[FunctionOfExponentialFunctionAux[u,x],x]]


FunctionOfExponentialFunctionAux::usage = "u is a function of F^v where v is linear in x, and the fluid variables $base$=F and $expon$=v.  FunctionOfExponentialFunctionAux[u,x] returns u with F^v replaced by x.";
FunctionOfExponentialFunctionAux[u_,x_] :=
  If[AtomQ[u],
    u,
  If[PowerQ[u] && FreeQ[u[[1]],x] && LinearQ[u[[2]],x],
    If[EqQ[Coefficient[$expon$,x,0],0],
      u[[1]]^Coefficient[u[[2]],x,0]*x^FullSimplify[Log[u[[1]]]*Coefficient[u[[2]],x,1]/(Log[$base$]*Coefficient[$expon$,x,1])],
    x^FullSimplify[Log[u[[1]]]*Coefficient[u[[2]],x,1]/(Log[$base$]*Coefficient[$expon$,x,1])]],
  If[HyperbolicQ[u] && LinearQ[u[[1]],x],
    Module[{tmp},
    tmp=x^FullSimplify[Coefficient[u[[1]],x,1]/(Log[$base$]*Coefficient[$expon$,x,1])];
    Switch[Head[u],
      Sinh, tmp/2-1/(2*tmp),
      Cosh, tmp/2+1/(2*tmp),
      Tanh, (tmp-1/tmp)/(tmp+1/tmp),
      Coth, (tmp+1/tmp)/(tmp-1/tmp),
      Sech, 2/(tmp+1/tmp),
      Csch, 2/(tmp-1/tmp)]],
  If[PowerQ[u] && FreeQ[u[[1]],x] && SumQ[u[[2]]],
    FunctionOfExponentialFunctionAux[u[[1]]^First[u[[2]]],x]*FunctionOfExponentialFunctionAux[u[[1]]^Rest[u[[2]]],x],
  Map[Function[FunctionOfExponentialFunctionAux[#,x]],u]]]]]


FunctionOfExponentialTest::usage = "FunctionOfExponentialTest[u,x] returns True iff u is a function of F^v where F is a constant and v is linear in x.  Before it is called, the fluid variables $base$ and $expon$ should be set to Null and $exponFlag$ to False.  If u is a function of F^v, $base$ and $expon$ are set to F and v, respectively.  If an explicit exponential occurs in u, $exponFlag$ is set to True.";
FunctionOfExponentialTest[u_,x_] :=
  If[FreeQ[u,x],
    True,
  If[u===x || CalculusQ[u],
    False,
  If[PowerQ[u] && FreeQ[u[[1]],x] && LinearQ[u[[2]],x],
    $exponFlag$=True;
    FunctionOfExponentialTestAux[u[[1]],u[[2]],x],
  If[HyperbolicQ[u] && LinearQ[u[[1]],x],
    FunctionOfExponentialTestAux[E,u[[1]],x],
  If[PowerQ[u] && FreeQ[u[[1]],x] && SumQ[u[[2]]],
    FunctionOfExponentialTest[u[[1]]^First[u[[2]]],x] && FunctionOfExponentialTest[u[[1]]^Rest[u[[2]]],x],
  Catch[Scan[Function[If[Not[FunctionOfExponentialTest[#,x]],Throw[False]]],u]; True]]]]]]


FunctionOfExponentialTestAux[base_,expon_,x_] :=
  If[$base$===Null,
    $base$=base;
    $expon$=expon;
    True,
  Module[{tmp},
  tmp=FullSimplify[Log[base]*Coefficient[expon,x,1]/(Log[$base$]*Coefficient[$expon$,x,1])];
  If[Not[RationalQ[tmp]],
    False,
  If[EqQ[Coefficient[$expon$,x,0],0] || NeQ[tmp,FullSimplify[Log[base]*Coefficient[expon,x,0]/(Log[$base$]*Coefficient[$expon$,x,0])]],
    ( If[IGtQ[base,0] && IGtQ[$base$,0] && base<$base$,
        $base$=base;
        $expon$=expon;
        tmp=1/tmp] );
    $expon$=Coefficient[$expon$,x,1]*x/Denominator[tmp];
    If[tmp<0 && NegQ[Coefficient[$expon$,x,1]],
      $expon$=-$expon$;
      True,
    True],
  ( If[IGtQ[base,0] && IGtQ[$base$,0] && base<$base$,
      $base$=base;
      $expon$=expon;
      tmp=1/tmp] );
(*$expon$=If[SumQ[$expon$], Map[Function[#/Denominator[tmp]],$expon$], $expon$/Denominator[tmp]]; *)
  $expon$=$expon$/Denominator[tmp];
  If[tmp<0 && NegQ[Coefficient[$expon$,x,1]],
    $expon$=-$expon$;
    True,
  True]]]]]


(* ::Subsection::Closed:: *)
(*FunctionOfTrigOfLinearQ*)


FunctionOfTrigOfLinearQ::usage = "If u is an algebraic function of trig functions of a linear function of x, FunctionOfTrigOfLinearQ[u,x] returns True; else it returns False.";
FunctionOfTrigOfLinearQ[u_,x_Symbol] :=
  If[MatchQ[u,(c_.+d_.*x)^m_.*(a_.+b_.*trig_[e_.+f_.*x])^n_. /; FreeQ[{a,b,c,d,e,f,m,n},x] && (TrigQ[trig] || HyperbolicQ[trig])],
    True,
  Not[MemberQ[{Null, False}, FunctionOfTrig[u,Null,x]]] && AlgebraicTrigFunctionQ[u,x]]


FunctionOfTrig::usage = "If u is a function of trig functions of v where v is a linear function of x, FunctionOfTrig[u,x] returns v; else it returns False.";
FunctionOfTrig[u_,x_Symbol] :=
  With[{v=FunctionOfTrig[ActivateTrig[u],Null,x]},
  If[v===Null, False, v]]

FunctionOfTrig[u_,v_,x_] :=
  If[AtomQ[u],
    If[u===x,
      False,
    v],
  If[TrigQ[u] && LinearQ[u[[1]],x],
    If[v===Null,
      u[[1]],
    With[{a=Coefficient[v,x,0],b=Coefficient[v,x,1],c=Coefficient[u[[1]],x,0],d=Coefficient[u[[1]],x,1]},
    If[EqQ[a*d-b*c,0] && RationalQ[b/d],
      a/Numerator[b/d]+b*x/Numerator[b/d],
    False]]],
  If[HyperbolicQ[u] && LinearQ[u[[1]],x],
    If[v===Null,
      I*u[[1]],
    With[{a=Coefficient[v,x,0],b=Coefficient[v,x,1],c=I*Coefficient[u[[1]],x,0],d=I*Coefficient[u[[1]],x,1]},
    If[EqQ[a*d-b*c,0] && RationalQ[b/d],
      a/Numerator[b/d]+b*x/Numerator[b/d],
    False]]],
  If[CalculusQ[u],
    False,
  Module[{w=v},Catch[
  Scan[Function[If[FalseQ[w=FunctionOfTrig[#,w,x]],Throw[False]]],u];
  w]]]]]]


AlgebraicTrigFunctionQ::usage = "If u is algebraic function of trig functions, AlgebraicTrigFunctionQ[u,x] returns True; else it returns False.";
AlgebraicTrigFunctionQ[u_,x_Symbol] :=
  If[AtomQ[u],
    True,
  If[TrigQ[u] && LinearQ[u[[1]],x],
    True,
  If[HyperbolicQ[u] && LinearQ[u[[1]],x],
    True,
  If[PowerQ[u] && FreeQ[u[[2]],x],
    AlgebraicTrigFunctionQ[u[[1]],x],
  If[ProductQ[u] || SumQ[u],
    Catch[Scan[Function[If[Not[AlgebraicTrigFunctionQ[#,x]],Throw[False]]],u]; True],
  False]]]]]


(* ::Subsection::Closed:: *)
(*FunctionOfHyperbolic*)


FunctionOfHyperbolic::usage = "If u is a function of hyperbolic trig functions of v where v is linear in x, FunctionOfHyperbolic[u,x] returns v; else it returns False.";
FunctionOfHyperbolic[u_,x_Symbol] :=
  With[{v=FunctionOfHyperbolic[u,Null,x]},
  If[v===Null, False, v]]

FunctionOfHyperbolic[u_,v_,x_] :=
  If[AtomQ[u],
    If[u===x,
      False,
    v],
  If[HyperbolicQ[u] && LinearQ[u[[1]],x],
    If[v===Null,
      u[[1]],
    With[{a=Coefficient[v,x,0],b=Coefficient[v,x,1],c=Coefficient[u[[1]],x,0],d=Coefficient[u[[1]],x,1]},
    If[EqQ[a*d-b*c,0] && RationalQ[b/d],
      a/Numerator[b/d]+b*x/Numerator[b/d],
    False]]],
  If[CalculusQ[u],
    False,
  Module[{w=v},Catch[
  Scan[Function[If[FalseQ[w=FunctionOfHyperbolic[#,w,x]],Throw[False]]],u];
  w]]]]]


(* ::Subsection::Closed:: *)
(*FunctionOfQ*)


FunctionOfQ::usage = "v is a function of x.  If u is a function of v, FunctionOfQ[v,u,x] returns True; else it returns False.";
FunctionOfQ[v_,u_,x_Symbol,PureFlag_:False] :=
  If[FreeQ[u,x],
    False,
  If[AtomQ[v],
    True,
  If[Not[InertTrigFreeQ[u]],
    FunctionOfQ[v,ActivateTrig[u],x,PureFlag],
  If[ProductQ[v] && NeQ[FreeFactors[v,x],1],
    FunctionOfQ[NonfreeFactors[v,x],u,x,PureFlag],

  If[PureFlag, Switch[Head[v],
    Sin, PureFunctionOfSinQ[u,v[[1]],x],
    Cos, PureFunctionOfCosQ[u,v[[1]],x],
    Tan, PureFunctionOfTanQ[u,v[[1]],x],
    Cot, PureFunctionOfCotQ[u,v[[1]],x],
    Sec, PureFunctionOfCosQ[u,v[[1]],x],
    Csc, PureFunctionOfSinQ[u,v[[1]],x],

    Sinh, PureFunctionOfSinhQ[u,v[[1]],x],
    Cosh, PureFunctionOfCoshQ[u,v[[1]],x],
    Tanh, PureFunctionOfTanhQ[u,v[[1]],x],
    Coth, PureFunctionOfCothQ[u,v[[1]],x],
    Sech, PureFunctionOfCoshQ[u,v[[1]],x],
    Csch, PureFunctionOfSinhQ[u,v[[1]],x],
    _, FunctionOfExpnQ[u,v,x]=!=False],

  Switch[Head[v],
    Sin, FunctionOfSinQ[u,v[[1]],x],
    Cos, FunctionOfCosQ[u,v[[1]],x],
    Tan, FunctionOfTanQ[u,v[[1]],x],
    Cot, FunctionOfTanQ[u,v[[1]],x],
    Sec, FunctionOfCosQ[u,v[[1]],x],
    Csc, FunctionOfSinQ[u,v[[1]],x],

    Sinh, FunctionOfSinhQ[u,v[[1]],x],
    Cosh, FunctionOfCoshQ[u,v[[1]],x],
    Tanh, FunctionOfTanhQ[u,v[[1]],x],
    Coth, FunctionOfTanhQ[u,v[[1]],x],
    Sech, FunctionOfCoshQ[u,v[[1]],x],
    Csch, FunctionOfSinhQ[u,v[[1]],x],
    _, FunctionOfExpnQ[u,v,x]=!=False]]]]]]


FunctionOfExpnQ[u_,v_,x_] :=
  If[u===v,
    1,
  If[AtomQ[u],
    If[u===x,
      False,
    0],
  If[CalculusQ[u],
    False,
  If[PowerQ[u] && FreeQ[u[[2]],x],
    If[EqQ[u[[1]],v],
      If[IntegerQ[u[[2]]], u[[2]], 1],
    If[PowerQ[v] && FreeQ[v[[2]],x] && EqQ[u[[1]],v[[1]]],
      If[RationalQ[v[[2]]],
        If[RationalQ[u[[2]]] && IntegerQ[u[[2]]/v[[2]]] && (v[[2]]>0 || u[[2]]<0), u[[2]]/v[[2]], False],
      If[IntegerQ[Simplify[u[[2]]/v[[2]]]], Simplify[u[[2]]/v[[2]]], False]],
    FunctionOfExpnQ[u[[1]],v,x]]],
  If[ProductQ[u] && NeQ[FreeFactors[u,x],1],
    FunctionOfExpnQ[NonfreeFactors[u,x],v,x],
  If[ProductQ[u] && ProductQ[v],
    Module[{deg1=FunctionOfExpnQ[First[u],First[v],x],deg2},
    If[deg1===False,
      False,
    deg2=FunctionOfExpnQ[Rest[u],Rest[v],x];
    If[deg1===deg2 && FreeQ[Simplify[u/v^deg1],x],
      deg1,
    False]]],
  With[{lst=Map[Function[FunctionOfExpnQ[#,v,x]],Apply[List,u]]},
  If[MemberQ[lst,False],
    False,
  Apply[GCD,lst]]]]]]]]]


(* ::Subsubsection::Closed:: *)
(*Pure function of trig functions predicates*)


PureFunctionOfSinQ::usage = "If u is a pure function of Sin[v] and/or Csc[v], PureFunctionOfSinQ[u,v,x] returns True; else it returns False.";
PureFunctionOfSinQ[u_,v_,x_] :=
  If[AtomQ[u],
    u=!=x,
  If[CalculusQ[u],
    False,
  If[TrigQ[u] && EqQ[u[[1]],v],
    Head[u]===Sin || Head[u]===Csc,
  Catch[Scan[Function[If[Not[PureFunctionOfSinQ[#,v,x]],Throw[False]]],u];True]]]]


PureFunctionOfCosQ::usage = "If u is a pure function of Cos[v] and/or Sec[v], PureFunctionOfCosQ[u,v,x] returns True; else it returns False.";
PureFunctionOfCosQ[u_,v_,x_] :=
  If[AtomQ[u],
    u=!=x,
  If[CalculusQ[u],
    False,
  If[TrigQ[u] && EqQ[u[[1]],v],
    Head[u]===Cos || Head[u]===Sec,
  Catch[Scan[Function[If[Not[PureFunctionOfCosQ[#,v,x]],Throw[False]]],u];True]]]]


PureFunctionOfTanQ::usage = "If u is a pure function of Tan[v] and/or Cot[v], PureFunctionOfTanQ[u,v,x] returns True; else it returns False.";
PureFunctionOfTanQ[u_,v_,x_] :=
  If[AtomQ[u],
    u=!=x,
  If[CalculusQ[u],
    False,
  If[TrigQ[u] && EqQ[u[[1]],v],
    Head[u]===Tan || Head[u]===Cot,
  Catch[Scan[Function[If[Not[PureFunctionOfTanQ[#,v,x]],Throw[False]]],u];True]]]]


PureFunctionOfCotQ::usage = "If u is a pure function of Cot[v], PureFunctionOfCotQ[u,v,x] returns True; else it returns False.";
PureFunctionOfCotQ[u_,v_,x_] :=
  If[AtomQ[u],
    u=!=x,
  If[CalculusQ[u],
    False,
  If[TrigQ[u] && EqQ[u[[1]],v],
    Head[u]===Cot,
  Catch[Scan[Function[If[Not[PureFunctionOfCotQ[#,v,x]],Throw[False]]],u];True]]]]


(* ::Subsubsection::Closed:: *)
(*Function of trig functions predicates*)


FunctionOfSinQ::usage = "If u is a function of Sin[v], FunctionOfSinQ[u,v,x] returns True; else it returns False.";
FunctionOfSinQ[u_,v_,x_] :=
  If[AtomQ[u],
    u=!=x,
  If[CalculusQ[u],
    False,
  If[TrigQ[u] && IntegerQuotientQ[u[[1]],v],
    If[OddQuotientQ[u[[1]],v],
(* Basis: If m odd, Sin[m*v]^n is a function of Sin[v]. *)
      Head[u]===Sin || Head[u]===Csc,
(* Basis: If m even, Cos[m*v]^n is a function of Sin[v]. *)
    Head[u]===Cos || Head[u]===Sec],
  If[IntegerPowerQ[u] && TrigQ[u[[1]]] && IntegerQuotientQ[u[[1,1]],v],
    If[EvenQ[u[[2]]],
(* Basis: If m integer and n even, Trig[m*v]^n is a function of Sin[v]. *)
      True,
    FunctionOfSinQ[u[[1]],v,x]],
  If[ProductQ[u],
    If[Head[u[[1]]]===Cos && Head[u[[2]]]===Sin && EqQ[u[[1,1]],v/2] && EqQ[u[[2,1]],v/2],
      FunctionOfSinQ[Drop[u,2],v,x],
    Module[{lst},
    lst=FindTrigFactor[Sin,Csc,u,v,False];
    If[ListQ[lst] && EvenQuotientQ[lst[[1]],v],
(* Basis: If m even and n odd, Sin[m*v]^n == Cos[v]*u where u is a function of Sin[v]. *)
      FunctionOfSinQ[Cos[v]*lst[[2]],v,x],
    lst=FindTrigFactor[Cos,Sec,u,v,False];
    If[ListQ[lst] && OddQuotientQ[lst[[1]],v],
(* Basis: If m odd and n odd, Cos[m*v]^n == Cos[v]*u where u is a function of Sin[v]. *)
      FunctionOfSinQ[Cos[v]*lst[[2]],v,x],
    lst=FindTrigFactor[Tan,Cot,u,v,True];
    If[ListQ[lst],
(* Basis: If m integer and n odd, Tan[m*v]^n == Cos[v]*u where u is a function of Sin[v]. *)
      FunctionOfSinQ[Cos[v]*lst[[2]],v,x],
    Catch[Scan[Function[If[Not[FunctionOfSinQ[#,v,x]],Throw[False]]],u];True]]]]]],
  Catch[Scan[Function[If[Not[FunctionOfSinQ[#,v,x]],Throw[False]]],u];True]]]]]]


FunctionOfCosQ::usage = "If u is a function of Cos[v], FunctionOfCosQ[u,v,x] returns True; else it returns False.";
FunctionOfCosQ[u_,v_,x_] :=
  If[AtomQ[u],
    u=!=x,
  If[CalculusQ[u],
    False,
  If[TrigQ[u] && IntegerQuotientQ[u[[1]],v],
(* Basis: If m integer, Cos[m*v]^n is a function of Cos[v]. *)
    Head[u]===Cos || Head[u]===Sec,
  If[IntegerPowerQ[u] && TrigQ[u[[1]]] && IntegerQuotientQ[u[[1,1]],v],
    If[EvenQ[u[[2]]],
(* Basis: If m integer and n even, Trig[m*v]^n is a function of Cos[v]. *)
      True,
    FunctionOfCosQ[u[[1]],v,x]],
  If[ProductQ[u],
    Module[{lst},
    lst=FindTrigFactor[Sin,Csc,u,v,False];
    If[ListQ[lst],
(* Basis: If m integer and n odd, Sin[m*v]^n == Sin[v]*u where u is a function of Cos[v]. *)
      FunctionOfCosQ[Sin[v]*lst[[2]],v,x],
    lst=FindTrigFactor[Tan,Cot,u,v,True];
    If[ListQ[lst],
(* Basis: If m integer and n odd, Tan[m*v]^n == Sin[v]*u where u is a function of Cos[v]. *)
      FunctionOfCosQ[Sin[v]*lst[[2]],v,x],
    Catch[Scan[Function[If[Not[FunctionOfCosQ[#,v,x]],Throw[False]]],u];True]]]],
  Catch[Scan[Function[If[Not[FunctionOfCosQ[#,v,x]],Throw[False]]],u];True]]]]]]


FunctionOfTanQ::usage = "If u is a function of the form f[Tan[v],Cot[v]] where f is independent of x, FunctionOfTanQ[u,v,x] returns True; else it returns False.";
FunctionOfTanQ[u_,v_,x_] :=
  If[AtomQ[u],
    u=!=x,
  If[CalculusQ[u],
    False,
  If[TrigQ[u] && IntegerQuotientQ[u[[1]],v],
    Head[u]===Tan || Head[u]===Cot || EvenQuotientQ[u[[1]],v],
  If[PowerQ[u] && EvenQ[u[[2]]] && TrigQ[u[[1]]] && IntegerQuotientQ[u[[1,1]],v],
    True,
  If[PowerQ[u] && EvenQ[u[[2]]] && SumQ[u[[1]]],
    FunctionOfTanQ[Expand[u[[1]]^2],v,x],
  If[ProductQ[u],
    Module[{lst=ReapList[Scan[Function[If[Not[FunctionOfTanQ[#,v,x]],Sow[#]]],u]]},
    If[lst==={},
      True,
    Length[lst]==2 && OddTrigPowerQ[lst[[1]],v,x] && OddTrigPowerQ[lst[[2]],v,x]]],
  Catch[Scan[Function[If[Not[FunctionOfTanQ[#,v,x]],Throw[False]]],u];True]]]]]]]

OddTrigPowerQ[u_,v_,x_] :=
  If[MemberQ[{Sin,Cos,Sec,Csc},Head[u]],
    OddQuotientQ[u[[1]],v],
  If[PowerQ[u],
    OddQ[u[[2]]] && OddTrigPowerQ[u[[1]],v,x],
  If[ProductQ[u],
    If[NeQ[FreeFactors[u,x],1],
      OddTrigPowerQ[NonfreeFactors[u,x],v,x],
    Module[{lst=ReapList[Scan[Function[If[Not[FunctionOfTanQ[#,v,x]],Sow[#]]],u]]},
    If[lst==={},
      True,
    Length[lst]==1 && OddTrigPowerQ[lst[[1]],v,x]]]],
  If[SumQ[u],
    Catch[Scan[Function[If[Not[OddTrigPowerQ[#,v,x]],Throw[False]]],u];True],
  False]]]]


FunctionOfTanWeight::usage = "u is a function of the form f[Tan[v],Cot[v]] where f is independent of x.  FunctionOfTanWeight[u,v,x] returns a nonnegative number if u is best considered a function of Tan[v]; else it returns a negative number.";
FunctionOfTanWeight[u_,v_,x_] :=
  If[AtomQ[u],
    0,
  If[CalculusQ[u],
    0,
  If[TrigQ[u] && IntegerQuotientQ[u[[1]],v],
    If[Head[u]===Tan && EqQ[u[[1]],v],
      1,
    If[Head[u]===Cot && EqQ[u[[1]],v],
      -1,
    0]],
  If[PowerQ[u] && EvenQ[u[[2]]] && TrigQ[u[[1]]] && IntegerQuotientQ[u[[1,1]],v],
    If[Head[u[[1]]]===Tan || Head[u[[1]]]===Cos || Head[u[[1]]]===Sec,
      1,
    -1],
  If[ProductQ[u],
    If[Catch[Scan[Function[If[Not[FunctionOfTanQ[#,v,x]],Throw[False]]],u];True],
      Apply[Plus,Map[Function[FunctionOfTanWeight[#,v,x]],Apply[List,u]]],
    0],
  Apply[Plus,Map[Function[FunctionOfTanWeight[#,v,x]],Apply[List,u]]]]]]]]


FunctionOfTrigQ::usage = "If u (x) is equivalent to an expression of the form f (Sin[v],Cos[v],Tan[v],Cot[v],Sec[v],Csc[v]) where f is independent of x, FunctionOfTrigQ[u,v,x] returns True; else it returns False.";
FunctionOfTrigQ[u_,v_,x_Symbol] :=
  If[AtomQ[u],
    u=!=x,
  If[CalculusQ[u],
    False,
  If[TrigQ[u] && IntegerQuotientQ[u[[1]],v],
    True,
  Catch[
    Scan[Function[If[Not[FunctionOfTrigQ[#,v,x]],Throw[False]]],u];
    True]]]]


(* ::Subsubsection::Closed:: *)
(*Pure function of hyperbolic functions predicates*)


PureFunctionOfSinhQ::usage = "If u is a pure function of Sinh[v] and/or Csch[v], PureFunctionOfSinhQ[u,v,x] returns True; else it returns False.";
PureFunctionOfSinhQ[u_,v_,x_] :=
  If[AtomQ[u],
    u=!=x,
  If[CalculusQ[u],
    False,
  If[HyperbolicQ[u] && EqQ[u[[1]],v],
    Head[u]===Sinh || Head[u]===Csch,
  Catch[Scan[Function[If[Not[PureFunctionOfSinhQ[#,v,x]],Throw[False]]],u];True]]]]


PureFunctionOfCoshQ::usage = "If u is a pure function of Cosh[v] and/or Sech[v], PureFunctionOfCoshQ[u,v,x] returns True; else it returns False.";
PureFunctionOfCoshQ[u_,v_,x_] :=
  If[AtomQ[u],
    u=!=x,
  If[CalculusQ[u],
    False,
  If[HyperbolicQ[u] && EqQ[u[[1]],v],
    Head[u]===Cosh || Head[u]===Sech,
  Catch[Scan[Function[If[Not[PureFunctionOfCoshQ[#,v,x]],Throw[False]]],u];True]]]]


PureFunctionOfTanhQ::usage = "If u is a pure function of Tanh[v] and/or Coth[v], PureFunctionOfTanhQ[u,v,x] returns True; else it returns False.";
PureFunctionOfTanhQ[u_,v_,x_] :=
  If[AtomQ[u],
    u=!=x,
  If[CalculusQ[u],
    False,
  If[HyperbolicQ[u] && EqQ[u[[1]],v],
    Head[u]===Tanh || Head[u]===Coth,
  Catch[Scan[Function[If[Not[PureFunctionOfTanhQ[#,v,x]],Throw[False]]],u];True]]]]


PureFunctionOfCothQ::usage = "If u is a pure function of Coth[v], PureFunctionOfCothQ[u,v,x] returns True; else it returns False.";
PureFunctionOfCothQ[u_,v_,x_] :=
  If[AtomQ[u],
    u=!=x,
  If[CalculusQ[u],
    False,
  If[HyperbolicQ[u] && EqQ[u[[1]],v],
    Head[u]===Coth,
  Catch[Scan[Function[If[Not[PureFunctionOfCothQ[#,v,x]],Throw[False]]],u];True]]]]


(* ::Subsubsection::Closed:: *)
(*Function of hyperbolic functions predicates*)


FunctionOfSinhQ::usage = "If u is a function of Sinh[v], FunctionOfSinhQ[u,v,x] returns True; else it returns False.";
FunctionOfSinhQ[u_,v_,x_] :=
  If[AtomQ[u],
    u=!=x,
  If[CalculusQ[u],
    False,
  If[HyperbolicQ[u] && IntegerQuotientQ[u[[1]],v],
    If[OddQuotientQ[u[[1]],v],
(* Basis: If m odd, Sinh[m*v]^n is a function of Sinh[v]. *)
      Head[u]===Sinh || Head[u]===Csch,
(* Basis: If m even, Cos[m*v]^n is a function of Sinh[v]. *)
    Head[u]===Cosh || Head[u]===Sech],
  If[IntegerPowerQ[u] && HyperbolicQ[u[[1]]] && IntegerQuotientQ[u[[1,1]],v],
    If[EvenQ[u[[2]]],
(* Basis: If m integer and n even, Hyper[m*v]^n is a function of Sinh[v]. *)
      True,
    FunctionOfSinhQ[u[[1]],v,x]],
  If[ProductQ[u],
    If[Head[u[[1]]]===Cosh && Head[u[[2]]]===Sinh && EqQ[u[[1,1]],v/2] && EqQ[u[[2,1]],v/2],
      FunctionOfSinhQ[Drop[u,2],v,x],
    Module[{lst},
    lst=FindTrigFactor[Sinh,Csch,u,v,False];
    If[ListQ[lst] && EvenQuotientQ[lst[[1]],v],
(* Basis: If m even and n odd, Sinh[m*v]^n == Cosh[v]*u where u is a function of Sinh[v]. *)
      FunctionOfSinhQ[Cosh[v]*lst[[2]],v,x],
    lst=FindTrigFactor[Cosh,Sech,u,v,False];
    If[ListQ[lst] && OddQuotientQ[lst[[1]],v],
(* Basis: If m odd and n odd, Cosh[m*v]^n == Cosh[v]*u where u is a function of Sinh[v]. *)
      FunctionOfSinhQ[Cosh[v]*lst[[2]],v,x],
    lst=FindTrigFactor[Tanh,Coth,u,v,True];
    If[ListQ[lst],
(* Basis: If m integer and n odd, Tanh[m*v]^n == Cosh[v]*u where u is a function of Sinh[v]. *)
      FunctionOfSinhQ[Cosh[v]*lst[[2]],v,x],
    Catch[Scan[Function[If[Not[FunctionOfSinhQ[#,v,x]],Throw[False]]],u];True]]]]]],
  Catch[Scan[Function[If[Not[FunctionOfSinhQ[#,v,x]],Throw[False]]],u];True]]]]]]


FunctionOfCoshQ::usage = "If u is a function of Cosh[v], FunctionOfCoshQ[u,v,x] returns True; else it returns False.";
FunctionOfCoshQ[u_,v_,x_] :=
  If[AtomQ[u],
    u=!=x,
  If[CalculusQ[u],
    False,
  If[HyperbolicQ[u] && IntegerQuotientQ[u[[1]],v],
(* Basis: If m integer, Cosh[m*v]^n is a function of Cosh[v]. *)
    Head[u]===Cosh || Head[u]===Sech,
  If[IntegerPowerQ[u] && HyperbolicQ[u[[1]]] && IntegerQuotientQ[u[[1,1]],v],
    If[EvenQ[u[[2]]],
(* Basis: If m integer and n even, Hyper[m*v]^n is a function of Cosh[v]. *)
      True,
    FunctionOfCoshQ[u[[1]],v,x]],
  If[ProductQ[u],
    Module[{lst},
    lst=FindTrigFactor[Sinh,Csch,u,v,False];
    If[ListQ[lst],
(* Basis: If m integer and n odd, Sinh[m*v]^n == Sinh[v]*u where u is a function of Cosh[v]. *)
      FunctionOfCoshQ[Sinh[v]*lst[[2]],v,x],
    lst=FindTrigFactor[Tanh,Coth,u,v,True];
    If[ListQ[lst],
(* Basis: If m integer and n odd, Tanh[m*v]^n == Sinh[v]*u where u is a function of Cosh[v]. *)
      FunctionOfCoshQ[Sinh[v]*lst[[2]],v,x],
    Catch[Scan[Function[If[Not[FunctionOfCoshQ[#,v,x]],Throw[False]]],u];True]]]],
  Catch[Scan[Function[If[Not[FunctionOfCoshQ[#,v,x]],Throw[False]]],u];True]]]]]]


FunctionOfTanhQ::usage = "If u is a function of the form f[Tanh[v],Coth[v]] where f is independent of x, FunctionOfTanhQ[u,v,x] returns True; else it returns False.";
FunctionOfTanhQ[u_,v_,x_] :=
  If[AtomQ[u],
    u=!=x,
  If[CalculusQ[u],
    False,
  If[HyperbolicQ[u] && IntegerQuotientQ[u[[1]],v],
    Head[u]===Tanh || Head[u]===Coth || EvenQuotientQ[u[[1]],v],
  If[PowerQ[u] && EvenQ[u[[2]]] && HyperbolicQ[u[[1]]] && IntegerQuotientQ[u[[1,1]],v],
    True,
  If[PowerQ[u] && EvenQ[u[[2]]] && SumQ[u[[1]]],
    FunctionOfTanhQ[Expand[u[[1]]^2],v,x],
  If[ProductQ[u],
    With[{lst=ReapList[Scan[Function[If[Not[FunctionOfTanhQ[#,v,x]],Sow[#]]],u]]},
    If[lst==={},
      True,
    Length[lst]==2 && OddHyperbolicPowerQ[lst[[1]],v,x] && OddHyperbolicPowerQ[lst[[2]],v,x]]],
  Catch[Scan[Function[If[Not[FunctionOfTanhQ[#,v,x]],Throw[False]]],u];True]]]]]]]

OddHyperbolicPowerQ[u_,v_,x_] :=
  If[MemberQ[{Sinh,Cosh,Sech,Csch},Head[u]],
    OddQuotientQ[u[[1]],v],
  If[PowerQ[u],
    OddQ[u[[2]]] && OddHyperbolicPowerQ[u[[1]],v,x],
  If[ProductQ[u],
    If[NeQ[FreeFactors[u,x],1],
      OddHyperbolicPowerQ[NonfreeFactors[u,x],v,x],
    With[{lst=ReapList[Scan[Function[If[Not[FunctionOfTanhQ[#,v,x]],Sow[#]]],u]]},
    If[lst==={},
      True,
    Length[lst]==1 && OddHyperbolicPowerQ[lst[[1]],v,x]]]],
  If[SumQ[u],
    Catch[Scan[Function[If[Not[OddHyperbolicPowerQ[#,v,x]],Throw[False]]],u];True],
  False]]]]


FunctionOfTanhWeight::usage = "u is a function of the form f[Tanh[v],Coth[v]] where f is independent of x.  FunctionOfTanhWeight[u,v,x] returns a nonnegative number if u is best considered a function of Tanh[v]; else it returns a negative number.";
FunctionOfTanhWeight[u_,v_,x_] :=
  If[AtomQ[u],
    0,
  If[CalculusQ[u],
    0,
  If[HyperbolicQ[u] && IntegerQuotientQ[u[[1]],v],
    If[Head[u]===Tanh && EqQ[u[[1]],v],
      1,
    If[Head[u]===Coth && EqQ[u[[1]],v],
      -1,
    0]],
  If[PowerQ[u] && EvenQ[u[[2]]] && HyperbolicQ[u[[1]]] && IntegerQuotientQ[u[[1,1]],v],
    If[Head[u[[1]]]===Tanh || Head[u[[1]]]===Cosh || Head[u[[1]]]===Sech,
      1,
    -1],
  If[ProductQ[u],
    If[Catch[Scan[Function[If[Not[FunctionOfTanhQ[#,v,x]],Throw[False]]],u];True],
      Apply[Plus,Map[Function[FunctionOfTanhWeight[#,v,x]],Apply[List,u]]],
    0],
  Apply[Plus,Map[Function[FunctionOfTanhWeight[#,v,x]],Apply[List,u]]]]]]]]


FunctionOfHyperbolicQ::usage = "If u (x) is equivalent to a function of the form f (Sinh[v],Cosh[v],Tanh[v],Coth[v],Sech[v],Csch[v]) where f is independent of x, FunctionOfHyperbolicQ[u,v,x] returns True; else it returns False.";
FunctionOfHyperbolicQ[u_,v_,x_Symbol] :=
  If[AtomQ[u],
    u=!=x,
  If[CalculusQ[u],
    False,
  If[HyperbolicQ[u] && IntegerQuotientQ[u[[1]],v],
    True,
  Catch[Scan[Function[If[FunctionOfHyperbolicQ[#,v,x],Null,Throw[False]]],u];True]]]]


FindTrigFactor::usage = "If func[w]^m is a factor of u where m is odd and w is an integer multiple of v, FindTrigFactor[func1,func2,u,v,True] returns the list {w,u/func[w]^n}; else it returns False.  If func[w]^m is a factor of u where m is odd and w is an integer multiple of v not equal to v, FindTrigFactor[func1,func2,u,v,False] returns the list {w,u/func[w]^n}; else it returns False.";
FindTrigFactor[func1_,func2_,u_,v_,flag_] :=
  If[u===1,
    False,
  If[(Head[LeadBase[u]]===func1 || Head[LeadBase[u]]===func2) &&
		OddQ[LeadDegree[u]] &&
		IntegerQuotientQ[LeadBase[u][[1]],v] &&
		(flag || NeQ[LeadBase[u][[1]],v]),
    {LeadBase[u][[1]], RemainingFactors[u]},
  With[{lst=FindTrigFactor[func1,func2,RemainingFactors[u],v,flag]},
  If[AtomQ[lst],
    False,
  {lst[[1]], LeadFactor[u]*lst[[2]]}]]]]


IntegerQuotientQ::usage = "If u/v is an integer, IntegerQuotientQ[u,v] returns True; else it returns False.";
IntegerQuotientQ[u_,v_] :=
(* u===v || EqQ[u,v] || IntegerQ[u/v] *)
  IntegerQ[Simplify[u/v]]

OddQuotientQ::usage = "If u/v is odd, OddQuotientQ[u,v] returns True; else it returns False.";
OddQuotientQ[u_,v_] :=
(* u===v || EqQ[u,v] || OddQ[u/v] *)
  OddQ[Simplify[u/v]]

EvenQuotientQ::usage = "If u/v is even, EvenQuotientQ[u,v] returns True; else it returns False.";
EvenQuotientQ[u_,v_] :=
  EvenQ[Simplify[u/v]]


(* ::Subsection::Closed:: *)
(*FunctionOfDensePolynomialsQ*)


FunctionOfDensePolynomialsQ::usage = "If all occurrences of x in u (x) are in dense polynomials, FunctionOfDensePolynomialsQ[u,x] returns True; else it returns False.";
FunctionOfDensePolynomialsQ[u_,x_Symbol] :=
  If[FreeQ[u,x],
    True,
  If[PolynomialQ[u,x],
    Length[Exponent[u,x,List]]>1,
  Catch[
  Scan[Function[If[FunctionOfDensePolynomialsQ[#,x],Null,Throw[False]]],u];
  True]]]


(* ::Subsection::Closed:: *)
(*FunctionOfLog*)


FunctionOfLog::usage = "If u (x) is equivalent to an expression of the form f (Log[a*x^n]), FunctionOfLog[u,x] returns the list {f (x),a*x^n,n}; else it returns False.";
FunctionOfLog[u_,x_Symbol] :=
  With[{lst=FunctionOfLog[u,False,False,x]},
  If[AtomQ[lst] || FalseQ[lst[[2]]],
    False,
  lst]]


FunctionOfLog[u_,v_,n_,x_] :=
  If[AtomQ[u],
    If[u===x,
      False,
    {u,v,n}],
  If[CalculusQ[u],
    False,
  Module[{lst},
  If[LogQ[u] && ListQ[lst=BinomialParts[u[[1]],x]] && EqQ[lst[[1]],0],
    If[FalseQ[v] || u[[1]]===v,
      {x,u[[1]],lst[[3]]},
    False],
  lst={0,v,n};
  Catch[
    {Map[Function[lst=FunctionOfLog[#,lst[[2]],lst[[3]],x];
				  If[AtomQ[lst],Throw[False],lst[[1]]]],
			u],lst[[2]],lst[[3]]}]]]]]


(* ::Subsection::Closed:: *)
(*PowerVariableExpn*)


PowerVariableExpn::usage = "If m is an integer, u is an expression of the form f[(c*x)^n] and g=GCD[m,n]>1, PowerVariableExpn[u,m,x] returns the list {x^(m/g)*f[(c*x)^(n/g)],g,c}; else it returns False.";
PowerVariableExpn[u_,m_,x_Symbol] :=
  If[IntegerQ[m],
    With[{lst=PowerVariableDegree[u,m,1,x]},
    If[AtomQ[lst],
      False,
    {x^(m/lst[[1]])*PowerVariableSubst[u,lst[[1]],x], lst[[1]], lst[[2]]}]],
  False]


PowerVariableDegree[u_,m_,c_,x_Symbol] :=
  If[FreeQ[u,x],
    {m, c},
  If[AtomQ[u] || CalculusQ[u],
    False,
  If[PowerQ[u] && FreeQ[u[[1]]/x,x],
    If[EqQ[m,0] || m===u[[2]] && c===u[[1]]/x,
      {u[[2]], u[[1]]/x},
    If[IntegerQ[u[[2]]] && IntegerQ[m] && GCD[m,u[[2]]]>1 && c===u[[1]]/x,
      {GCD[m,u[[2]]], c},
    False]],
  Catch[Module[{lst={m, c}},
  Scan[Function[lst=PowerVariableDegree[#,lst[[1]],lst[[2]],x];If[AtomQ[lst],Throw[False]]],u];
  lst]]]]]


PowerVariableSubst[u_,m_,x_Symbol] :=
  If[FreeQ[u,x] || AtomQ[u] ||CalculusQ[u],
    u,
  If[PowerQ[u] && FreeQ[u[[1]]/x,x],
    x^(u[[2]]/m),
  Map[Function[PowerVariableSubst[#,m,x]],u]]]


(* ::Subsection::Closed:: *)
(*EulerIntegrandQ*)


(* ::Subsubsection::Closed:: *)
(*Note: If an Euler substitution should be used to integrate u wrt x,  EulerIntegrandQ[u,x] returns True.*)


EulerIntegrandQ[(a_.*x_+b_.*u_^n_)^p_,x_Symbol] :=
  True /;
FreeQ[{a,b},x] && IntegerQ[n+1/2] && QuadraticQ[u,x] && (Not[RationalQ[p]] || ILtQ[p,0] && Not[BinomialQ[u,x]])


EulerIntegrandQ[v_^m_.*(a_.*x_+b_.*u_^n_)^p_,x_Symbol] :=
  True /;
FreeQ[{a,b},x] && EqQ[u,v] && IntegersQ[2*m,n+1/2] && QuadraticQ[u,x] &&
  (Not[RationalQ[p]] || ILtQ[p,0] && Not[BinomialQ[u,x]])


EulerIntegrandQ[v_^m_.*(a_.*x_+b_.*u_^n_)^p_,x_Symbol] :=
  True /;
FreeQ[{a,b},x] && EqQ[u,v] && IntegersQ[2*m,n+1/2] && QuadraticQ[u,x] &&
  (Not[RationalQ[p]] || ILtQ[p,0] && Not[BinomialQ[u,x]])


EulerIntegrandQ[u_^n_*v_^p_,x_Symbol] :=
  True /;
ILtQ[p,0] && IntegerQ[n+1/2] && QuadraticQ[u,x] && QuadraticQ[v,x] && Not[BinomialQ[v,x]]

EulerIntegrandQ[u_,x_Symbol] :=
  False


(* ::Subsection::Closed:: *)
(*FunctionOfSquareRootOfQuadratic*)


(*
Euler substitution #2:
  If u is an expression of the form f (Sqrt[a+b*x+c*x^2],x), f (x,x) is a rational function, and
	PosQ[c], FunctionOfSquareRootOfQuadratic[u,x] returns the 3-element list {
		f ((a*Sqrt[c]+b*x+Sqrt[c]*x^2)/(b+2*Sqrt[c]*x),(-a+x^2)/(b+2*Sqrt[c]*x))*
		  (a*Sqrt[c]+b*x+Sqrt[c]*x^2)/(b+2*Sqrt[c]*x)^2,
		Sqrt[c]*x+Sqrt[a+b*x+c*x^2], 2 };

Euler substitution #1:
  If u is an expression of the form f (Sqrt[a+b*x+c*x^2],x), f (x,x) is a rational function, and
	PosQ[a], FunctionOfSquareRootOfQuadratic[u,x] returns the two element list {
		f ((c*Sqrt[a]-b*x+Sqrt[a]*x^2)/(c-x^2),(-b+2*Sqrt[a]*x)/(c-x^2))*
		  (c*Sqrt[a]-b*x+Sqrt[a]*x^2)/(c-x^2)^2,
		(-Sqrt[a]+Sqrt[a+b*x+c*x^2])/x, 1 };

Euler substitution #3:
  If u is an expression of the form f (Sqrt[a+b*x+c*x^2],x), f (x,x) is a rational function, and
	NegQ[a] and NegQ[c], FunctionOfSquareRootOfQuadratic[u,x] returns the two element list {
		-Sqrt[b^2-4*a*c]*
		f (-Sqrt[b^2-4*a*c]*x/(c-x^2),-(b*c+c*Sqrt[b^2-4*a*c]+(-b+Sqrt[b^2-4*a*c])*x^2)/(2*c*(c-x^2)))*
		  x/(c-x^2)^2,
		2*c*Sqrt[a+b*x+c*x^2]/(b-Sqrt[b^2-4*a*c]+2*c*x), 3 };

  else it returns False. *)

FunctionOfSquareRootOfQuadratic[u_,x_Symbol] :=
  If[MatchQ[u,x^m_.*(a_+b_.*x^n_.)^p_ /; FreeQ[{a,b,m,n,p},x]],
    False,
  Module[{tmp=FunctionOfSquareRootOfQuadratic[u,False,x]},
  If[AtomQ[tmp] || FalseQ[tmp[[1]]],
    False,
  tmp=tmp[[1]];
  Module[{a=Coefficient[tmp,x,0],b=Coefficient[tmp,x,1],c=Coefficient[tmp,x,2],sqrt,q,r},
  If[EqQ[a,0] && EqQ[b,0] || EqQ[b^2-4*a*c,0],
    False,
  If[PosQ[c],
    sqrt=Rt[c,2];
    q=a*sqrt+b*x+sqrt*x^2;
    r=b+2*sqrt*x;
    {Simplify[SquareRootOfQuadraticSubst[u,q/r,(-a+x^2)/r,x]*q/r^2],
     Simplify[sqrt*x+Sqrt[tmp]],
     2},
  If[PosQ[a],
    sqrt=Rt[a,2];
    q=c*sqrt-b*x+sqrt*x^2;
    r=c-x^2;
    {Simplify[SquareRootOfQuadraticSubst[u,q/r,(-b+2*sqrt*x)/r,x]*q/r^2],
     Simplify[(-sqrt+Sqrt[tmp])/x],
     1},
  sqrt=Rt[b^2-4*a*c,2];
  r=c-x^2;
  {Simplify[-sqrt*SquareRootOfQuadraticSubst[u,-sqrt*x/r,-(b*c+c*sqrt+(-b+sqrt)*x^2)/(2*c*r),x]*x/r^2],
   FullSimplify[2*c*Sqrt[tmp]/(b-sqrt+2*c*x)],
   3}]]]]]]]


FunctionOfSquareRootOfQuadratic[u_,v_,x_Symbol] :=
  If[AtomQ[u] || FreeQ[u,x],
    {v},
  If[PowerQ[u] && FreeQ[u[[2]],x],
    If[FractionQ[u[[2]]] && Denominator[u[[2]]]==2 && PolynomialQ[u[[1]],x] && Exponent[u[[1]],x]==2,
      If[(FalseQ[v] || u[[1]]===v),
        {u[[1]]},
      False],
    FunctionOfSquareRootOfQuadratic[u[[1]],v,x]],
  If[ProductQ[u] || SumQ[u],
    Catch[Module[{lst={v}},
    Scan[Function[lst=FunctionOfSquareRootOfQuadratic[#,lst[[1]],x];If[AtomQ[lst],Throw[False]]],u];
    lst]],
  False]]]


SquareRootOfQuadraticSubst::usage = "SquareRootOfQuadraticSubst[u,vv,xx,x] returns u with fractional powers replaced by vv raised to the power and x replaced by xx.";
SquareRootOfQuadraticSubst[u_,vv_,xx_,x_Symbol] :=
  If[AtomQ[u] || FreeQ[u,x],
    If[u===x,
      xx,
    u],
  If[PowerQ[u] && FreeQ[u[[2]],x],
    If[FractionQ[u[[2]]] && Denominator[u[[2]]]==2 && PolynomialQ[u[[1]],x] && Exponent[u[[1]],x]==2,
      vv^Numerator[u[[2]]],
    SquareRootOfQuadraticSubst[u[[1]],vv,xx,x]^u[[2]]],
  Map[Function[SquareRootOfQuadraticSubst[#,vv,xx,x]],u]]]


(* ::Section::Closed:: *)
(*Substitution functions*)


(* ::Subsection::Closed:: *)
(*Subst[u,v,w]*)


Subst::usage = "Subst[u,x,v] returns u with all nondummy occurences of x replaced by v and resulting constant terms replaced by 0.";
Subst[u_,x_Symbol,v_] :=
  If[PowerQ[v] && Not[IntegerQ[v[[2]]]] &&
      MatchQ[v[[1]],a_+b_.*x+c_.*x^2 /; FreeQ[{a,b,c},x] && Not[AtomQ[b]]] &&
      LeafCount[Simplify[v[[1]]]]<2/3*LeafCount[v[[1]]],
    Subst[u,x,Simplify[v[[1]]]^v[[2]]],
  If[SumQ[u],
    If[BinomialQ[v,x],
      SimplifyAntiderivative[Map[Function[SubstAux[#,x,v,True]],u],x],
    SimplifyAntiderivative[Map[Function[SubstAux[#,x,v,False]],u],x]],
  SimplifyAntiderivative[SubstAux[u,x,v,BinomialQ[v,x]],x]]]

Subst[u_,Rule[x_Symbol,v_]] := Subst[u,x,v]


Subst[u_,(a_.*x_)^n_,v_] :=
  If[AtomQ[u],
    u,
  If[RationalQ[n] && Numerator[n]!=1,
    Subst[u,(a*x)^(1/Denominator[n]),v/(a*x)^(n-1/Denominator[n])],
  If[PowerQ[u] && FreeQ[u[[2]],x] && u[[1]]===a*x,
    If[IntegerQ[u[[2]]/n],
      Simplify[v^(u[[2]]/n)],
    If[SumQ[u[[2]]],
      Apply[Times,Map[Function[Subst[u[[1]]^#,(a*x)^n,v]],Apply[List,u[[2]]]]],
    With[{w=Expand[u[[2]]]},
    If[SumQ[w],
      Apply[Times,Map[Function[Subst[u[[1]]^#,(a*x)^n,v]],Apply[List,w]]],
    With[{m=NumericFactor[u[[2]]]},
    If[Numerator[m]!=1,
      Subst[u[[1]]^(m/Numerator[m]*NonnumericFactors[u[[2]]]),(a*x)^n,v]^Numerator[m],
    Subst[u[[1]],(a*x)^n,v]^u[[2]]]]]]]],
  If[CalculusQ[u] && Not[FreeQ[x,u[[2]]]] || HeldFormQ[u] && Head[u]=!=Defer[AppellF1],
    Defer[Subst][u,(a*x)^n,v],
  Map[Function[Subst[#,(a*x)^n,v]],u]]]]] /;
FreeQ[{a,n},x]


Subst[u_,v_,w_] :=
  If[u===v,
    w,
  If[AtomQ[u],
    u,
  If[CalculusQ[u] && Not[FreeQ[v,u[[2]]]] || HeldFormQ[u] && Head[u]=!=Defer[AppellF1],
    Defer[Subst][u,v,w],
  Map[Function[Subst[#,v,w]],u]]]]


(* ::Subsection::Closed:: *)
(*SubstAux[u,x,v]*)


SubstAux::usage = "x is a variable symbol.  SubstAux[u,x,v,flag] returns u with all nondummy occurences of x replaced by v.  If flag is True, SubstAux assumes v is a binomial in x.";
SubstAux[a_+b_.*x_,x_,c_.*F_[z_]^2,False] :=
  a*Simplify[1-F[z]^2] /;
FreeQ[{a,b,c},x] && MemberQ[{Sin,Cos,Sec,Csc,Cosh,Tanh,Coth,Sech},F] && EqQ[a+b*c,0]

SubstAux[a_+b_.*x_,x_,c_.*F_[z_]^2,False] :=
  a*Simplify[1+F[z]^2] /;
FreeQ[{a,b,c},x] && MemberQ[{Tan,Cot,Sinh,Csch},F] && EqQ[a-b*c,0]


SubstAux[a_+b_.*x_^2,x_,c_.*F_[z_],False] :=
  a*Simplify[1-F[z]^2] /;
FreeQ[{a,b,c},x] && MemberQ[{Sin,Cos,Sec,Csc,Cosh,Tanh,Coth,Sech},F] && EqQ[a+b*c^2,0]

SubstAux[a_+b_.*x_^2,x_,c_.*F_[z_],False] :=
  a*Simplify[1+F[z]^2] /;
FreeQ[{a,b,c},x] && MemberQ[{Tan,Cot,Sinh,Csch},F] && EqQ[a-b*c^2,0]


SubstAux[F_[a_.*x_^m_.],x_,b_.*x_^n_,flag_] :=
  Switch[F,
    ArcSin, ArcCsc,  ArcCos, ArcSec,  ArcTan, ArcCot,  ArcCot, ArcTan,  ArcSec, ArcCos,  ArcCsc, ArcSin,
    ArcSinh, ArcCsch,  ArcCosh, ArcSech,  ArcTanh, ArcCoth,  ArcCoth, ArcTanh,  ArcSech, ArcCosh,  ArcCsch, ArcSinh][x^(-m*n)/(a*b^m)] /;
FreeQ[{a,b},x] && IGtQ[m,0] && ILtQ[n,0] && MemberQ[{ArcSin,ArcCos,ArcTan,ArcCot,ArcSec,ArcCsc,ArcSinh,ArcCosh,ArcTanh,ArcCoth,ArcSech,ArcCsch},F]


SubstAux[e_+f_.*x_,x_,(a_.+b_.*x_)/(c_.+d_.*x_),flag_] :=
  Together[c*e+a*f]/(c+d*x) /;
FreeQ[{a,b,c,d,e,f},x] && EqQ[d*e+b*f,0]


SubstAux[u_,x_,v_,flag_] :=
  If[AtomQ[u],
    If[u===x,
      v,
    u],
  If[FreeQ[u,x],
    u,
  If[TrueQ[flag] && PowerQ[u],
    If[Not[IntegerQ[u[[2]]]] && LinearQ[u[[1]],x],
      Simp[SubstAux[u[[1]],x,v,flag],x]^SubstAux[u[[2]],x,v,flag],
    SubstAux[u[[1]],x,v,flag]^SubstAux[u[[2]],x,v,flag]],
  If[Head[u]===Defer[Subst],
    If[u[[2]]===x || FreeQ[u[[1]],x],
      SubstAux[u[[1]],u[[2]],SubstAux[u[[3]],x,v,flag],flag],
    Defer[Subst][u,x,v]],
  If[SimplifyFlag && MemberQ[{Unintegrable,CannotIntegrate},Head[u]] && u[[2]]===x,
    With[{w=Simplify[D[v,x]]}, FreeFactors[w,x]*Head[u][Subst[u[[1]],x,v]*NonfreeFactors[w,x],x]],
  If[CalculusQ[u] && Not[FreeQ[x,u[[2]]]] || HeldFormQ[u] && Head[u]=!=Defer[AppellF1],
    Defer[Subst][u,x,v],
  If[TrueQ[flag] && Length[u]==1 && LinearQ[u[[1]],x],
    Head[u][Simplify[SubstAux[u[[1]],x,v,flag]]],
  If[TrueQ[flag] && Head[u]===PolyLog && Length[u]==2 && LinearQ[u[[2]],x],
    PolyLog[SubstAux[u[[1]],x,v,flag],Simplify[SubstAux[u[[2]],x,v,flag]]],
  With[{w=Simp[Map[Function[SubstAux[#,x,v,flag]],u],x]},
  If[PolyQ[w,x],
    With[{z=If[LinearQ[v,x] || MonomialQ[v,x],ExpandToSum[w,x],Simplify[w]]},
(*  Print[{u,v,w,z}]; *)
    If[LeafCount[z]<=If[LinearQ[u,x], 3/4, 9/10]*LeafCount[w],
      If[EqQ[NumericFactor[z]^2,1],
        z,
      NumericFactor[z]*NonnumericFactors[z]],
    If[EqQ[NumericFactor[w]^2,1],
      w,
    NumericFactor[w]*NonnumericFactors[w]]]],
  With[{ulst=BinomialParts[u,x]},
  If[Not[FalseQ[ulst]] && IGtQ[ulst[[3]],0] && NeQ[ulst[[1]],0] && (NeQ[ulst[[3]],1] || AlgebraicFunctionQ[v,x]),
    With[{z=Simplify[w]},
    If[LeafCount[z]<9/10*LeafCount[w],
      z,
    w]],
  w]]]]]]]]]]]]


(* ::Subsection::Closed:: *)
(*SimplifyAntiderivative[u,x]*)


SimplifyAntiderivative::usage = "SimplifyAntiderivative[u,x] returns the simplest, continuous expression whose derivative wrt x equals the derivative of u wrt x.";
Clear[SimplifyAntiderivative];


(* ::Item:: *)
(*Basis: D[c*F[x], x] == c*D[F[x], x]*)


SimplifyAntiderivative[c_*u_,x_Symbol] :=
  With[{v=SimplifyAntiderivative[u,x]},
  If[SumQ[v] && NonsumQ[u],
    Map[Function[c*#],v],
  c*v]] /;
FreeQ[c,x]


(* ::Item:: *)
(*Basis: D[Log[c*F[x]], x] == D[Log[F[x]], x]*)


SimplifyAntiderivative[Log[c_*u_],x_Symbol] :=
  SimplifyAntiderivative[Log[u],x] /;
FreeQ[c,x]


(* ::Item:: *)
(*Basis: D[Log[F[x]^n], x] == n*D[Log[F[x]], x]*)


SimplifyAntiderivative[Log[u_^n_],x_Symbol] :=
  n*SimplifyAntiderivative[Log[u],x] /;
FreeQ[n,x]


(* ::Item:: *)
(*Basis: If  F\[Element]{Log,ArcTan,ArcCot}, then D[F[G[x]], x] == -D[F[1/G[x]], x]*)


SimplifyAntiderivative[F_[G_[u_]],x_Symbol] :=
  -SimplifyAntiderivative[F[1/G[u]],x] /;
MemberQ[{Log,ArcTan,ArcCot},F] && MemberQ[{Cot,Sec,Csc,Coth,Sech,Csch},G]


(* ::Item:: *)
(*Basis: If  F\[Element]{ArcTanh,ArcCoth}, then D[F[G[x]], x] == D[F[1/G[x]], x]*)


SimplifyAntiderivative[F_[G_[u_]],x_Symbol] :=
  SimplifyAntiderivative[F[1/G[u]],x] /;
MemberQ[{ArcTanh,ArcCoth},F] && MemberQ[{Cot,Sec,Csc,Coth,Sech,Csch},G]


SimplifyAntiderivative[Log[F_[u_]],x_Symbol] :=
  -SimplifyAntiderivative[Log[1/F[u]],x] /;
MemberQ[{Cot,Sec,Csc,Coth,Sech,Csch},F]


(* ::Item:: *)
(*Basis: D[Log[f^G[x]], x] == Log[f]*D[G[x], x]*)


SimplifyAntiderivative[Log[f_^u_],x_Symbol] :=
  Log[f]*SimplifyAntiderivative[u,x] /;
FreeQ[f,x]


(* ::Item:: *)
(*Basis: If  a^2+b^2==0, then D[Log[a + b*Tan[F[x]]], x] == (b*D[F[x], x])/a - D[Log[Cos[F[x]]], x]*)


SimplifyAntiderivative[Log[a_+b_.*Tan[u_]],x_Symbol] :=
  b/a*SimplifyAntiderivative[u,x] - SimplifyAntiderivative[Log[Cos[u]],x] /;
FreeQ[{a,b},x] && EqQ[a^2+b^2,0]


(* ::Item:: *)
(*Basis: If  a^2+b^2==0, then D[Log[a + b*Cot[F[x]]], x] == -((b/a)*D[F[x], x]) - D[Log[Sin[F[x]]], x]*)


SimplifyAntiderivative[Log[a_+b_.*Cot[u_]],x_Symbol] :=
  -b/a*SimplifyAntiderivative[u,x] - SimplifyAntiderivative[Log[Sin[u]],x] /;
FreeQ[{a,b},x] && EqQ[a^2+b^2,0]


(* ::Input:: *)
(* *)


(* ::Item:: *)
(*Note: Should also recognize that Tan[z/2]==Sin[z]/(1+Cos[z])==Tan[z]/(1+Sec[z])*)


SimplifyAntiderivative[ArcTan[a_.*Tan[u_]],x_Symbol] :=
  RectifyTangent[u,a,1,x] /;
FreeQ[a,x] && GtQ[a^2,0] && ComplexFreeQ[u]


(* ::Item:: *)
(*Basis: D[ArcCot[a*Tan[f[x]]], x] == -D[ArcTan[a*Tan[f[x]]], x]*)


SimplifyAntiderivative[ArcCot[a_.*Tan[u_]],x_Symbol] :=
  RectifyTangent[u,a,-1,x] /;
FreeQ[a,x] && GtQ[a^2,0] && ComplexFreeQ[u]


(* ::Item:: *)
(*Basis: ArcTan[a Tanh[z]]==-ArcTan[I a Tan[I z]]*)


(* SimplifyAntiderivative[ArcTan[a_.*Tanh[u_]],x_Symbol] :=
  RectifyTangent[I*u,I*a,-1,x] /;
FreeQ[a,x] && GtQ[a^2,0] && ComplexFreeQ[u] *)


(* ::Item:: *)
(*Basis: D[ArcCot[a*Tanh[f[x]]], x] == -D[ArcTan[a*Tanh[f[x]]], x]*)


SimplifyAntiderivative[ArcCot[a_.*Tanh[u_]],x_Symbol] :=
  -SimplifyAntiderivative[ArcTan[a*Tanh[u]],x] /;
FreeQ[a,x] && ComplexFreeQ[u]


(* ::Item:: *)
(*Basis: ArcTanh[a Tan[z]]==-I ArcTan[I a Tan[z]]*)


SimplifyAntiderivative[ArcTanh[a_.*Tan[u_]],x_Symbol] :=
  RectifyTangent[u,I*a,-I,x] /;
FreeQ[a,x] && GtQ[a^2,0] && ComplexFreeQ[u]


(* ::Item:: *)
(*Basis: D[ArcCoth[a*Tan[f[x]]], x] == -(I*D[ArcTan[I*a*Tan[f[x]]], x])*)


SimplifyAntiderivative[ArcCoth[a_.*Tan[u_]],x_Symbol] :=
  RectifyTangent[u,I*a,-I,x] /;
FreeQ[a,x] && GtQ[a^2,0] && ComplexFreeQ[u]


(* ::Item::Closed:: *)
(*Basis: ArcTanh[a Tanh[z]]==-I ArcTan[a Tan[I z]]*)


(* ::Item:: *)
(*Note: Although this makes the antiderivative continuous on the imaginary line, it makes it discontinous on the real line.*)


(* SimplifyAntiderivative[ArcTanh[a_.*Tanh[u_]],x_Symbol] :=
  RectifyTangent[I*u,a,-I,x] /;
FreeQ[a,x] && GtQ[a^2,0] && ComplexFreeQ[u] *)


(* ::Item:: *)
(*Basis: D[ArcTanh[Tanh[f[x]]], x] == D[f[x], x]*)


SimplifyAntiderivative[ArcTanh[Tanh[u_]],x_Symbol] :=
  SimplifyAntiderivative[u,x]


(* ::Item::Closed:: *)
(*Basis: D[ArcCoth[a*Tanh[f[x]]], x] == -(I*D[ArcTan[a*Tan[I*f[x]]], x])*)


(* ::Item:: *)
(*Note: Although this makes the antiderivative continuous on the imaginary line, it makes it discontinous on the real line.*)


(* SimplifyAntiderivative[ArcCoth[a_.*Tanh[u_]],x_Symbol] :=
  RectifyTangent[I*u,a,-I,x] /;
FreeQ[a,x] && GtQ[a^2,0] && ComplexFreeQ[u] *)


(* ::Item:: *)
(*Basis: D[ArcCoth[Tanh[f[x]]], x] == D[f[x], x]*)


SimplifyAntiderivative[ArcCoth[Tanh[u_]],x_Symbol] :=
  SimplifyAntiderivative[u,x]


(* ::Input:: *)
(* *)


SimplifyAntiderivative[ArcCot[a_.*Cot[u_]],x_Symbol] :=
  RectifyCotangent[u,a,1,x] /;
FreeQ[a,x] && GtQ[a^2,0] && ComplexFreeQ[u]


(* ::Item:: *)
(*Basis: D[ArcTan[a*Cot[f[x]]], x] == -D[ArcCot[a*Cot[f[x]]], x]*)


SimplifyAntiderivative[ArcTan[a_.*Cot[u_]],x_Symbol] :=
  RectifyCotangent[u,a,-1,x] /;
FreeQ[a,x] && GtQ[a^2,0] && ComplexFreeQ[u]


(* ::Item:: *)
(*Basis: ArcCot[a Coth[z]]==ArcCot[I a Cot[I z]]*)


(* SimplifyAntiderivative[ArcCot[a_.*Coth[u_]],x_Symbol] :=
  RectifyCotangent[I*u,I*a,1,x] /;
FreeQ[a,x] && GtQ[a^2,0] && ComplexFreeQ[u] *)


(* ::Item:: *)
(*Basis: D[ArcTan[a*Coth[f[x]]], x] == -D[ArcTan[Tanh[f[x]]/a], x]*)


SimplifyAntiderivative[ArcTan[a_.*Coth[u_]],x_Symbol] :=
  -SimplifyAntiderivative[ArcTan[Tanh[u]/a],x] /;
FreeQ[a,x] && ComplexFreeQ[u]


(* ::Item:: *)
(*Basis: ArcCoth[a Cot[z]]==I ArcCot[I a Cot[z]]*)


SimplifyAntiderivative[ArcCoth[a_.*Cot[u_]],x_Symbol] :=
  RectifyCotangent[u,I*a,I,x] /;
FreeQ[a,x] && GtQ[a^2,0] && ComplexFreeQ[u]


(* ::Item:: *)
(*Basis: D[ArcTanh[a*Cot[f[x]]], x] == I*D[ArcCot[I*a*Cot[f[x]]], x]*)


SimplifyAntiderivative[ArcTanh[a_.*Cot[u_]],x_Symbol] :=
  RectifyCotangent[u,I*a,I,x] /;
FreeQ[a,x] && GtQ[a^2,0] && ComplexFreeQ[u]


(* ::Item::Closed:: *)
(*Basis: ArcCoth[a Coth[z]]==-I ArcCot[a Cot[I z]]*)


(* ::Item:: *)
(*Note: Although this makes the antiderivative continuous on the imaginary line, it makes it discontinous on the real line.*)


(* SimplifyAntiderivative[ArcCoth[a_.*Coth[u_]],x_Symbol] :=
  RectifyCotangent[I*u,a,-I,x] /;
FreeQ[a,x] && GtQ[a^2,0] && ComplexFreeQ[u] *)


(* ::Item:: *)
(*Basis: D[ArcCoth[Coth[f[x]]], x] == D[f[x], x]*)


SimplifyAntiderivative[ArcCoth[Coth[u_]],x_Symbol] :=
  SimplifyAntiderivative[u,x]


(* ::Item:: *)
(*Basis: D[ArcTanh[a*Coth[f[x]]], x] == D[ArcTanh[Tanh[f[x]]/a], x]*)


SimplifyAntiderivative[ArcTanh[a_.*Coth[u_]],x_Symbol] :=
  SimplifyAntiderivative[ArcTanh[Tanh[u]/a],x] /;
FreeQ[a,x] && ComplexFreeQ[u]


(* ::Item:: *)
(*Basis: D[ArcTanh[Coth[f[x]]], x] == D[f[x], x]*)


SimplifyAntiderivative[ArcTanh[Coth[u_]],x_Symbol] :=
  SimplifyAntiderivative[u,x]


(* ::Input:: *)
(* *)


SimplifyAntiderivative[ArcTan[c_.*(a_+b_.*Tan[u_])],x_Symbol] :=
  RectifyTangent[u,a*c,b*c,1,x] /;
FreeQ[{a,b,c},x] && GtQ[a^2*c^2,0] && GtQ[b^2*c^2,0] && ComplexFreeQ[u]


(* ::Item:: *)
(*Basis: ArcTanh[a+b Tan[z]]==-I ArcTan[I a+I b Tan[z]]*)


SimplifyAntiderivative[ArcTanh[c_.*(a_+b_.*Tan[u_])],x_Symbol] :=
  RectifyTangent[u,I*a*c,I*b*c,-I,x] /;
FreeQ[{a,b,c},x] && GtQ[a^2*c^2,0] && GtQ[b^2*c^2,0] && ComplexFreeQ[u]


(* ::Input:: *)
(* *)


SimplifyAntiderivative[ArcTan[c_.*(a_+b_.*Cot[u_])],x_Symbol] :=
  RectifyCotangent[u,a*c,b*c,1,x] /;
FreeQ[{a,b,c},x] && GtQ[a^2*c^2,0] && GtQ[b^2*c^2,0] && ComplexFreeQ[u]


(* ::Item:: *)
(*Basis: ArcTanh[a+b Cot[z]]==-I ArcTan[I a+I b Cot[z]]*)


SimplifyAntiderivative[ArcTanh[c_.*(a_+b_.*Cot[u_])],x_Symbol] :=
  RectifyCotangent[u,I*a*c,I*b*c,-I,x] /;
FreeQ[{a,b,c},x] && GtQ[a^2*c^2,0] && GtQ[b^2*c^2,0] && ComplexFreeQ[u]


(* ::Input:: *)
(* *)


(* ::Item:: *)
(*Basis: D[ArcTan[a + b*Tan[F[x]] + c*Tan[F[x]]^2], x] == D[ArcTan[(c + (a - c - 1)*Cos[F[x]]^2 + b*Cos[F[x]]*Sin[F[x]])/(c + (a - c + 1)*Cos[F[x]]^2 + b*Cos[F[x]]*Sin[F[x]])], x] == D[ArcTan[(a + c - 1 + (a - c - 1)*Cos[2*F[x]] + b*Sin[2*F[x]])/(a + c + 1 + (a - c + 1)*Cos[2*F[x]] + b*Sin[2*F[x]])], x]*)


SimplifyAntiderivative[ArcTan[a_.+b_.*Tan[u_]+c_.*Tan[u_]^2],x_Symbol] :=
  If[EvenQ[Denominator[NumericFactor[Together[u]]]],
    ArcTan[NormalizeTogether[(a+c-1+(a-c-1)*Cos[2*u]+b*Sin[2*u])/(a+c+1+(a-c+1)*Cos[2*u]+b*Sin[2*u])]],
  ArcTan[NormalizeTogether[(c+(a-c-1)*Cos[u]^2+b*Cos[u]*Sin[u])/(c+(a-c+1)*Cos[u]^2+b*Cos[u]*Sin[u])]]] /;
FreeQ[{a,b,c},x] && ComplexFreeQ[u]

SimplifyAntiderivative[ArcTan[a_.+b_.*(d_.+e_.*Tan[u_])+c_.*(f_.+g_.*Tan[u_])^2],x_Symbol] :=
  SimplifyAntiderivative[ArcTan[a+b*d+c*f^2+(b*e+2*c*f*g)*Tan[u]+c*g^2*Tan[u]^2],x] /;
FreeQ[{a,b,c},x] && ComplexFreeQ[u]


(* ::Item:: *)
(*Basis: D[ArcTan[a + c*Tan[F[x]]^2], x] == D[ArcTan[(c + (a - c - 1)*Cos[F[x]]^2)/(c + (a - c + 1)*Cos[F[x]]^2)], x] == D[ArcTan[(a + c - 1 + (a - c - 1)*Cos[2*F[x]])/(a + c + 1 + (a - c + 1)*Cos[2*F[x]])], x]*)


SimplifyAntiderivative[ArcTan[a_.+c_.*Tan[u_]^2],x_Symbol] :=
  If[EvenQ[Denominator[NumericFactor[Together[u]]]],
    ArcTan[NormalizeTogether[(a+c-1+(a-c-1)*Cos[2*u])/(a+c+1+(a-c+1)*Cos[2*u])]],
  ArcTan[NormalizeTogether[(c+(a-c-1)*Cos[u]^2)/(c+(a-c+1)*Cos[u]^2)]]] /;
FreeQ[{a,c},x] && ComplexFreeQ[u]

SimplifyAntiderivative[ArcTan[a_.+c_.*(f_.+g_.*Tan[u_])^2],x_Symbol] :=
  SimplifyAntiderivative[ArcTan[a+c*f^2+(2*c*f*g)*Tan[u]+c*g^2*Tan[u]^2],x] /;
FreeQ[{a,c},x] && ComplexFreeQ[u]


(* ::Item:: *)
(*Basis: D[c, x] == D[0, x]*)


SimplifyAntiderivative[u_,x_Symbol] :=
  If[FreeQ[u,x],
    0,
  If[LogQ[u],
    With[{v=RemoveContent[u[[1]],x]},
    If[v===u[[1]],
      u,
    SimplifyAntiderivative[Log[v],x]]],
  If[SumQ[u],
    SimplifyAntiderivativeSum[Map[Function[SimplifyAntiderivative[#,x]],u],x],
  u]]]


(* ::Subsection::Closed:: *)
(*SimplifyAntiderivativeSum*)


Clear[SimplifyAntiderivativeSum];

SimplifyAntiderivativeSum[v_.+A_.*Log[a_+b_.*Tan[u_]^n_.]+B_.*Log[Cos[u_]],x_Symbol] :=
  SimplifyAntiderivativeSum[v,x] + A*Log[RemoveContent[a*Cos[u]^n+b*Sin[u]^n,x]] /;
FreeQ[{a,b,A,B},x] && IntegerQ[n] && EqQ[n*A-B,0]


SimplifyAntiderivativeSum[v_.+A_.*Log[a_+b_.*Cot[u_]^n_.]+B_.*Log[Sin[u_]],x_Symbol] :=
  SimplifyAntiderivativeSum[v,x] + A*Log[RemoveContent[a*Sin[u]^n+b*Cos[u]^n,x]] /;
FreeQ[{a,b,A,B},x] && IntegerQ[n] && EqQ[n*A-B,0]


SimplifyAntiderivativeSum[v_.+A_.*Log[a_+b_.*Tan[u_]^n_.]+B_.*Log[c_+d_.*Tan[u_]^n_.],x_Symbol] :=
  SimplifyAntiderivativeSum[v,x] + A*Log[RemoveContent[a*Cos[u]^n+b*Sin[u]^n,x]] + B*Log[RemoveContent[c*Cos[u]^n+d*Sin[u]^n,x]] /;
FreeQ[{a,b,c,d,A,B},x] && IntegerQ[n] && EqQ[A+B,0]


SimplifyAntiderivativeSum[v_.+A_.*Log[a_+b_.*Cot[u_]^n_.]+B_.*Log[c_+d_.*Cot[u_]^n_.],x_Symbol] :=
  SimplifyAntiderivativeSum[v,x] + A*Log[RemoveContent[b*Cos[u]^n+a*Sin[u]^n,x]] + B*Log[RemoveContent[d*Cos[u]^n+c*Sin[u]^n,x]] /;
FreeQ[{a,b,c,d,A,B},x] && IntegerQ[n] && EqQ[A+B,0]


SimplifyAntiderivativeSum[v_.+A_.*Log[a_+b_.*Tan[u_]^n_.]+B_.*Log[c_+d_.*Tan[u_]^n_.]+C_.*Log[e_+f_.*Tan[u_]^n_.],x_Symbol] :=
  SimplifyAntiderivativeSum[v,x] + A*Log[RemoveContent[a*Cos[u]^n+b*Sin[u]^n,x]] +
	B*Log[RemoveContent[c*Cos[u]^n+d*Sin[u]^n,x]] + C*Log[RemoveContent[e*Cos[u]^n+f*Sin[u]^n,x]] /;
FreeQ[{a,b,c,d,e,f,A,B,C},x] && IntegerQ[n] && EqQ[A+B+C,0]


SimplifyAntiderivativeSum[v_.+A_.*Log[a_+b_.*Cot[u_]^n_.]+B_.*Log[c_+d_.*Cot[u_]^n_.]+C_.*Log[e_+f_.*Cot[u_]^n_.],x_Symbol] :=
  SimplifyAntiderivativeSum[v,x] + A*Log[RemoveContent[b*Cos[u]^n+a*Sin[u]^n,x]] +
	B*Log[RemoveContent[d*Cos[u]^n+c*Sin[u]^n,x]] + C*Log[RemoveContent[f*Cos[u]^n+e*Sin[u]^n,x]] /;
FreeQ[{a,b,c,d,e,f,A,B,C},x] && IntegerQ[n] && EqQ[A+B+C,0]


SimplifyAntiderivativeSum[u_,x_Symbol] := u


(* ::Subsection::Closed:: *)
(*RectifyTangent*)


(* ::Item::Closed:: *)
(*Basis: ArcTan[I a Tan[f[x]]]==I ArcTanh[a Tan[f[x]]]*)


(* ::Item:: *)
(*Basis: D[ArcTanh[(c/e)*Tan[f[x]]], x] == D[Log[e*Cos[f[x]] + c*Sin[f[x]]]/2 - Log[e*Cos[f[x]] - c*Sin[f[x]]]/2, x]*)


(* ::Item:: *)
(*Note: Note unlike the above, the log terms in the following two identities are real-valued when c, e and f[x] are real.*)


(* ::Item:: *)
(*Basis: D[ArcTanh[(c/e)*Tan[f[x]]], x] == D[Log[c^2 + e^2 - (c^2 - e^2)*Cos[2*f[x]] + 2*c*e*Sin[2*f[x]]]/4 - Log[c^2 + e^2 - (c^2 - e^2)*Cos[2*f[x]] - 2*c*e*Sin[2*f[x]]]/4, x]*)


(* ::Item:: *)
(*Basis: D[ArcTanh[(c/e)*Tan[f[x]]], x] == D[Log[e^2 + 2*c*e*Cos[f[x]]*Sin[f[x]] + (c^2 - e^2)*Sin[f[x]]^2]/4 - Log[e^2 - 2*c*e*Cos[f[x]]*Sin[f[x]] + (c^2 - e^2)*Sin[f[x]]^2]/4, x]*)


(* ::Item:: *)
(*Basis: D[ArcTan[a*Tan[f[x]]], x] == D[f[x] - ArcTan[Sin[2*f[x]]/((1 + a)/(1 - a) + Cos[2*f[x]])], x]*)


(* ::Item:: *)
(*Basis: D[ArcTan[a*Tan[f[x]]], x] == D[f[x] + ArcTan[(Cos[f[x]]*Sin[f[x]])/(1/(a - 1) + Sin[f[x]]^2)], x] == D[f[x] - ArcTan[(Cos[f[x]]*Sin[f[x]])/(a/(1 - a) + Cos[f[x]]^2)], x]*)


(* ::Item:: *)
(*Note: If  a>0 and f[x] is real, then 1/(1-a)-Sin[f[x]]^2 is nonzero and f[x]-ArcTan[(Cos[f[x]] Sin[f[x]])/(1/(1-a)-Sin[f[x]]^2)] is continuous, unlike ArcTan[a Tan[f[x]]].*)


RectifyTangent::usage = "RectifyTangent[u,a,b,x] returns an expression whose derivative equals the derivative of b*ArcTan[a*Tan[u]] wrt x.  RectifyTangent[u,a,b,r,x] returns an expression whose derivative equals the derivative of r*ArcTan[a+b*Tan[u]] wrt x.";
RectifyTangent[u_,a_,b_,x_Symbol] :=
  If[MatchQ[Together[a],d_.*Complex[0,c_]],
    Module[{c=a/I,e},
    If[LtQ[c,0],
      RectifyTangent[u,-a,-b,x],
    If[EqQ[c,1],
      If[EvenQ[Denominator[NumericFactor[Together[u]]]],
        I*b*ArcTanh[Sin[2*u]]/2,
      I*b*ArcTanh[2*Cos[u]*Sin[u]]/2],
    e=SmartDenominator[c];
    c=c*e;
(*  If[EvenQ[Denominator[NumericFactor[Together[u]]]],
      I*b*Log[RemoveContent[c^2+e^2-(c^2-e^2)*Cos[2*u]+2*c*e*Sin[2*u],x]]/4 -
      I*b*Log[RemoveContent[c^2+e^2-(c^2-e^2)*Cos[2*u]-2*c*e*Sin[2*u],x]]/4,
    I*b*Log[RemoveContent[e^2+2*c*e*Cos[u]*Sin[u]+(c^2-e^2)*Sin[u]^2,x]]/4 -
    I*b*Log[RemoveContent[e^2-2*c*e*Cos[u]*Sin[u]+(c^2-e^2)*Sin[u]^2,x]]/4]]]], *)
    I*b*Log[RemoveContent[e*Cos[u]+c*Sin[u],x]]/2 -
    I*b*Log[RemoveContent[e*Cos[u]-c*Sin[u],x]]/2]]],
  If[LtQ[a,0],
    RectifyTangent[u,-a,-b,x],
  If[EqQ[a,1],
    b*SimplifyAntiderivative[u,x],
  Module[{c,numr,denr},
  If[EvenQ[Denominator[NumericFactor[Together[u]]]],
    c=Simplify[(1+a)/(1-a)];
    numr=SmartNumerator[c];
    denr=SmartDenominator[c];
    b*SimplifyAntiderivative[u,x] - b*ArcTan[NormalizeLeadTermSigns[denr*Sin[2*u]/(numr+denr*Cos[2*u])]],
  If[GtQ[a,1],
    c=Simplify[1/(a-1)];
    numr=SmartNumerator[c];
    denr=SmartDenominator[c];
    b*SimplifyAntiderivative[u,x] + b*ArcTan[NormalizeLeadTermSigns[denr*Cos[u]*Sin[u]/(numr+denr*Sin[u]^2)]],
  c=Simplify[a/(1-a)];
  numr=SmartNumerator[c];
  denr=SmartDenominator[c];
  b*SimplifyAntiderivative[u,x] - b*ArcTan[NormalizeLeadTermSigns[denr*Cos[u]*Sin[u]/(numr+denr*Cos[u]^2)]]]]]]]]


(* ::Input:: *)
(* *)


(* ::Item::Closed:: *)
(*Basis: ArcTan[I a+I b Tan[f[x]]]==I ArcTanh[a+b Tan[f[x]]]*)


(* ::Item:: *)
(*Basis: D[ArcTanh[c/e + (d/e)*Tan[f[x]]], x] == D[Log[(c + e)^2 + d^2 + ((c + e)^2 - d^2)*Cos[2*f[x]] + 2*(c + e)*d*Sin[2*f[x]]]/4 - Log[(c - e)^2 + d^2 + ((c - e)^2 - d^2)*Cos[2*f[x]] + 2*(c - e)*d*Sin[2*f[x]]]/4, x]*)


(* ::Item:: *)
(*Basis: D[ArcTanh[c/e + (d/e)*Tan[f[x]]], x] == D[Log[(c + e)^2 + 2*(c + e)*d*Cos[f[x]]*Sin[f[x]] - ((c + e)^2 - d^2)*Sin[f[x]]^2]/4 - Log[(c - e)^2 + 2*(c - e)*d*Cos[f[x]]*Sin[f[x]] - ((c - e)^2 - d^2)*Sin[f[x]]^2]/4, x]*)


(* ::Item:: *)
(*Basis: D[ArcTan[a + b*Tan[f[x]]], x] == D[f[x] + ArcTan[(2*a*b*Cos[2*f[x]] - (1 + a^2 - b^2)*Sin[2*f[x]])/(a^2 + (1 + b)^2 + (1 + a^2 - b^2)*Cos[2*f[x]] + 2*a*b*Sin[2*f[x]])], x]*)


(* ::Item:: *)
(*Basis: D[ArcTan[a + b*Tan[f[x]]], x] == D[f[x] - ArcTan[(a*b - 2*a*b*Cos[f[x]]^2 + (1 + a^2 - b^2)*Cos[f[x]]*Sin[f[x]])/(b*(1 + b) + (1 + a^2 - b^2)*Cos[f[x]]^2 + 2*a*b*Cos[f[x]]*Sin[f[x]])], x]*)


RectifyTangent[u_,a_,b_,r_,x_Symbol] :=
  If[MatchQ[Together[a],d_.*Complex[0,c_]] && MatchQ[Together[b],d_.*Complex[0,c_]],
    Module[{c=a/I,d=b/I,e},
    If[LtQ[d,0],
      RectifyTangent[u,-a,-b,-r,x],
    e=SmartDenominator[Together[c+d*x]];
    c=c*e;
    d=d*e;
    If[EvenQ[Denominator[NumericFactor[Together[u]]]],
      I*r*Log[RemoveContent[Simplify[(c+e)^2+d^2]+Simplify[(c+e)^2-d^2]*Cos[2*u]+Simplify[2*(c+e)*d]*Sin[2*u],x]]/4 -
      I*r*Log[RemoveContent[Simplify[(c-e)^2+d^2]+Simplify[(c-e)^2-d^2]*Cos[2*u]+Simplify[2*(c-e)*d]*Sin[2*u],x]]/4,
    I*r*Log[RemoveContent[Simplify[(c+e)^2]+Simplify[2*(c+e)*d]*Cos[u]*Sin[u]-Simplify[(c+e)^2-d^2]*Sin[u]^2,x]]/4 -
    I*r*Log[RemoveContent[Simplify[(c-e)^2]+Simplify[2*(c-e)*d]*Cos[u]*Sin[u]-Simplify[(c-e)^2-d^2]*Sin[u]^2,x]]/4]]],
  If[LtQ[b,0],
    RectifyTangent[u,-a,-b,-r,x],
  If[EvenQ[Denominator[NumericFactor[Together[u]]]],
    r*SimplifyAntiderivative[u,x] +
    r*ArcTan[Simplify[(2*a*b*Cos[2*u]-(1+a^2-b^2)*Sin[2*u])/(a^2+(1+b)^2+(1+a^2-b^2)*Cos[2*u]+2*a*b*Sin[2*u])]],
  r*SimplifyAntiderivative[u,x] -
  r*ArcTan[ActivateTrig[Simplify[(a*b-2*a*b*cos[u]^2+(1+a^2-b^2)*cos[u]*sin[u])/(b*(1+b)+(1+a^2-b^2)*cos[u]^2+2*a*b*cos[u]*sin[u])]]]]]]


(* ::Input:: *)
(* *)


(* ::Item::Closed:: *)
(*Basis: D[ArcTanh[a + b*Tanh[f[x]]], x] == D[f[x] - ArcTanh[(2*a*b*Cosh[2*f[x]] - (1 - a^2 - b^2)*Sinh[2*f[x]])/(a^2 - (1 + b)^2 - (1 - a^2 - b^2)*Cosh[2*f[x]] + 2*a*b*Sinh[2*f[x]])], x]*)


(* ::Item:: *)
(*Basis: D[ArcTanh[a + b*Tanh[f[x]]], x] == D[f[x] - ArcTanh[(a*b - 2*a*b*Cosh[f[x]]^2 + (1 - a^2 - b^2)*Cosh[f[x]]*Sinh[f[x]])/(b*(1 + b) + (1 - a^2 - b^2)*Cosh[f[x]]^2 - 2*a*b*Cosh[f[x]]*Sinh[f[x]])], x]*)


(* SimplifyAntiderivative[ArcTanh[c_.*(a_+b_.*Tanh[u_])],x_Symbol] :=
  If[EvenQ[Denominator[NumericFactor[Together[u]]]],
    SimplifyAntiderivative[u,x] -
    ArcTanh[NormalizeTogether[(2*a*b*c^2*Cosh[2*u]-(1-a^2*c^2-b^2*c^2)*Sinh[2*u])/(a^2*c^2-(1+b*c)^2-(1-a^2*c^2-b^2*c^2)*Cosh[2*u]+2*a*b*c^2*Sinh[2*u])]],
  SimplifyAntiderivative[u,x] -
  ArcTanh[NormalizeTogether[(a*b*c^2-2*a*b*c^2*Cosh[u]^2+(1-a^2*c^2-b^2*c^2)*Cosh[u]*Sinh[u])/(b*c*(1+b*c)+(1-a^2*c^2-b^2*c^2)*Cosh[u]^2-2*a*b*c^2*Cosh[u]*Sinh[u])]]] /;
FreeQ[{a,b,c},x] *)


(* ::Subsection::Closed:: *)
(*RectifyCotangent*)


(* ::Item::Closed:: *)
(*Basis: ArcCot[I a Cot[f[x]]]==-I ArcCoth[a Cot[f[x]]]*)


(* ::Item:: *)
(*Basis: D[ArcCoth[(c/e)*Cot[f[x]]], x] == D[Log[c*Cos[f[x]] + e*Sin[f[x]]]/2 - Log[c*Cos[f[x]] - e*Sin[f[x]]]/2, x]*)


(* ::Item:: *)
(*Note: Note unlike the above, the log terms in the following two identities are real-valued when c, e and f[x] are real.*)


(* ::Item:: *)
(*Basis: D[ArcTanh[(c/e)*Cot[f[x]]], x] == D[Log[c^2 + e^2 + (c^2 - e^2)*Cos[2*f[x]] + 2*c*e*Sin[2*f[x]]]/4 - Log[c^2 + e^2 + (c^2 - e^2)*Cos[2*f[x]] - 2*c*e*Sin[2*f[x]]]/4, x]*)


(* ::Item:: *)
(*Basis: D[ArcTanh[(c/e)*Cot[f[x]]], x] == D[Log[e^2 + (c^2 - e^2)*Cos[f[x]]^2 + 2*c*e*Cos[f[x]]*Sin[f[x]]]/4 - Log[e^2 + (c^2 - e^2)*Cos[f[x]]^2 - 2*c*e*Cos[f[x]]*Sin[f[x]]]/4, x]*)


(* ::Item:: *)
(*Basis: D[ArcCot[a*Cot[f[x]]], x] == D[f[x] + ArcTan[Sin[2*f[x]]/((1 + a)/(1 - a) - Cos[2*f[x]])], x]*)


(* ::Item:: *)
(*Basis: D[ArcCot[a*Cot[f[x]]], x] == D[f[x] - ArcTan[(Cos[f[x]]*Sin[f[x]])/(1/(a - 1) + Cos[f[x]]^2)], x] == D[f[x] + ArcTan[(Cos[f[x]]*Sin[f[x]])/(a/(1 - a) + Sin[f[x]]^2)], x]*)


RectifyCotangent::usage = "RectifyCotangent[u,a,b,x] returns an expression whose derivative equals the derivative of b*ArcCot[a*Cot[u]] wrt x.  RectifyCotangent[u,a,b,r,x] returns an expression whose derivative equals the derivative of r*ArcTan[a+b*Cot[u]] wrt x.";
RectifyCotangent[u_,a_,b_,x_Symbol] :=
  If[MatchQ[Together[a],d_.*Complex[0,c_]],
    Module[{c=a/I,e},
    If[LtQ[c,0],
      RectifyCotangent[u,-a,-b,x],
    If[EqQ[c,1],
      If[EvenQ[Denominator[NumericFactor[Together[u]]]],
        -I*b*ArcTanh[Sin[2*u]]/2,
      -I*b*ArcTanh[2*Cos[u]*Sin[u]]/2],
    e=SmartDenominator[c];
    c=c*e;
(*  If[EvenQ[Denominator[NumericFactor[Together[u]]]],
      -I*b*Log[RemoveContent[c^2+e^2+(c^2-e^2)*Cos[2*u]+2*c*e*Sin[2*u],x]]/4 +
       I*b*Log[RemoveContent[c^2+e^2+(c^2-e^2)*Cos[2*u]-2*c*e*Sin[2*u],x]]/4,
    -I*b*Log[RemoveContent[e^2+(c^2-e^2)*Cos[u]^2+2*c*e*Cos[u]*Sin[u],x]]/4 +
     I*b*Log[RemoveContent[e^2+(c^2-e^2)*Cos[u]^2-2*c*e*Cos[u]*Sin[u],x]]/4]]]], *)
    -I*b*Log[RemoveContent[c*Cos[u]+e*Sin[u],x]]/2 +
    I*b*Log[RemoveContent[c*Cos[u]-e*Sin[u],x]]/2]]],
  If[LtQ[a,0],
    RectifyCotangent[u,-a,-b,x],
  If[EqQ[a,1],
    b*SimplifyAntiderivative[u,x],
  Module[{c,numr,denr},
  If[EvenQ[Denominator[NumericFactor[Together[u]]]],
    c=Simplify[(1+a)/(1-a)];
    numr=SmartNumerator[c];
    denr=SmartDenominator[c];
    b*SimplifyAntiderivative[u,x] + b*ArcTan[NormalizeLeadTermSigns[denr*Sin[2*u]/(numr-denr*Cos[2*u])]],
  If[GtQ[a,1],
    c=Simplify[1/(a-1)];
    numr=SmartNumerator[c];
    denr=SmartDenominator[c];
    b*SimplifyAntiderivative[u,x] - b*ArcTan[NormalizeLeadTermSigns[denr*Cos[u]*Sin[u]/(numr+denr*Cos[u]^2)]],
  c=Simplify[a/(1-a)];
  numr=SmartNumerator[c];
  denr=SmartDenominator[c];
  b*SimplifyAntiderivative[u,x] + b*ArcTan[NormalizeLeadTermSigns[denr*Cos[u]*Sin[u]/(numr+denr*Sin[u]^2)]]]]]]]]


(* ::Input:: *)
(* *)


(* ::Item::Closed:: *)
(*Basis: ArcTan[I a+I b Cot[f[x]]]==I ArcTanh[a+b Cot[f[x]]]*)


(* ::Item:: *)
(*Basis: D[ArcTanh[c/e + (d/e)*Cot[f[x]]], x] == D[Log[(c + e)^2 + d^2 - ((c + e)^2 - d^2)*Cos[2*f[x]] + 2*(c + e)*d*Sin[2*f[x]]]/4 - Log[(c - e)^2 + d^2 - ((c - e)^2 - d^2)*Cos[2*f[x]] + 2*(c - e)*d*Sin[2*f[x]]]/4, x]*)


(* ::Item:: *)
(*Basis: D[ArcTanh[c/e + (d/e)*Cot[f[x]]], x] == D[Log[(c + e)^2 - ((c + e)^2 - d^2)*Cos[f[x]]^2 + 2*(c + e)*d*Cos[f[x]]*Sin[f[x]]]/4 - Log[(c - e)^2 - ((c - e)^2 - d^2)*Cos[f[x]]^2 + 2*(c - e)*d*Cos[f[x]]*Sin[f[x]]]/4, x]*)


(* ::Item:: *)
(*Basis: D[ArcTan[a + b*Cot[f[x]]], x] == D[-f[x] - ArcTan[(2*a*b*Cos[2*f[x]] + (1 + a^2 - b^2)*Sin[2*f[x]])/(a^2 + (1 + b)^2 - (1 + a^2 - b^2)*Cos[2*f[x]] + 2*a*b*Sin[2*f[x]])], x]*)


(* ::Item:: *)
(*Basis: D[ArcTan[a + b*Cot[f[x]]], x] == D[-f[x] - ArcTan[(a*b - 2*a*b*Sin[f[x]]^2 + (1 + a^2 - b^2)*Cos[f[x]]*Sin[f[x]])/(b*(1 + b) + (1 + a^2 - b^2)*Sin[f[x]]^2 + 2*a*b*Cos[f[x]]*Sin[f[x]])], x]*)


RectifyCotangent[u_,a_,b_,r_,x_Symbol] :=
  If[MatchQ[Together[a],d_.*Complex[0,c_]] && MatchQ[Together[b],d_.*Complex[0,c_]],
    Module[{c=a/I,d=b/I,e},
    If[LtQ[d,0],
      RectifyTangent[u,-a,-b,-r,x],
    e=SmartDenominator[Together[c+d*x]];
    c=c*e;
    d=d*e;
    If[EvenQ[Denominator[NumericFactor[Together[u]]]],
      I*r*Log[RemoveContent[Simplify[(c+e)^2+d^2]-Simplify[(c+e)^2-d^2]*Cos[2*u]+Simplify[2*(c+e)*d]*Sin[2*u],x]]/4 -
      I*r*Log[RemoveContent[Simplify[(c-e)^2+d^2]-Simplify[(c-e)^2-d^2]*Cos[2*u]+Simplify[2*(c-e)*d]*Sin[2*u],x]]/4,
    I*r*Log[RemoveContent[Simplify[(c+e)^2]-Simplify[(c+e)^2-d^2]*Cos[u]^2+Simplify[2*(c+e)*d]*Cos[u]*Sin[u],x]]/4 -
    I*r*Log[RemoveContent[Simplify[(c-e)^2]-Simplify[(c-e)^2-d^2]*Cos[u]^2+Simplify[2*(c-e)*d]*Cos[u]*Sin[u],x]]/4]]],
  If[LtQ[b,0],
    RectifyCotangent[u,-a,-b,-r,x],
  If[EvenQ[Denominator[NumericFactor[Together[u]]]],
    -r*SimplifyAntiderivative[u,x] -
    r*ArcTan[Simplify[(2*a*b*Cos[2*u]+(1+a^2-b^2)*Sin[2*u])/(a^2+(1+b)^2-(1+a^2-b^2)*Cos[2*u]+2*a*b*Sin[2*u])]],
  -r*SimplifyAntiderivative[u,x] -
  r*ArcTan[ActivateTrig[Simplify[(a*b-2*a*b*sin[u]^2+(1+a^2-b^2)*cos[u]*sin[u])/(b*(1+b)+(1+a^2-b^2)*sin[u]^2+2*a*b*cos[u]*sin[u])]]]]]]


(* ::Input:: *)
(* *)


(* ::Item::Closed:: *)
(*Basis: D[ArcTanh[a + b*Coth[f[x]]], x] == D[f[x] - ArcTanh[(2*a*b*Cosh[2*f[x]] - (1 - a^2 - b^2)*Sinh[2*f[x]])/(-a^2 + (1 + b)^2 - (1 - a^2 - b^2)*Cosh[2*f[x]] + 2*a*b*Sinh[2*f[x]])], x]*)


(* ::Item:: *)
(*Basis: D[ArcTanh[a + b*Coth[f[x]]], x] == D[f[x] - ArcTanh[(a*b + 2*a*b*Sinh[f[x]]^2 - (1 - a^2 - b^2)*Cosh[f[x]]*Sinh[f[x]])/(b*(1 + b) - (1 - a^2 - b^2)*Sinh[f[x]]^2 + 2*a*b*Cosh[f[x]]*Sinh[f[x]])], x]*)


(* SimplifyAntiderivative[ArcTanh[c_.*(a_+b_.*Coth[u_])],x_Symbol] :=
  If[EvenQ[Denominator[NumericFactor[Together[u]]]],
    SimplifyAntiderivative[u,x] -
    ArcTanh[NormalizeTogether[(2*a*b*c^2*Cosh[2*u]-(1-a^2*c^2-b^2*c^2)*Sinh[2*u])/(-a^2*c^2+(1+b*c)^2-(1-a^2*c^2-b^2*c^2)*Cosh[2*u]+2*a*b*c^2*Sinh[2*u])]],
  SimplifyAntiderivative[u,x] -
  ArcTanh[NormalizeTogether[(a*b*c^2+2*a*b*c^2*Sinh[u]^2-(1-a^2*c^2-b^2*c^2)*Cosh[u]*Sinh[u])/(b*c*(1+b*c)-(1-a^2*c^2-b^2*c^2)*Sinh[u]^2+2*a*b*c^2*Cosh[u]*Sinh[u])]]] /;
FreeQ[{a,b,c},x] *)


SmartNumerator[u_^n_] := SmartDenominator[u^(-n)] /; RationalQ[n] && n<0

SmartNumerator[u_*v_] := SmartNumerator[u]*SmartNumerator[v]

SmartNumerator[u_] := Numerator[u]


SmartDenominator[u_^n_] := SmartNumerator[u^(-n)] /; RationalQ[n] && n<0

SmartDenominator[u_*v_] := SmartDenominator[u]*SmartDenominator[v]

SmartDenominator[u_] := Denominator[u]


(* ::Subsection::Closed:: *)
(*SubstFor*)


SubstFor::usage = "u is a function of v.  SubstFor[v,u,x] returns u with v replaced by x.  SubstFor[w,v,u,x] returns w*SubstFor[v,u,x].";
SubstFor[w_,v_,u_,x_] :=
  SimplifyIntegrand[w*SubstFor[v,u,x],x]


SubstFor[v_,u_,x_] :=
  If[AtomQ[v],
    If[Head[v]===Symbol,
      SubstAux[u,v,x,False],
    u],
  If[Not[InertTrigFreeQ[u]],
    SubstFor[v,ActivateTrig[u],x],
  If[NeQ[FreeFactors[v,x],1],
    SubstFor[NonfreeFactors[v,x],u,x/FreeFactors[v,x]],
  Switch[Head[v],
    Sin, SubstForTrig[u,x,Sqrt[1-x^2],v[[1]],x],
    Cos, SubstForTrig[u,Sqrt[1-x^2],x,v[[1]],x],
    Tan, SubstForTrig[u,x/Sqrt[1+x^2],1/Sqrt[1+x^2],v[[1]],x],
    Cot, SubstForTrig[u,1/Sqrt[1+x^2],x/Sqrt[1+x^2],v[[1]],x],
    Sec, SubstForTrig[u,1/Sqrt[1-x^2],1/x,v[[1]],x],
    Csc, SubstForTrig[u,1/x,1/Sqrt[1-x^2],v[[1]],x],
    Sinh, SubstForHyperbolic[u,x,Sqrt[1+x^2],v[[1]],x],
    Cosh, SubstForHyperbolic[u,Sqrt[-1+x^2],x,v[[1]],x],
    Tanh, SubstForHyperbolic[u,x/Sqrt[1-x^2],1/Sqrt[1-x^2],v[[1]],x],
    Coth, SubstForHyperbolic[u,1/Sqrt[-1+x^2],x/Sqrt[-1+x^2],v[[1]],x],
    Sech, SubstForHyperbolic[u,1/Sqrt[-1+x^2],1/x,v[[1]],x],
    Csch, SubstForHyperbolic[u,1/x,1/Sqrt[1+x^2],v[[1]],x],
    _, SubstForAux[u,v,x]]]]]


SubstForAux::usage = "u is a function of v.  SubstForAux[u,v,x] returns u with v replaced by x.";
SubstForAux[u_,v_,x_] :=
  If[u===v,
    x,
  If[AtomQ[u],
    If[PowerQ[v] && FreeQ[v[[2]],x] && EqQ[u,v[[1]]],
      x^Simplify[1/v[[2]]],
    u],
  If[PowerQ[u] && FreeQ[u[[2]],x],
    If[EqQ[u[[1]],v],
      x^u[[2]],
    If[PowerQ[v] && FreeQ[v[[2]],x] && EqQ[u[[1]],v[[1]]],
      x^Simplify[u[[2]]/v[[2]]],
    SubstForAux[u[[1]],v,x]^u[[2]]]],
  If[ProductQ[u] && NeQ[FreeFactors[u,x],1],
    FreeFactors[u,x]*SubstForAux[NonfreeFactors[u,x],v,x],
  If[ProductQ[u] && ProductQ[v],
    SubstForAux[First[u],First[v],x],
  Map[Function[SubstForAux[#,v,x]],u]]]]]]


SubstForTrig::usage = " u(v) is an expression of the form f(Sin[v],Cos[v],Tan[v],Cot[v],Sec[v],Csc[v]).  SubstForTrig[u,sin,cos,v,x] returns the expression f(sin,cos,sin/cos,cos/sin,1/cos,1/sin).";
SubstForTrig[u_,sin_,cos_,v_,x_] :=
  If[AtomQ[u],
    u,
  If[TrigQ[u] && IntegerQuotientQ[u[[1]],v],
    If[u[[1]]===v || EqQ[u[[1]],v],
      Switch[Head[u], Sin, sin,  Cos, cos,  Tan, sin/cos,  Cot, cos/sin,  Sec, 1/cos,  Csc, 1/sin],
    Map[Function[SubstForTrig[#,sin,cos,v,x]],
			ReplaceAll[TrigExpand[Head[u][Simplify[u[[1]]/v]*x]],x->v]]],
  If[ProductQ[u] && Head[u[[1]]]===Cos && Head[u[[2]]]===Sin && EqQ[u[[1,1]],v/2] && EqQ[u[[2,1]],v/2],
    sin/2*SubstForTrig[Drop[u,2],sin,cos,v,x],
  Map[Function[SubstForTrig[#,sin,cos,v,x]],u]]]]


SubstForHyperbolic::usage = "u (v) is an expression of the form f (Sinh[v],Cosh[v],Tanh[v],Coth[v],Sech[v],Csch[v]).  SubstForHyperbolic[u,sinh,cosh,v,x] returns the expression f(sinh,cosh,sinh/cosh,cosh/sinh,1/cosh,1/sinh).";
SubstForHyperbolic[u_,sinh_,cosh_,v_,x_] :=
  If[AtomQ[u],
    u,
  If[HyperbolicQ[u] && IntegerQuotientQ[u[[1]],v],
    If[u[[1]]===v || EqQ[u[[1]],v],
      Switch[Head[u], Sinh, sinh,  Cosh, cosh,  Tanh, sinh/cosh,  Coth, cosh/sinh,  Sech, 1/cosh, Csch, 1/sinh],
    Map[Function[SubstForHyperbolic[#,sinh,cosh,v,x]],
			ReplaceAll[TrigExpand[Head[u][Simplify[u[[1]]/v]*x]],x->v]]],
  If[ProductQ[u] && Head[u[[1]]]===Cosh && Head[u[[2]]]===Sinh && EqQ[u[[1,1]],v/2] && EqQ[u[[2,1]],v/2],
    sinh/2*SubstForHyperbolic[Drop[u,2],sinh,cosh,v,x],
  Map[Function[SubstForHyperbolic[#,sinh,cosh,v,x]],u]]]]


(* ::Subsection::Closed:: *)
(*SubstForFractionalPowerOfLinear*)


SubstForFractionalPowerOfLinear::usage = "If u has a subexpression of the form (a+b*x)^(m/n) where m and n>1 are integers, SubstForFractionalPowerOfLinear[u,x] returns the list {v,n,a+b*x,1/b} where v is u with subexpressions of the form (a+b*x)^(m/n) replaced by x^m and x replaced by -a/b+x^n/b, and all times x^(n-1); else it returns False.";
SubstForFractionalPowerOfLinear[u_,x_Symbol] :=
  With[{lst=FractionalPowerOfLinear[u,1,False,x]},
  If[AtomQ[lst] || FalseQ[lst[[2]]],
    False,
  With[{n=lst[[1]], a=Coefficient[lst[[2]],x,0], b=Coefficient[lst[[2]],x,1]},
  With[{tmp=Simplify[x^(n-1)*SubstForFractionalPower[u,lst[[2]],n,-a/b+x^n/b,x]]},
  {NonfreeFactors[tmp,x],n,lst[[2]],FreeFactors[tmp,x]/b}]]]]


FractionalPowerOfLinear::usage = "If u has a subexpression of the form (a+b*x)^(m/n), FractionalPowerOfLinear[u,1,False,x] returns {n,a+b*x}; else it returns False.";
FractionalPowerOfLinear[u_,n_,v_,x_] :=
  If[AtomQ[u] || FreeQ[u,x],
    {n,v},
  If[CalculusQ[u],
    False,
  If[FractionalPowerQ[u] && LinearQ[u[[1]],x] && (FalseQ[v] || EqQ[u[[1]],v]),
    {LCM[Denominator[u[[2]]],n],u[[1]]},
  Catch[Module[{lst={n,v}},
    Scan[Function[If[AtomQ[lst=FractionalPowerOfLinear[#,lst[[1]],lst[[2]],x]],Throw[False]]],u];
    lst]]]]]


(* ::Subsection::Closed:: *)
(*InverseFunctionOfLinear*)


InverseFunctionOfLinear::usage = "If u has a subexpression of the form g[a+b*x] where g is an inverse function, InverseFunctionOfLinear[u,x] returns g[a+b*x]; else it returns False.";
InverseFunctionOfLinear[u_,x_Symbol] :=
  If[AtomQ[u] || CalculusQ[u] || FreeQ[u,x],
    False,
  If[InverseFunctionQ[u] && LinearQ[u[[1]],x],
    u,
  Module[{tmp},
  Catch[
    Scan[Function[If[Not[AtomQ[tmp=InverseFunctionOfLinear[#,x]]],Throw[tmp]]],u];
    False]]]]


(* ::Subsection::Closed:: *)
(*TryPureTanSubst*)


TryPureTanSubst[u_,x_Symbol] :=
(*  Not[MatchQ[u,Log[v_]]] &&
  Not[MatchQ[u,f_[v_]^2 /; LinearQ[v,x]]] &&
  Not[MatchQ[u,ArcTan[a_.*Tan[v_]] /; FreeQ[a,x]]] &&
  Not[MatchQ[u,ArcTan[a_.*Cot[v_]] /; FreeQ[a,x]]] &&
  Not[MatchQ[u,ArcCot[a_.*Tan[v_]] /; FreeQ[a,x]]] &&
  Not[MatchQ[u,ArcCot[a_.*Cot[v_]] /; FreeQ[a,x]]] &&
  u===ExpandIntegrand[u,x] *)
  Not[MatchQ[u,F_[c_.*(a_.+b_.*G_[v_])] /; FreeQ[{a,b,c},x] && MemberQ[{ArcTan,ArcCot,ArcTanh,ArcCoth},F] && MemberQ[{Tan,Cot,Tanh,Coth},G] && LinearQ[v,x]]]


(* ::Subsection::Closed:: *)
(*TryPureTanhSubst*)


MemberQ[{Sinh,Cosh,Sech,Csch},f]


TryTanhSubst[u_,x_Symbol] :=
  FalseQ[FunctionOfLinear[u,x]] &&
  Not[MatchQ[u,r_.*(s_+t_)^n_. /; IntegerQ[n] && n>0]] &&
(*Not[MatchQ[u,Log[f_[x]^2] /; MemberQ[{Sinh,Cosh,Sech,Csch},f]]]  && *)
  Not[MatchQ[u,Log[v_]]]  &&
  Not[MatchQ[u,1/(a_+b_.*f_[x]^n_) /; MemberQ[{Sinh,Cosh,Sech,Csch},f] && IntegerQ[n] && n>2]] &&
  Not[MatchQ[u,f_[m_.*x]*g_[n_.*x] /; IntegersQ[m,n] && MemberQ[{Sinh,Cosh,Sech,Csch},f] && MemberQ[{Sinh,Cosh,Sech,Csch},g]]] &&
  Not[MatchQ[u,r_.*(a_.*s_^m_)^p_ /; FreeQ[{a,m,p},x] && Not[m===2 && (s===Sech[x] || s===Csch[x])]]] &&
  u===ExpandIntegrand[u,x]


TryPureTanhSubst[u_,x_Symbol] :=
  Not[MatchQ[u,Log[v_]]]  &&
  Not[MatchQ[u,ArcTanh[a_.*Tanh[v_]] /; FreeQ[a,x]]] &&
  Not[MatchQ[u,ArcTanh[a_.*Coth[v_]] /; FreeQ[a,x]]] &&
  Not[MatchQ[u,ArcCoth[a_.*Tanh[v_]] /; FreeQ[a,x]]] &&
  Not[MatchQ[u,ArcCoth[a_.*Coth[v_]] /; FreeQ[a,x]]] &&
  u===ExpandIntegrand[u,x]


(* ::Subsection::Closed:: *)
(*SubstPower[F[x],x,n]*)


SubstPower::usage = "SubstPower[F[x],x,n] returns F[x] with x^m replaced by x^(m*n) and x replaced by x^n.";
SubstPower[Fx_,x_Symbol,n_Integer] :=
  If[AtomQ[Fx],
    If[Fx===x,
      x^n,
    Fx],
  If[PowerQ[Fx] && Fx[[1]]===x && FreeQ[Fx[[2]],x],
    x^(n*Fx[[2]]),
  Map[Function[SubstPower[#,x,n]],Fx]]]


(* ::Section::Closed:: *)
(*Inert trig functions*)


(* ::Subsection::Closed:: *)
(*InertTrigQ*)


InertTrigQ[f_] := MemberQ[{sin,cos,tan,cot,sec,csc},f]

InertTrigQ[f_,g_] :=
  If[f===g,
    InertTrigQ[f],
  InertReciprocalQ[f,g] || InertReciprocalQ[g,f]]

InertTrigQ[f_,g_,h_] := InertTrigQ[f,g] && InertTrigQ[g,h]


InertReciprocalQ[f_,g_] := f===sin && g===csc || f===cos && g===sec || f===tan && g===cot


InertTrigFreeQ[u_] := FreeQ[u,sin] && FreeQ[u,cos] && FreeQ[u,tan] && FreeQ[u,cot] && FreeQ[u,sec] && FreeQ[u,csc]


(* ::Subsection::Closed:: *)
(*ActivateTrig[u]*)


ActivateTrig[u_] :=
  ReplaceAll[u,{sin->Sin,cos->Cos,tan->Tan,cot->Cot,sec->Sec,csc->Csc}]


(* ::Subsection::Closed:: *)
(*DeactivateTrig[u,x]*)


DeactivateTrig::usage = "u is a function of trig functions of a linear function of x.  DeactivateTrig[u,x] returns u with the trig functions replaced with inert trig functions.";
DeactivateTrig[(c_.+d_.*x_)^m_.*(a_.+b_.*trig_[e_.+f_.*x_])^n_.,x_] :=
  (c+d*x)^m*(a+b*DeactivateTrig[trig[e+f*x],x])^n /;
FreeQ[{a,b,c,d,e,f,m,n},x] && (TrigQ[trig] || HyperbolicQ[trig])


DeactivateTrig[u_,x_] :=
  UnifyInertTrigFunction[FixInertTrigFunction[DeactivateTrigAux[u,x],x],x]


DeactivateTrigAux[u_,x_] :=
  If[AtomQ[u],
    u,
  If[TrigQ[u] && LinearQ[u[[1]],x],
    With[{v=ExpandToSum[u[[1]],x]},
    Switch[Head[u],
	  Sin, ReduceInertTrig[sin,v],
	  Cos, ReduceInertTrig[cos,v],
	  Tan, ReduceInertTrig[tan,v],
	  Cot, ReduceInertTrig[cot,v],
	  Sec, ReduceInertTrig[sec,v],
	  Csc, ReduceInertTrig[csc,v]]],
  If[HyperbolicQ[u] && LinearQ[u[[1]],x],
    With[{v=ExpandToSum[I*u[[1]],x]},
    Switch[Head[u],
	  Sinh, -I*ReduceInertTrig[sin,v],
	  Cosh, ReduceInertTrig[cos,v],
	  Tanh, -I*ReduceInertTrig[tan,v],
	  Coth, I*ReduceInertTrig[cot,v],
	  Sech, ReduceInertTrig[sec,v],
	  Csch, I*ReduceInertTrig[csc,v]]],
  Map[Function[DeactivateTrigAux[#,x]],u]]]]


(* ::Subsection::Closed:: *)
(*FixInertTrigFunction[u,x]*)


Clear[FixInertTrigFunction]

FixInertTrigFunction[a_*u_,x_] :=
  a*FixInertTrigFunction[u,x] /;
FreeQ[a,x]

FixInertTrigFunction[u_.*(a_*(b_+v_))^n_,x_] :=
  FixInertTrigFunction[u*(a*b+a*v)^n,x] /;
FreeQ[{a,b,n},x] && Not[FreeQ[v,x]]


FixInertTrigFunction[csc[v_]^m_.*(c_.*sin[w_])^n_.,x_] :=
  sin[v]^(-m)*(c*sin[w])^n /;
FreeQ[{c,n},x] && IntegerQ[m]

FixInertTrigFunction[sec[v_]^m_.*(c_.*cos[w_])^n_.,x_] :=
  cos[v]^(-m)*(c*cos[w])^n /;
FreeQ[{c,n},x] && IntegerQ[m]

FixInertTrigFunction[cot[v_]^m_.*(c_.*tan[w_])^n_.,x_] :=
  tan[v]^(-m)*(c*tan[w])^n /;
FreeQ[{c,n},x] && IntegerQ[m]

FixInertTrigFunction[tan[v_]^m_.*(c_.*cot[w_])^n_.,x_] :=
  cot[v]^(-m)*(c*cot[w])^n /;
FreeQ[{c,n},x] && IntegerQ[m]

FixInertTrigFunction[cos[v_]^m_.*(c_.*sec[w_])^n_.,x_] :=
  sec[v]^(-m)*(c*sec[w])^n /;
FreeQ[{c,n},x] && IntegerQ[m]

FixInertTrigFunction[sin[v_]^m_.*(c_.*csc[w_])^n_.,x_] :=
  csc[v]^(-m)*(c*csc[w])^n /;
FreeQ[{c,n},x] && IntegerQ[m]


FixInertTrigFunction[sec[v_]^m_.*(c_.*sin[w_])^n_.,x_] :=
  cos[v]^(-m)*(c*sin[w])^n /;
FreeQ[{c,n},x] && IntegerQ[m]

FixInertTrigFunction[csc[v_]^m_.*(c_.*cos[w_])^n_.,x_] :=
  sin[v]^(-m)*(c*cos[w])^n /;
FreeQ[{c,n},x] && IntegerQ[m]

FixInertTrigFunction[cos[v_]^m_.*(c_.*tan[w_])^n_.,x_] :=
  sec[v]^(-m)*(c*tan[w])^n /;
FreeQ[{c,n},x] && IntegerQ[m]

FixInertTrigFunction[sin[v_]^m_.*(c_.*cot[w_])^n_.,x_] :=
  csc[v]^(-m)*(c*cot[w])^n /;
FreeQ[{c,n},x] && IntegerQ[m]

FixInertTrigFunction[sin[v_]^m_.*(c_.*sec[w_])^n_.,x_] :=
  csc[v]^(-m)*(c*sec[w])^n /;
FreeQ[{c,n},x] && IntegerQ[m]

FixInertTrigFunction[cos[v_]^m_.*(c_.*csc[w_])^n_.,x_] :=
  sec[v]^(-m)*(c*csc[w])^n /;
FreeQ[{c,n},x] && IntegerQ[m]


FixInertTrigFunction[cot[v_]^m_.*(c_.*sin[w_])^n_.,x_] :=
  tan[v]^(-m)*(c*sin[w])^n /;
FreeQ[{c,n},x] && IntegerQ[m]

FixInertTrigFunction[tan[v_]^m_.*(c_.*cos[w_])^n_.,x_] :=
  cot[v]^(-m)*(c*cos[w])^n /;
FreeQ[{c,n},x] && IntegerQ[m]

FixInertTrigFunction[csc[v_]^m_.*(c_.*tan[w_])^n_.,x_] :=
  sin[v]^(-m)*(c*tan[w])^n /;
FreeQ[{c,n},x] && IntegerQ[m]

FixInertTrigFunction[sec[v_]^m_.*(c_.*cot[w_])^n_.,x_] :=
  cos[v]^(-m)*(c*cot[w])^n /;
FreeQ[{c,n},x] && IntegerQ[m]

FixInertTrigFunction[cot[v_]^m_.*(c_.*sec[w_])^n_.,x_] :=
  tan[v]^(-m)*(c*sec[w])^n /;
FreeQ[{c,n},x] && IntegerQ[m]

FixInertTrigFunction[tan[v_]^m_.*(c_.*csc[w_])^n_.,x_] :=
  cot[v]^(-m)*(c*csc[w])^n /;
FreeQ[{c,n},x] && IntegerQ[m]


FixInertTrigFunction[sec[v_]^m_.*sec[w_]^n_.,x_] :=
  cos[v]^(-m)*cos[w]^(-n) /;
IntegersQ[m,n]

FixInertTrigFunction[csc[v_]^m_.*csc[w_]^n_.,x_] :=
  sin[v]^(-m)*sin[w]^(-n) /;
IntegersQ[m,n]


FixInertTrigFunction[u_*tan[v_]^m_.*(a_+b_.*sin[w_])^n_.,x_] :=
  sin[v]^m/cos[v]^m*FixInertTrigFunction[u*(a+b*sin[w])^n,x] /;
FreeQ[{a,b,n},x] && IntegerQ[m]

FixInertTrigFunction[u_*cot[v_]^m_.*(a_+b_.*sin[w_])^n_.,x_] :=
  cos[v]^m/sin[v]^m*FixInertTrigFunction[u*(a+b*sin[w])^n,x] /;
FreeQ[{a,b,n},x] && IntegerQ[m]

FixInertTrigFunction[u_*tan[v_]^m_.*(a_+b_.*cos[w_])^n_.,x_] :=
  sin[v]^m/cos[v]^m*FixInertTrigFunction[u*(a+b*cos[w])^n,x] /;
FreeQ[{a,b,n},x] && IntegerQ[m]

FixInertTrigFunction[u_*cot[v_]^m_.*(a_+b_.*cos[w_])^n_.,x_] :=
  cos[v]^m/sin[v]^m*FixInertTrigFunction[u*(a+b*cos[w])^n,x] /;
FreeQ[{a,b,n},x] && IntegerQ[m]


FixInertTrigFunction[cot[v_]^m_.*(a_.+b_.*(c_.*sin[w_])^p_.)^n_.,x_] :=
  tan[v]^(-m)*(a+b*(c*sin[w])^p)^n /;
FreeQ[{a,b,c,n,p},x] && IntegerQ[m]

FixInertTrigFunction[tan[v_]^m_.*(a_.+b_.*(c_.*cos[w_])^p_.)^n_.,x_] :=
  cot[v]^(-m)*(a+b*(c*cos[w])^p)^n /;
FreeQ[{a,b,c,n,p},x] && IntegerQ[m]


FixInertTrigFunction[u_.*(c_.*sin[v_]^n_.)^p_.*w_,x_] :=
  (c*sin[v]^n)^p*FixInertTrigFunction[u*w,x] /;
FreeQ[{c,p},x] && PowerOfInertTrigSumQ[w,sin,x]

FixInertTrigFunction[u_.*(c_.*cos[v_]^n_.)^p_.*w_,x_] :=
  (c*cos[v]^n)^p*FixInertTrigFunction[u*w,x] /;
FreeQ[{c,p},x] && PowerOfInertTrigSumQ[w,cos,x]

FixInertTrigFunction[u_.*(c_.*tan[v_]^n_.)^p_.*w_,x_] :=
  (c*tan[v]^n)^p*FixInertTrigFunction[u*w,x] /;
FreeQ[{c,p},x] && PowerOfInertTrigSumQ[w,tan,x]

FixInertTrigFunction[u_.*(c_.*cot[v_]^n_.)^p_.*w_,x_] :=
  (c*cot[v]^n)^p*FixInertTrigFunction[u*w,x] /;
FreeQ[{c,p},x] && PowerOfInertTrigSumQ[w,cot,x]

FixInertTrigFunction[u_.*(c_.*sec[v_]^n_.)^p_.*w_,x_] :=
  (c*sec[v]^n)^p*FixInertTrigFunction[u*w,x] /;
FreeQ[{c,p},x] && PowerOfInertTrigSumQ[w,sec,x]

FixInertTrigFunction[u_.*(c_.*csc[v_]^n_.)^p_.*w_,x_] :=
  (c*csc[v]^n)^p*FixInertTrigFunction[u*w,x] /;
FreeQ[{c,p},x] && PowerOfInertTrigSumQ[w,csc,x]


FixInertTrigFunction[u_.*sec[v_]^n_.*w_,x_] :=
  cos[v]^(-n)*FixInertTrigFunction[u*w,x] /;
PowerOfInertTrigSumQ[w,cos,x] && IntegerQ[n]

FixInertTrigFunction[u_.*csc[v_]^n_.*w_,x_] :=
  sin[v]^(-n)*FixInertTrigFunction[u*w,x] /;
PowerOfInertTrigSumQ[w,sin,x] && IntegerQ[n]

FixInertTrigFunction[u_.*sec[v_]^n_.*w_,x_] :=
  cos[v]^(-n)*FixInertTrigFunction[u*w,x] /;
PowerOfInertTrigSumQ[w,sin,x] && IntegerQ[n]

FixInertTrigFunction[u_.*csc[v_]^n_.*w_,x_] :=
  sin[v]^(-n)*FixInertTrigFunction[u*w,x] /;
PowerOfInertTrigSumQ[w,cos,x] && IntegerQ[n]


FixInertTrigFunction[u_.*cot[v_]^n_.*w_,x_] :=
  tan[v]^(-n)*FixInertTrigFunction[u*w,x] /;
PowerOfInertTrigSumQ[w,tan,x] && IntegerQ[n]

FixInertTrigFunction[u_.*cos[v_]^n_.*w_,x_] :=
  sec[v]^(-n)*FixInertTrigFunction[u*w,x] /;
PowerOfInertTrigSumQ[w,tan,x] && IntegerQ[n]

FixInertTrigFunction[u_.*cos[v_]^n_*w_,x_] :=
  sec[v]^(-n)*FixInertTrigFunction[u*w,x] /;
PowerOfInertTrigSumQ[w,tan,x] && IntegerQ[n]

FixInertTrigFunction[u_.*csc[v_]^n_.*w_,x_] :=
  sin[v]^(-n)*FixInertTrigFunction[u*w,x] /;
PowerOfInertTrigSumQ[w,tan,x] && IntegerQ[n]


FixInertTrigFunction[u_.*tan[v_]^n_.*w_,x_] :=
  cot[v]^(-n)*FixInertTrigFunction[u*w,x] /;
PowerOfInertTrigSumQ[w,cot,x] && IntegerQ[n]

FixInertTrigFunction[u_.*sin[v_]^n_.*w_,x_] :=
  csc[v]^(-n)*FixInertTrigFunction[u*w,x] /;
PowerOfInertTrigSumQ[w,cot,x] && IntegerQ[n]

FixInertTrigFunction[u_.*sin[v_]^n_.*w_,x_] :=
  csc[v]^(-n)*FixInertTrigFunction[u*w,x] /;
PowerOfInertTrigSumQ[w,cot,x] && IntegerQ[n]

FixInertTrigFunction[u_.*sec[v_]^n_.*w_,x_] :=
  cos[v]^(-n)*FixInertTrigFunction[u*w,x] /;
PowerOfInertTrigSumQ[w,cot,x] && IntegerQ[n]


FixInertTrigFunction[u_.*cos[v_]^n_.*w_,x_] :=
  sec[v]^(-n)*FixInertTrigFunction[u*w,x] /;
PowerOfInertTrigSumQ[w,sec,x] && IntegerQ[n]

FixInertTrigFunction[u_.*cot[v_]^n_.*w_,x_] :=
  tan[v]^(-n)*FixInertTrigFunction[u*w,x] /;
PowerOfInertTrigSumQ[w,sec,x] && IntegerQ[n]

FixInertTrigFunction[u_.*csc[v_]^n_.*w_,x_] :=
  sin[v]^(-n)*FixInertTrigFunction[u*w,x] /;
PowerOfInertTrigSumQ[w,sec,x] && IntegerQ[n]


FixInertTrigFunction[u_.*sin[v_]^n_.*w_,x_] :=
  csc[v]^(-n)*FixInertTrigFunction[u*w,x] /;
PowerOfInertTrigSumQ[w,csc,x] && IntegerQ[n]

FixInertTrigFunction[u_.*tan[v_]^n_.*w_,x_] :=
  cot[v]^(-n)*FixInertTrigFunction[u*w,x] /;
PowerOfInertTrigSumQ[w,csc,x] && IntegerQ[n]

FixInertTrigFunction[u_.*sec[v_]^n_.*w_,x_] :=
  cos[v]^(-n)*FixInertTrigFunction[u*w,x] /;
PowerOfInertTrigSumQ[w,csc,x] && IntegerQ[n]


FixInertTrigFunction[u_.*tan[v_]^m_.*(a_.*sin[v_]+b_.*cos[v_])^n_.,x_] :=
  sin[v]^m*cos[v]^(-m)*FixInertTrigFunction[u*(a*sin[v]+b*cos[v])^n,x] /;
FreeQ[{a,b,n},x] && IntegerQ[m]

FixInertTrigFunction[u_.*cot[v_]^m_.*(a_.*sin[v_]+b_.*cos[v_])^n_.,x_] :=
  cos[v]^m*sin[v]^(-m)*FixInertTrigFunction[u*(a*sin[v]+b*cos[v])^n,x] /;
FreeQ[{a,b,n},x] && IntegerQ[m]

FixInertTrigFunction[u_.*sec[v_]^m_.*(a_.*sin[v_]+b_.*cos[v_])^n_.,x_] :=
  cos[v]^(-m)*FixInertTrigFunction[u*(a*sin[v]+b*cos[v])^n,x] /;
FreeQ[{a,b,n},x] && IntegerQ[m]

FixInertTrigFunction[u_.*csc[v_]^m_.*(a_.*sin[v_]+b_.*cos[v_])^n_.,x_] :=
  sin[v]^(-m)*FixInertTrigFunction[u*(a*sin[v]+b*cos[v])^n,x] /;
FreeQ[{a,b,n},x] && IntegerQ[m]


FixInertTrigFunction[f_[v_]^m_.*(A_.+B_.*g_[v_]+C_.*g_[v_]^2),x_] :=
  g[v]^(-m)*(A+B*g[v]+C*g[v]^2) /;
FreeQ[{A,B,C},x] && IntegerQ[m] && (InertReciprocalQ[f,g] || InertReciprocalQ[g,f])

FixInertTrigFunction[f_[v_]^m_.*(A_.+C_.*g_[v_]^2),x_] :=
  g[v]^(-m)*(A+C*g[v]^2) /;
FreeQ[{A,C},x] && IntegerQ[m] && (InertReciprocalQ[f,g] || InertReciprocalQ[g,f])


FixInertTrigFunction[f_[v_]^m_.*(A_.+B_.*g_[v_]+C_.*g_[v_]^2)*(a_.+b_.*g_[v_])^n_.,x_] :=
  g[v]^(-m)*(A+B*g[v]+C*g[v]^2)*(a+b*g[v])^n /;
FreeQ[{a,b,A,B,C,n},x] && IntegerQ[m] && (InertReciprocalQ[f,g] || InertReciprocalQ[g,f])

FixInertTrigFunction[f_[v_]^m_.*(A_.+C_.*g_[v_]^2)*(a_.+b_.*g_[v_])^n_.,x_] :=
  g[v]^(-m)*(A+C*g[v]^2)*(a+b*g[v])^n /;
FreeQ[{a,b,A,C,n},x] && IntegerQ[m] && (InertReciprocalQ[f,g] || InertReciprocalQ[g,f])


FixInertTrigFunction[u_,x_] := u


PowerOfInertTrigSumQ[u_,func_,x_] :=
  MatchQ[u, (a_.+b_.*(c_.*func[w_])^n_.)^p_. /; FreeQ[{a,b,c,n,p},x] && Not[EqQ[a,0] && (IntegerQ[p] || EqQ[n,1])]] ||
  MatchQ[u, (a_.+b_.*(d_.*func[w_])^p_.+c_.*(d_.*func[w_])^q_.)^n_. /; FreeQ[{a,b,c,d,n,p,q},x]]


(* ::Subsection::Closed:: *)
(*ReduceInertTrig[func,a,b,x]*)


ReduceInertTrig[func_,m_.*(n_.*Pi+u_.)+v_.] :=
  ReduceInertTrig[func,m*n,m*u+v] /;
RationalQ[m,n]

ReduceInertTrig[func_,m_.*Complex[0,mz_]*(n_.*Complex[0,nz_]*Pi+u_.)+v_.] :=
  ReduceInertTrig[func,-m*mz*n*nz,m*mz*I*u+v] /;
RationalQ[m,mz,n,nz]

ReduceInertTrig[func_,u_] :=
  func[u]


ReduceInertTrig::usage = "func is an inert function and m is rational  ReduceInertTrig[func,m_,u_] returns func[m*Pi+u] with m reduced 0<=m<1/2.";
ReduceInertTrig[func_,m_,u_] :=
  If[m<0,
    If[m>=-1/4,
      func[m*Pi+u],
    Switch[func,
	  sin, -ReduceInertTrig[sin,-m,-u],
	  cos, ReduceInertTrig[cos,-m,-u],
	  tan, -ReduceInertTrig[tan,-m,-u],
	  cot, -ReduceInertTrig[cot,-m,-u],
	  sec, ReduceInertTrig[sec,-m,-u],
	  csc, -ReduceInertTrig[csc,-m,-u]]],
  If[m>=2,
    ReduceInertTrig[func,Mod[m,2],u],
  If[m>=1,
    Switch[func,
	  sin, -ReduceInertTrig[sin,m-1,u],
	  cos, -ReduceInertTrig[cos,m-1,u],
	  tan, ReduceInertTrig[tan,m-1,u],
	  cot, ReduceInertTrig[cot,m-1,u],
	  sec, -ReduceInertTrig[sec,m-1,u],
	  csc, -ReduceInertTrig[csc,m-1,u]],
  If[m>=1/2,
    Switch[func,
	  sin, ReduceInertTrig[cos,m-1/2,u],
	  cos, -ReduceInertTrig[sin,m-1/2,u],
	  tan, -ReduceInertTrig[cot,m-1/2,u],
	  cot, -ReduceInertTrig[tan,m-1/2,u],
	  sec, -ReduceInertTrig[csc,m-1/2,u],
	  csc, ReduceInertTrig[sec,m-1/2,u]],
  func[m*Pi+u]]]]] /;
RationalQ[m]


(* ::Subsection::Closed:: *)
(*UnifyInertTrigFunction[u,x]*)


Clear[UnifyInertTrigFunction]


UnifyInertTrigFunction[a_*u_,x_] :=
  a*UnifyInertTrigFunction[u,x] /;
FreeQ[a,x]


(* ::Subsubsection:: *)
(*Cosine to sine*)


(* ::Subsubsection::Closed:: *)
(*1.0 (a sin)^m (b trg)^n*)


(* ::Text:: *)
(*(a Cos[e+f x])^m (b Csc[e+f x])^n == (a Sin[e+Pi/2+f x])^m (-b Sec[e+Pi/2+f x])^n*)


UnifyInertTrigFunction[(a_.*cos[e_.+f_.*x_])^m_.*(b_.*csc[e_.+f_.*x_])^n_.,x_] :=
  (a*sin[e+Pi/2+f*x])^m*(-b*sec[e+Pi/2+f*x])^n /;
FreeQ[{a,b,e,f,m,n},x]


(* ::Text:: *)
(*(a Cos[e+f x])^m (b Sec[e+f x])^n == (a Sin[e+Pi/2+f x])^m (b Csc[e+Pi/2+f x])^n*)


UnifyInertTrigFunction[(a_.*cos[e_.+f_.*x_])^m_.*(b_.*sec[e_.+f_.*x_])^n_.,x_] :=
  (a*sin[e+Pi/2+f*x])^m*(b*csc[e+Pi/2+f*x])^n /;
FreeQ[{a,b,e,f,m,n},x]


(* ::Subsubsection::Closed:: *)
(*1.1.1 (a+b sin)^n*)


(* ::Text:: *)
(*(a+b Cos[e+f x])^n == (a+b Sin[e+Pi/2+f x])^n*)


UnifyInertTrigFunction[(a_.+b_.*cos[e_.+f_.*x_])^n_.,x_] :=
  (a+b*sin[e+Pi/2+f*x])^n /;
FreeQ[{a,b,e,f,n},x]


(* ::Subsubsection::Closed:: *)
(*1.1.2 (g cos)^p (a+b sin)^m*)


(* ::Text:: *)
(*(g Sin[e+f x])^p (a+b Cos[e+f x])^m == (g Cos[e-Pi/2+f x])^p (a-b Sin[e-Pi/2+f x])^m*)


UnifyInertTrigFunction[(g_.*sin[e_.+f_.*x_])^p_.*(a_+b_.*cos[e_.+f_.*x_])^m_.,x_] :=
  (g*cos[e-Pi/2+f*x])^p*(a-b*sin[e-Pi/2+f*x])^m /;
FreeQ[{a,b,e,f,g,m,p},x]


(* ::Text:: *)
(*(g Csc[e+f x])^p (a+b Cos[e+f x])^m == (g Sec[e-Pi/2+f x])^p (a-b Sin[e-Pi/2+f x])^m*)


UnifyInertTrigFunction[(g_.*csc[e_.+f_.*x_])^p_.*(a_+b_.*cos[e_.+f_.*x_])^m_.,x_] :=
  (g*sec[e-Pi/2+f*x])^p*(a-b*sin[e-Pi/2+f*x])^m /;
FreeQ[{a,b,e,f,g,m,p},x]


(* ::Subsubsection::Closed:: *)
(*1.1.3 (g tan)^p (a+b sin)^m*)


(* ::Text:: *)
(*(g Cot[e+f x])^p (a+b Cos[e+f x])^m == (-g Tan[e-Pi/2+f x])^p (a-b Sin[e-Pi/2+f x])^m*)


(* ::Text:: *)
(*(g Cot[e+f x])^p (a+b Cos[e+f x])^m == (-g Tan[e+Pi/2+f x])^p (a+b Sin[e+Pi/2+f x])^m*)


UnifyInertTrigFunction[(g_.*cot[e_.+f_.*x_])^p_.*(a_+b_.*cos[e_.+f_.*x_])^m_.,x_] :=
  If[True,
    (-g*tan[e-Pi/2+f*x])^p*(a-b*sin[e-Pi/2+f*x])^m,
  (-g*tan[e+Pi/2+f*x])^p*(a+b*sin[e+Pi/2+f*x])^m] /;
FreeQ[{a,b,e,f,g,m,p},x]


(* ::Text:: *)
(*(g Tan[e+f x])^p (a+b Cos[e+f x])^m == (-g Cot[e+Pi/2+f x])^p (a+b Sin[e+Pi/2+f x])^m*)


UnifyInertTrigFunction[(g_.*tan[e_.+f_.*x_])^p_.*(a_+b_.*cos[e_.+f_.*x_])^m_.,x_] :=
  (-g*cot[e+Pi/2+f*x])^p*(a+b*sin[e+Pi/2+f*x])^m /;
FreeQ[{a,b,e,f,g,m,p},x]


(* ::Subsubsection::Closed:: *)
(*1.2.1 (a+b sin)^m (c+d sin)^n*)


(* ::Text:: *)
(*(a+b Cos[e+f x])^m (c+d Cos[e+f x])^n == (a+b Sin[e+Pi/2+f x])^m (c+d Sin[e+Pi/2+f x])^n*)


UnifyInertTrigFunction[(a_.+b_.*cos[e_.+f_.*x_])^m_.*(c_.+d_.*cos[e_.+f_.*x_])^n_.,x_] :=
  (a+b*sin[e+Pi/2+f*x])^m*(c+d*sin[e+Pi/2+f*x])^n /;
FreeQ[{a,b,c,d,e,f,m,n},x]


(* ::Text:: *)
(*(a+b Cos[e+f x])^m (c+d Sec[e+f x])^n == (a+b Sin[e+Pi/2+f x])^m (c+d Csc[e+Pi/2+f x])^n*)


UnifyInertTrigFunction[(a_.+b_.*cos[e_.+f_.*x_])^m_.*(c_.+d_.*sec[e_.+f_.*x_])^n_.,x_] :=
  (a+b*sin[e+Pi/2+f*x])^m*(c+d*csc[e+Pi/2+f*x])^n /;
FreeQ[{a,b,c,d,e,f,m,n},x]


(* ::Subsubsection::Closed:: *)
(*1.2.2 (g cos)^p (a+b sin)^m (c+d sin)^n*)


(* ::Text:: *)
(*(g Sin[e+f x])^p (a+b Cos[e+f x])^m (c+d Cos[e+f x])^n == (g Cos[e-Pi/2+f x])^p (a-b Sin[e-Pi/2+f x])^m (c-d Sin[e-Pi/2+f x])^n*)


(* ::Text:: *)
(*(g Sin[e+f x])^p (a+b Cos[e+f x])^m (c+d Cos[e+f x])^n == (-g Cos[e+Pi/2+f x])^p (a+b Sin[e+Pi/2+f x])^m (c+d Sin[e+Pi/2+f x])^n*)


UnifyInertTrigFunction[(g_.*sin[e_.+f_. x_])^p_.*(a_.+b_.*cos[e_.+f_.*x_])^m_.*(c_.+d_.*cos[e_.+f_.*x_])^n_.,x_] :=
  If[IntegerQ[2*p] && p<0 && IntegerQ[2*n],
    (g*cos[e-Pi/2+f*x])^p*(a-b*sin[e-Pi/2+f*x])^m*(c-d*sin[e-Pi/2+f*x])^n,
  (-g*cos[e+Pi/2+f*x])^p*(a+b*sin[e+Pi/2+f*x])^m*(c+d*sin[e+Pi/2+f*x])^n] /;
FreeQ[{a,b,c,d,e,f,g,m,n,p},x]


(* ::Text:: *)
(*(g Csc[e+f x])^p (a+b Cos[e+f x])^m (c+d Cos[e+f x])^n == (g Sec[e-Pi/2+f x])^p (a-b Sin[e-Pi/2+f x])^m (c-d Sin[e-Pi/2+f x])^n*)


UnifyInertTrigFunction[(g_.*csc[e_.+f_.*x_])^p_.*(a_.+b_.*cos[e_.+f_.*x_])^m_.*(c_.+d_.*cos[e_.+f_.*x_])^n_.,x_] :=
  (g*sec[e-Pi/2+f*x])^p*(a-b*sin[e-Pi/2+f*x])^m*(c-d*sin[e-Pi/2+f*x])^n /;
FreeQ[{a,b,c,d,e,f,g,m,n,p},x]


(* ::Subsubsection::Closed:: *)
(*1.2.3 (g sin)^p (a+b sin)^m (c+d sin)^n*)


(* ::Text:: *)
(*(g Cos[e+f x])^p (a+b Cos[e+f x])^m (c+d Cos[e+f x])^n == (g Sin[e+Pi/2+f x])^p (a+b Sin[e+Pi/2+f x])^m (c+d Sin[e+Pi/2+f x])^n*)


UnifyInertTrigFunction[(g_.*cos[e_.+f_.*x_])^p_.*(a_.+b_.*cos[e_.+f_.*x_])^m_.*(c_.+d_.*cos[e_.+f_.*x_])^n_.,x_] :=
  (g*sin[e+Pi/2+f*x])^p*(a+b*sin[e+Pi/2+f*x])^m*(c+d*sin[e+Pi/2+f*x])^n /;
FreeQ[{a,b,c,d,e,f,g,m,n,p},x]


(* ::Text:: *)
(*(g Cos[e+f x])^p (a+b Cos[e+f x])^m (c+d Sec[e+f x])^n == (g Sin[e+Pi/2+f x])^p (a+b Sin[e+Pi/2+f x])^m (c+d Csc[e+Pi/2+f x])^n*)


UnifyInertTrigFunction[(g_.*cos[e_.+f_.*x_])^p_.*(a_.+b_.*cos[e_.+f_.*x_])^m_.*(c_.+d_.*sec[e_.+f_.*x_])^n_.,x_] :=
  (g*sin[e+Pi/2+f*x])^p*(a+b*sin[e+Pi/2+f*x])^m*(c+d*csc[e+Pi/2+f*x])^n /;
FreeQ[{a,b,c,d,e,f,g,m,n,p},x]


(* ::Text:: *)
(*(g Sec[e+f x])^p (a+b Cos[e+f x])^m (c+d Cos[e+f x])^n == (g Csc[e+Pi/2+f x])^p (a+b Sin[e+Pi/2+f x])^m (c+d Sin[e+Pi/2+f x])^n*)


UnifyInertTrigFunction[(g_.*sec[e_.+f_.*x_])^p_.*(a_.+b_.*cos[e_.+f_.*x_])^m_.*(c_.+d_.*cos[e_.+f_.*x_])^n_.,x_] :=
  (g*csc[e+Pi/2+f*x])^p*(a+b*sin[e+Pi/2+f*x])^m*(c+d*sin[e+Pi/2+f*x])^n /;
FreeQ[{a,b,c,d,e,f,g,m,n,p},x]


(* ::Text:: *)
(*(g Sec[e+f x])^p (a+b Cos[e+f x])^m (c+d Sec[e+f x])^n == (g Csc[e+Pi/2+f x])^p (a+b Sin[e+Pi/2+f x])^m (c+d Csc[e+Pi/2+f x])^n*)


UnifyInertTrigFunction[(g_.*sec[e_.+f_.*x_])^p_.*(a_.+b_.*cos[e_.+f_.*x_])^m_.*(c_.+d_.*sec[e_.+f_.*x_])^n_.,x_] :=
  (g*csc[e+Pi/2+f*x])^p*(a+b*sin[e+Pi/2+f*x])^m*(c+d*csc[e+Pi/2+f*x])^n /;
FreeQ[{a,b,c,d,e,f,g,m,n,p},x]


(* ::Subsubsection::Closed:: *)
(*1.3.1 (a+b sin)^m (c+d sin)^n (A+B sin)*)


(* ::Text:: *)
(*(a+b Cos[e+f x])^m (c+d Cos[e+f x])^n (A+B Cos[e+f x]) == (a+b Sin[e+Pi/2+f x])^m (c+d Sin[e+Pi/2+f x])^n (A+B Sin[e+Pi/2+f x])*)


UnifyInertTrigFunction[(a_.+b_.*cos[e_.+f_.*x_])^m_.*(c_.+d_.*cos[e_.+f_.*x_])^n_.*(A_.+B_.*cos[e_.+f_.*x_]),x_] :=
  (a+b*sin[e+Pi/2+f*x])^m*(c+d*sin[e+Pi/2+f*x])^n*(A+B*sin[e+Pi/2+f*x]) /;
FreeQ[{a,b,c,d,e,f,A,B,m,n},x]


(* ::Subsubsection::Closed:: *)
(*1.4.1 (a+b sin)^m (A+B sin+C sin^2)*)


(* ::Text:: *)
(*(a+b Cos[e+f x])^m (A+B Cos[e+f x]+C Cos[e+f x]^2) == (a+b Sin[e+Pi/2+f x])^m (A+B Sin[e+Pi/2+f x]+C Sin[e+Pi/2+f x]^2)*)


UnifyInertTrigFunction[(a_.+b_.*cos[e_.+f_.*x_])^m_.*(A_.+B_.*cos[e_.+f_.*x_]+C_.*cos[e_.+f_.*x_]^2),x_] :=
  (a+b*sin[e+Pi/2+f*x])^m*(A+B*sin[e+Pi/2+f*x]+C*sin[e+Pi/2+f*x]^2) /;
FreeQ[{a,b,c,e,f,A,B,C,m},x]


(* ::Text:: *)
(*(a+b Cos[e+f x])^m (A+C Cos[e+f x]^2) == (a+b Sin[e+Pi/2+f x])^m (A+C Sin[e+Pi/2+f x]^2)*)


UnifyInertTrigFunction[(a_.+b_.*cos[e_.+f_.*x_])^m_.*(A_.+C_.*cos[e_.+f_.*x_]^2),x_] :=
  (a+b*sin[e+Pi/2+f*x])^m*(A+C*sin[e+Pi/2+f*x]^2) /;
FreeQ[{a,b,c,e,f,A,C,m},x]


(* ::Subsubsection::Closed:: *)
(*1.4.2 (a+b sin)^m (c+d sin)^n (A+B sin+C sin^2)*)


(* ::Text:: *)
(*(a+b Cos[e+f x])^m (c+d Cos[e+f x])^n (A+B Cos[e+f x]+C Cos[e+f x]^2) == (a+b Sin[e+Pi/2+f x])^m (c+d Sin[e+Pi/2+f x])^n (A+B Sin[e+Pi/2+f x]+C Sin[e+Pi/2+f x]^2)*)


UnifyInertTrigFunction[(a_.+b_.*cos[e_.+f_.*x_])^m_.*(c_.+d_.*cos[e_.+f_.*x_])^n_.*(A_.+B_.*cos[e_.+f_.*x_]+C_.*cos[e_.+f_.*x_]^2),x_] :=
  (a+b*sin[e+Pi/2+f*x])^m*(c+d*sin[e+Pi/2+f*x])^n*(A+B*sin[e+Pi/2+f*x]+C*sin[e+Pi/2+f*x]^2) /;
FreeQ[{a,b,c,d,e,f,A,B,C,m,n},x]


(* ::Text:: *)
(*(a+b Cos[e+f x])^m (c+d Cos[e+f x])^n (A+C Cos[e+f x]^2) == (a+b Sin[e+Pi/2+f x])^m (c+d Sin[e+Pi/2+f x])^n (A+C Sin[e+Pi/2+f x]^2)*)


UnifyInertTrigFunction[(a_.+b_.*cos[e_.+f_.*x_])^m_.*(c_.+d_.*cos[e_.+f_.*x_])^n_.*(A_.+C_.*cos[e_.+f_.*x_]^2),x_] :=
  (a+b*sin[e+Pi/2+f*x])^m*(c+d*sin[e+Pi/2+f*x])^n*(A+C*sin[e+Pi/2+f*x]^2) /;
FreeQ[{a,b,c,d,e,f,A,C,m,n},x]


(* ::Subsubsection::Closed:: *)
(*1.7 (d trig)^m (a+b (c sin)^n)^p*)


(* ::Text:: *)
(*(a+b (c Cos[e+f x])^n)^p == (a+b (c Sin[e+Pi/2+f x])^n)^p*)


UnifyInertTrigFunction[(a_.+b_.*(c_.*cos[e_.+f_.*x_])^n_)^p_,x_] :=
  (a+b*(c*sin[e+Pi/2+f*x])^n)^p /;
FreeQ[{a,b,e,f,n,p},x] && Not[EqQ[a,0] && IntegerQ[p]]


(* ::Text:: *)
(*(d Cos[e+f x])^m (a+b (c Cos[e+f x])^n)^p == (d Sin[e+Pi/2+f x])^m (a+b (c Sin[e+Pi/2+f x])^n)^p*)


UnifyInertTrigFunction[(d_.*cos[e_.+f_.*x_])^m_.*(a_.+b_.*(c_.*cos[e_.+f_.*x_])^n_)^p_.,x_] :=
  (d*sin[e+Pi/2+f*x])^m*(a+b*(c*sin[e+Pi/2+f*x])^n)^p /;
FreeQ[{a,b,c,d,e,f,m,n,p},x] && Not[EqQ[a,0] && IntegerQ[p]]


(* ::Text:: *)
(*(d Sin[e+f x])^m (a+b (c Cos[e+f x])^n)^p == (-d Cos[e+Pi/2+f x])^m (a+b (c Sin[e+Pi/2+f x])^n)^p*)


UnifyInertTrigFunction[(d_.*sin[e_.+f_.*x_])^m_.*(a_.+b_.*(c_.*cos[e_.+f_.*x_])^n_)^p_.,x_] :=
  (-d*cos[e+Pi/2+f*x])^m*(a+b*(c*sin[e+Pi/2+f*x])^n)^p /;
FreeQ[{a,b,c,d,e,f,m,n,p},x] && Not[EqQ[a,0] && IntegerQ[p]]


(* ::Text:: *)
(*(d Cot[e+f x])^m (a+b (c Cos[e+f x])^n)^p == (-d Tan[e+Pi/2+f x])^m (a+b (c Sin[e+Pi/2+f x])^n)^p*)


UnifyInertTrigFunction[(d_.*cot[e_.+f_.*x_])^m_.*(a_.+b_.*(c_.*cos[e_.+f_.*x_])^n_)^p_.,x_] :=
  (-d*tan[e+Pi/2+f*x])^m*(a+b*(c*sin[e+Pi/2+f*x])^n)^p /;
FreeQ[{a,b,c,d,e,f,m,n,p},x] && Not[EqQ[a,0] && IntegerQ[p]]


(* ::Text:: *)
(*(d Tan[e+f x])^m (a+b (c Cos[e+f x])^n)^p == (-d Cot[e+Pi/2+f x])^m (a+b (c Sin[e+Pi/2+f x])^n)^p*)


UnifyInertTrigFunction[(d_.*tan[e_.+f_.*x_])^m_.*(a_.+b_.*(c_.*cos[e_.+f_.*x_])^n_)^p_.,x_] :=
  (-d*cot[e+Pi/2+f*x])^m*(a+b*(c*sin[e+Pi/2+f*x])^n)^p /;
FreeQ[{a,b,c,d,e,f,m,n,p},x] && Not[EqQ[a,0] && IntegerQ[p]]


(* ::Text:: *)
(*(d Csc[e+f x])^m (a+b (c Cos[e+f x])^n)^p == (-d Sec[e+Pi/2+f x])^m (a+b (c Sin[e+Pi/2+f x])^n)^p*)


UnifyInertTrigFunction[(d_.*csc[e_.+f_.*x_])^m_.*(a_.+b_.*(c_.*cos[e_.+f_.*x_])^n_)^p_.,x_] :=
  (-d*sec[e+Pi/2+f*x])^m*(a+b*(c*sin[e+Pi/2+f*x])^n)^p /;
FreeQ[{a,b,c,d,e,f,m,n,p},x] && Not[EqQ[a,0] && IntegerQ[p]]


(* ::Text:: *)
(*(d Sec[e+f x])^m (a+b (c Cos[e+f x])^n)^p == (d Csc[e+Pi/2+f x])^m (a+b (c Sin[e+Pi/2+f x])^n)^p*)


UnifyInertTrigFunction[(d_.*sec[e_.+f_.*x_])^m_.*(a_.+b_.*(c_.*cos[e_.+f_.*x_])^n_)^p_.,x_] :=
  (d*csc[e+Pi/2+f*x])^m*(a+b*(c*sin[e+Pi/2+f*x])^n)^p /;
FreeQ[{a,b,c,d,e,f,m,n,p},x] && Not[EqQ[a,0] && IntegerQ[p]]


(* ::Text:: *)
(*(a+b Cos[e+f x]^n)^m (A+B Cos[e+f x]^n) == (a+b Sin[e+Pi/2+f x]^n)^m (A+B Sin[e+Pi/2+f x]^n)*)


UnifyInertTrigFunction[(a_.+b_.*cos[e_.+f_.*x_]^n_)^m_.*(A_.+B_.*cos[e_.+f_.*x_]^n_),x_] :=
  (a+b*sin[e+Pi/2+f*x]^n)^m*(A+B*sin[e+Pi/2+f*x]^n) /;
FreeQ[{a,b,e,f,A,B,m,n},x] && Not[EqQ[a,0] && IntegerQ[m]]


(* ::Subsubsection:: *)
(*Cotangent to tangent*)


(* ::Subsubsection::Closed:: *)
(*2.0 (a trg)^m (b tan)^n*)


(* ::Text:: *)
(*(a Cos[e+f x])^m (b Cot[e+f x])^n == (a Sin[e+Pi/2+f x])^m (-b Tan[e+Pi/2+f x])^n*)


UnifyInertTrigFunction[(a_.*cos[e_.+f_.*x_])^m_.*(b_.*cot[e_.+f_.*x_])^n_.,x_] :=
  (a*sin[e+Pi/2+f*x])^m*(-b*tan[e+Pi/2+f*x])^n /;
FreeQ[{a,b,e,f,m,n},x]


(* ::Text:: *)
(*(a Sin[e+f x])^m (b Cot[e+f x])^n == (a Cos[e-Pi/2+f x])^m (-b Tan[e-Pi/2+f x])^n*)


UnifyInertTrigFunction[(a_.*sin[e_.+f_.*x_])^m_.*(b_.*cot[e_.+f_.*x_])^n_.,x_] :=
  (a*cos[e-Pi/2+f*x])^m*(-b*tan[e-Pi/2+f*x])^n /;
FreeQ[{a,b,e,f,m,n},x]


(* ::Text:: *)
(*(a Csc[e+f x])^m (b Cot[e+f x])^n == (a Sec[e-Pi/2+f x])^m (-b Tan[e-Pi/2+f x])^n*)


UnifyInertTrigFunction[(a_.*csc[e_.+f_.*x_])^m_.*(b_.*cot[e_.+f_.*x_])^n_.,x_] :=
  (a*sec[e-Pi/2+f*x])^m*(-b*tan[e-Pi/2+f*x])^n /;
FreeQ[{a,b,e,f,m,n},x]


(* ::Text:: *)
(*(a Sec[e+f x])^m (b Cot[e+f x])^n == (a Csc[e+Pi/2+f x])^m (-b Tan[e+Pi/2+f x])^n*)


UnifyInertTrigFunction[(a_.*sec[e_.+f_.*x_])^m_.*(b_.*cot[e_.+f_.*x_])^n_.,x_] :=
  (a*csc[e+Pi/2+f*x])^m*(-b*tan[e+Pi/2+f*x])^n /;
FreeQ[{a,b,e,f,m,n},x]


(* ::Subsubsection::Closed:: *)
(*2.1.1 (a+b tan)^n*)


(* ::Text:: *)
(*(a+b Cot[e+f x])^n == (a-b Tan[e+Pi/2+f x])^n*)


UnifyInertTrigFunction[(a_.+b_.*cot[e_.+f_.*x_])^n_.,x_] :=
  (a-b*tan[e+Pi/2+f*x])^n /;
FreeQ[{a,b,e,f,n},x]


(* ::Subsubsection::Closed:: *)
(*2.1.2 (d sec)^m (a+b tan)^n*)


(* ::Text:: *)
(*(d Csc[e+f x])^m (a+b Cot[e+f x])^n == (d Sec[e-Pi/2+f x])^m (a-b Tan[e-Pi/2+f x])^n*)


UnifyInertTrigFunction[(d_.*csc[e_.+f_.*x_])^m_.*(a_+b_.*cot[e_.+f_.*x_])^n_.,x_] :=
  (d*sec[e-Pi/2+f*x])^m*(a-b*tan[e-Pi/2+f*x])^n /;
FreeQ[{a,b,d,e,f,m,n},x]


(* ::Text:: *)
(*(d Sin[e+f x])^m (a+b Cot[e+f x])^n == (d Cos[e-Pi/2+f x])^m (a-b Tan[e-Pi/2+f x])^n*)


UnifyInertTrigFunction[(d_.*sin[e_.+f_.*x_])^m_.*(a_+b_.*cot[e_.+f_.*x_])^n_.,x_] :=
  (d*cos[e-Pi/2+f*x])^m*(a-b*tan[e-Pi/2+f*x])^n /;
FreeQ[{a,b,d,e,f,m,n},x]


(* ::Subsubsection::Closed:: *)
(*2.1.3 (d sin)^m (a+b tan)^n*)


(* ::Text:: *)
(*(d Cos[e+f x])^m (a+b Cot[e+f x])^n == (d Sin[e+Pi/2+f x])^m (a-b Tan[e+Pi/2+f x])^n*)


UnifyInertTrigFunction[(d_.*cos[e_.+f_.*x_])^m_.*(a_+b_.*cot[e_.+f_.*x_])^n_.,x_] :=
  (d*sin[e+Pi/2+f*x])^m*(a-b*tan[e+Pi/2+f*x])^n /;
FreeQ[{a,b,d,e,f,m,n},x]


(* ::Text:: *)
(*(d Sec[e+f x])^m (a+b Cot[e+f x])^n == (d Csc[e+Pi/2+f x])^m (a-b Tan[e+Pi/2+f x])^n*)


UnifyInertTrigFunction[(d_.*sec[e_.+f_.*x_])^m_.*(a_+b_.*cot[e_.+f_.*x_])^n_.,x_] :=
  (d*csc[e+Pi/2+f*x])^m*(a-b*tan[e+Pi/2+f*x])^n /;
FreeQ[{a,b,d,e,f,m,n},x]


(* ::Subsubsection::Closed:: *)
(*2.2.1 (a+b tan)^m (c+d tan)^n*)


(* ::Text:: *)
(*(a+b Cot[e+f x])^m (c+d Cot[e+f x])^n == (a-b Tan[e+Pi/2+f x])^m (c-d Tan[e+Pi/2+f x])^n*)


UnifyInertTrigFunction[(a_.+b_.*cot[e_.+f_.*x_])^m_.*(c_.+d_.*cot[e_.+f_.*x_])^n_.,x_] :=
  (a-b*tan[e+Pi/2+f*x])^m*(c-d*tan[e+Pi/2+f*x])^n /;
FreeQ[{a,b,c,d,e,f,m,n},x]


(* ::Subsubsection::Closed:: *)
(*2.2.3 (g tan)^p (a+b tan)^m (c+d tan)^n*)


(* ::Text:: *)
(*(g Cot[e+f x])^p (a+b Cot[e+f x])^m (c+d Cot[e+f x])^n == (-g Tan[e+Pi/2+f x])^p (a-b Tan[e+Pi/2+f x])^m (c-d Tan[e+Pi/2+f x])^n*)


UnifyInertTrigFunction[(g_.*cot[e_.+f_.*x_])^p_.*(a_.+b_.*cot[e_.+f_.*x_])^m_.*(c_.+d_.*cot[e_.+f_.*x_])^n_.,x_] :=
  (-g*tan[e+Pi/2+f*x])^p*(a-b*tan[e+Pi/2+f*x])^m*(c-d*tan[e+Pi/2+f*x])^n /;
FreeQ[{a,b,c,d,e,f,g,m,n,p},x]


(* ::Text:: *)
(*(g Cot[e+f x])^p (a+b Cot[e+f x])^m (c+d Tan[e+f x])^n == (-g Tan[e+Pi/2+f x])^p (a-b Tan[e+Pi/2+f x])^m (c-d Cot[e+Pi/2+f x])^n*)


UnifyInertTrigFunction[(g_.*cot[e_.+f_.*x_])^p_.*(a_.+b_.*cot[e_.+f_.*x_])^m_.*(c_.+d_.*tan[e_.+f_.*x_])^n_.,x_] :=
  (-g*tan[e+Pi/2+f*x])^p*(a-b*tan[e+Pi/2+f*x])^m*(c-d*cot[e+Pi/2+f*x])^n /;
FreeQ[{a,b,c,d,e,f,g,m,n,p},x]


(* ::Subsubsection::Closed:: *)
(*2.3.1 (a+b tan)^m (c+d tan)^n (A+B tan)*)


(* ::Text:: *)
(*(a+b Cot[e+f x])^m (c+d Cot[e+f x])^n (A+B Cot[e+f x]) == (a-b Tan[e+Pi/2+f x])^m (c-d Tan[e+Pi/2+f x])^n (A-B Tan[e+Pi/2+f x])*)


UnifyInertTrigFunction[(a_.+b_.*cot[e_.+f_.*x_])^m_.*(c_.+d_.*cot[e_.+f_.*x_])^n_.*(A_.+B_.*cot[e_.+f_.*x_]),x_] :=
  (a-b*tan[e+Pi/2+f*x])^m*(c-d*tan[e+Pi/2+f*x])^n*(A-B*tan[e+Pi/2+f*x]) /;
FreeQ[{a,b,c,d,e,f,A,B,m,n},x]


(* ::Subsubsection::Closed:: *)
(*2.4.1 (a+b tan)^m (A+B tan+C tan^2)*)


(* ::Text:: *)
(*(a+b Cot[e+f x])^m (A+B Cot[e+f x]+C Cot[e+f x]^2) == (a-b Tan[e+Pi/2+f x])^m (A-B Tan[e+Pi/2+f x]+C Tan[e+Pi/2+f x]^2)*)


UnifyInertTrigFunction[(a_.+b_.*cot[e_.+f_.*x_])^m_.*(A_.+B_.*cot[e_.+f_.*x_]+C_.*cot[e_.+f_.*x_]^2),x_] :=
  (a-b*tan[e+Pi/2+f*x])^m*(A-B*tan[e+Pi/2+f*x]+C*tan[e+Pi/2+f*x]^2) /;
FreeQ[{a,b,e,f,A,B,C,m},x]


(* ::Text:: *)
(*(a+b Cot[e+f x])^m (A+C Cot[e+f x]^2) == (a-b Tan[e+Pi/2+f x])^m (A+C Tan[e+Pi/2+f x]^2)*)


UnifyInertTrigFunction[(a_.+b_.*cot[e_.+f_.*x_])^m_.*(A_.+C_.*cot[e_.+f_.*x_]^2),x_] :=
  (a-b*tan[e+Pi/2+f*x])^m*(A+C*tan[e+Pi/2+f*x]^2) /;
FreeQ[{a,b,e,f,A,C,m},x]


(* ::Subsubsection::Closed:: *)
(*2.4.2 (a+b tan)^m (c+d tan)^n (A+B tan+C tan^2)*)


(* ::Text:: *)
(*(a+b Cot[e+f x])^m (c+d Cot[e+f x])^n (A+B Cot[e+f x]+C Cot[e+f x]^2) == *)
(*		(a-b Tan[e+Pi/2+f x])^m (c-d Tan[e+Pi/2+f x])^n (A-B Tan[e+Pi/2+f x]+C Tan[e+Pi/2+f x]^2)*)


UnifyInertTrigFunction[(a_.+b_.*cot[e_.+f_.*x_])^m_.*(c_.+d_.*cot[e_.+f_.*x_])^n_.*(A_.+B_.*cot[e_.+f_.*x_]+C_.*cot[e_.+f_.*x_]^2),x_] :=
  (a-b*tan[e+Pi/2+f*x])^m*(c-d*tan[e+Pi/2+f*x])^n*(A-B*tan[e+Pi/2+f*x]+C*tan[e+Pi/2+f*x]^2) /;
FreeQ[{a,b,c,d,e,f,A,B,C,m,n},x]


(* ::Text:: *)
(*(a+b Cot[e+f x])^m (c+d Cot[e+f x])^n (A+C Cot[e+f x]^2) == (a-b Tan[e+Pi/2+f x])^m (c-d Tan[e+Pi/2+f x])^n (A+C Tan[e+Pi/2+f x]^2)*)


UnifyInertTrigFunction[(a_.+b_.*cot[e_.+f_.*x_])^m_.*(c_.+d_.*cot[e_.+f_.*x_])^n_.*(A_.+C_.*cot[e_.+f_.*x_]^2),x_] :=
  (a-b*tan[e+Pi/2+f*x])^m*(c-d*tan[e+Pi/2+f*x])^n*(A+C*tan[e+Pi/2+f*x]^2) /;
FreeQ[{a,b,c,d,e,f,A,C,m,n},x]


(* ::Subsubsection::Closed:: *)
(*2.7 trig^m (a+b (c tan)^n)^p*)


(* ::Text:: *)
(*(a+b (c Cot[e+f x])^n)^p == (a+b (-c Tan[e+Pi/2+f x])^n)^p*)


UnifyInertTrigFunction[(a_.+b_.*(c_.*cot[e_.+f_.*x_])^n_)^p_,x_] :=
  (a+b*(-c*tan[e+Pi/2+f*x])^n)^p /;
FreeQ[{a,b,c,e,f,n,p},x] && Not[EqQ[a,0] && IntegerQ[p]]


(* ::Text:: *)
(*(d Cos[e+f x])^m (a+b (c Cot[e+f x])^n)^p == (d Sin[e+Pi/2+f x])^m (a+b (-c Tan[e+Pi/2+f x])^n)^p*)


UnifyInertTrigFunction[(d_.*cos[e_.+f_.*x_])^m_.*(a_.+b_.*(c_.*cot[e_.+f_.*x_])^n_)^p_.,x_] :=
  (d*sin[e+Pi/2+f*x])^m*(a+b*(-c*tan[e+Pi/2+f*x])^n)^p /;
FreeQ[{a,b,c,d,e,f,m,n,p},x] && Not[EqQ[a,0] && IntegerQ[p]]


(* ::Text:: *)
(*(d Sin[e+f x])^m (a+b (c Cot[e+f x])^n)^p == (-d Cos[e+Pi/2+f x])^m (a+b (-c Tan[e+Pi/2+f x])^n)^p*)


UnifyInertTrigFunction[(d_.*sin[e_.+f_.*x_])^m_.*(a_.+b_.*(c_.*cot[e_.+f_.*x_])^n_)^p_.,x_] :=
  (-d*cos[e+Pi/2+f*x])^m*(a+b*(-c*tan[e+Pi/2+f*x])^n)^p /;
FreeQ[{a,b,c,d,e,f,m,n,p},x] && Not[EqQ[a,0] && IntegerQ[p]]


(* ::Text:: *)
(*(d Cot[e+f x])^m (a+b (c Cot[e+f x])^n)^p == (-d Tan[e+Pi/2+f x])^m (a+b (-c Tan[e+Pi/2+f x])^n)^p*)


UnifyInertTrigFunction[(d_.*cot[e_.+f_.*x_])^m_.*(a_.+b_.*(c_.*cot[e_.+f_.*x_])^n_)^p_.,x_] :=
  (-d*tan[e+Pi/2+f*x])^m*(a+b*(-c*tan[e+Pi/2+f*x])^n)^p /;
FreeQ[{a,b,c,d,e,f,m,n,p},x] && Not[EqQ[a,0] && IntegerQ[p]]


(* ::Text:: *)
(*(d Tan[e+f x])^m (a+b (c Cot[e+f x])^n)^p == (-d Cot[e+Pi/2+f x])^m (a+b (-c Tan[e+Pi/2+f x])^n)^p*)


UnifyInertTrigFunction[(d_.*tan[e_.+f_.*x_])^m_.*(a_.+b_.*(c_.*cot[e_.+f_.*x_])^n_)^p_.,x_] :=
  (-d*cot[e+Pi/2+f*x])^m*(a+b*(-c*tan[e+Pi/2+f*x])^n)^p /;
FreeQ[{a,b,c,d,e,f,m,n,p},x] && Not[EqQ[a,0] && IntegerQ[p]]


(* ::Text:: *)
(*(d Csc[e+f x])^m (a+b (c Cot[e+f x])^n)^p == (-d Sec[e+Pi/2+f x])^m (a+b (-c Tan[e+Pi/2+f x])^n)^p*)


UnifyInertTrigFunction[(d_.*csc[e_.+f_.*x_])^m_.*(a_.+b_.*(c_.*cot[e_.+f_.*x_])^n_)^p_.,x_] :=
  (-d*sec[e+Pi/2+f*x])^m*(a+b*(-c*tan[e+Pi/2+f*x])^n)^p /;
FreeQ[{a,b,c,d,e,f,m,n,p},x] && Not[EqQ[a,0] && IntegerQ[p]]


(* ::Text:: *)
(*(d Sec[e+f x])^m (a+b (c Cot[e+f x])^n)^p == (d Csc[e+Pi/2+f x])^m (a+b (-c Tan[e+Pi/2+f x])^n)^p*)


UnifyInertTrigFunction[(d_.*sec[e_.+f_.*x_])^m_.*(a_.+b_.*(c_.*cot[e_.+f_.*x_])^n_)^p_.,x_] :=
  (d*csc[e+Pi/2+f*x])^m*(a+b*(-c*tan[e+Pi/2+f*x])^n)^p /;
FreeQ[{a,b,c,d,e,f,m,n,p},x] && Not[EqQ[a,0] && IntegerQ[p]]


(* ::Subsubsection:: *)
(*Cosecant to secant*)


(* ::Subsubsection::Closed:: *)
(*3.1.1 (a+b sec)^n*)


(* ::Text:: *)
(*(a+b Sec[e+f x])^n == (a+b Csc[e+Pi/2+f x])^n*)


UnifyInertTrigFunction[(a_.+b_.*sec[e_.+f_.*x_])^n_.,x_] :=
  (a+b*csc[e+Pi/2+f*x])^n /;
FreeQ[{a,b,e,f,n},x]


(* ::Subsubsection::Closed:: *)
(*3.1.2 (d sec)^n (a+b sec)^m*)


(* ::Text:: *)
(*(g Sec[e+f x])^p (a+b Sec[e+f x])^m == (g Csc[e+Pi/2+f x])^p (a+b Csc[e+Pi/2+f x])^m*)


UnifyInertTrigFunction[(g_.*sec[e_.+f_.*x_])^p_.*(a_+b_.*sec[e_.+f_.*x_])^m_.,x_] :=
  (g*csc[e+Pi/2+f*x])^p*(a+b*csc[e+Pi/2+f*x])^m /;
FreeQ[{a,b,e,f,g,m,p},x]


(* ::Subsubsection::Closed:: *)
(*3.1.3 (d sin)^n (a+b sec)^m*)


(* ::Text:: *)
(*(g Sin[e+f x])^p (a+b Sec[e+f x])^m == (g Cos[e-Pi/2+f x])^p (a-b Csc[e-Pi/2+f x])^m*)


UnifyInertTrigFunction[(g_.*sin[e_.+f_.*x_])^p_.*(a_+b_.*sec[e_.+f_.*x_])^m_.,x_] :=
  (g*cos[e-Pi/2+f*x])^p*(a-b*csc[e-Pi/2+f*x])^m /;
FreeQ[{a,b,e,f,g,m,p},x]


(* ::Text:: *)
(*(g Csc[e+f x])^p (a+b Sec[e+f x])^m == (g Sec[e-Pi/2+f x])^p (a-b Csc[e-Pi/2+f x])^m*)


UnifyInertTrigFunction[(g_.*csc[e_.+f_.*x_])^p_.*(a_+b_.*sec[e_.+f_.*x_])^m_.,x_] :=
  (g*sec[e-Pi/2+f*x])^p*(a-b*csc[e-Pi/2+f*x])^m /;
FreeQ[{a,b,e,f,g,m,p},x]


(* ::Subsubsection::Closed:: *)
(*3.1.4 (d tan)^n (a+b sec)^m*)


(* ::Text:: *)
(*(g Tan[e+f x])^p (a+b Sec[e+f x])^m == (-g Cot[e+Pi/2+f x])^p (a+b Csc[e+Pi/2+f x])^m*)


UnifyInertTrigFunction[(g_.*tan[e_.+f_.*x_])^p_.*(a_+b_.*sec[e_.+f_.*x_])^m_.,x_] :=
  (-g*cot[e+Pi/2+f*x])^p*(a+b*csc[e+Pi/2+f*x])^m /;
FreeQ[{a,b,e,f,g,m,p},x]


(* ::Subsubsection::Closed:: *)
(*3.2.1 (a+b sec)^m (c+d sec)^n*)


(* ::Text:: *)
(*(a+b Sec[e+f x])^m (c+d Sec[e+f x])^n == (a+b Csc[e+Pi/2+f x])^m (c+d Csc[e+Pi/2+f x])^n*)


UnifyInertTrigFunction[(a_.+b_.*sec[e_.+f_.*x_])^m_.*(c_.+d_.*sec[e_.+f_.*x_])^n_.,x_] :=
  (a+b*csc[e+Pi/2+f*x])^m*(c+d*csc[e+Pi/2+f*x])^n /;
FreeQ[{a,b,c,d,e,f,m,n},x]


(* ::Subsubsection::Closed:: *)
(*3.2.2 (g sec)^p (a+b sec)^m (c+d sec)^n*)


(* ::Text:: *)
(*(g Sec[e+f x])^p (a+b Sec[e+f x])^m (c+d Sec[e+f x])^n == (g Csc[e+Pi/2+f x])^p (a+b Csc[e+Pi/2+f x])^m (c+d Csc[e+Pi/2+f x])^n*)


UnifyInertTrigFunction[(g_.*sec[e_.+f_.*x_])^p_.*(a_.+b_.*sec[e_.+f_.*x_])^m_.*(c_.+d_.*sec[e_.+f_.*x_])^n_.,x_] :=
  (g*csc[e+Pi/2+f*x])^p*(a+b*csc[e+Pi/2+f*x])^m*(c+d*csc[e+Pi/2+f*x])^n /;
FreeQ[{a,b,c,d,e,f,g,m,n,p},x]


(* ::Text:: *)
(*(g Cos[e+f x])^p (a+b Sec[e+f x])^m (c+d Sec[e+f x])^n == (g Sin[e+Pi/2+f x])^p (a+b Csc[e+Pi/2+f x])^m (c+d Csc[e+Pi/2+f x])^n*)


UnifyInertTrigFunction[(g_.*cos[e_.+f_.*x_])^p_.*(a_.+b_.*sec[e_.+f_.*x_])^m_.*(c_.+d_.*sec[e_.+f_.*x_])^n_.,x_] :=
  (g*sin[e+Pi/2+f*x])^p*(a+b*csc[e+Pi/2+f*x])^m*(c+d*csc[e+Pi/2+f*x])^n /;
FreeQ[{a,b,c,d,e,f,g,m,n,p},x]


(* ::Subsubsection::Closed:: *)
(*3.3.1 (a+b sec)^m (d sec)^n (A+B sec)*)


(* ::Text:: *)
(*(a+b Sec[e+f x])^m (d Sec[e+f x])^n (A+B Sec[e+f x]) == (a+b Csc[e+Pi/2+f x])^m (d Csc[e+Pi/2+f x])^n (A+B Csc[e+Pi/2+f x])*)


UnifyInertTrigFunction[(a_.+b_.*sec[e_.+f_.*x_])^m_.*(d_.*sec[e_.+f_.*x_])^n_.*(A_.+B_.*sec[e_.+f_.*x_]),x_] :=
  (a+b*csc[e+Pi/2+f*x])^m*(d*csc[e+Pi/2+f*x])^n*(A+B*csc[e+Pi/2+f*x]) /;
FreeQ[{a,b,d,e,f,A,B,m,n},x]


(* ::Text:: *)
(*(a+b Sec[e+f x])^m (c+d Sec[e+f x])^n (A+B Sec[e+f x])^p == (a+b Csc[e+Pi/2+f x])^m (c+d Csc[e+Pi/2+f x])^n (A+B Csc[e+Pi/2+f x])^p*)


UnifyInertTrigFunction[(a_.+b_.*sec[e_.+f_.*x_])^m_.*(c_.+d_.*sec[e_.+f_.*x_])^n_.*(A_.+B_.*sec[e_.+f_.*x_])^p_.,x_] :=
  (a+b*csc[e+Pi/2+f*x])^m*(c+d*csc[e+Pi/2+f*x])^n*(A+B*csc[e+Pi/2+f*x])^p /;
FreeQ[{a,b,c,d,e,f,A,B,m,n,p},x]


(* ::Subsubsection::Closed:: *)
(*3.4.1 (a+b sec)^m (A+B sec+C sec^2)*)


(* ::Text:: *)
(*(a+b Sec[e+f x])^m (A+B Sec[e+f x]+C Sec[e+f x]^2) == (a+b Csc[e+Pi/2+f x])^m (A+B Csc[e+Pi/2+f x]+C Csc[e+Pi/2+f x]^2)*)


UnifyInertTrigFunction[(a_.+b_.*sec[e_.+f_.*x_])^m_.*(A_.+B_.*sec[e_.+f_.*x_]+C_.*sec[e_.+f_.*x_]^2),x_] :=
  (a+b*csc[e+Pi/2+f*x])^m*(A+B*csc[e+Pi/2+f*x]+C*csc[e+Pi/2+f*x]^2) /;
FreeQ[{a,b,e,f,A,B,C,m},x]


(* ::Text:: *)
(*(a+b Sec[e+f x])^m (A+B Sec[e+f x]+C Sec[e+f x]^2) == (a+b Csc[e+Pi/2+f x])^m (A+C Csc[e+Pi/2+f x]^2)*)


UnifyInertTrigFunction[(a_.+b_.*sec[e_.+f_.*x_])^m_.*(A_.+C_.*sec[e_.+f_.*x_]^2),x_] :=
  (a+b*csc[e+Pi/2+f*x])^m*(A+C*csc[e+Pi/2+f*x]^2) /;
FreeQ[{a,b,e,f,A,C,m},x]


(* ::Subsubsection::Closed:: *)
(*3.4.2 (a+b sec)^m (d sec)^n (A+B sec+C sec^2)*)


(* ::Text:: *)
(*(a+b Sec[e+f x])^m (d Sec[e+f x])^n (A+B Sec[e+f x]+C Sec[e+f x]^2) == *)
(*		(a+b Csc[e+Pi/2+f x])^m (d Csc[e+Pi/2+f x])^n (A+B Csc[e+Pi/2+f x]+C Csc[e+Pi/2+f x]^2)*)


UnifyInertTrigFunction[(a_.+b_.*sec[e_.+f_.*x_])^m_.*(d_.*sec[e_.+f_.*x_])^n_.*(A_.+B_.*sec[e_.+f_.*x_]+C_.*sec[e_.+f_.*x_]^2),x_] :=
  (a+b*csc[e+Pi/2+f*x])^m*(d*csc[e+Pi/2+f*x])^n*(A+B*csc[e+Pi/2+f*x]+C*csc[e+Pi/2+f*x]^2) /;
FreeQ[{a,b,d,e,f,A,B,C,m,n},x]


(* ::Text:: *)
(*(a+b Sec[e+f x])^m (d Sec[e+f x])^n (A+C Sec[e+f x]^2) == (a+b Csc[e+Pi/2+f x])^m (d Csc[e+Pi/2+f x])^n (A+C Csc[e+Pi/2+f x]^2)*)


UnifyInertTrigFunction[(a_.+b_.*sec[e_.+f_.*x_])^m_.*(d_.*sec[e_.+f_.*x_])^n_.*(A_.+C_.*sec[e_.+f_.*x_]^2),x_] :=
  (a+b*csc[e+Pi/2+f*x])^m*(d*csc[e+Pi/2+f*x])^n*(A+C*csc[e+Pi/2+f*x]^2) /;
FreeQ[{a,b,d,e,f,A,C,m,n},x]


UnifyInertTrigFunction[u_,x_] := u


(* ::Subsubsection::Closed:: *)
(*3.7 (d trig)^m (a+b (c sec)^n)^p*)


(* ::Text:: *)
(*(a+b (c Csc[e+f x])^n)^p == (a+b (-c Sec[e+Pi/2+f x])^n)^p*)


UnifyInertTrigFunction[(a_.+b_.*(c_.*csc[e_.+f_.*x_])^n_)^p_,x_] :=
  (a+b*(-c*sec[e+Pi/2+f*x])^n)^p /;
FreeQ[{a,b,c,e,f,n,p},x] && Not[EqQ[a,0] && IntegerQ[p]]


(* ::Text:: *)
(*(d Cos[e+f x])^m (a+b (c Csc[e+f x])^n)^p == (d Sin[e+Pi/2+f x])^m (a+b (-c Sec[e+Pi/2+f x])^n)^p*)


UnifyInertTrigFunction[(d_.*cos[e_.+f_.*x_])^m_.*(a_.+b_.*(c_.*csc[e_.+f_.*x_])^n_)^p_.,x_] :=
  (d*sin[e+Pi/2+f*x])^m*(a+b*(-c*sec[e+Pi/2+f*x])^n)^p /;
FreeQ[{a,b,c,d,e,f,m,n,p},x] && Not[EqQ[a,0] && IntegerQ[p]]


(* ::Text:: *)
(*(d Sin[e+f x])^m (a+b (c Csc[e+f x])^n)^p == (-d Cos[e+Pi/2+f x])^m (a+b (-c Sec[e+Pi/2+f x])^n)^p*)


UnifyInertTrigFunction[(d_.*sin[e_.+f_.*x_])^m_.*(a_.+b_.*(c_.*csc[e_.+f_.*x_])^n_)^p_.,x_] :=
  (-d*cos[e+Pi/2+f*x])^m*(a+b*(-c*sec[e+Pi/2+f*x])^n)^p /;
FreeQ[{a,b,c,d,e,f,m,n,p},x] && Not[EqQ[a,0] && IntegerQ[p]]


(* ::Text:: *)
(*(d Cot[e+f x])^m (a+b (c Csc[e+f x])^n)^p == (-d Tan[e+Pi/2+f x])^m (a+b (-c Sec[e+Pi/2+f x])^n)^p*)


UnifyInertTrigFunction[(d_.*cot[e_.+f_.*x_])^m_.*(a_.+b_.*(c_.*csc[e_.+f_.*x_])^n_)^p_.,x_] :=
  (-d*tan[e+Pi/2+f*x])^m*(a+b*(-c*sec[e+Pi/2+f*x])^n)^p /;
FreeQ[{a,b,c,d,e,f,m,n,p},x] && Not[EqQ[a,0] && IntegerQ[p]]


(* ::Text:: *)
(*(d Tan[e+f x])^m (a+b (c Csc[e+f x])^n)^p == (-d Cot[e+Pi/2+f x])^m (a+b (-c Sec[e+Pi/2+f x])^n)^p*)


UnifyInertTrigFunction[(d_.*tan[e_.+f_.*x_])^m_.*(a_.+b_.*(c_.*csc[e_.+f_.*x_])^n_)^p_.,x_] :=
  (-d*cot[e+Pi/2+f*x])^m*(a+b*(-c*sec[e+Pi/2+f*x])^n)^p /;
FreeQ[{a,b,c,d,e,f,m,n,p},x] && Not[EqQ[a,0] && IntegerQ[p]]


(* ::Text:: *)
(*(d Csc[e+f x])^m (a+b (c Csc[e+f x])^n)^p == (-d Sec[e+Pi/2+f x])^m (a+b (-c Sec[e+Pi/2+f x])^n)^p*)


UnifyInertTrigFunction[(d_.*csc[e_.+f_.*x_])^m_.*(a_.+b_.*(c_.*csc[e_.+f_.*x_])^n_)^p_.,x_] :=
  (-d*sec[e+Pi/2+f*x])^m*(a+b*(-c*sec[e+Pi/2+f*x])^n)^p /;
FreeQ[{a,b,c,d,e,f,m,n,p},x] && Not[EqQ[n,2] && EqQ[p,1]] && Not[EqQ[a,0] && IntegerQ[p]]


(* ::Text:: *)
(*(d Sec[e+f x])^m (a+b (c Csc[e+f x])^n)^p == (d Csc[e+Pi/2+f x])^m (a+b (-c Sec[e+Pi/2+f x])^n)^p*)


UnifyInertTrigFunction[(d_.*sec[e_.+f_.*x_])^m_.*(a_.+b_.*(c_.*csc[e_.+f_.*x_])^n_)^p_.,x_] :=
  (d*csc[e+Pi/2+f*x])^m*(a+b*(-c*sec[e+Pi/2+f*x])^n)^p /;
FreeQ[{a,b,c,d,e,f,m,n,p},x] && Not[EqQ[a,0] && IntegerQ[p]]


(* ::Subsection::Closed:: *)
(*KnownTrigIntegrandQ[u,x]*)


KnownSineIntegrandQ[u_,x_Symbol] :=
  KnownTrigIntegrandQ[{sin,cos},u,x]


KnownTangentIntegrandQ[u_,x_Symbol] :=
  KnownTrigIntegrandQ[{tan},u,x]


KnownCotangentIntegrandQ[u_,x_Symbol] :=
  KnownTrigIntegrandQ[{cot},u,x]


KnownSecantIntegrandQ[u_,x_Symbol] :=
  KnownTrigIntegrandQ[{sec,csc},u,x]


KnownTrigIntegrandQ[list_,u_,x_Symbol] :=
  u===1 ||
  MatchQ[u,(a_.+b_.*func_[e_.+f_.*x])^m_. /; MemberQ[list,func] && FreeQ[{a,b,e,f,m},x]] ||
  MatchQ[u,(a_.+b_.*func_[e_.+f_.*x])^m_.*(A_.+B_.*func_[e_.+f_.*x]) /; MemberQ[list,func] && FreeQ[{a,b,e,f,A,B,m},x]] ||
  MatchQ[u,(A_.+C_.*func_[e_.+f_.*x]^2) /; MemberQ[list,func] && FreeQ[{e,f,A,C},x]] ||
  MatchQ[u,(A_.+B_.*func_[e_.+f_.*x]+C_.*func_[e_.+f_.*x]^2) /; MemberQ[list,func] && FreeQ[{e,f,A,B,C},x]] ||
  MatchQ[u,(a_.+b_.*func_[e_.+f_.*x])^m_.*(A_.+C_.*func_[e_.+f_.*x]^2) /; MemberQ[list,func] && FreeQ[{a,b,e,f,A,C,m},x]] ||
  MatchQ[u,(a_.+b_.*func_[e_.+f_.*x])^m_.*(A_.+B_.*func_[e_.+f_.*x]+C_.*func_[e_.+f_.*x]^2) /; MemberQ[list,func] && FreeQ[{a,b,e,f,A,B,C,m},x]]


(* ::Section::Closed:: *)
(*Inert inverse hyperbolic functions*)


(* ::Subsection::Closed:: *)
(*DeactivateInverseHyperbolic[u,x]*)


(* DeactivateInverseHyperbolic::usage = "DeactivateInverseHyperbolic[u,x] returns u with inverse trig and hyperbolic functions replaced with inert inverse hyperbolic functions."; *)
(* DeactivateInverseHyperbolic[u_,x_] :=
  If[AtomQ[u],
    u,
  If[Length[u]==1,
    With[{v=DeactivateInverseHyperbolic[u[[1]],x]},
    Switch[Head[u],
	  ArcSin, -I*arcsinh[I*v],
	  ArcCos, Pi/2+I*arcsinh[I*x],
	  ArcTan, -I*arctanh[I*v],
	  ArcCot, I*arccoth[I*v],
	  ArcSec, Pi/2-I*arccsch[I*v],
	  ArcCsc, I*arccsch[I*v],
	  ArcSinh, arcsinh[v],
	  ArcCosh, arccosh[v],
	  ArcTanh, arctanh[v],
	  ArcCoth, arccoth[v],
	  ArcSech, arcsech[v],
	  ArcCsch, arccsch[v],
      _, Head[u]v]],
  Map[Function[DeactivateInverseHyperbolic[#,x]],u]]] *)


(* ::Section::Closed:: *)
(*Derivative divides function*)


PiecewiseLinearQ::usage = "If the derivative of u wrt x is a constant wrt x, PiecewiseLinearQ[u,x] returns True; else it returns False.";
PiecewiseLinearQ[u_,v_,x_Symbol] :=
  PiecewiseLinearQ[u,x] && PiecewiseLinearQ[v,x]

PiecewiseLinearQ[u_,x_Symbol] :=
  LinearQ[u,x] (* && Not[MonomialQ[u,x]] *) ||
  MatchQ[u,Log[c_.*F_^(v_)] /; FreeQ[{F,c},x] && LinearQ[v,x]] ||
  MatchQ[u,F_[G_[v_]] /; LinearQ[v,x] && MemberQ[{
	{ArcTanh,Tanh},{ArcTanh,Coth},{ArcCoth,Coth},{ArcCoth,Tanh},
	{ArcTan,Tan},{ArcTan,Cot},{ArcCot,Cot},{ArcCot,Tan}
   },{F,G}]]


Divides::usage = "If u divided by y is free of x, Divides[y,u,x] returns the quotient; else it returns False.";
Divides[y_,u_,x_Symbol] :=
  With[{v=Simplify[u/y]},
  If[FreeQ[v,x],
    v,
  False]]


DerivativeDivides::usage = "If y not equal to x, y is easy to differentiate wrt x, and u divided by the derivative of y is free of x, DerivativeDivides[y,u,x] returns the quotient; else it returns False.";
DerivativeDivides[y_,u_,x_Symbol] :=
  If[MatchQ[y,a_.*x /; FreeQ[a,x]],
    False,
  If[If[PolynomialQ[y,x], PolynomialQ[u,x] && Exponent[u,x]==Exponent[y,x]-1, EasyDQ[y,x]],
    Module[{v=Block[{$ShowSteps=False}, ReplaceAll[D[y,x],Sinc[z_]->Sin[z]/z]]},
    If[EqQ[v,0],
      False,
    v=Simplify[u/v];
    If[FreeQ[v,x],
      v,
    False]]],
  False]]


EasyDQ::usage = "If u is easy to differentiate wrt x, EasyDQ[u,x] returns True; else it returns False.";
EasyDQ[u_.*x_^m_.,x_Symbol] :=
  EasyDQ[u,x] /;
FreeQ[m,x]

EasyDQ[u_,x_Symbol] :=
  If[AtomQ[u] || FreeQ[u,x] || Length[u]==0,
    True,
  If[CalculusQ[u],
    False,
  If[Length[u]==1,
    EasyDQ[u[[1]],x],
  If[BinomialQ[u,x] || ProductOfLinearPowersQ[u,x],
    True,
  If[RationalFunctionQ[u,x] && RationalFunctionExponents[u,x]==={1,1},
    True,
  If[ProductQ[u],
    If[FreeQ[First[u],x],
      EasyDQ[Rest[u],x],
    If[FreeQ[Rest[u],x],
      EasyDQ[First[u],x],
    False]],
  If[SumQ[u],
    EasyDQ[First[u],x] && EasyDQ[Rest[u],x],
  If[Length[u]==2,
    If[FreeQ[u[[1]],x],
      EasyDQ[u[[2]],x],
    If[FreeQ[u[[2]],x],
      EasyDQ[u[[1]],x],
    False]],
  False]]]]]]]]


ProductOfLinearPowersQ::usage = "ProductOfLinearPowersQ[u,x] returns True iff u is a product of factors of the form v^n where v is linear in x.";
ProductOfLinearPowersQ[u_,x_Symbol] :=
  FreeQ[u,x] ||
  MatchQ[u, v_^n_. /; LinearQ[v,x] && FreeQ[n,x]] ||
  ProductQ[u] && ProductOfLinearPowersQ[First[u],x] && ProductOfLinearPowersQ[Rest[u],x]


(* ::Section::Closed:: *)
(*Simplest nth root function*)


(* ::Subsection::Closed:: *)
(*Rt[u,n]*)


Rt::usage = "Rt[u,n] returns the simplest nth root of u.";
Rt[u_,n_Integer] :=
  RtAux[TogetherSimplify[u],n]


(* ::Subsection::Closed:: *)
(*NthRoot[u,n]*)


NthRoot[u_,n_] := u^(1/n)


(* ::Subsection::Closed:: *)
(*TrigSquare[u]*)


TrigSquare::usage = "If u is an expression of the form a-a*Sin[z]^2 or a-a*Cos[z]^2, TrigSquare[u] returns Cos[z]^2 or Sin[z]^2 respectively, else it returns False.";
TrigSquare[u_] :=
  If[SumQ[u],
    With[{lst=SplitSum[Function[SplitProduct[TrigSquareQ,#]],u]},
    If[Not[AtomQ[lst]] && EqQ[lst[[1,2]]+lst[[2]],0],
      If[Head[lst[[1,1]][[1]]]===Sin,
        lst[[2]]*Cos[lst[[1,1]][[1,1]]]^2,
      lst[[2]]*Sin[lst[[1,1]][[1,1]]]^2],
    False]],
  False]


(* ::Section::Closed:: *)
(*Simplest nth root helper functions*)


(* ::Subsection::Closed:: *)
(*RtAux[u,n]*)


RtAux[u_,n_] :=
  If[PowerQ[u],
    u[[1]]^(u[[2]]/n),
  If[ProductQ[u],
    Module[{lst},
    lst=SplitProduct[Function[GtQ[#,0]],u];
    If[ListQ[lst],
      RtAux[lst[[1]],n]*RtAux[lst[[2]],n],
    lst=SplitProduct[Function[LtQ[#,0]],u];
    If[ListQ[lst],
      If[EqQ[lst[[1]],-1],
        With[{v=lst[[2]]},
        If[PowerQ[v] && LtQ[v[[2]],0],
          1/RtAux[-v[[1]]^(-v[[2]]),n],
        If[ProductQ[v],
          If[ListQ[SplitProduct[SumBaseQ,v]],
            lst=SplitProduct[AllNegTermQ,v];
            If[ListQ[lst],
              RtAux[-lst[[1]],n]*RtAux[lst[[2]],n],
            lst=SplitProduct[NegSumBaseQ,v];
            If[ListQ[lst],
              RtAux[-lst[[1]],n]*RtAux[lst[[2]],n],
            lst=SplitProduct[SomeNegTermQ,v];
            If[ListQ[lst],
              RtAux[-lst[[1]],n]*RtAux[lst[[2]],n],
            lst=SplitProduct[SumBaseQ,v];
            RtAux[-lst[[1]],n]*RtAux[lst[[2]],n]]]],
          lst=SplitProduct[AtomBaseQ,v];
          If[ListQ[lst],
            RtAux[-lst[[1]],n]*RtAux[lst[[2]],n],
          RtAux[-First[v],n]*RtAux[Rest[v],n]]],
        If[OddQ[n],
          -RtAux[v,n],
        NthRoot[u,n]]]]],
      RtAux[-lst[[1]],n]*RtAux[-lst[[2]],n]],
    lst=SplitProduct[AllNegTermQ,u];
    If[ListQ[lst] && ListQ[SplitProduct[SumBaseQ,lst[[2]]]],
      RtAux[-lst[[1]],n]*RtAux[-lst[[2]],n],
    lst=SplitProduct[NegSumBaseQ,u];
    If[ListQ[lst] && ListQ[SplitProduct[NegSumBaseQ,lst[[2]]]],
      RtAux[-lst[[1]],n]*RtAux[-lst[[2]],n],
    Map[Function[RtAux[#,n]],u]]]]]],
  With[{v=TrigSquare[u]},
  If[Not[AtomQ[v]],
    RtAux[v,n],
  If[OddQ[n] && LtQ[u,0],
    -RtAux[-u,n],
  If[ComplexNumberQ[u],
    With[{a=Re[u],b=Im[u]},
    If[Not[IntegerQ[a] && IntegerQ[b]] && IntegerQ[a/(a^2+b^2)] && IntegerQ[b/(a^2+b^2)],
(* Basis: a+b*I==1/(a/(a^2+b^2)-b/(a^2+b^2)*I) *)
      1/RtAux[a/(a^2+b^2)-b/(a^2+b^2)*I,n],
    NthRoot[u,n]]],
  If[OddQ[n] && NegQ[u] && PosQ[-u],
    -RtAux[-u,n],
  NthRoot[u,n]]]]]]]]


(* ::Subsection::Closed:: *)
(*Factor base predicates*)


AtomBaseQ::usage = "If u is an atom or an atom raised to an odd degree, AtomBaseQ[u] returns True; else it returns False.";
AtomBaseQ[u_] :=
  AtomQ[u] || PowerQ[u] && OddQ[u[[2]]] && AtomBaseQ[u[[1]]]


SumBaseQ::usage = "If u is an sum or a sum raised to an odd degree, SumBaseQ[u] returns True; else it returns False.";
SumBaseQ[u_] :=
  SumQ[u] || PowerQ[u] && OddQ[u[[2]]] && SumBaseQ[u[[1]]]


NegSumBaseQ::usage = "If u is a sum or a sum raised to an odd degree whose lead term has a negative form, NegSumBaseQ[u] returns True; else it returns False.";
NegSumBaseQ[u_] :=
  SumQ[u] && NegQ[First[u]] || PowerQ[u] && OddQ[u[[2]]] && NegSumBaseQ[u[[1]]]


AllNegTermQ::usage = "If all terms of u have a negative form, AllNegTermQ[u] returns True; else it returns False.";
AllNegTermQ[u_] :=
  If[PowerQ[u] && OddQ[u[[2]]],
    AllNegTermQ[u[[1]]],
  If[SumQ[u],
    NegQ[First[u]] && AllNegTermQ[Rest[u]],
  NegQ[u]]]


SomeNegTermQ::usage = "If some term of u has a negative form, SomeNegTermQ[u] returns True; else it returns False.";
SomeNegTermQ[u_] :=
  If[PowerQ[u] && OddQ[u[[2]]],
    SomeNegTermQ[u[[1]]],
  If[SumQ[u],
    NegQ[First[u]] || SomeNegTermQ[Rest[u]],
  NegQ[u]]]


(* ::Subsection::Closed:: *)
(*TrigSquareQ[u]*)


TrigSquareQ::usage = "If u is an expression of the form Sin[z]^2 or Cos[z]^2, TrigSquareQ[u] returns True, else it returns False.";
TrigSquareQ[u_] :=
  PowerQ[u] && EqQ[u[[2]],2] && MemberQ[{Sin,Cos},Head[u[[1]]]]


(* ::Subsection::Closed:: *)
(*SplitProduct[func,u]*)


SplitProduct::usage = "If func[v] is True for a factor v of u, SplitProduct[func,u] returns {v, u/v} where v is the first such factor; else it returns False.";
SplitProduct[func_,u_] :=
  If[ProductQ[u],
    If[func[First[u]],
      {First[u], Rest[u]},
    With[{lst=SplitProduct[func,Rest[u]]},
    If[AtomQ[lst],
      False,
    {lst[[1]],First[u]*lst[[2]]}]]],
  If[func[u],
    {u, 1},
  False]]


(* ::Subsection::Closed:: *)
(*SplitSum[func,u]*)


SplitSum::usage = "If func[v] is nonatomic for a term v of u, SplitSum[func,u] returns {func[v], u-v} where v is the first such term; else it returns False.";
SplitSum[func_,u_] :=
  If[SumQ[u],
    If[Not[AtomQ[func[First[u]]]],
      {func[First[u]], Rest[u]},
    With[{lst=SplitSum[func,Rest[u]]},
    If[AtomQ[lst],
      False,
    {lst[[1]],First[u]+lst[[2]]}]]],
  If[Not[AtomQ[func[u]]],
    {func[u], 0},
  False]]


(* ::Section::Closed:: *)
(*IntSum*)


IntSum::usage = "If u is free of x or of the form c*(a+b*x)^m, IntSum[u,x] returns the antiderivative of u wrt x; else it returns d*Int[v,x] where d*v=u and d is free of x.";
IntSum[u_,x_Symbol] :=
  Simp[FreeTerms[u,x]*x,x] + IntTerm[NonfreeTerms[u,x],x]


IntTerm::usage = "If u is of the form c*(a+b*x)^m, IntTerm[u,x] returns the antiderivative of u wrt x; else it returns d*Int[v,x] where d*v=u and d is free of x.";
IntTerm[u_,x_Symbol] :=
  Map[Function[IntTerm[#,x]],u] /;
SumQ[u]

IntTerm[c_.*v_^m_.,x_Symbol] :=
  With[{u=Cancel[v]},
  If[EqQ[m,-1],
    Simp[c*Log[RemoveContent[u,x]]/Coeff[u,x,1],x],
  If[EqQ[m,1] && EqQ[c,1] && SumQ[u],
    IntSum[u,x],
  Simp[c*u^(m+1)/(Coeff[u,x,1]*(m+1)),x]]]] /;
FreeQ[{c,m},x] && LinearQ[v,x]

IntTerm[u_,x_Symbol] :=
  Star[FreeFactors[u,x], Int[NonfreeFactors[u,x],x]]


(* ::Section::Closed:: *)
(*Fix integration rules functions*)


(* ::Subsection::Closed:: *)
(*RuleName[name]*)


$RuleNameList={};


RuleName[name_] :=
 (AppendTo[$RuleNameList,name]; Null)


(* ::Subsection::Closed:: *)
(*FixIntRules[rulelist]*)


ClearAll[FixIntRules,FixIntRule,FixRhsIntRule]


FixIntRules[] :=
  (DownValues[Int]=FixIntRules[DownValues[Int]]; Null)


FixIntRules[rulelist_] := Block[{Int, Subst, Simp, Star},
  SetAttributes[{Int, Subst, Simp, Star},HoldAll];
  Map[Function[FixIntRule[#,#[[1,1,2,1]]]], rulelist]]


(* ::Subsection::Closed:: *)
(*FixIntRule[rule]*)


FixIntRule[rule_] :=
  If[AtomQ[rule[[1,1,-1]]],
    FixIntRule[rule,rule[[1,1,-1]]],
  If[Head[rule[[1,1,-1]]]===Pattern && AtomQ[rule[[1,1,-1,1]]],
    FixIntRule[rule,rule[[1,1,-1,1]]],
  Print["Invalid integration rule: ",rule[[1,1,-1]]]]]


(* ::Subsection::Closed:: *)
(*FixIntRule[rule,x]*)


FixIntRule[RuleDelayed[lhs_,F_[G_[list_,F_[u_+v_,test2_]],test1_]],x_] :=
  ReplacePart[RuleDelayed[lhs,F[G[list,F[u+v,test2]],test1]],{{2,1,2,1,1}->FixRhsIntRule[u,x],{2,1,2,1,2}->FixRhsIntRule[v,x]}] /;
F===Condition && (G===With || G===Module || G===Block)

FixIntRule[RuleDelayed[lhs_,G_[list_,F_[u_+v_,test2_]]],x_] :=
  ReplacePart[RuleDelayed[lhs,G[list,F[u+v,test2]]],{{2,2,1,1}->FixRhsIntRule[u,x],{2,2,1,2}->FixRhsIntRule[v,x]}] /;
F===Condition && (G===With || G===Module || G===Block)

FixIntRule[RuleDelayed[lhs_,F_[G_[list_,u_+v_],test_]],x_] :=
  ReplacePart[RuleDelayed[lhs,F[G[list,u+v],test]],{{2,1,2,1}->FixRhsIntRule[u,x],{2,1,2,2}->FixRhsIntRule[v,x]}] /;
F===Condition && (G===With || G===Module || G===Block)

FixIntRule[RuleDelayed[lhs_,G_[list_,u_+v_]],x_] :=
  ReplacePart[RuleDelayed[lhs,G[list,u+v]],{{2,2,1}->FixRhsIntRule[u,x],{2,2,2}->FixRhsIntRule[v,x]}] /;
(G===With || G===Module || G===Block)

FixIntRule[RuleDelayed[lhs_,F_[u_+v_,test_]],x_] :=
  ReplacePart[RuleDelayed[lhs,F[u+v,test]],{{2,1,1}->FixRhsIntRule[u,x],{2,1,2}->FixRhsIntRule[v,x]}] /;
F===Condition

FixIntRule[RuleDelayed[lhs_,u_+v_],x_] :=
  ReplacePart[RuleDelayed[lhs,u+v],{{2,1}->FixRhsIntRule[u,x],{2,2}->FixRhsIntRule[v,x]}]


FixIntRule[RuleDelayed[lhs_,F_[G_[list1_,F_[H_[list2_,u_],test2_]],test1_]],x_] :=
  ReplacePart[RuleDelayed[lhs,F[G[list1,F[H[list2,u],test2]],test1]],{2,1,2,1,2}->FixRhsIntRule[u,x]] /;
F===Condition && (G===With || G===Module || G===Block) && (H===With || H===Module || H===Block)

FixIntRule[RuleDelayed[lhs_,F_[G_[list1_,H_[list2_,u_]],test1_]],x_] :=
  ReplacePart[RuleDelayed[lhs,F[G[list1,H[list2,u]],test1]],{2,1,2,2}->FixRhsIntRule[u,x]] /;
F===Condition && (G===With || G===Module || G===Block) && (H===With || H===Module || H===Block)

FixIntRule[RuleDelayed[lhs_,F_[G_[list_,F_[H_[str1_,str2_,str3_,J_[u_]],test2_]],test1_]],x_] :=
  ReplacePart[RuleDelayed[lhs,F[G[list,F[H[str1,str2,str3,J[u]],test2]],test1]],{2,1,2,1,4,1}->FixRhsIntRule[u,x]] /;
F===Condition && (G===With || G===Module || G===Block) && H===ShowStep && J===Hold

FixIntRule[RuleDelayed[lhs_,F_[G_[list_,F_[u_,test2_]],test1_]],x_] :=
  ReplacePart[RuleDelayed[lhs,F[G[list,F[u,test2]],test1]],{2,1,2,1}->FixRhsIntRule[u,x]] /;
F===Condition && (G===With || G===Module || G===Block)

FixIntRule[RuleDelayed[lhs_,G_[list_,F_[u_,test2_]]],x_] :=
  ReplacePart[RuleDelayed[lhs,G[list,F[u,test2]]],{2,2,1}->FixRhsIntRule[u,x]] /;
F===Condition && (G===With || G===Module || G===Block)

FixIntRule[RuleDelayed[lhs_,F_[G_[list_,u_],test_]],x_] :=
  ReplacePart[RuleDelayed[lhs,F[G[list,u],test]],{2,1,2}->FixRhsIntRule[u,x]] /;
F===Condition && (G===With || G===Module || G===Block)

FixIntRule[RuleDelayed[lhs_,G_[list_,u_]],x_] :=
  ReplacePart[RuleDelayed[lhs,G[list,u]],{2,2}->FixRhsIntRule[u,x]] /;
(G===With || G===Module || G===Block)

FixIntRule[RuleDelayed[lhs_,F_[u_,test_]],x_] :=
  ReplacePart[RuleDelayed[lhs,F[u,test]],{2,1}->FixRhsIntRule[u,x]] /;
F===Condition

FixIntRule[RuleDelayed[lhs_,u_],x_] :=
  ReplacePart[RuleDelayed[lhs,u],{2}->FixRhsIntRule[u,x]]


(* ::Subsection::Closed:: *)
(*FixRhsIntRule[rhs,x]*)


SetAttributes[FixRhsIntRule,HoldAll];

FixRhsIntRule[u_+v_,x_] :=
  FixRhsIntRule[u,x]+FixRhsIntRule[v,x]

FixRhsIntRule[u_-v_,x_] :=
  FixRhsIntRule[u,x]-FixRhsIntRule[v,x]

FixRhsIntRule[-u_,x_] :=
  -FixRhsIntRule[u,x]

FixRhsIntRule[u_,x_] :=
  If[MemberQ[{Int, Unintegrable, CannotIntegrate, Subst, Simp}, Head[Unevaluated[u]]],
    u,
  Simp[u,x]]
