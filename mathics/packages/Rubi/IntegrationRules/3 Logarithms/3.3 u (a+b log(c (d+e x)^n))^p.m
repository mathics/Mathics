(* ::Package:: *)

(************************************************************************)
(* This file was generated automatically by the Mathematica front end.  *)
(* It contains Initialization cells from a Notebook file, which         *)
(* typically will have the same name as this file except ending in      *)
(* ".nb" instead of ".m".                                               *)
(*                                                                      *)
(* This file is intended to be loaded into the Mathematica kernel using *)
(* the package loading commands Get or Needs.  Doing so is equivalent   *)
(* to using the Evaluate Initialization Cells menu command in the front *)
(* end.                                                                 *)
(*                                                                      *)
(* DO NOT EDIT THIS FILE.  This entire file is regenerated              *)
(* automatically each time the parent Notebook file is saved in the     *)
(* Mathematica front end.  Any changes you make to this file will be    *)
(* overwritten.                                                         *)
(************************************************************************)



(* ::Code:: *)
Int[(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])^p_.,x_Symbol] :=
  1/e*Subst[Int[(a+b*Log[c*x^n])^p,x],x,d+e*x] /;
FreeQ[{a,b,c,d,e,n,p},x]


(* ::Code:: *)
Int[(f_+g_. x_)^q_.*(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])^p_.,x_Symbol] :=
  1/e*Subst[Int[(f*x/d)^q*(a+b*Log[c*x^n])^p,x],x,d+e*x] /;
FreeQ[{a,b,c,d,e,f,g,n,p,q},x] && EqQ[e*f-d*g,0]


(* ::Code:: *)
Int[Log[c_.*(d_+e_.*x_^n_.)]/x_,x_Symbol] :=
  -PolyLog[2,-c*e*x^n]/n /;
FreeQ[{c,d,e,n},x] && EqQ[c*d,1]


(* ::Code:: *)
Int[(a_.+b_.*Log[c_.*(d_+e_.*x_)])/x_,x_Symbol] :=
  (a+b*Log[c*d])*Log[x] + b*Int[Log[1+e*x/d]/x,x] /;
FreeQ[{a,b,c,d,e},x] && GtQ[c*d,0]


(* ::Code:: *)
Int[(a_.+b_.*Log[c_.*(d_+e_.*x_)])/(f_.+g_. x_),x_Symbol] :=
  1/g*Subst[Int[(a+b*Log[1+c*e*x/g])/x,x],x,f+g*x] /;
FreeQ[{a,b,c,d,e,f,g},x] && NeQ[e*f-d*g,0] && EqQ[g+c*(e*f-d*g),0]


(* ::Code:: *)
Int[(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])/(f_.+g_. x_),x_Symbol] :=
  Log[e*(f+g*x)/(e*f-d*g)]*(a+b*Log[c*(d+e*x)^n])/g - b*e*n/g*Int[Log[(e*(f+g*x))/(e*f-d*g)]/(d+e*x),x] /;
FreeQ[{a,b,c,d,e,f,g,n},x] && NeQ[e*f-d*g,0]


(* ::Code:: *)
Int[(f_.+g_.*x_)^q_.*(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.]),x_Symbol] :=
  (f+g*x)^(q+1)*(a+b*Log[c*(d+e*x)^n])/(g*(q+1)) - 
  b*e*n/(g*(q+1))*Int[(f+g*x)^(q+1)/(d+e*x),x] /;
FreeQ[{a,b,c,d,e,f,g,n,q},x] && NeQ[e*f-d*g,0] && NeQ[q,-1]


(* ::Code:: *)
Int[(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])^p_/(f_.+g_. x_),x_Symbol] :=
  Log[e*(f+g*x)/(e*f-d*g)]*(a+b*Log[c*(d+e*x)^n])^p/g - 
  b*e*n*p/g*Int[Log[(e*(f+g*x))/(e*f-d*g)]*(a+b*Log[c*(d+e*x)^n])^(p-1)/(d+e*x),x] /;
FreeQ[{a,b,c,d,e,f,g,n,p},x] && NeQ[e*f-d*g,0] && IGtQ[p,1]


(* ::Code:: *)
Int[(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])^p_/(f_.+g_.*x_)^2,x_Symbol] :=
  (d+e*x)*(a+b*Log[c*(d+e*x)^n])^p/((e*f-d*g)*(f+g*x)) - 
  b*e*n*p/(e*f-d*g)*Int[(a+b*Log[c*(d+e*x)^n])^(p-1)/(f+g*x),x] /;
FreeQ[{a,b,c,d,e,f,g,n},x] && NeQ[e*f-d*g,0] && GtQ[p,0]


(* ::Code:: *)
Int[(f_.+g_.*x_)^q_.*(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])^p_,x_Symbol] :=
  (f+g*x)^(q+1)*(a+b*Log[c*(d+e*x)^n])^p/(g*(q+1)) - 
  b*e*n*p/(g*(q+1))*Int[(f+g*x)^(q+1)*(a+b*Log[c*(d+e*x)^n])^(p-1)/(d+e*x),x] /;
FreeQ[{a,b,c,d,e,f,g,n,q},x] && NeQ[e*f-d*g,0] && GtQ[p,0] && NeQ[q,-1] && IntegersQ[2*p,2*q] && 
  (Not[IGtQ[q,0]] || EqQ[p,2] && NeQ[q,1])


(* ::Code:: *)
Int[(f_.+g_.*x_)^q_./(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.]),x_Symbol] :=
  Int[ExpandIntegrand[(f+g*x)^q/(a+b*Log[c*(d+e*x)^n]),x],x] /;
FreeQ[{a,b,c,d,e,f,g,n},x] && NeQ[e*f-d*g,0] && IGtQ[q,0]


(* ::Code:: *)
Int[(f_.+g_.*x_)^q_.*(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])^p_,x_Symbol] :=
  (d+e*x)*(f+g*x)^q*(a+b*Log[c*(d+e*x)^n])^(p+1)/(b*e*n*(p+1)) + 
  q*(e*f-d*g)/(b*e*n*(p+1))*Int[(f+g*x)^(q-1)*(a+b*Log[c*(d+e*x)^n])^(p+1),x] - 
  (q+1)/(b*n*(p+1))*Int[(f+g*x)^q*(a+b*Log[c*(d+e*x)^n])^(p+1),x] /;
FreeQ[{a,b,c,d,e,f,g,n},x] && NeQ[e*f-d*g,0] && LtQ[p,-1] && GtQ[q,0]


(* ::Code:: *)
Int[(f_.+g_.*x_)^q_.*(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])^p_,x_Symbol] :=
  Int[ExpandIntegrand[(f+g*x)^q*(a+b*Log[c*(d+e*x)^n])^p,x],x] /;
FreeQ[{a,b,c,d,e,f,g,n,p},x] && NeQ[e*f-d*g,0] && IGtQ[q,0]


(* ::Code:: *)
Int[Log[c_./(d_+e_.*x_)]/(f_+g_.*x_^2),x_Symbol] :=
  -e/g*Subst[Int[Log[2*d*x]/(1-2*d*x),x],x,1/(d+e*x)] /;
FreeQ[{c,d,e,f,g},x] && EqQ[c,2*d] && EqQ[e^2*f+d^2*g,0]


(* ::Code:: *)
Int[(a_.+b_.*Log[c_./(d_+e_.*x_)])/(f_+g_.*x_^2),x_Symbol] :=
  (a+b*Log[c/(2*d)])*Int[1/(f+g*x^2),x] + b*Int[Log[2*d/(d+e*x)]/(f+g*x^2),x] /;
FreeQ[{a,b,c,d,e,f,g},x] && EqQ[e^2*f+d^2*g,0] && GtQ[c/(2*d),0]


(* ::Code:: *)
Int[(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])/Sqrt[f_+g_.*x_^2],x_Symbol] :=
  With[{u=IntHide[1/Sqrt[f+g*x^2],x]},  
  u*(a+b*Log[c*(d+e*x)^n]) - b*e*n*Int[SimplifyIntegrand[u/(d+e*x),x],x]] /;
FreeQ[{a,b,c,d,e,f,g,n},x] && GtQ[f,0]


(* ::Code:: *)
Int[(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])/(Sqrt[f1_+g1_.*x_]*Sqrt[f2_+g2_.*x_]),x_Symbol] :=
  With[{u=IntHide[1/Sqrt[f1*f2+g1*g2*x^2],x]},  
  u*(a+b*Log[c*(d+e*x)^n]) - b*e*n*Int[SimplifyIntegrand[u/(d+e*x),x],x]] /;
FreeQ[{a,b,c,d,e,f1,g1,f2,g2,n},x] && EqQ[f2*g1+f1*g2,0] && GtQ[f1,0] && GtQ[f2,0]


(* ::Code:: *)
Int[(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])/Sqrt[f_+g_.*x_^2],x_Symbol] :=
  Sqrt[1+g/f*x^2]/Sqrt[f+g*x^2]*Int[(a+b*Log[c*(d+e*x)^n])/Sqrt[1+g/f*x^2],x] /;
FreeQ[{a,b,c,d,e,f,g,n},x] && Not[GtQ[f,0]]


(* ::Code:: *)
Int[(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])/(Sqrt[f1_+g1_.*x_]*Sqrt[f2_+g2_.*x_]),x_Symbol] :=
  Sqrt[1+g1*g2/(f1*f2)*x^2]/(Sqrt[f1+g1*x]*Sqrt[f2+g2*x])*Int[(a+b*Log[c*(d+e*x)^n])/Sqrt[1+g1*g2/(f1*f2)*x^2],x] /;
FreeQ[{a,b,c,d,e,f1,g1,f2,g2,n},x] && EqQ[f2*g1+f1*g2,0]


(* ::Code:: *)
Int[(f_.+g_.*x_^r_)^q_.*(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])^p_.,x_Symbol] :=
  With[{k=Denominator[r]},
  k*Subst[Int[x^(k-1)*(f+g*x^(k*r))^q*(a+b*Log[c*(d+e*x^k)^n])^p,x],x,x^(1/k)]] /;
FreeQ[{a,b,c,d,e,f,g,n,p,q},x] && FractionQ[r] && IGtQ[p,0]


(* ::Code:: *)
Int[(f_+g_.*x_^r_)^q_.*(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])^p_.,x_Symbol] :=
  Int[ExpandIntegrand[(a+b*Log[c*(d+e*x)^n])^p,(f+g*x^r)^q,x],x] /;
FreeQ[{a,b,c,d,e,f,g,n,r},x] && IGtQ[p,0] && IntegerQ[q] && (GtQ[q,0] || IntegerQ[r] && NeQ[r,1])


(* ::Code:: *)
Int[x_^m_.*Log[c_.*(d_+e_.*x_)]/(f_+g_. x_),x_Symbol] :=
  Int[ExpandIntegrand[Log[c*(d+e*x)],x^m/(f+g*x),x],x] /;
FreeQ[{c,d,e,f,g},x] && EqQ[e*f-d*g,0] && EqQ[c*d,1] && IntegerQ[m]


(* ::Code:: *)
Int[(f_.+g_. x_)^q_.*(h_.+i_. x_)^r_.*(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])^p_.,x_Symbol] :=
  1/e*Subst[Int[(g*x/e)^q*((e*h-d*i)/e+i*x/e)^r*(a+b*Log[c*x^n])^p,x],x,d+e*x] /;
FreeQ[{a,b,c,d,e,f,g,h,i,n,p,q,r},x] && EqQ[e*f-d*g,0] && (IGtQ[p,0] || IGtQ[r,0]) && IntegerQ[2*r]


(* ::Code:: *)
Int[x_^m_.*(f_+g_./x_)^q_.*(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])^p_.,x_Symbol] :=
  Int[(g+f*x)^q*(a+b*Log[c*(d+e*x)^n])^p,x] /;
FreeQ[{a,b,c,d,e,f,g,m,n,p,q},x] && EqQ[m,q] && IntegerQ[q]


(* ::Code:: *)
Int[x_^m_.*(f_.+g_.*x_^r_.)^q_.*(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])^p_.,x_Symbol] :=
  (f+g*x^r)^(q+1)*(a+b*Log[c*(d+e*x)^n])^p/(g*r*(q+1)) - 
  b*e*n*p/(g*r*(q+1))*Int[(f+g*x^r)^(q+1)*(a+b*Log[c*(d+e*x)^n])^(p-1)/(d+e*x),x] /;
FreeQ[{a,b,c,d,e,f,g,m,n,q,r},x] && EqQ[m,r-1] && NeQ[q,-1] && IGtQ[p,0]


(* ::Code:: *)
Int[x_^m_.*(f_+g_.*x_^r_.)^q_.*(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.]),x_Symbol] :=
  With[{u=IntHide[x^m*(f+g*x^r)^q,x]},  
  Dist[(a+b*Log[c*(d+e*x)^n]),u,x] - b*e*n*Int[SimplifyIntegrand[u/(d+e*x),x],x] /;
 InverseFunctionFreeQ[u,x]] /;
FreeQ[{a,b,c,d,e,f,g,m,n,q,r},x] && IntegerQ[m] && IntegerQ[q] && IntegerQ[r]


(* ::Code:: *)
Int[x_^m_.*(f_.+g_.*x_^r_)^q_.*(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])^p_.,x_Symbol] :=
  With[{k=Denominator[r]},
  k*Subst[Int[x^(k*(m+1)-1)*(f+g*x^(k*r))^q*(a+b*Log[c*(d+e*x^k)^n])^p,x],x,x^(1/k)]] /;
FreeQ[{a,b,c,d,e,f,g,n,p,q},x] && FractionQ[r] && IGtQ[p,0] && IntegerQ[m]


(* ::Code:: *)
Int[(h_.*x_)^m_.*(f_+g_.*x_^r_.)^q_.*(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])^p_.,x_Symbol] :=
  Int[ExpandIntegrand[(a+b*Log[c*(d+e*x)^n])^p,(h*x)^m*(f+g*x^r)^q,x],x] /;
FreeQ[{a,b,c,d,e,f,g,h,m,n,p,q,r},x] && IntegerQ[m] && IntegerQ[q]


(* ::Code:: *)
Int[Polyx_*(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])^p_.,x_Symbol] :=
  Int[ExpandIntegrand[Polyx*(a+b*Log[c*(d+e*x)^n])^p,x],x] /;
FreeQ[{a,b,c,d,e,n,p},x] && PolynomialQ[Polyx,x]


(* ::Code:: *)
Int[RFx_*(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])^p_.,x_Symbol] :=
  With[{u=ExpandIntegrand[(a+b*Log[c*(d+e*x)^n])^p,RFx,x]},
  Int[u,x] /;
 SumQ[u]] /;
FreeQ[{a,b,c,d,e,n},x] && RationalFunctionQ[RFx,x] && IntegerQ[p]


(* ::Code:: *)
Int[RFx_*(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])^p_.,x_Symbol] :=
  With[{u=ExpandIntegrand[RFx*(a+b*Log[c*(d+e*x)^n])^p,x]},
  Int[u,x] /;
 SumQ[u]] /;
FreeQ[{a,b,c,d,e,n},x] && RationalFunctionQ[RFx,x] && IntegerQ[p]


(* ::Code:: *)
Int[AFx_*(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])^p_.,x_Symbol] :=
  Unintegrable[AFx*(a+b*Log[c*(d+e*x)^n])^p,x] /;
FreeQ[{a,b,c,d,e,n,p},x] && AlgebraicFunctionQ[AFx,x,True]


(* ::Code:: *)
Int[u_^q_.*(a_.+b_.*Log[c_.*v_^n_.])^p_.,x_Symbol] :=
  Int[ExpandToSum[u,x]^q*(a+b*Log[c*ExpandToSum[v,x]^n])^p,x] /;
FreeQ[{a,b,c,n,p,q},x] && BinomialQ[u,x] && LinearQ[v,x] && Not[BinomialMatchQ[u,x] && LinearMatchQ[v,x]]


(* ::Code:: *)
Int[Log[f_.*x_^m_.]*(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.]),x_Symbol] :=
  -x*(m-Log[f*x^m])*(a+b*Log[c*(d+e*x)^n]) + b*e*m*n*Int[x/(d+e*x),x] - b*e*n*Int[(x*Log[f*x^m])/(d+e*x),x] /;
FreeQ[{a,b,c,d,e,f,m,n},x]


(* ::Code:: *)
Int[Log[f_.*x_^m_.]*(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])^p_,x_Symbol] :=
  With[{u=IntHide[(a+b*Log[c*(d+e*x)^n])^p,x]},
  Dist[Log[f*x^m],u,x] - m*Int[Dist[1/x,u,x],x]] /;
FreeQ[{a,b,c,d,e,f,m,n},x] && IGtQ[p,1]


(* ::Code:: *)
Int[Log[f_.*x_^m_.]*(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])^p_.,x_Symbol] :=
  Unintegrable[Log[f*x^m]*(a+b*Log[c*(d+e*x)^n])^p,x] /;
FreeQ[{a,b,c,d,e,f,m,n,p},x]


(* ::Code:: *)
Int[Log[f_.*x_^m_.]*(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])/x_,x_Symbol] :=
  Log[f*x^m]^2*(a+b*Log[c*(d+e*x)^n])/(2*m) - b*e*n/(2*m)*Int[Log[f*x^m]^2/(d+e*x),x] /;
FreeQ[{a,b,c,d,e,f,m,n},x]


(* ::Code:: *)
Int[(g_.*x_)^q_.*Log[f_.*x_^m_.]*(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.]),x_Symbol] :=
  -1/(g*(q+1))*(m*(g*x)^(q+1)/(q+1)-(g*x)^(q+1)*Log[f*x^m])*(a+b*Log[c*(d+e*x)^n]) + 
  b*e*m*n/(g*(q+1)^2)*Int[(g*x)^(q+1)/(d+e*x),x] - 
  b*e*n/(g*(q+1))*Int[(g*x)^(q+1)*Log[f*x^m]/(d+e*x),x] /;
FreeQ[{a,b,c,d,e,f,g,m,n,q},x] && NeQ[q,-1]


(* ::Code:: *)
Int[Log[f_.*x_^m_.]*(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])^p_./x_,x_Symbol] :=
  Log[f*x^m]^2*(a+b*Log[c*(d+e*x)^n])^p/(2*m) - b*e*n*p/(2*m)*Int[Log[f*x^m]^2*(a+b*Log[c*(d+e*x)^n])^(p-1)/(d+e*x),x] /;
FreeQ[{a,b,c,d,e,f,m,n},x] && IGtQ[p,0]


(* ::Code:: *)
Int[(g_.*x_)^q_.*Log[f_.*x_^m_.]*(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])^p_,x_Symbol] :=
  With[{u=IntHide[(g*x)^q*(a+b*Log[c*(d+e*x)^n])^p,x]},
  Dist[Log[f*x^m],u,x] - m*Int[Dist[1/x,u,x],x]] /;
FreeQ[{a,b,c,d,e,f,g,m,n,q},x] && IGtQ[p,1] && IGtQ[q,0]


(* ::Code:: *)
(* Int[(g_.*x_)^q_.*Log[f_.*x_^m_.]*(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])^p_,x_Symbol] :=
  With[{u=IntHide[(a+b*Log[c*(d+e*x)^n])^p,x]},
  Dist[(g*x)^q*Log[f*x^m],u,x] - g*m*Int[Dist[(g*x)^(q-1),u,x],x] - g*q*Int[Dist[(g*x)^(q-1)*Log[f*x^m],u,x],x]] /;
FreeQ[{a,b,c,d,e,f,g,m,n,q},x] && IGtQ[p,1] *)


(* ::Code:: *)
Int[(g_.*x_)^q_.*Log[f_.*x_^m_.]*(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])^p_.,x_Symbol] :=
  Unintegrable[(g*x)^q*Log[f*x^m]*(a+b*Log[c*(d+e*x)^n])^p,x] /;
FreeQ[{a,b,c,d,e,f,g,m,n,p,q},x]


(* ::Code:: *)
Int[Log[f_.*(g_.+h_.*x_)^m_.]*(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])^p_.,x_Symbol] :=
  1/e*Subst[Int[Log[f*(g*x/d)^m]*(a+b*Log[c*x^n])^p,x],x,d+e*x] /;
FreeQ[{a,b,c,d,e,f,g,h,m,n,p},x] && EqQ[e*f-d*g,0]


(* ::Code:: *)
Int[(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])*(f_.+g_.*Log[c_.*(d_+e_.*x_)^n_.]),x_Symbol] :=
  x*(a+b*Log[c*(d+e*x)^n])*(f+g*Log[c*(d+e*x)^n]) - 
  e*n*Int[(x*(b*f+a*g+2*b*g*Log[c*(d+e*x)^n]))/(d+e*x),x] /;
FreeQ[{a,b,c,d,e,f,g,n},x]


(* ::Code:: *)
Int[(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])^p_.*(f_.+g_.*Log[h_.*(i_.+j_.*x_)^m_.]),x_Symbol] :=
  x*(a+b*Log[c*(d+e*x)^n])^p*(f+g*Log[h*(i+j*x)^m]) - 
  g*j*m*Int[x*(a+b*Log[c*(d+e*x)^n])^p/(i+j*x),x] - 
  b*e*n*p*Int[x*(a+b*Log[c*(d+e*x)^n])^(p-1)*(f+g*Log[h*(i+j*x)^m])/(d+e*x),x] /;
FreeQ[{a,b,c,d,e,f,g,h,i,j,m,n},x] && IGtQ[p,0]


(* ::Code:: *)
Int[(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])^p_.*(f_.+g_.*Log[h_.*(i_.+j_.*x_)^m_.])^q_.,x_Symbol] :=
  Unintegrable[(a+b*Log[c*(d+e*x)^n])^p*(f+g*Log[h*(i+j*x)^m])^q,x] /;
FreeQ[{a,b,c,d,e,f,g,h,i,j,m,n,p},x]


(* ::Code:: *)
Int[(k_.+l_.*x_)^r_.*(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])^p_.*(f_.+g_.*Log[h_.*(i_.+j_.*x_)^m_.]),x_Symbol] :=
  1/e*Subst[Int[(k*x/d)^r*(a+b*Log[c*x^n])^p*(f+g*Log[h*((e*i-d*j)/e+j*x/e)^m]),x],x,d+e*x] /;
FreeQ[{a,b,c,d,e,f,g,h,i,j,k,l,n,p,r},x] && EqQ[e*k-d*l,0]


(* ::Code:: *)
Int[(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])*(f_.+g_.*Log[c_.*(d_+e_.*x_)^n_.])/x_,x_Symbol] :=
  Log[x]*(a+b*Log[c*(d+e*x)^n])*(f+g*Log[c*(d+e*x)^n]) - 
  e*n*Int[(Log[x]*(b*f+a*g+2*b*g*Log[c*(d+e*x)^n]))/(d+e*x),x] /;
FreeQ[{a,b,c,d,e,f,g,n},x]


(* ::Code:: *)
Int[x_^m_.*(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])*(f_.+g_.*Log[c_.*(d_+e_.*x_)^n_.]),x_Symbol] :=
  x^(m+1)*(a+b*Log[c*(d+e*x)^n])*(f+g*Log[c*(d+e*x)^n])/(m+1) - 
  e*n/(m+1)*Int[(x^(m+1)*(b*f+a*g+2*b*g*Log[c*(d+e*x)^n]))/(d+e*x),x] /;
FreeQ[{a,b,c,d,e,f,g,n,m},x] && NeQ[m,-1]


(* ::Code:: *)
Int[(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])*(f_.+g_.*Log[h_.*(i_.+j_.*x_)^m_.])/x_,x_Symbol] :=
  Log[x]*(a+b*Log[c*(d+e*x)^n])*(f+g*Log[h*(i+j*x)^m]) - 
  e*g*m*Int[Log[x]*(a+b*Log[c*(d+e*x)^n])/(d+e*x),x] - 
  b*j*n*Int[Log[x]*(f+g*Log[h*(i+j*x)^m])/(i+j*x),x]/;
FreeQ[{a,b,c,d,e,f,g,h,i,j,m,n},x] && EqQ[e*i-d*j,0]


(* ::Code:: *)
Int[Log[a_+b_.*x_]*Log[c_+d_.*x_]/x_,x_Symbol] :=
  Log[-b*x/a]*Log[a+b*x]*Log[c+d*x] - 
  1/2*(Log[-b*x/a]-Log[-d*x/c])*(Log[a+b*x]+Log[a*(c+d*x)/(c*(a+b*x))])^2 + 
  1/2*(Log[-b*x/a]-Log[-(b*c-a*d)*x/(a*(c+d*x))]+Log[(b*c-a*d)/(b*(c+d*x))])*Log[a*(c+d*x)/(c*(a+b*x))]^2 + 
  (Log[c+d*x]-Log[a*(c+d*x)/(c*(a+b*x))])*PolyLog[2,1+b*x/a] + 
  (Log[a+b*x]+Log[a*(c+d*x)/(c*(a+b*x))])*PolyLog[2,1+d*x/c] - 
  Log[a*(c+d*x)/(c*(a+b*x))]*PolyLog[2,d*(a+b*x)/(b*(c+d*x))] + 
  Log[a*(c+d*x)/(c*(a+b*x))]*PolyLog[2,c*(a+b*x)/(a*(c+d*x))] - 
  PolyLog[3,1+b*x/a] - PolyLog[3,1+d*x/c] - PolyLog[3,d*(a+b*x)/(b*(c+d*x))] + PolyLog[3,c*(a+b*x)/(a*(c+d*x))]/;
FreeQ[{a,b,c,d},x] && NeQ[b*c-a*d,0]


(* ::Code:: *)
Int[Log[v_]*Log[w_]/x_,x_Symbol] :=
  Int[Log[ExpandToSum[v,x]]*Log[ExpandToSum[w,x]]/x,x] /;
LinearQ[{v,w},x] && Not[LinearMatchQ[{v,w},x]]


(* ::Code:: *)
Int[Log[c_.*(d_+e_.*x_)^n_.]*Log[h_.*(i_.+j_.*x_)^m_.]/x_,x_Symbol] :=
  m*Int[Log[i+j*x]*Log[c*(d+e*x)^n]/x,x] - (m*Log[i+j*x]-Log[h*(i+j*x)^m])*Int[Log[c*(d+e*x)^n]/x,x]/;
FreeQ[{c,d,e,h,i,j,m,n},x] && NeQ[e*i-d*j,0] && NeQ[i+j*x,h*(i+j*x)^m]


(* ::Code:: *)
Int[(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])*(f_+g_.*Log[h_.*(i_.+j_.*x_)^m_.])/x_,x_Symbol] :=
  f*Int[(a+b*Log[c*(d+e*x)^n])/x,x] + g*Int[Log[h*(i+j*x)^m]*(a+b*Log[c*(d+e*x)^n])/x,x]/;
FreeQ[{a,b,c,d,e,f,g,h,i,j,m,n},x] && NeQ[e*i-d*j,0]


(* ::Code:: *)
Int[x_^r_.*(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])^p_.*(f_.+g_.*Log[h_.*(i_.+j_.*x_)^m_.]),x_Symbol] :=
  x^(r+1)*(a+b*Log[c*(d+e*x)^n])^p*(f+g*Log[h*(i+j*x)^m])/(r+1) - 
  g*j*m/(r+1)*Int[x^(r+1)*(a+b*Log[c*(d+e*x)^n])^p/(i+j*x),x] - 
  b*e*n*p/(r+1)*Int[x^(r+1)*(a+b*Log[c*(d+e*x)^n])^(p-1)*(f+g*Log[h*(i+j*x)^m])/(d+e*x),x] /;
FreeQ[{a,b,c,d,e,f,g,h,i,j,m,n},x] && IGtQ[p,0] && IntegerQ[r] && (EqQ[p,1] || GtQ[r,0]) && NeQ[r,-1]


(* ::Code:: *)
Int[(k_+l_.*x_)^r_.*(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])*(f_.+g_.*Log[h_.*(i_.+j_.*x_)^m_.]),x_Symbol] :=
  1/l*Subst[Int[x^r*(a+b*Log[c*(-(e*k-d*l)/l+e*x/l)^n])*(f+g*Log[h*(-(j*k-i*l)/l+j*x/l)^m]),x],x,k+l*x]/;
FreeQ[{a,b,c,d,e,f,g,h,i,j,k,l,m,n},x] && IntegerQ[r]


(* ::Code:: *)
Int[(k_.+l_.*x_)^r_.*(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])^p_.*(f_.+g_.*Log[h_.*(i_.+j_.*x_)^m_.])^q_.,x_Symbol] :=
  Unintegrable[(k+l*x)^r*(a+b*Log[c*(d+e*x)^n])^p*(f+g*Log[h*(i+j*x)^m])^q,x] /;
FreeQ[{a,b,c,d,e,f,g,h,i,j,k,l,m,n,p,q,r},x]


(* ::Code:: *)
Int[PolyLog[k_,h_+i_.*x_]*(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.])^p_./(f_+g_.*x_),x_Symbol] :=
  1/g*Subst[Int[PolyLog[k,h*x/d]*(a+b*Log[c*x^n])^p/x,x],x,d+e*x] /;
FreeQ[{a,b,c,d,e,f,g,h,i,k,n},x] && EqQ[e*f-d*g,0] && EqQ[g*h-f*i,0] && IGtQ[p,0]


(* ::Code:: *)
Int[Px_.*F_[f_.*(g_.+h_.*x_)]*(a_.+b_.*Log[c_.*(d_+e_.*x_)^n_.]),x_Symbol] :=
  With[{u=IntHide[Px*F[f*(g+h*x)],x]},
  Dist[(a+b*Log[c*(d+e*x)^n]),u,x] - b*e*n*Int[SimplifyIntegrand[u/(d+e*x),x],x]] /;
FreeQ[{a,b,c,d,e,f,g,h,n},x] && PolynomialQ[Px,x] && 
  MemberQ[{ArcSin, ArcCos, ArcTan, ArcCot, ArcSinh, ArcCosh, ArcTanh, ArcCoth},F]


(* ::Code:: *)
Int[u_.*(a_.+b_.*Log[c_.*v_^n_.])^p_.,x_Symbol] :=
  Int[u*(a+b*Log[c*ExpandToSum[v,x]^n])^p,x] /;
FreeQ[{a,b,c,n,p},x] && LinearQ[v,x] && Not[LinearMatchQ[v,x]] && Not[EqQ[n,1] && MatchQ[c*v,e_.*(f_+g_.*x) /; FreeQ[{e,f,g},x]]]


(* ::Code:: *)
Int[u_.*(a_.+b_.*Log[c_.*(d_.*(e_.+f_. x_)^m_.)^n_])^p_.,x_Symbol] :=
  Subst[Int[u*(a+b*Log[c*d^n*(e+f*x)^(m*n)])^p,x],c*d^n*(e+f*x)^(m*n),c*(d*(e+f*x)^m)^n] /;
FreeQ[{a,b,c,d,e,f,m,n,p},x] && Not[IntegerQ[n]] && Not[EqQ[d,1] && EqQ[m,1]] && 
  IntegralFreeQ[IntHide[u*(a+b*Log[c*d^n*(e+f*x)^(m*n)])^p,x]]


(* ::Code:: *)
Int[AFx_*(a_.+b_.*Log[c_.*(d_.*(e_.+f_. x_)^m_.)^n_])^p_.,x_Symbol] :=
  Unintegrable[AFx*(a+b*Log[c*(d*(e+f*x)^m)^n])^p,x] /;
FreeQ[{a,b,c,d,e,f,m,n,p},x] && AlgebraicFunctionQ[AFx,x,True]



