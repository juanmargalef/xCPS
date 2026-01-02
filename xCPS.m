(* ::Package:: *)

(* ::Title:: *)
(*xCPS*)


(* ::Subtitle:: *)
(*Covariant Phase Space Formalism for xAct*)


(* ::Author:: *)
(*Juan Margalef-Bentabol*)


(* ::Affiliation:: *)
(*juan.margalef@umontreal.ca*)
(*Universit\[EAcute] de Montr\[EAcute]al, Canada*)


(* ::Author:: *)
(*Laura S\[AAcute]nchez Cotta*)


(* ::Affiliation:: *)
(*100527266@alumnos.uc3m.es*)
(*Universidad Carlos III de Madrid, Spain*)


(* ::Author:: *)
(*(c) 2025, under GPL*)
(**)
(*http://www.xAct.es/*)
(*http://groups.google.com/group/xAct*)
(*https://github.com/juanmargalef/xCPS*)


(* ::Abstract:: *)
(*xCPS is a Covariant Phase Space package in xAct.*)
(**)
(*xCPS is distributed under the GNU General Public License, and runs on top of xTensor which is a free package for fast manipulation of abstract tensor expressions that can be downloaded from http://www.xact.es*)


(* ::Input:: *)
(*DateList[]*)


(* ::Input::Initialization:: *)
xAct`xCPS`$Version={"1.0.1",{2025,31,12}};
xAct`xCPS`$xTensorVersionExpected={"1.1.4",{2020,2,16}};


(* ::Chapter:: *)
(*1. Initialization *)


(* ::Section:: *)
(*1.1. GPL*)


(* ::Input::Initialization:: *)
(* xCPS: Covariant Phase Space Formalism in Field Theories *)

(* Copyright 2025 (C) Juan Margalef-Bentabol, Laura S\[AAcute]nchez Cotta *)

(* This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as
 published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place-Suite 330, Boston, MA 02111-1307, USA. 
*)


(* ::Section:: *)
(*1.2. Info package*)


(* ::Input::Initialization:: *)
(* :Title: xCPS *)

(* :Author: Juan Margalef-Bentabol and Laura S\[AAcute]nchez Cotta *)

(* :Summary: Covariant Phase Space formalism in Field Theories *)

(* :Brief Discussion:
   - xCPS extends xAct to work with vertical differentiable forms (differential forms in the space of fields).
   - Introduces the vertical exterior algebra and variational vector fields.
   - Computes the symplectic form of a field theory through variational calculus.
   - Computes the Noether currents of Noether symmetries.
   
*)
  
(* :Context: xAct`xCPS` *)

(* :Package Version: 1 *)

(* :Copyright: Juan Margalef-Bentabol and Laura S\[AAcute]nchez Cotta (2025) *)

(* :History: See xCPS.History *)

(* :Keywords: *)

(* :Source: xCPS.nb *)

(* :Warning: *)

(* :Mathematica Version: 9.0 and later *)

(* :Limitations: None *)


(* ::Section:: *)
(*1.3. Begin package*)


(* ::Text:: *)
(*Protect against multiple loading of the package:*)


(* ::Input::Initialization:: *)
With[{xAct`xCPS`Private`xCPSSymbols=DeleteCases[Join[Names["xAct`xCPS`*"],Names["xAct`xCPS`Private`*"]],"$Version"|"xAct`xCPS`$Version"|"$xTensorVersionExpected"|"xAct`xCPS`$xTensorVersionExpected"]},
Unprotect/@xAct`xCPS`Private`xCPSSymbols;
Clear/@xAct`xCPS`Private`xCPSSymbols;
]


(* ::Text:: *)
(*Decide which is the last package being read:*)


(* ::Input::Initialization:: *)
If[Unevaluated[xAct`xCore`Private`$LastPackage]===xAct`xCore`Private`$LastPackage,xAct`xCore`Private`$LastPackage="xAct`xCPS`"];


(* ::Text:: *)
(*Explicit (not hidden) import of other packages.*)


(* ::Input::Initialization:: *)
BeginPackage["xAct`xCPS`",{"xAct`xTensor`","xAct`xPerm`","xAct`xCore`"}]


(* ::Text:: *)
(*Check version of xTensor. We simply compare dates:*)


(* ::Input::Initialization:: *)
If[Not@OrderedQ@Map[Last,{xAct`xCPS`$xTensorVersionExpected,xAct`xTensor`$Version}],Throw@Message[General::versions,"xTensor",xAct`xTensor`$Version,xAct`xCPS`$xTensorVersionExpected]]


(* ::Text:: *)
(*Welcome message:*)


(* ::Input::Initialization:: *)
Print[xAct`xCore`Private`bars];
Print["Package xAct`xCPS` version ",$Version[[1]],", ",$Version[[2]]];
Print["CopyRight (C) 2025, Juan Margalef-Bentabol and Laura S\[AAcute]nchez Cotta, under the General Public License."];


(* ::Text:: *)
(*We specify the context xAct`xCPS` to avoid overriding the Disclaimer of the other packages. However we need to turn off the message General:shdw temporarily:*)


(* ::Input::Initialization:: *)
Off[General::shdw]
xAct`xCPS`Disclaimer[]:=Print["These are points 11 and 12 of the General Public License:\n\nBECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM `AS IS\.b4 WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU. SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.\n\nIN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR REDISTRIBUTE THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS), EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES."]
On[General::shdw]


(* ::Text:: *)
(*If this is the last package show the GPL short disclaimer:*)


(* ::Input::Initialization:: *)
If[xAct`xCore`Private`$LastPackage==="xAct`xCPS`",
Unset[xAct`xCore`Private`$LastPackage];
Print[xAct`xCore`Private`bars];
Print["These packages come with ABSOLUTELY NO WARRANTY; for details type Disclaimer[]. This is free software, and you are welcome to redistribute it under certain conditions. See the General Public License for details."];
Print[xAct`xCore`Private`bars]];


(* ::Section:: *)
(*1.4. Usage messages*)


(* ::Subsection:: *)
(*1.4.1. Functions*)


AddVariationalRelation::usage="AddVariationalRelation[masterTensor -> dependentTensor] adds a variational dependency to the global $VariationalGraph.";

CoefficientsOfVVF::usage="CoefficientsOfVVF[VVF] returns the coefficients associated with the variational vector field (VVF), extracted from InfoFromVVF.";

ComponentsOfGeneralizedVVF::usage="ComponentsOfGeneralizedVVF[GVVF] returns the list of components of the generalized variational vector field 'GVVF' (the 'directions' of 'GVVF' in the space of fields).";

ComponentsOfVVF::usage="ComponentsOfVVF[VVF] returns the list of components of the generalized variational vector field 'VVF' (the 'directions' of 'VVF' in the space of tensors).";

CovDOfNormal::usage="CovDOfNormal[n] returns the covariant derivative associated with the normal vector 'n'.";

CovDOfTotalDerivative::usage="CovDOfTotalDerivative[TotD] returns the covariant derivative operator associated with the total derivative 'TotD'.";

CovDToTotalDerivative::usage="CovDToTotalDerivative[expr] rewrites covariant derivatives (with the argument kept with Keep[]) in 'expr' using the corresponding head TotalDerivative[der].";

CurrentFromVector::usage="CurrentFromVector[vector][tensors, der][Lagrangian] is a function that computes the current associated with a given vector field. For a given Lagrangian (defined as a top-form), the current is given by \!\(\*SubscriptBox[\(Q\), \(v\)]\)=\!\(\*SubscriptBox[\(i\), \(v\)]\)L-\!\(\*SubscriptBox[\(\[ImaginaryI]\), \(\(VVFFromLieD[v]\)[tensors]\)]\)\[CapitalTheta] where \[CapitalTheta] is the SymplecticPotential. There is an overall sign controlled by $NoetherCurrentSign.
- 'tensors' is optional. It is a list of heads of tensors to be considered as dynamic. If not specified, it considers all the tensors present in 'Lagrangian'.
- 'der' is optional. If not specified, it finds the only CovD present (if several are present, an error is thrown).";

DefGeneralizedVVF::usage="DefGeneralizedVVF[name, options][pairs] defines a generalized variational vector field (GVVF) as a list of replacements for k-vertical-forms.
- Options:
  * VanishOverOtherForms->True";

DefPartialPartial::usage="DefPartialPartial[function, list_of_tensors] defines a tensor representing the n-th partial derivative of 'function' with respect to the 'list_of_tensors'. The resulting tensor has the indices with opposite character to the corresponding 'list_of_tensors' (with the same symmetries).";

DependenciesOfScalar::usage="DependenciesOfScalar[scalar] returns the list of tensors on which the scalar has been declared to depend as an option of DefScalarFunction.";

DiscardTotalDerivative::usage="DiscardTotalDerivative[expr] removes all total derivative terms from 'expr'.";

DivergenceQ::usage="DivergenceQ[{metric, LC}, der, options][expr, optionalfunctions] checks whether the scalar expression 'expr' is a total divergence with respect to 'der'. 'metric' is an auxiliary metric and LC its associated Levi-Civitta connection (possibly extended to act over the same bundles as 'der').
- 'LC' is optional. If not included, DivergenceQ[metric, der, options][expr, optionalfunctions], LC=CovDOfMetric[metric].
- 'optionalfunctions' are functions that are applied in the intermediate steps to simplify the expressions.
- Options:
  * CheckZero->False: If True, DivergenceQ uses === instead of == to check if the expressions vanish.";

dlLagrangianQ::usage="It verifies whether 'expr' is a scalar with VertDeg[expr]=1 and one exact vertical form per summand.";

dlNormalOfCovDQ::usage="dlNormalOfCovDQ[n] returns True if 'n' is the vertical differential of a fiducial normal vector.";

dlNormalOfCovDToCovD::usage="dlNormalOfCovDToCovD[expr] rewrites expressions involving the vertical differential of a fiducial normal vector in terms of its covariant derivative.";

EnergyMomentum::usage="EnergyMomentum[Lagrangian] computes the energy-momentum tensor associated with the given 'Lagrangian' (proportional to the EOM with respect to the metric).";

EOM::usage="EOM[tensor, der][Lagrangian] is equivalent to EOMOf1Form[tensor, der][VertDiff@Lagrangian].";

EOMOf1Form::usage="EOMOf1Form[HeadOfTensor, der][dlLagrangian] extracts the equations of motion from the variation of a Lagrangian with respect to HeadOfTensor using der to 'integrate by parts' (Leibniz rule).
- 'der' is optional. If not specified, it finds the only CovD present (if several are present, an error is thrown).";

ExpandVertDiff::usage="ExpandVertDiff[options][expr] expands the vertical exterior derivatives appearing in 'expr'.
- Options:
  * SeparateMetric->True
  * Explode->True
  * ExpandVertDiffCovD->True
  * ExpandVertDiffLieD->True
  * ExpandVertDiffBracket->True
  * ExpandVertDiffTotalDerivative\[Rule]True
  * ExpandVertDiffScalarFunction->True
  * ConstantTensors->{}
  * NonConstantTensors->{}
  * HoldExpandVertDiff->None";

ExpandVertInt::usage="ExpandVertInt[options][expr] expands the vertical interior products appearing in 'expr'.
- Options:
  * Same options as ExpandVertDiff
  * ExpandVertIntCovD->True
  * ExpandVertIntLieD->True
  * ExpandVertIntBracket->True
  * ExpandVertIntTotalDerivative->True
  * HoldExpandVertInt->None.";

FindCyclicVariationalRelations::usage="FindCyclicVariationalRelations[graph, option] returns a list of cyclic variational relations in the given graph and prints the cycles.
- Option:
  * ShowGraph\[Rule]True";

FindPotentialDivergence::usage="FindPotentialDivergence[der, iteration][expr, optionalfunctions] looks for 'potential' such that potential expr=der[ind][potential].\n
FindPotentialDivergence[metric, iteration][expr, optionalfunctions]=FindPotentialDivergence[CovDOfMetric[metric], iteration][expr, optionalfunctions].
- 'iteration' is an optional positive integer that limits the amount of iteration in order to obtain the middle steps.
- 'optionalfunctions' are functions that are applied in the intermediate steps to simplify the expressions.";

FirstVariation::usage="FirstVariation[tensors, der, options][Lagrangian] is equivalent to FirstVariationOf1Form[tensors, der, options][VertDiff@Lagrangian].";

FirstVariationOf1Form::usage="FirstVariationOf1Form[tensors, der, options][dlLagrangian] computes the first variation of a dlLagrangian with respect to tensors using der to 'integrate by parts' (Leibniz rule) y keeping the total derivative term TotalDerivative[der][\[CapitalTheta]].";

FunctionOfPartialPartial::usage="FunctionOfPartialPartial[PartialPartialTensor] returns the scalar function associated with 'PartialPartialTensor'.";

GeneralizedVVFQ::usage="GeneralizedVVFQ[GVVF] checks if 'GVVF' has been defined as a generalized variational vector field.";

GenerateExpandVertDiffRule::usage="GenerateExpandVertDiffRule[{dltensor, expansion}, options] generates the expansion rule dltensor->expansion dos 'dltensor' to be used by ExpandVertDiff. It checks if VertDeg match and if the indices are in the natural position before creating the rule (it can be deactivated with the option CheckGenerateExpandVertDiffRule->False).";

KretschmannToRiemann::usage = "KretschmannToRiemann[expr] substitutes the Krestchmann tensors for the 'square' of the Riemann tensor.";

LagrangianQ::usage="LagrangianQ[expr] verifies whether 'expr' is a scalar with zero vertical degree.";

ListOfVariationalConstantsOf::usage="ListOfVariationalConstantsOf[constantTensors] returns the list of tensors influenced by the given constant tensors.";

ListVariationalRelationsOf::usage="ListVariationalRelationsOf[tensor, options] returns the list of tensors variationally related to 'tensor'.
- Options:
  * Directed -> 'direction': only the outward/inward/both vertices of the seed tensors are included ('direction' can be 'in', 'out' or 'both').";

MakeVertRule::usage="MakeVertRule[{lhs, rhs}, opts] calls MakeRule twice: one for {'lhs' \[RightArrow] 'rhs'} and the other for {ExpandVertDiff[options][VertDiff[lhs]] \[RightArrow] ExpandVertDiff[options][VertDiff[rhs]]}. It accepts the options of both MakeRule and ExpandVertDiff.";

NoetherCurrent::usage="NoetherCurrent[vvf][der, iteration][Lagrangian, optionalfunctions] computes the Noether current associated with a given vector field and Lagrangian. It is given by NoetherPotential[vvf][der][Lagrangian]-VertInt[vvf][\[CapitalTheta]] where \[CapitalTheta] is the SymplecticPotential. There is an overall sign controlled by $NoetherCurrentSign.";

NoetherPotential::usage="NoetherPotential[vvf][der][Lagrangian] computes the Noether potential associated with the Lagrangian.";

NoetherSymmetryQ::usage="NoetherSymmetryQ[vvf][tensors, der][Lagrangian] checks whether the variational vector field vvf is a Noether symmetry of the Lagrangian.";

NonZeroVertDegQ::usage = "NonZeroVertDegQ[expr] returns True if the vertical degree of 'expr' is nonzero and False otherwise."; 

NormalOfCovD::usage="NormalOfCovD[expr] replaces the fiducial normal vector by its corresponding vertical differential representation. It uses $NormalOfCovD to perform the replacements.";

NormalOfCovDQ::usage="NormalOfCovDQ[expr] returns True if 'expr' contains the fiducial normal vector.";

NormalOfCovDToCovD::usage="NormalOfCovDToCovD[expr] rewrites expressions containing the fiducial normal vector into covariant derivatives.";

NormalOfManifold::usage="NormalOfManifold[manifold] returns the normal vector field associated with the given 'manifold'.";

NormalOfTotalDerivative::usage="NormalOfTotalDerivative[expr] rewrites total derivatives into expressions containing the fiducial normal vector.";

OnlyTotalDerivative::usage="OnlyTotalDerivative[expr] extracts the total derivative terms from 'expr'.";

OrderOfPartialPartial::usage="OrderOfPartialPartial[PartialPartialTensor] returns the order of the derivative associated with 'PartialPartialTensor'.";

PartialPartial::usage="PartialPartial[function, {list_of_tensors}] represents the n-th partial derivative of 'function' with respect to 'list_of_tensors'.";

PartialPartialQ::usage="PartialPartialQ[expr] returns True if 'expr' is a PartialPartial tensor.";

PartialPartialsOfFunction::usage="PartialPartialsOfFunction[function] returns the list of PartialPartial tensors associated with 'function'.";

PartialPartialsOfTensor::usage="PartialPartialsOfTensor[tensor] returns the list of PartialPartial tensors associated with 'tensor'.";

RemoveExpandVertDiffRule::usage="RemoveExpandVertDiffRule[pattern] removes all the expansion rules associated with 'pattern'.";

RemoveVariationalRelation::usage="RemoveVariationalRelation[masterTensor -> dependentTensor] removes a variational dependency from the global $VariationalGraph.";

ResetSession::usage="ResetSession[] resets the session, clearing all definitions and variables.";

RulesOfGeneralizedVVFQ::usage="RulesOfGeneralizedVVFQ[GVVF] returns the replacement rules used to define the generalized variational vector field 'GVVF'.";

SetSigns::usage="SetSigns[signs] sets the global signs used in the calculations.";

SortVertOperators::usage="SortVertOperators[expr] sorts the vertical operators appearing in 'expr' into a standard order.";

SymplecticCurrent::usage="SymplecticCurrent[der][Lagrangian] is equivalent to SymplecticCurrentOf1Form[der][VertDiff@Lagrangian].";

SymplecticCurrentOf1Form::usage="SymplecticCurrentOf1Form[der][dlLagrangian] computes the symplectic current associated with a dlLagrangian.";

SymplecticPotential::usage="SymplecticPotential[der][Lagrangian] is equivalent to SymplecticPotentialOf1Form[der][VertDiff@Lagrangian].";

SymplecticPotentialOf1Form::usage="SymplecticPotentialOf1Form[der][dlLagrangian] computes the symplectic potential associated with a dlLagrangian.";

TensorWithIndices::usage="TensorWithIndices[tensor, indices] returns the tensor 'tensor' with the given 'indices'.";

TensorsOfPartialPartial::usage="TensorsOfPartialPartial[PartialPartialTensor] returns the list of tensors with respect to which 'PartialPartialTensor' is defined.";

TotalDerivativeDivergenceQ::usage="TotalDerivativeDivergenceQ[der][expr] checks whether 'expr' is a total derivative divergence with respect to 'der'.";

TotalDerivativeOfCovD::usage="TotalDerivativeOfCovD[expr] rewrites covariant derivatives into total derivatives.";

TotalDerivativeOfManifold::usage="TotalDerivativeOfManifold[manifold] returns the total derivative operator associated with 'manifold'.";

TotalDerivativeOfNormal::usage="TotalDerivativeOfNormal[expr] rewrites expressions containing the fiducial normal vector into total derivatives.";

TotalDerivativePotential::usage="TotalDerivativePotential[der, iteration][expr] attempts to find a potential for the total derivative 'expr'.";

TotalDerivativeQ::usage="TotalDerivativeQ[expr] returns True if 'expr' is a total derivative.";

TotalDerivativeToCovD::usage="TotalDerivativeToCovD[expr] rewrites total derivatives into covariant derivatives.";

UndefGeneralizedVVF::usage="UndefGeneralizedVVF[name] removes the definition of the generalized variational vector field 'name'.";

UnprotectVertDiffRule::usage = "UnprotectVertDiffRule[dltensor] unprotects the VertDiffRules of 'dltensor' to allow GenerateExpandVertDiffRule and RemoveExpandVertDiffRule for 'dltensor'.";

VariationalRelationsOf::usage="VariationalRelationsOf[tensor, options] returns the list of tensors variationally related to 'tensor'.
- Options:
  * Directed -> 'direction': only the outward/inward/both vertices of the seed tensors are included ('direction' can be 'in', 'out' or 'both').";

VariationalVector::usage="VariationalVector[tensor] returns the variational vector associated with 'tensor'.";

VariationalVectorQ::usage="VariationalVectorQ[expr] returns True if 'expr' is a variational vector.";

VariationallyConstantQ::usage="VariationallyConstantQ[tensor] returns True if 'tensor' is variationally constant.";

VertBracket::usage="VertBracket[vvf1, vvf2] computes the vertical bracket of two variational vector fields.";

VertBracketToVertLie::usage="VertBracketToVertLie[expr] rewrites vertical brackets as vertical Lie derivatives.";

VertCartanMagicFormula::usage="VertCartanMagicFormula[vvf][expr] applies the vertical Cartan magic formula to 'expr'.";

VertDeg::usage="VertDeg[expr] returns the vertical degree of 'expr'.";

VertDiff::usage="VertDiff[expr] computes the vertical exterior derivative of 'expr'.";

VertExactHeadQ::usage="VertExactHeadQ[expr] returns True if 'expr' has VertExact as head.";

VertExactQ::usage="VertExactQ[expr] returns True if 'expr' is vertically exact.";

VertInt::usage="VertInt[vvf][expr] computes the vertical interior product of 'vvf' acting on 'expr'.";

VertLie::usage="VertLie[vvf][expr] computes the vertical Lie derivative of 'vvf' acting on 'expr'.";

VVFFromLieD::usage="VVFFromLieD[vector][tensors] computes the variational vector field associated with the Lie derivative of 'vector' acting on 'tensors'.";

VVFFromList::usage="VVFFromList[replacementRules] constructs a variational vector field from a list of replacement rules.";

VVFQ::usage="VVFQ[expr] returns True if 'expr' is a variational vector field.";

WWedge::usage="WWedge[a, b] returns the wedge product of two forms 'a' and 'b'.";
\:2a55::usage="\:2a55[a, b] or a\:2a55b returns the wedge product of two forms 'a' and 'b'.";

ZeroVertDegQ::usage = "ZeroVertDegQ[expr] returns True if the vertical degree of 'expr' is zero and False otherwise.";


(* ::Subsection:: *)
(*1.4.2. Constants*)


$dlTensors::usage = "$dlTensors=$VertExactForms";

$GeneralizedVVF::usage = "$GeneralizedVVF is the list of generalized variational vector fields (GVVF) defined within the session.";

$MasterTensors::usage = "$MasterTensors returns the list of defined tensors in the session that are neither exact vertical forms nor variational vector fields.";

$NameVerticalExteriorDerivative::usage = "$NameVerticalExteriorDerivative is a string representing the name of the vertical exterior derivative, by default 'dl' (to remind both the symbols \[DifferentialD] and \[Delta]).";

$NamesOfSigns::usage = "$NamesOfSigns returns the symbols of $ConstantSymbols whose names begins with '$' and ends in 'Sign'.";

$NoetherCurrentSign::usage = "$NoetherCurrentSign is a constant that controls the sign of the Noether current.";

$NormalsOfCovD::usage = "$NormalsOfCovD is the list of fiducial normals associated with all the defined covariant derivatives.";

$NormalsOfPD::usage = "$NormalsOfPD is the sublist of $NormalsOfCovD formed by fiducial normals defined from partial derivatives (PD). There is one for each defined manifold.";

$PartialPartialTensors::usage = "$PartialPartialTensors is a global variable that stores all tensors defined as partial derivatives of scalar functions with respect to other tensors, i.e. those created via DefPartialPartial.";

$RemoveParenthesesPrintAs::usage = "$RemoveParenthesesPrintAs is a global variable that controls the behavior of the private function RemoveOuterParanthesis (used to set the PrintAs of the associated tensors VertDiff[tensor] and VariationalVector[tensor]). If set to True, the function removes enclosing parentheses from strings when present; if False, the strings are returned unchanged.";

$SortVertOperatorsOrder::usage = "$SortVertOperatorsOrder is a global variable which encodes the default ordering of the vertical operators used in SortVertOperators. This ordering determines how expressions with multiple vertical operators are sorted.";

$SymbolVerticalExteriorDerivative::usage = "$SymbolVerticalExteriorDerivative is the symbol representing the vertical exterior derivative, typically denoted as \[DifferentialD] or \[Delta].";

$SymplecticCurrentSign::usage = "$SymplecticCurrentSign is a constant that controls the sign of the symplectic current.";

$TotalDerivatives::usage = "$TotalDerivatives is the list of total derivatives associated with all the defined covariant derivatives.";

$UseInverseMetric::usage = "$UseInverseMetric is a boolean flag indicating whether the inverse metric is used as the fundamental field instead of the metric. This constant should be fixed at the beginning, and posterior changes might produce unexpected results.";

$VariationalGraph::usage = "$VariationalGraph is a directed graph that encodes the variational dependency relations among tensors within the session.";

$VariationalVectors::usage = "$VariationalVectors is the list of tensors defined in the session as variational vectors.";

$ValuesOfSigns::usage = "$ValuesOfSigns returns the list of current values of the symbols in $NamesOfSigns.";

$VertExactForms::usage = "$VertExactForms returns the list of tensors defined in the session as vertically exact forms.";


(* ::Subsection:: *)
(*1.4.3. Options*)


Both::usage = "Both is a possible value for the Directed option in VariationalRelationsOf. It specifies that both inward and outward variational dependencies should be considered.";

CheckGenerateExpandVertDiffRule::usage = "CheckGenerateExpandVertDiffRule is an option for GenerateExpandVertDiffRule. If True, the vertical degree and index positions are validated; if False, the rule is generated without checks.";

CheckZero::usage = "CheckZero is an option for DivergenceQ. When set to True, SameQ (===) is used to check if the equations of motion vanish (hence returnin either True or False); when False, Equal (==) is used possibly returning some unevaluated boolean conditions.";

ConstantTensors::usage = "ConstantTensors is an option for ExpandVertDiff and related functions. It lists tensors to be treated as variationally constant, meaning their vertical exterior derivative (VertDiff) is assumed to vanish.";

Directed::usage = "Directed is an option for SubGraphRelations and VariationalRelationsOf. It specifies whether to consider Inward, Outward, or both types of variational dependencies.";

ExpandVertDiffBracket::usage = "ExpandVertDiffBracket is an option for ExpandVertDiff. If True, terms with VertDiff@Bracket are expanded.";

ExpandVertDiffCovD::usage = "ExpandVertDiffCovD is an option for ExpandVertDiff and ExpandVertDiffRules. If True, terms with VertDiff@der are expanded.";

ExpandVertDiffLieD::usage = "ExpandVertDiffLieD is an option for ExpandVertDiff. If True, terms with VertDiff@LieD are expanded.";

ExpandVertDiffScalarFunction::usage = "ExpandVertDiffScalarFunction is an option for ExpandVertDiff and related functions. If True, scalar functions depending on tensor fields are expanded using the chain rule (some tensors representing partial derivatives might be defined on the fly).";

ExpandVertDiffTotalDerivative::usage = "ExpandVertDiffTotalDerivative is an option for ExpandVertDiff and ExpandVertDiffRules. When True, VertDiff and TotalDerivativeOfCovD[der] commute.";

ExpandVertIntBracket::usage = "ExpandVertIntBracket is an option for ExpandVertInt. If True, terms with VertInt[vvf]@LieD are expanded according to the rules provided by 'vvf'.";

ExpandVertIntCovD::usage = "ExpandVertIntCovD is an option for ExpandVertInt. If True, terms with VertInt[vvf]@der are expanded according to the rules provided by 'vvf'.";

ExpandVertIntLieD::usage = "ExpandVertIntLieD is an option for ExpandVertInt. If True, Lie derivatives inside a vertical integral (VertInt) are explicitly expanded.";

ExpandVertIntTotalDerivative::usage = "ExpandVertIntTotalDerivative is an option for ExpandVertInt. If True, terms with VertInt[vvf]@TotalDerivative are expanded according to the rules provided by 'vvf'.";

HideTrivialRelations::usage = "HideTrivialRelations is an option for VariationalRelationsOf. When True, it removes the trivial relations that link any 'tensor' to VertDiff[tensor] and VariationalVector[tensor].";

HoldExpandVertDiff::usage = "HoldExpandVertDiff is an option for ExpandVertDiff and related functions. It specifies tensors whose vertical exterior derivative (VertDiff) should not be expanded.";

HoldExpandVertInt::usage = "HoldExpandVertInt is an option for ExpandVertInt. It prevents the application of the replacement rules given by a variational vector field during the expansion.";

NonConstantTensors::usage = "NonConstantTensors is an option for ExpandVertDiff and related functions. It lists tensors to be treated as variationally non-constant. All other tensors are treated as variationally constant, meaning their vertical exterior derivative (VertDiff) is assumed to vanish.";

ShowGraph::usage = "ShowGraph is an option for FindCyclicVariationalRelations. When True, it displays the variational graph and highlights detected cycles.";

Signs::usage = "Signs is an option for ResetSession. It determines the sign convention to be used when redefining constants. It admits the numerical values 1 or -1. If the option 0 is chosen, the numerical values are removed leaving the symbolic signs.";

UndefInfo::usage = "UndefInfo is an option for ResetSession. When True, ResetSession displays information about undefined objects.";

VanishOverOtherForms::usage = "VanishOverOtherForms is an option for DefGeneralizedVVF. If True, the generalized vector field 'GVVF' is assumed to vanish over k-vertical-forms not explicitly included in its definition. This behaviour appears when using ExpandVertInt.";


(* ::Subsection:: *)
(*1.4.4. Filter types*)


NonZeroVertDeg::usage = "NonZeroVertDeg is a type that represents tensors with nonzero vertical degree. It can be used with FindAllOfType to extract such tensors from expressions.";

NormalOfPD::usage = "NormalOfPD is a type that represents fiducial normals defined from partial derivatives (PD). It can be used with FindAllOfType to extract such fiducial normals from expressions.";

VertDiffExact::usage = "VertDiffExact is a type that represents tensors which are exact under vertical differentiation. It is used with FindAllOfType to locate vertically exact tensors in expressions.";

ZeroVertDeg::usage = "ZeroVertDeg is a type that represents tensors with vertical degree equal to zero. It can be used with FindAllOfType to extract them from expressions.";


(* ::Section:: *)
(*1.5. Begin private*)


Begin["xAct`xCPS`Private`"]


(* ::Chapter:: *)
(*2. Initial definitions*)


(* ::Section:: *)
(*2.1. Constants*)


(* Constants *)
$SymbolDoubleWedge="\:2a55";
$SymbolVerticalExteriorDerivative="\[DifferentialD]";
$NameVerticalExteriorDerivative="dl";
$SymbolVertInt="\[ImaginaryI]";
$SymbolVertLie="\[DoubleStruckCapitalL]";

$MasterTensors:=Complement[$Tensors,Select[$Tensors,VertExactHeadQ[#]||VariationalVectorQ[#]&]];
$VertExactForms:=Select[$Tensors,VertExactHeadQ];
$VariationalVectors:=Select[$Tensors,VariationalVectorQ];
$dlTensors:=Select[$Tensors,VertExactHeadQ];
$NormalsOfCovD:=Select[$Tensors,NormalOfCovDQ];
$NormalsOfPD:=Select[$Tensors,NormalOfPDQ];
$PartialPartialTensors:=Select[$Tensors,PartialPartialQ];
$GeneralizedVVF={};
$TotalDerivatives:=TotalDerivativeOfNormal/@$NormalsOfCovD;

$UseInverseMetric=False;
$printAddVariationalRelation=True;
$VerticalOperators={VertLie,VertInt,VertDiff};
$SortVertOperatorsOrder=$VerticalOperators;
$AddVariationalRelationDagger=True;
$RemoveVariationalRelation=True;
$GenerateExpandVertDiffRuleDagger=True;

xAct`xTensor`Private`DefSign[$SymplecticCurrentSign,"\!\(\*SubscriptBox[\(s\), \(symp\)]\)",1];
xAct`xTensor`Private`DefSign[$NoetherCurrentSign,"\!\(\*SubscriptBox[\(s\), \(Noet\)]\)",1];

$VariationalGraph=Graph[{Zero}, {}]; (* Added the Zero tensor because an empty graph gives problems that apparently are bugs of Mathematica *)


InstallInputAlias[name_String,alias_String]:=Module[{cur},
If[Head[$FrontEnd]===FrontEndObject,cur=CurrentValue[$FrontEnd,InputAliases];
	(
	If[!ListQ[cur],cur={}];
	cur=DeleteCases[cur,(name->_)];
	CurrentValue[$FrontEnd,InputAliases]=Append[cur,name->alias];
	),
	Null]
];

InstallInputAlias["ww", "\:2a55"];


(* ::Section:: *)
(*2.2. WWedge product*)


(* ::Subsection:: *)
(*2.2.1. Definition*)


(* Definition of WWedge as a DefProduct *)
DefProduct[WWedge,
	AssociativeProductQ->True,
	CommutativityOfProduct->"SuperCommutative",
	GradedProductQ->True,
	IdentityElementOfProduct->1,
	ScalarsOfProduct->(SameQ[Grade[#//ReleaseHold,WWedge],0]&), (* ReleaseHold prevents Grade[HoldForm[expr]] to be evaluated as zero removing the WWedge *) 
	PrintAs->$SymbolDoubleWedge, 
	ProtectNewSymbol:>$ProtectNewSymbols,
	DefInfo->Null
];

MakeExpression[RowBox[{x_,"\:2a55",y__}],StandardForm]:=MakeExpression[RowBox[{"WWedge","[",x,",",Sequence@@Riffle[DeleteCases[{y},"\:2a55"], ","],"]"}],StandardForm]
\:2a55[x__]:=WWedge[x]

(* Relation between Wedge and Times. *)
WWedge/:GradeOfProduct[Times,WWedge]=0;

(* This forces Leibniz for PD and LieD. This condition is later imposed on every newly defined covariant derivative *)
Unprotect[PD,LieD];
PD[a_][expr_WWedge]:=Sum[MapAt[PD[a][#]&,expr,i],{i,1,Length[expr]}]; 
LieD[a_][expr_WWedge]:=Sum[MapAt[LieD[a][#]&,expr,i],{i,1,Length[expr]}]; 
Protect[PD,LieD];

(* Behavior of the WWedge product with respect to Dagger *)
Unprotect@Dagger;
Dagger@expr_WWedge:=Dagger/@expr
Protect@Dagger;

Protect[WWedge];


(* ::Subsection:: *)
(*2.2.2. Vertical degree*)


(* This prevents to assign Zero to HoldForm[tensor] *)
VertDeg[x_HoldForm]:=HoldForm[VertDeg@@x]

(* It returns the VertDeg of a tensor (without indices) *)
VertDeg[delta]:=0
VertDeg[tensor_?xTensorQ]:=VertDeg[TensorWithIndices[tensor]]

(* It returns the VertDeg of a an expression with indices *)
VertDeg[expr_]:=Grade[expr,WWedge]

VertDeg[expr_?TotalDerivativeQ]:=VertDeg@@expr
VertDeg[HoldPattern[der_?CovDQ[ind_][Keep[expr_]]]]:=VertDeg[expr]


(* ::Subsection:: *)
(*2.2.3. ZeroVertDegQ and NonZeroVertDegQ*)


ZeroVertDegQ[x_HoldForm]:=HoldForm[ZeroVertDegQ@@x]
ZeroVertDegQ[expr_]:=(VertDeg[expr]==0)
NonZeroVertDegQ[expr_]:=!ZeroVertDegQ[expr]

Protect[ZeroVertDegQ,NonZeroVertDegQ];


(* ::Subsection:: *)
(*2.2.4. VertExactHeadQ*)


VertExactHeadQ[_]:=False; (* It will be set true for specific cases *)


(* ::Subsection:: *)
(*2.2.5. VertExactHeadQ*)


VertExactQ[tensor_?VertExactHeadQ[inds___]]/;SlotsOfTensor[tensor]===xAct`xTensor`Private`SignedVBundleOfIndex/@{inds}:=True; (* A tensor defined as VertExact with the right indices *)


(* ::Subsection:: *)
(*2.2.6. CTensors and xCoba (not fully tested, to be tested for future versions)*)


(* ::Input:: *)
(*(* In this section we implement the WWedge product of CTensor objects. This code has been supplied by Jos\[EAcute] Mart\[IAcute]n-Garc\[IAcute]a for the xTerior package. This part has not been tested and will be included with more care in the future. *)*)


(* ::Input::Initialization:: *)
Unprotect[WWedge];

(* Contracted wedge product of CTensor objects *)
WWedge[ctensor1_CTensor[left1___,a_,right1___],ctensor2_CTensor[left2___,-a_,right2___]]:=Module[{n1=Length[{left1,a}],n2=Length[{left2,-a}],res},res=xAct`xCoba`Private`CTensorContract[ctensor1,ctensor2,{n1,n2},WWedge];
res[left1,right1,left2,right2]/;FreeQ[res,$Failed]];

WWedge[ctensor1_CTensor[left1___,-a_,right1___],ctensor2_CTensor[left2___,a_,right2___]]:=Module[{n1=Length[{left1,a}],n2=Length[{left2,-a}],res},res=xAct`xCoba`Private`CTensorContract[ctensor1,ctensor2,{n1,n2},WWedge];
res[left1,right1,left2,right2]/;FreeQ[res,$Failed]];


(* ::Section:: *)
(*2.3. Modifications and extensions to important xAct functions*)


(* ::Subsection:: *)
(*2.3.1. Extend FindAllOfType*)


Unprotect[FindAllOfType];
FindAllOfType[expr_,VertDiffExact]:=Cases[expr//Evaluate,_?xTensorQ[___]?VertExactQ,{0,DirectedInfinity[1]},Heads->True]/.{-dltensor_?VertExactHeadQ[inds___]:>dltensor[inds]}
FindAllOfType[expr_,ZeroVertDeg]:=Cases[expr,_?xTensorQ[___]?ZeroVertDegQ,{0,DirectedInfinity[1]},Heads->True]
FindAllOfType[expr_,NonZeroVertDeg]:=Cases[expr,_?xTensorQ[___]?NonZeroVertDegQ,{0,DirectedInfinity[1]},Heads->True]
FindAllOfType[expr_,VariationalVector] :=Cases[expr, _?(xTensorQ[#]&&VariationalVectorQ[#]&)[___], {0, \[Infinity]}, Heads -> True]/.{-vv_?VariationalVectorQ[inds___]:>vv[inds]}
FindAllOfType[expr_,NormalOfCovD] :=Cases[expr, _?(xTensorQ[#]&&NormalOfCovDQ[#]&)[___], {0, \[Infinity]}, Heads -> True]
FindAllOfType[expr_,NormalOfPD] :=Cases[expr, _?(xTensorQ[#]&&NormalOfPDQ[#]&)[___], {0, \[Infinity]}, Heads -> True]
FindAllOfType[expr_,PartialPartial] :=Cases[expr, _?(xTensorQ[#]&&PartialPartialQ[#]&)[___], {0, \[Infinity]}, Heads -> True]/.{-pp_?PartialPartialQ[inds___]:>pp[inds]}
Protect[FindAllOfType];


(* ::Subsection:: *)
(*2.3.2. Modification to SeparateDir*)


(* We change Times for WWedge *)
xAct`xTensor`Private`separateDir[expr_,Dir[v_]]:=Module[
	{ultraindex=xAct`xTensor`Private`UltraindexOf[v],dummy},
	dummy=DummyAs[ultraindex];
	ReplaceIndex[v,ultraindex->dummy]~WWedge~xAct`xTensor`Private`changeDir[expr,-dummy]]


(* ::Subsection:: *)
(*2.3.3. Modifications to LieD and LieDToCovD*)


Unprotect[LieD];

(* LieD\.08[v][v] is not necessarily zero, only for Even VertDeg *)
Quiet[LieD[xAct`xTensor`Private`v_?xTensorQ[_Symbol]][(xAct`xTensor`Private`v_)[_Symbol]]=.];
LieD[v_?xTensorQ[_Symbol]][v_[_Symbol]]/;EvenQ[VertDeg[v]]:= 0;

(* We change Times for WWedge *)
xAct`xTensor`Private`lieDcovDdiff[expr_,vector_,covd_,vb_,tmpchrhead_]:=With[
	{
	indv=xAct`xTensor`Private`UltraindexOf[vector],
	vbQ=xAct`xTensor`Private`VBundleIndexQ[vb],
	inds=FindFreeIndices[expr],
	dummy=DummyIn[vb],
	dummy2=DummyIn[vb]
	},
	If[!CovDQ[covd],Throw[Message[LieD::unknown,"derivative",covd];ERROR[LieD[vector,covd][expr]]]];
	xAct`xTensor`Private`ValidateDir[Dir[vector]];
	If[covd=!=PD&&ManifoldOfCovD[covd]=!=BaseOfVBundle[vb],Throw[Message[LieD::error,"Invalid derivative in Lie Derivative."];ERROR[LieD[vector,covd][expr]]]];
	With[{
		  torsion=Torsion[covd],
		  newv=If[TorsionQ[covd],ReplaceIndex[vector,indv->dummy2],0]
		  },
		  Expand[Plus@@((tmpchrhead[covd[-#1][ReplaceIndex[vector,indv->dummy]]]
		  +xAct`xTensor`$TorsionSign newv torsion[dummy,-dummy2,-#1])~WWedge~ReplaceIndex[expr,-#1->-dummy]&)/@Select[ChangeIndex/@inds,vbQ]-Plus@@((tmpchrhead[covd[-dummy][ReplaceIndex[vector,indv->#1]]]
		  +xAct`xTensor`$TorsionSign newv torsion[#1,-dummy2,-dummy])~WWedge~ReplaceIndex[expr,#1->dummy]&)/@Select[inds,vbQ]+If[xAct`xTensor`Private`WeightedCovDQ[covd],With[{weight=WeightOf[expr,WeightedWithBasis[covd]]},If[weight=!=0,weight expr ~WWedge~ covd[-indv][vector],0]],0]]
		]
	]

Protect[LieD];


(* ::Subsection:: *)
(*2.3.4. Modification to Bracket and BracketToCovD*)


(* \.08[v,v] is not necessarily zero, only for Even VertDeg *)

Quiet[Bracket[v_,v_]=.];
Bracket[v_,v_]:=Zero/;EvenQ[VertDeg[v]]

(* We change Times for WWedge *)
Bracket[HoldPattern[WWedge[x___,s_?ScalarQ,y___]],v2_][a_]:=Module[{v1,rs=ReplaceDummies[s]},
	v1=WWedge[x,y]; 
	(-1)^(VertDeg[s] Total[VertDeg/@{x}])(
	rs~WWedge~ Bracket[v1,v2][a]-(-1)^(VertDeg[v2](VertDeg[rs]+VertDeg[v1])) PD[Dir[v2]][rs]~WWedge~ReplaceIndex[v1,UltraindexOf[v1]->a])
];

Bracket[w1_,HoldPattern[WWedge[x___,s_?ScalarQ,y___]]][a_]:=Module[{v1,v2=w1,rs=ReplaceDummies[s]},
	v1=WWedge[x,y];
	-(-1)^(VertDeg[v1](Total[VertDeg/@{x}]+VertDeg[s]+Total[VertDeg/@{y}]))(-1)^(Total[VertDeg/@{x}]VertDeg[s])
	(rs~WWedge~ Bracket[v1,v2][a]-(-1)^(VertDeg[v2](VertDeg[rs]+VertDeg[v1])) PD[Dir[v2]][rs]~WWedge~ReplaceIndex[v1,UltraindexOf[v1]->a])
];

(* We stablish an order that depends on the VertDeg *)
SubValues[Bracket]=DeleteCases[SubValues[Bracket],f_[Bracket[xAct`xTensor`Private`expr1_,xAct`xTensor`Private`expr2_][xAct`xTensor`Private`i_]]:>h_[-Bracket[xAct`xTensor`Private`expr2,xAct`xTensor`Private`expr1][xAct`xTensor`Private`i],OrderedQ[{xAct`xTensor`Private`expr2,xAct`xTensor`Private`expr1}]]/;h===Condition];

Bracket[expr1_,expr2_][b_]/;(!OrderedQ[{expr1,expr2}]):=-(-1)^(VertDeg[expr1]VertDeg[expr2])Bracket[expr2,expr1][b];

(* Protect[Bracket]; Bracket is not protected on xTensor *)


(* We change the Times for WWedge *)

BracketToCovD[expr_,covd_:PD]:=
expr/. Bracket[v1_,v2_][a_]:>With[
	{
	u1=xAct`xTensor`Private`UltraindexOf[v1],
	u2=xAct`xTensor`Private`UltraindexOf[v2]
	},
	With[
		{
		b=DummyAs[u1],c=DummyAs[u2],
		rv1=ReplaceDummies[v1],rv2=ReplaceDummies[v2]
		},
		ReplaceIndex[rv1,u1->b]~WWedge~covd[-b][ReplaceIndex[rv2,u2->a]]- covd[-b][ReplaceIndex[rv1,u1->a]]~WWedge~ReplaceIndex[rv2,u2->b]-$TorsionSign Torsion[covd][a,-b,-c] ReplaceIndex[rv1,u1->b]~WWedge~ ReplaceIndex[rv2,u2->c]
		]
	]


(* ::Subsection:: *)
(*2.3.5. Extension to Validate*)


Unprotect[xAct`xTensor`Private`UncatchedValidate];

Validate::invprod="Times is not a valid product for vertical forms, use WWedge instead.";

xAct`xTensor`Private`UncatchedValidate[Times[expr1_,expr2_]]:=(
	If[VertDeg[expr1]VertDeg[expr2]>0,Throw@Message[Validate::invprod]];
	FindIndices[expr1 expr2];
	xAct`xTensor`Private`UncatchedValidate/@Unevaluated[expr1 expr2])
	
Protect[xAct`xTensor`Private`UncatchedValidate];


(* ::Subsection:: *)
(*2.3.6. Modification to ContractMetric*)


xAct`xTensor`Private`differentexpressionsQ[expr1_WWedge,expr2_List]:=xAct`xTensor`Private`differentexpressionsQ[List@@expr1,expr2]

(* We change the Times for WWedge *)
(CM:xAct`xTensor`Private`ContractMetric1[{od_,aud_},{metric_,nv_}])[rest_. HoldPattern[WWedge[der_?FirstDerQ[expr1_],expr2__]]met:metric_[b_,c_]]:=
Module[{dm=der[met],result},
	If[(od||dm===0)&&xAct`xTensor`Private`differentexpressionsQ[result=CM[expr1 met],{expr1,met}],
	rest WWedge[CM[der[result]],expr2]-rest WWedge[CM[dm expr1],expr2],rest WWedge[CM[met der[expr1]],expr2]
	]]/;(MemberQ[FindFreeIndices[expr1],ChangeIndex[c]|ChangeIndex[b]]&&Head[expr1]=!=metric)

(CM:xAct`xTensor`Private`ContractMetric1[{od_,aud_},{metric_,nv_}])[rest_. HoldPattern[WWedge[expr2__,der_?FirstDerQ[expr1_]]]met:metric_[b_,c_]]:=
	Module[{dm=der[met],result},
	If[(od||dm===0)&&xAct`xTensor`Private`differentexpressionsQ[result=CM[expr1 met],{expr1,met}],
	rest WWedge[expr2,CM[der[result]]]-rest WWedge[expr2,CM[dm expr1]],rest WWedge[CM[expr2,met der[expr1]]]
	]]/;(MemberQ[FindFreeIndices[expr1],ChangeIndex[c]|ChangeIndex[b]]&&Head[expr1]=!=metric)


(* ::Subsection:: *)
(*2.3.7. Modification to ContractDir*)


(CM:xAct`xTensor`Private`ContractDir1[vector_,od_])[rest1_. HoldPattern[WWedge[rest2___,vector_[a_],rest3___,tensor_?xTensorQ[indsL___,b_,indsR___],rest4___]]]:=(-1)^(VertDeg[vector[a]]VertDeg[rest3]) CM[rest1 WWedge[rest2,rest3,tensor[indsL,Dir[vector[a]],indsR],rest4]]/;PairQ[a,b]
(CM:xAct`xTensor`Private`ContractDir1[vector_,od_])[rest1_. HoldPattern[WWedge[rest2___,tensor_?xTensorQ[indsL___,b_,indsR___],rest3___,vector_[a_],rest4___]]]:=(-1)^(VertDeg[vector[a]](VertDeg[rest3]+VertDeg[tensor])) CM[rest1 WWedge[rest2,tensor[indsL,Dir[vector[a]],indsR],rest3,rest4]]/;PairQ[a,b]
(CM:xAct`xTensor`Private`ContractDir1[vector_,od_])[rest1_. vector_[a_]HoldPattern[WWedge[rest2___,tensor_?xTensorQ[indsL___,b_,indsR___],rest3___]]]:=CM[rest1 WWedge[rest2,tensor[indsL,Dir[vector[a]],indsR],rest3]]/;PairQ[a,b]

(CM:xAct`xTensor`Private`ContractDir1[vector_,od_])[rest1_. HoldPattern[WWedge[rest2___,vector_[a_],rest3___, covd_?CovDQ[indsL___,b_,indsR___][expr1_],rest4___]]]:=(-1)^(VertDeg[vector[a]]VertDeg[rest3]) CM[rest1 WWedge[rest2,rest3,covd[indsL,Dir[vector[a]],indsR][expr1],rest4]]/;PairQ[a,b]
(CM:xAct`xTensor`Private`ContractDir1[vector_,od_])[rest1_. HoldPattern[WWedge[rest2___, covd_?CovDQ[indsL___,b_,indsR___][expr1_],rest3___,vector_[a_],rest4___]]]:=(-1)^(VertDeg[vector[a]](VertDeg[rest3]+VertDeg[covd[indsL,b,indsR][expr1]])) CM[rest1 WWedge[rest2,covd[indsL,Dir[vector[a]],indsR][expr1],rest3,rest4]]/;PairQ[a,b]
(CM:xAct`xTensor`Private`ContractDir1[vector_,od_])[rest1_. vector_[a_]HoldPattern[WWedge[rest2___,covd_?CovDQ[indsL___,b_,indsR___][expr1_],rest3___]]]:=CM[rest1 WWedge[rest2,covd[indsL,Dir[vector[a]],indsR][expr1],rest3]]/;PairQ[a,b]

(CM:xAct`xTensor`Private`ContractDir1[vector_,od_])[rest1_. HoldPattern[WWedge[rest2___,vector_[a_],rest3___, der_?FirstDerQ[expr_],rest4___]]]:=Module[{dv=der[vector[a]],result},If[(dv===0||od)&&xAct`xTensor`Private`differentexpressionsQ[result=CM[vector[a]~WWedge~expr ],{expr,vector[a]}],(-1)^(VertDeg[vector[a]]VertDeg[rest3]) (CM[rest1 WWedge[rest2,rest3,der[result],rest4]]-CM[rest1 WWedge[rest2,rest3,dv,expr,rest4]]),(-1)^(VertDeg[der[expr]]VertDeg[rest4]) CM[rest1 WWedge[rest2,vector[a],rest3,rest4]]~WWedge~ der[expr]]]/;IsIndexOf[expr,-a]&&Head[expr]=!=vector
(CM:xAct`xTensor`Private`ContractDir1[vector_,od_])[rest1_. HoldPattern[WWedge[rest2___,der_?FirstDerQ[expr_],rest3___,vector_[a_],rest4___]]]:=Module[{dv=der[vector[a]],result},If[(dv===0||od)&&xAct`xTensor`Private`differentexpressionsQ[result=CM[vector[a]~WWedge~expr ],{expr,vector[a]}],(-1)^(VertDeg[vector[a]](VertDeg[rest3]+VertDeg[der[expr]])) (CM[rest1 WWedge[rest2,der[result],rest3,rest4]]-CM[rest1 WWedge[rest2,dv,expr,rest3,rest4]]),(-1)^(VertDeg[der[expr]](VertDeg[rest3]+VertDeg[vector[a]]+VertDeg[rest4])) CM[rest1 WWedge[rest2,vector[a],rest3,rest4]]~WWedge~ der[expr]]]/;IsIndexOf[expr,-a]&&Head[expr]=!=vector


(* ::Subsection:: *)
(*2.3.8. Modification to SeparateMetric*)


SubValues[SeparateMetric]={HoldPattern[SeparateMetric[args___][HoldPattern[VertInt[arg1_][arg2_]]]]:>VertInt[arg1][SeparateMetric[args][arg2]]}~Join~(SubValues@SeparateMetric);


(* ::Subsection:: *)
(*2.3.9. Modification to WeightOf*)


Unprotect[WeightOf];

WeightOf[ih_?InertHeadQ[__]]=.
WeightOf[HoldPattern[VertInt[v_][expr_]]]:=WeightOf[v]+WeightOf[expr]
WeightOf[HoldPattern[VertLie[v_][expr_]]]:=WeightOf[v]+WeightOf[expr]
WeightOf[HoldPattern[VertDiff[expr_]]]:=WeightOf[expr]
WeightOf[ih_?InertHeadQ[__]]:=Throw[Message[WeightOf::error,"WeightOf is generically undefined on inert heads."]]

Protect[WeightOf];


(* ::Subsection:: *)
(*2.3.10. Modification to MakeDaggerSymbol*)


Unprotect[MakeDaggerSymbol];
MakeDaggerSymbol[symbol_Symbol]/;PartialPartialQ[symbol]:=NameOfDaggerPartialPartial[symbol]
Protect[MakeDaggerSymbol];


(* ::Subsection:: *)
(*2.3.11. Modifications to IndexCoefficient*)


Unprotect[IndexCoefficient];

Clear[IndexCoefficient,xAct`xTensor`Private`IndexCoefficient1,xAct`xTensor`Private`IndexCoefficient2];

(* New ones to handle constants *)
IndexCoefficient[0,form_]:=0
IndexCoefficient[expr_,number_?RealValuedNumericQ form_]/;number!=0 :=1/number IndexCoefficient[expr,form]

(* New one to expand the expresion *)
IndexCoefficient[expr_,coeff_]/;Expand[expr]=!=expr:=IndexCoefficient[Expand[expr],coeff]

(* New one to handle WWedge in the coefficient *)
IndexCoefficient[expr_,HoldPattern[WWedge[form1_,form2_]]]:=IndexCoefficient[Expand[IndexCoefficient[expr,form1]],form2]

(* Unchanged *)
IndexCoefficient[expr_,form1_ form2_]:=IndexCoefficient[Expand[IndexCoefficient[expr,form1]],form2]
IndexCoefficient[expr_,form_]:=xAct`xTensor`Private`average[xAct`xTensor`Private`SymmetryIndexCoefficient[ReplaceDummies[expr],xAct`xTensor`Private`SymmetryEquivalentsOf[form]]]
IndexCoefficient[_]:=Message[IndexCoefficient::argr,IndexCoefficient,2]
IndexCoefficient[_,_,xAct`xCore`Private`x__]:=Message[IndexCoefficient::argrx,IndexCoefficient,2+Length[{xAct`xCore`Private`x}],2]
IndexCoefficient[expr_,form_?xAct`xTensor`Private`NonIndexedScalarQ]:=Coefficient[expr,form]

(* Changed division by DivisionWWedge and List by splitFactors *)
xAct`xTensor`Private`IndexCoefficient1[expr_Plus,form_]:=(xAct`xTensor`Private`IndexCoefficient1[#1,form]&)/@expr
xAct`xTensor`Private`IndexCoefficient1[expr_,form_]:=
xAct`xTensor`Private`average[(xAct`xTensor`Private`IndexCoefficient2[DivisionWWedge[expr,#1,ReturnZeroOrError->Zero],#1,form]&)/@splitFactors@expr]

(* Changed Times by WWedge *)
xAct`xTensor`Private`IndexCoefficient2[rest_,expr_,-form_]:=-xAct`xTensor`Private`IndexCoefficient2[rest,expr,form]
xAct`xTensor`Private`IndexCoefficient2[rest_,expr_,form_]:=WWedge[rest ,xAct`xTensor`Private`IndexCoefficient3[FindIndices[expr],FindIndices[form]]]/;xAct`xTensor`Private`SameExpressionsQ[expr,form]
xAct`xTensor`Private`IndexCoefficient2[rest_,expr_,form_]:=0

Protect[IndexCoefficient];


(* ::Subsection:: *)
(*2.3.12. Modifications to Explode*)


Unprotect[Explode];
Explode[expr_,tensor_?xTensorQ]=.;
Explode[expr_,tensor_?xTensorQ]:=expr/.{tensor?VertExactHeadQ[inds___]:>VertDiff[xAct`xTensor`Private`ExplodeTensor[MasterOfCPSTensor[tensor][inds]]],tensor?VariationalVectorQ[inds___]:>tensor[inds],tensor[inds___]:>xAct`xTensor`Private`ExplodeTensor[tensor[inds]]};
Protect[Explode];


(* ::Subsection:: *)
(*2.3.13. Modifications to Implode*)


Unprotect[Implode];
Implode[HoldPattern[OverDot[tensor_?VertExactHeadQ[inds___]]]]:=VertDiff[Implode[OverDot[MasterOfCPSTensor[tensor][inds]]]]
Implode[HoldPattern[ParamD[param__][tensor_?VertExactHeadQ[inds___]]]]:=VertDiff[Implode[ParamD[param][MasterOfCPSTensor[tensor][inds]]]]
Protect[Implode];


(* ::Subsection:: *)
(*2.3.14. Modifications to BreakChristoffel (makeChristoffelRule)*)


ChristoffelAUX[PD]:=Zero
AChristoffelAUX[PD]:=Zero

ChristoffelAUX[PD,PD]:=Zero
AChristoffelAUX[PD,PD]:=Zero

ChristoffelAUX[covd_?CovDQ,covd_?CovDQ]:=Zero
AChristoffelAUX[covd_?CovDQ,covd_?CovDQ]:=Zero


Unprotect@xAct`xTensor`Private`makeChristoffelRule;

xAct`xTensor`Private`makeChristoffelRule[PD][{chrname_,{_,covd1_,covd2_},_}]:=
	Sequence[
		chrname[inds__]:>xAct`xTensor`Private`breakChristoffel[covd1,covd2,PD][inds], (* Same definition *)
		VertDiff[chrname][inds__]:>breakdlChristoffel[covd1,covd2,PD][inds] (* New definition *)
	]

xAct`xTensor`Private`makeChristoffelRule[covd_][{chrname_,{_,covd1_,covd2_},_}]:=
	If[xAct`xTensor`Private`CompatibleCovDsQ[covd,covd1]&&xAct`xTensor`Private`CompatibleCovDsQ[covd,covd2],
	Sequence@@{
	chrname[inds__]:>xAct`xTensor`Private`breakChristoffel[covd1,covd2,covd][inds], (* Same definition *)
	VertDiff[chrname][inds__]:>breakdlChristoffel[covd1,covd2,covd][inds] (* New definition *)
	},
	{}]


breakdlChristoffel[covd1_,covd2_,covd_][a_,b_,c_]:=If[Length@DeleteDuplicates[VBundleOfIndex/@{a,b,c}]===1,
	VertDiff[ChristoffelAUX[covd1,covd]][a,b,c]-VertDiff[ChristoffelAUX[covd2,covd]][a,b,c],(* New definition *)
	VertDiff[AChristoffelAUX[covd1,covd]][a,b,c]-VertDiff[AChristoffelAUX[covd2,covd]][a,b,c](* New definition *)
	]


(* ::Subsection:: *)
(*2.3.15. Modifications to ChangeTorsion*)


xAct`xTensor`Private`makeChangeTorsionRule[covd2_][covd1_]:=
If[TorsionQ[covd1]&&xAct`xTensor`Private`CompatibleCovDsQ[covd1,covd2],
	{
	Torsion[covd1][inds__]:>xAct`xTensor`Private`changeTorsion[covd1,covd2][inds], (* Same definition *)
	VertDiff[Torsion[covd1]][inds2__]:>changedlTorsion[covd1,covd2][inds2] (* New definition *)
	},
	{}
	]


changedlTorsion[covd1_,covd2_][a_,b_,c_]:=With[{chr=HeadOfTensor[Christoffel[covd1,covd2][a,b,c],{a,b,c}]},VertDiff[Torsion[covd2]][a,b,c]+xAct`xTensor`$TorsionSign(VertDiff[chr][a,b,c]-VertDiff[chr][a,c,b])]


(* ::Subsection:: *)
(*2.3.16. Modifications to ChangeCurvature*)


(* ::Subsubsection:: *)
(*2.3.16.1. Riemann*)


(* With no need of a metric. riemann can be either Riemann or FRiemann. We have to be careful since dlRiemann[-a,-b,-c,-d]=!=VertDiff[Riemann[-a,-b,-c,-d]] *)
changedlRiemann[covd1_,covd2_,{_,_,_},riemann_][-c_Symbol,-d_Symbol,-b_Symbol,a_Symbol]/;Length[SymmetryGroupOfTensor[Riemann@covd1][[2]]]===1:=
Module[
	{
	chr=HeadOfTensor[Christoffel[covd1,covd2][a,-c,-b],{a,-c,-b}],dlchr,
	chr2=HeadOfTensor[Christoffel[covd2][a,-c,-b],{a,-c,-b}],dlchr2,
	e=DummyAs[a]
	},
	dlchr=VertDiff@chr;
	dlchr2=VertDiff@chr2;
	VertDiff[riemann[covd2][-c,-d,-b,a]]+$RiemannSign(-covd2[-c][dlchr[a,-d,-b]]+covd2[-d][dlchr[a,-c,-b]]-chr[e,-d,-b]dlchr[a,-c,-e]+chr[e,-c,-b]dlchr[a,-d,-e]-chr[a,-c,-e]dlchr[e,-d,-b]+chr[a,-d,-e]dlchr[e,-c,-b]-chr[e,-d,-b]dlchr2[a,-c,-e]+chr[e,-c,-b]dlchr2[a,-d,-e]-chr[a,-c,-e]dlchr2[e,-d,-b]+chr[a,-d,-e]dlchr2[e,-c,-b]+If[riemann===Riemann,-$TorsionSign Torsion[covd2][e,-c,-d]dlchr[a,-e,-b],With[{f=DummyAs[c]},(VertDiff[ChristoffelAUX[covd2]][f,-c,-d]-VertDiff[ChristoffelAUX[covd2]][f,-d,-c])chr[a,-f,-b]]])
]

changedlRiemann[covd1_,covd2_,{_,_,_},riemann_][1,inds__]:=
Module[{chr=ChristoffelAUX[covd1,covd2],dlchr,chr2=ChristoffelAUX[covd2],dlchr2}, (* No FRiemann possible *)
	dlchr=VertDiff@chr;
	dlchr2=VertDiff@chr2;
	VertDiff[ChangeCurvature[Riemann[covd1][inds],covd1,covd2]]//ExpandVertDiff[HoldExpandVertDiff->{Riemann[covd2],ChristoffelAUX[covd1,covd2],ChristoffelAUX[covd2]}]
]

(* All other cases *)
changedlRiemann[covd1_,covd2_,{tmetric_,vmetric_,vdagmetric_},riemann_][c_,d_,b_,a_]:=With[
	{
	vbundle=VBundleOfIndex[a],
	tbundle=VBundleOfIndex[c],
	vdmetric=If[riemann===FRiemann&&HasDaggerCharacterQ[riemann],vdagmetric,If[riemann===FRiemann,vmetric,tmetric]]
	},
	Module[{c1=DummyIn[tbundle],d1=DummyIn[tbundle],b1=DummyIn[vbundle],a1=DummyIn[vbundle]},
		If[Length[SymmetryGroupOfTensor[Riemann@covd1][[2]]]===1,
			tmetric[c,c1]tmetric[d,d1]vdmetric[b,b1]vdmetric[a,a1]changedlRiemann[covd1,covd2,{delta,delta,delta},riemann][-c1,-d1,-b1,a1],
			tmetric[c,c1]tmetric[d,d1]vdmetric[b,b1]vdmetric[a,a1]changedlRiemann[covd1,covd2,{delta,delta,delta},riemann][1,-c1,-d1,-b1,-a1]
		]
	]
]


xAct`xTensor`Private`makeChangeRiemannRule[covd2_,{tmetric_,vmetric_,vdagmetric_},riemann_][covd1_]:=With[
	{
	riemann1=riemann@covd1,
	dlriemann1=VertDiff@riemann@covd1
	},
	{
	HoldPattern[riemann1[c_,d_,b_,a_]]:>xAct`xTensor`Private`changeRiemann[covd1,covd2,{tmetric,vmetric,vdagmetric},riemann][c,d,b,a],
	HoldPattern[dlriemann1[c_,d_,b_,a_]]:>changedlRiemann[covd1,covd2,{tmetric,vmetric,vdagmetric},riemann][c,d,b,a]
	}
]


(* ::Subsubsection:: *)
(*2.3.16.2. Ricci*)


changedlRicci[covd1_,covd2_,_][-c_Symbol,-b_Symbol]:=
Module[
	{
	a=DummyAs[c],e=DummyAs[b],
	chr,dlchr,
	chr2,dlchr2
	},
	chr=HeadOfTensor[Christoffel[covd1,covd2][a,-c,-b],{a,-c,-b}];
	chr2=HeadOfTensor[Christoffel[covd2][a,-c,-b],{a,-c,-b}];
	dlchr=VertDiff@chr; dlchr2=VertDiff@chr2;
	VertDiff[Ricci[covd2][-c,-b]]+$RiemannSign $RicciSign(-covd2[-c][dlchr[a,-a,-b]]+covd2[-a][dlchr[a,-c,-b]]-chr[e,-a,-b]dlchr[a,-c,-e]+chr[e,-c,-b]dlchr[a,-a,-e]-chr[a,-c,-e]dlchr[e,-a,-b]+chr[a,-a,-e]dlchr[e,-c,-b]-chr[e,-a,-b]dlchr2[a,-c,-e]+chr[e,-c,-b]dlchr2[a,-a,-e]-chr[a,-c,-e]dlchr2[e,-a,-b]+chr[a,-a,-e]dlchr2[e,-c,-b]-$TorsionSign Torsion[covd2][e,-c,-a]dlchr[a,-e,-b])
]

changedlRicci[covd1_,covd2_,tmetric_][c_,b_]:=With[{vbundle=VBundleOfIndex[c]},Module[{c1=DummyIn[vbundle],b1=DummyIn[vbundle]},
tmetric[c,c1]tmetric[b,b1]changedlRicci[covd1,covd2,HELLO][-c1,-b1]]]


xAct`xTensor`Private`makeChangeRicciRule[covd2_,metric_][covd1_]:=With[{ricci=Ricci[covd1],dlricci=VertDiff@Ricci@covd1},
	{
	HoldPattern[ricci[inds__]]:>xAct`xTensor`Private`changeRicci[covd1,covd2,metric][inds],
	HoldPattern[dlricci[inds__]]:>changedlRicci[covd1,covd2,metric][inds]
	}
	];


(* ::Subsubsection:: *)
(*2.3.16.3. RicciScalar*)


changedlRicciScalar[covd1_,covd2_,metricofcovd1_][]:=Module[
	{
	a=DummyIn[xAct`xTensor`Private`TangentBundleOfCovD[covd1]],
	b=DummyIn[xAct`xTensor`Private`TangentBundleOfCovD[covd1]]
	},
	Scalar[VertDiff[ToExpression["Inv"<>ToString[metricofcovd1]]][a,b]xAct`xTensor`Private`changeRicci[covd1,covd2,HELLO][-a,-b]]+Scalar[Inv[metricofcovd1][a,b]changedlRicci[covd1,covd2,HELLO][-a,-b]]
];


xAct`xTensor`Private`makeChangeRicciScalarRule[covd2_,metricofcovd1_][covd1_]:=With[{ricciscalar=RicciScalar[covd1],dlricciscalar=VertDiff@RicciScalar@covd1},
	{
	HoldPattern[ricciscalar[]]:>xAct`xTensor`Private`changeRicciScalar[covd1,covd2,metricofcovd1][],
	HoldPattern[dlricciscalar[]]:>changedlRicciScalar[covd1,covd2,metricofcovd1][]
	}
];


(* ::Subsection:: *)
(*2.3.17. Modifications to Keep (ToCanonical and FindIndices)*)


(* TODO: Is this a good idea? It allows to handle Keep betten within xAct *)
Unprotect[ToCanonical,Keep,FindIndices,SymmetryOf];

FindIndices[Keep[expr_]]:=FindIndices[expr]
ToCanonical[HoldPattern[Keep[expr_]]]:=Keep[ToCanonical[expr]]
ToCanonical[HoldPattern[der_?CovDQ[ind_][expr_Keep]]]:=der[ind][Keep[ToCanonical[expr]]]
SymmetryOf[HoldPattern[Keep[expr_]]]:=SymmetryOf[expr]
Keep[0]:=0

Protect[ToCanonical,Keep,FindIndices,SymmetryOf];


(* ::Subsection:: *)
(*2.3.18. Modification to Grade (for InertHeads)*)


Unprotect[Grade];

(* Grade[xAct`xTensor`Private`TMPChristoffel[expr]] gives an error *)
Grade[xAct`xTensor`Private`ih:_?InertHeadQ[__],xAct`xTensor`Private`prod_?GradedProductQ]=.
Grade[xAct`xTensor`Private`expr_,xAct`xTensor`Private`prod_?ProductQ]=.

Grade/:Grade[HoldPattern[xAct`xTensor`Private`TMPChristoffel[expr_]],WWedge]:=VertDeg[expr] 

Grade[xAct`xTensor`Private`ih:_?InertHeadQ[__],xAct`xTensor`Private`prod_?GradedProductQ]:=Throw[Message[Grade::unknown,"grade of inert-head expression",xAct`xTensor`Private`ih]]
Grade[xAct`xTensor`Private`expr_,xAct`xTensor`Private`prod_?ProductQ]:=0

Protect \.08[Grade];


(* ::Section:: *)
(*2.4. Vertical operators*)


(* ::Subsection:: *)
(*2.4.1. Graded derivative*)


GradeOfDer[head_[v_,rest___],WWedge]:=GradeOfDer[head,WWedge]+VertDeg[v];
GradeOfDer[head_[v_,rest___],prod_]:=GradeOfDer[head,prod]+Grade[v,prod];


(* DefGradedDer and MakeDerivation are based on xTerior *)

Options[DefGradedDer]={PrintAs->Identity};

DefGradedDer[der_,prod_?ProductQ,dergrade_:0,options:OptionsPattern[]]:=With[{head=SubHead[der]},
	Module[{pa},

		{pa}=OptionValue[{PrintAs}]; (* The brackets are important *)
 
		(* DefInertHead will take care of scalar-homogeneity and linearity *)
		DefInertHead[der,LinearQ->True,ContractThrough->{delta},PrintAs->pa,DefInfo->Null];
		
		(* Nonatomic derivation *) 
		If[der=!=head,
			head[0][__]:=0;
			head[v_Plus][args__]:=head[#][args]&/@v;
		
			(* Subscript vector argument for formatting *)
			If[pa===Identity,pa=PrintAs[head]];
			head/:MakeBoxes[head[v_][form_],StandardForm]:=xAct`xTensor`Private`interpretbox[head[v][form],RowBox[{SubscriptBox[pa,MakeBoxes[v,StandardForm]],"[",MakeBoxes[form,StandardForm],"]"}]];
		];
		
		(* Other properties of a derivation *)
		MakeDerivation[head,der,NoPattern[der],prod,dergrade];
	]
];



(*This part is separated in order to avoid renaming confusion between derL and derR:*)
MakeDerivation[head_,derL_,derR_,prod_,dergrade_]:=(

	(* Addition of grades in algebra *)
	head/:GradeOfDer[head,prod]:=dergrade;
	head/:Grade[derL[expr_,___],prod]:=Grade[expr,prod]+GradeOfDer[derR,prod];
	
	(* The (graded) Leibniz rule *)
	derL[expr_prod,rest___]:=With[
		{sumgrades=FoldList[Plus,0,Grade[#,WWedge]&/@List@@expr]},
		Sum[(-1)^(GradeOfDer[derR,prod] * sumgrades[[i]] )MapAt[derR[#,rest]&,expr,i],{i,1,Length[expr]}]
		];
	
	(* Chain rule *)
	derL[func_?ScalarFunctionButNotDefinedQ[args__],rest___]:=xAct`xTensor`Private`multiD[derR[#,rest]&,func[args]]; (* This works for functions like Sqrt[x] and others but not with DefScalarFunctions, that is handled better with ExpandVertDiff below *)
	
	(* Dependencies *)
	If[!AtomQ[derR],head/:DependenciesOfInertHead[derL]:=DependenciesOf[First[derR]]];
)


(* ::Subsection:: *)
(*2.4.2. Definition vertical exterior derivative (VertDiff)*)


DefGradedDer[VertDiff,WWedge,+1,PrintAs->$SymbolVerticalExteriorDerivative];
VertDeg[VertDiff]:=1;
SetNumberOfArguments[VertDiff,1]

VertDiff/:MakeBoxes[VertDiff[form_],StandardForm]:=xAct`xTensor`Private`interpretbox[VertDiff[form],RowBox[{PrintAs[VertDiff],"[",MakeBoxes[form,StandardForm],"]"}]]
VertDiff[expr_?ArrayQ]:=VertDiff[#]&/@expr
VertDiff[expr_Equal]:=VertDiff[#]&/@expr

(* \[DifferentialD]^2=0 *)
VertDiff[expr_VertDiff]:=0;
VertDiff[tensor_[inds___]]/;VertExactQ[tensor[inds]]:=0;
VertDiff[tensor_?VertExactHeadQ]:=Zero;

(* \[DifferentialD](constant)=0 *)
VertDiff[x_?ConstantQ]:=0;
VertDiff[Zero]:=Zero;

(* Dagger@\[DifferentialD]=\[DifferentialD]@Dagger *)
VertDiff/:HoldPattern[Dagger[VertDiff[expr_]]]:=VertDiff[Dagger[expr]];

(* \[DifferentialD] commutes with PD *)
VertDiff[PD[a_][expr_]]:=PD[a]@VertDiff[expr] 

(* dl commutes with MultiplyHead *)
VertDiff[MultiplyHead[integer_,tensor_]]:=MultiplyHead[integer,VertDiff[tensor]]

(* delta does not depend on any field *)
VertDiff[delta[-a_,b_]]:=0 
VertDiff[Gdelta[inds___]]:=0/;With[{allInds={inds}},EvenQ[Length[allInds]]&&AllTrue[Take[allInds,Length[allInds]/2],DownIndexQ]&&AllTrue[Take[allInds,-Length[allInds]/2],UpIndexQ]]
VertDiff[delta][inds__]:=VertDiff[delta[inds]]
VertDiff[Gdelta][inds__]:=VertDiff[Gdelta[inds]]

(* This produces expanded expressions and is much faster when there are many scalars *)
VertDiff[expr_Times]:=Module[{grades=Grade[#,WWedge]&/@List@@expr,pos,scalar,form},
	pos=Position[grades,_?Positive,1,Heads->False];
	Which[
		Length[pos]>1,
			Throw[Message[VertDiff::error1,"Found Times product of forms with VertDeg>0. Use WWedge instead.",expr]],
		Length[pos]===1,
			pos=pos[[1,1]];
			scalar=Delete[expr,{pos}];
			form=expr[[pos]];
			scalar VertDiff[form]+VertDiff0[scalar,form],
		Length[pos]===0,
			VertDiff0[expr]
	]
];

(* Only scalars *)
VertDiff0[scalar_Times]:=Sum[MapAt[VertDiff[#]&,scalar,i],{i,1,Length[scalar]}];
VertDiff0[scalar_]:=VertDiff[scalar];

(* Scalars and a form *)
VertDiff0[scalar_Times,form_]:=Sum[MapAt[VertDiff0[#,form]&,scalar,i],{i,1,Length[scalar]}];
VertDiff0[scalar_,form_]:=WWedge[VertDiff[scalar],form];

Protect[VertDiff];


(* ::Subsection:: *)
(*2.4.3. Definition vertical Interior derivative (VertInt)*)


DefGradedDer[VertInt[v_],WWedge,-1,PrintAs->$SymbolVertInt];

VertDeg[VertInt]:=-1
VertDeg[VertInt[v_][expr_]]:=VertDeg[expr]-1+VertDeg[v]
VertDeg[VertInt[v_]]:=-1+VertDeg[v]

VertInt[v_][expr_?ArrayQ]:=VertInt[v][#]&/@expr
VertInt[v_][expr_Equal]:=VertInt[v][#]&/@expr
VertInt[integer_?IntegerQ v_][expr_]:=integer VertInt[v][expr]

(* VertInt[vvf] vanishes over zero-VertDeg elements *)
VertInt[vvf_][tensor_?ZeroVertDegQ] := 0

(* VertInt[vvf1]@VertInt[vvf2] *)
HoldPattern[VertInt[vvf_][VertInt[vvf_][_]]]/;OddQ[VertDeg[VertInt[vvf]]]:=0

(* This produces expanded expressions and is much faster when there are many scalars *)
VertInt[v_][expr_Times]:=Module[{grades=Grade[#,WWedge]&/@List@@expr,pos,scalar,form},
	pos=Position[grades,_?(#=!=0&),1,Heads->False];
	Which[
		Length[pos]>1,
			Throw[Message[VertInt::error1,"Found Times product of forms with VertDeg>0. Use WWedge instead.",expr]],
		Length[pos]===1,
			pos=pos[[1,1]];
			scalar=Delete[expr,{pos}];
			form=expr[[pos]];
			scalar VertInt[v][form]+VertInt0[v][scalar,form],
		Length[pos]===0,
			VertInt0[v][expr]
	]
];

(* Only scalars *)
VertInt0[v_][expr_Times]:=Sum[MapAt[VertInt[v],expr,i],{i,1,Length[expr]}];
VertInt0[v_][expr_]:= VertInt[v][expr];

(* Scalars and a form *)
VertInt0[v_][expr_Times,form_]:=Sum[MapAt[VertInt0[v][#,form]&,expr,i],{i,1,Length[expr]}];
VertInt0[v_][expr_,form_]:=WWedge[VertInt[v][expr],form];

(* This makes sure that the indices in the argument are alwasys screened *)
VertInt[vvf_ /; !TrueQ[vvf === (vvf // ScreenDollarIndices)]][expr_] := VertInt[vvf][expr// ScreenDollarIndices]
VertInt[vvf_ /; !TrueQ[vvf === (vvf // ScreenDollarIndices)]] := VertInt[vvf// ScreenDollarIndices]

Protect[VertInt];


(* ::Subsection:: *)
(*2.4.4. Definition vertical Lie derivative (VertLie)*)


DefGradedDer[VertLie[v_],WWedge,0,PrintAs->$SymbolVertLie];

VertDeg[VertLie[v_][expr_]]:=VertDeg[expr]+VertDeg[v];
VertDeg[VertLie[v_]]:=VertDeg[v];

(* This produces expanded expressions and is much faster when there are many scalars *)
VertLie[v_][expr_Times]:=Module[{grades=Grade[#,WWedge]&/@List@@expr,pos,scalar,form},
	pos=Position[grades,_?(#=!=0&),1,Heads->False];
	Which[
		Length[pos]>1,
			Throw[Message[VertLie::error1,"Found Times product of forms with VertDeg>0. Use WWedge instead.",expr]],
		Length[pos]===1,
			pos=pos[[1,1]];
			scalar=Delete[expr,{pos}];
			form=expr[[pos]];
			scalar VertLie[v][form]+VertLie0[v][scalar,form],
		Length[pos]===0,
			VertLie0[v][expr]
	]
];

(* Only scalars *)
VertLie0[v_][expr_Times]:=Sum[MapAt[VertLie[v],expr,i],{i,1,Length[expr]}];
VertLie0[v_][expr_]:= VertLie[v][expr];

(* Scalars and a form *)
VertLie0[v_][expr_Times,form_]:=Sum[MapAt[VertLie0[v][#,form]&,expr,i],{i,1,Length[expr]}];
VertLie0[v_][expr_,form_]:=WWedge[VertLie[v][expr],form];

VertLie[v_][expr_?ConstantQ]:=0;

(* delta does not depend on any field *)
VertLie[v_][delta[-a_,b_]]:=0 
VertLie[v_][Gdelta[inds___]]:=0/;With[{allInds={inds}},EvenQ[Length[allInds]]&&AllTrue[Take[allInds,Length[allInds]/2],DownIndexQ]&&AllTrue[Take[allInds,-Length[allInds]/2],UpIndexQ]]
VertLie[c_?ConstantQ vvf1_][expr_]:=c VertLie[vvf1][expr]


(* ::Subsection:: *)
(*2.4.5. Definition certical Lie bracket (VertBracket)*)


VertDeg[VertBracket[vvf1_,vvf2_]]:=VertDeg[vvf1]+VertDeg[vvf2]

VertBracket[vvf1_,vvf2_]/;(!FreeQ[vvf1,_?xTensorQ]&&!VVFQ[vvf1]&&!GeneralizedVVFQ[vvf1])||(!FreeQ[vvf2,_?xTensorQ]&&!VVFQ[vvf2]&&!GeneralizedVVFQ[vvf2]):=0;
VertBracket[vvf1_Plus,vvf2_]:=VertBracket[#,vvf2]&/@vvf1
VertBracket[vvf1_,vvf2_Plus]:=VertBracket[vvf1,#]&/@vvf2
VertBracket[vvf1_SeriesData,vvf2_]:=SeriesDataMap[VertBracket[#,vvf2]&,vvf1]
VertBracket[vvf1_,vvf2_SeriesData]:=SeriesDataMap[VertBracket[vvf1,#]&,vvf2];
VertBracket[c_?ConstantQ vvf1_,vvf2_]:=c VertBracket[vvf1,vvf2];
VertBracket[vvf1_,c_?ConstantQ vvf2_]:=c VertBracket[vvf1,vvf2];

VertBracket[vvf2_,vvf1_]:=-(-1)^(VertDeg[vvf2]VertDeg[vvf1])VertBracket[vvf1,vvf2]/;!OrderedQ[{vvf2,vvf1}]
VertBracket[vvf_,vvf_]/;EvenQ[VertDeg[vvf]]:=0

VVFQ[HoldPattern[VertBracket[vvf1_?VVFQ,vvf2_?VVFQ]]]:=True;
VertBracket/:MakeBoxes[VertBracket[vvf1_,vvf2_],StandardForm]:=RowBox[{"\[LeftDoubleBracket]",xAct`xTensor`Private`boxof@MakeBoxes[vvf1,StandardForm],",",xAct`xTensor`Private`boxof@MakeBoxes[vvf2,StandardForm],"\[RightDoubleBracket]"}];
PrintAs[VertBracket[vvf1__,vvf2__]]^:=Block[{$WarningFrom="Bracket Formatting"},RowBox[{"\[LeftDoubleBracket]",xAct`xTensor`Private`boxof@MakeBoxes[vvf1,StandardForm],",",xAct`xTensor`Private`boxof@MakeBoxes[vvf2,StandardForm],"\[RightDoubleBracket]"}]];


(* ::Subsection:: *)
(*2.4.6. Vertical operators and CTensors (not fully tested, to be tested for future versions)*)


Unprotect[VertDiff,VertInt,VertLie];

VertDiff[CTensor[array_,bases_List,weight_][inds__]]:=CTensor[VertDiff[array],bases,weight][inds];
VertInt[v_][CTensor[array_,bases_List,weight_][inds__]]:=CTensor[VertInt[v][array],bases,weight][inds];
VertLie[v_][CTensor[array_,bases_List,weight_][inds__]]:=CTensor[VertLie[v][array],bases,weight][inds];

Protect[VertDiff,VertInt,VertLie,VertBracket];


(* ::Subsection:: *)
(*2.4.7. Relations between vertical operators*)


(*Cartan identity:*) (* Careful: Not always true. As far as I know, there is no way to check within xAct *)
VertCartanMagicFormula[expr_]:=expr/.{HoldPattern[VertLie[vvf_][expr1_]]/;Length@FindAllOfType[expr1,VariationalVector]==0:>VertDiff@VertInt[vvf]@expr1-(-1)^(VertDeg[VertDiff]VertDeg[VertInt[vvf]]) VertInt[vvf]@VertDiff@expr1}


(* I am unsure of this kind of notation, but I think that this is what people mean *)

VertBracketToVertLie[expr_]:= expr//.
{VertBracket[vvf1_?VVFQ,vvf2_?VVFQ]/;FreeQ[vvf1,Plus]&&FreeQ[vvf2,Plus]:>
With[{info1=CoefficientsOfVVF[ReplaceDummies[vvf1]][[1]],info2=CoefficientsOfVVF[ReplaceDummies[vvf2]][[1]]},
VertLie[vvf1][info2[[2]]]~WWedge~info2[[1]]-(-1)^(VertDeg@vvf1 VertDeg@vvf2) VertLie[vvf2][info1[[2]]]~WWedge~info1[[1]]]}


(* ::Input::Initialization:: *)
(* Based on the xTerior package *)
(* Careful: These rules are not always true. As far as I know, there is no way to check it within xAct *)

SortVertOperatorsRule[VertDiff,VertDiff]={};
SortVertOperatorsRule[VertInt,VertInt]={HoldPattern[VertInt[vvf2_]@VertInt[vvf1_]@expr_]/;!OrderedQ[{vvf2,vvf1}]:>(-1)^(VertDeg[VertInt[vvf2]]VertDeg[VertInt[vvf1]]) VertInt[vvf1]@VertInt[vvf2]@expr};

SortVertOperatorsRule[VertLie,VertLie]={HoldPattern[VertLie[vvf2_]@VertLie[vvf1_]@expr_]/;!OrderedQ[{vvf2,vvf1}]:>VertLie[vvf1]@VertLie[vvf2]@expr-VertLie[VertBracket[vvf1,vvf2]]@expr};

SortVertOperatorsRule[VertInt,VertDiff]={HoldPattern[VertDiff@VertInt[vvf_]@expr_]:>VertLie[vvf]@expr+(-1)^(VertDeg[VertDiff]VertDeg[VertInt[vvf]]) VertInt[vvf]@VertDiff[expr]};
SortVertOperatorsRule[VertDiff,VertInt]={
HoldPattern[VertInt[vvf_]@VertDiff[expr_]]:>-(-1)^(VertDeg[VertDiff]VertDeg[VertInt[vvf]])(VertLie[vvf]@expr-VertDiff@VertInt[vvf]@expr),
HoldPattern[VertInt[vvf_][dltensor_?VertExactHeadQ[inds___]]]:>-(-1)^(VertDeg[VertDiff]VertDeg[VertInt[vvf]])(VertLie[vvf][MasterOfCPSTensor[dltensor][inds]]-VertDiff[VertInt[vvf][MasterOfCPSTensor[dltensor][inds]]])};

SortVertOperatorsRule[VertLie,VertDiff]={HoldPattern[VertDiff@VertLie[vvf_]@expr_]:>(-1)^(VertDeg[VertDiff]VertDeg[VertLie[vvf]]) VertLie[vvf]@VertDiff@expr};
SortVertOperatorsRule[VertDiff,VertLie]={
HoldPattern[VertLie[v_]@VertDiff[expr_]]:>(-1)^(VertDeg[VertDiff]VertDeg[VertLie[vvf]]) VertDiff@VertLie[v]@expr,
HoldPattern[VertLie[vvf_][dltensor_?VertExactHeadQ[inds___]]]:>(-1)^(VertDeg[VertDiff]VertDeg[VertLie[vvf]]) VertDiff[VertLie[vvf][MasterOfCPSTensor[dltensor][inds]]]};

SortVertOperatorsRule[VertInt,VertLie]={HoldPattern[VertLie[vvf1_]@VertInt[vvf2_]@expr_]:>VertInt[vvf2]@VertLie[vvf1]@expr+VertInt[VertBracket[vvf1,vvf2]]@expr};
SortVertOperatorsRule[VertLie,VertInt]={HoldPattern[VertInt[vvf1_]@VertLie[vvf2_]@expr_]:>VertLie[vvf2]@VertInt[vvf1]@expr+VertInt[VertBracket[vvf1,vvf2]]@expr};


(* ::Input::Initialization:: *)
SortVertOperators[][expr_]:=SortVertOperators[$SortVertOperatorsOrder][expr]
SortVertOperators[orderlist_List][expr_]:=Module[{order=Join[orderlist,Complement[$VerticalOperators,orderlist]]},

(* Make sure that order is some permutation of $VerticalOperators *)
If[Sort@order=!=Sort@$VerticalOperators,Throw@Message[SortVertOperators::invalid,"order",order];];

expr//.Join@@Table[Join@@(SortVertOperatorsRule[order[[i]],#]&/@Drop[order,i]),{i,1,Length@order-1}]//.Join@@(SortVertOperatorsRule[#,#]&/@order)
];

Protect[SortVertOperators,VertBracketToVertLie,VertCartanMagicFormula]; 


(* ::Section:: *)
(*2.5. Auxiliary functions*)


(* ::Subsection:: *)
(*2.5.1. SetSigns*)


symbolNames=Replace[First@OwnValues[$ConstantSymbols],HoldPattern[_:>list_]:>Map[HoldForm,Unevaluated[list]]];
stringNames=StringDrop[StringDrop[StringReplace[ToString[symbolNames,InputForm],{" $"->"$",", "->",","HoldForm["->"","],"->","}],-2],1];
$NamesOfSigns=Cases[StringSplit[stringNames,", "],s_String/;StringMatchQ[StringTrim[s],"$*Sign"]];
$ValuesOfSigns:=ToExpression[#,InputForm]&/@$NamesOfSigns;

SetSigns[x_]:=Catch@Module[{},
	Which[
		x===1,(SetDelayed[#,1]&/@(ToExpression[#,InputForm,Unevaluated]&/@$NamesOfSigns);If[$DefInfoQ,Print["** SetSigns: All constant signs set to 1."]];),
		x===-1,(SetDelayed[#,-1]&/@(ToExpression[#,InputForm,Unevaluated]&/@$NamesOfSigns);If[$DefInfoQ,Print["** SetSigns: All constant signs set to -1."]];),
		x===0,(Unset/@(ToExpression[#,InputForm,Unevaluated]&/@$NamesOfSigns);If[$DefInfoQ,Print["** SetSigns: All constant signs unset."]];),
		!MemberQ[{1,-1,0},x],Throw@Message[SetSigns::invalid,x,"argument. SetSigns only accepts 1/0/-1 argument"]
	];
]

Protect[SetSigns,$NamesOfSigns,$ValuesOfSigns];


(* ::Subsection:: *)
(*2.5.2. UnDefConstantsExceptSigns*)


UnDefConstantsExceptSigns:=Catch@Module[{symbolNames,stringNames,ListOfOtherConstants},
	symbolNames=Replace[First@OwnValues[$ConstantSymbols],HoldPattern[_:>list_]:>Map[HoldForm,Unevaluated[list]]];
	stringNames=StringSplit[StringDrop[StringDrop[StringReplace[ToString[symbolNames,InputForm],{" $"->"$",", "->",","HoldForm["->"","],"->","}],-2],1],", "];
	ListOfOtherConstants=DeleteCases[Drop[stringNames,Length@$ValuesOfSigns],s_String/;StringMatchQ[StringTrim[s],"$*Sign"]];

	Unset/@(ToExpression[#,InputForm,Unevaluated]&/@ListOfOtherConstants);
	UndefConstantSymbol/@(ToExpression[#,InputForm,Unevaluated]&/@ListOfOtherConstants);
	
	symbolNames=Replace[First@OwnValues[$ConstantSymbols],HoldPattern[_:>list_]:>Map[HoldForm,Unevaluated[list]]];
	stringNames=StringSplit[StringDrop[StringDrop[StringReplace[ToString[symbolNames,InputForm],{" $"->"$",", "->",","HoldForm["->"","],"->","}],-2],1],", "];
	ListOfOtherConstants=DeleteCases[Drop[stringNames,Length@$ValuesOfSigns],s_String/;StringMatchQ[StringTrim[s],"$*Sign"]];
	If[Length@ListOfOtherConstants>1,Print["** ResetSession: Some constants could not be undefined: ",ListOfOtherConstants,". Try running ResetSession again\ "]];	
	]


(* ::Subsection:: *)
(*2.5.3. xActQ*)


xActQ[x_]:=ConstantSymbolQ[x]||CovDQ[x]||InertHeadQ[x]||ManifoldQ[x]||MappingQ[x]||ParameterQ[x]||ScalarFunctionQ[x]||VBundleQ[x]||xTensorQ[x]||AbstractIndexQ[x]


(* ::Subsection:: *)
(*2.5.4. ResetSession*)


Options[ResetSession]:={UndefInfo->False,Signs->1};

(* I am sure that there are much better ways to define this function. ResetSession undefines everything with some order but not much. Quiet is used to avoid the error messages *) 

ResetSession[opt:OptionsPattern[Options[ResetSession]]]:=Module[{originalUndefInfo=$UndefInfoQ,symbolNames,stringNames,ListOfOtherConstants,listOfNonxActSymbols,listOfNonxActSymbolsWithOwnValues,brokenSymbols},
	
	listOfNonxActSymbols=Select[Names["Global`*"],!xActQ[ToExpression@#]&]//Quiet;
	listOfNonxActSymbolsWithOwnValues=Select[listOfNonxActSymbols,OwnValues[#]=!={}&&!ConstantQ[ToExpression[#]]&]; (* ConstantSymbols can have a value, but they are handled separately *)
	
	$UndefInfoQ=OptionValue[UndefInfo];
	
	UnDefConstantsExceptSigns//Quiet; 
	UndefCovD/@Select[$CovDs,!MetricQ@MetricOfCovD[#]&&#=!=PD &]//Quiet;
	UndefMetric/@$Metrics//Quiet;
	UndefScalarFunction/@$ScalarFunctions//Quiet;
	UndefParameter/@$Parameters//Quiet;
	UndefTensor/@$MasterTensors//Quiet;
	UndefParameter/@$Parameters//Quiet;
	UndefGeneralizedVVF/@$GeneralizedVVF//Quiet;
	UndefMapping/@$Mappings//Quiet;
	UndefVBundle/@Select[$VBundles,!TangentBundleQ[#]&]//Quiet;
	UndefManifold/@$Manifolds//Quiet;
	
	UnDefConstantsExceptSigns//Quiet;
	UnDefConstantsExceptSigns;
	SetSigns[OptionValue[Signs]];
	
	UndefCovD/@Select[$CovDs,!MetricQ@MetricOfCovD[#]&&#=!=PD &]//Quiet; (* Due to some dependencies (like ExtendedFrom), some CovDs might be removed twice and an error is thrown. Thus the first time we make it quiet and we try to remove them all again. If a problem persists, it will show up *)
	UndefCovD/@Select[$CovDs,!MetricQ@MetricOfCovD[#]&&#=!=PD &];
	If[Length[Select[$CovDs,!MetricQ@MetricOfCovD[#]&&#=!=PD &]]>1,Print["** ResetSession: Some CovDs could not be undefined: ",Select[$CovDs,!MetricQ@MetricOfCovD[#]&&#=!=PD &],". Try running ResetSession again.\ "]];

	UndefMetric/@$Metrics//Quiet;
	UndefMetric/@$Metrics;
	If[Length[$Metrics]>0,Print["** ResetSession: Some metrics could not be undefined: ",$Metrics,". Try running ResetSession again.\ "]];

	UndefScalarFunction/@$ScalarFunctions//Quiet;
	UndefScalarFunction/@$ScalarFunctions;
	If[Length[$ScalarFunctions]>0,Print["** ResetSession: Some scalar functions could not be undefined: ",$ScalarFunctions,". Try running ResetSession again.\ "]];

	UndefTensor/@$MasterTensors//Quiet;
	UndefCovD/@Select[$CovDs,#=!=PD &]; (* This is needed for the imploded tensors *)
	UndefTensor/@$MasterTensors;
	If[Length[$MasterTensors]>0,Print["** ResetSession: Some vertical forms could not be undefined:", $MasterTensors,". Try running ResetSession again.\ "]];

	UndefParameter/@$Parameters//Quiet;
	UndefParameter/@$Parameters;
	If[Length[$Parameters]>0,Print["** ResetSession: Some parameters could not be undefined: ",$Parameters,". Try running ResetSession again.\ "]];

	UndefGeneralizedVVF/@$GeneralizedVVF//Quiet;
	UndefGeneralizedVVF/@$GeneralizedVVF;
	If[Length[$GeneralizedVVF]>0,Print["** ResetSession: Some generalized vector fields could not be undefined: ",$GeneralizedVVF,". Try running ResetSession again.\ "]];

	UndefMapping/@$Mappings//Quiet;
	UndefMapping/@$Mappings;
	If[Length[$Mappings]>0,Print["** ResetSession: Some mappings could not be undefined: ",$Mappings,". Try running ResetSession again.\ "]];

	UndefVBundle/@Select[$VBundles,!TangentBundleQ[#]&]//Quiet;
	UndefVBundle/@Select[$VBundles,!TangentBundleQ[#]&];
	If[Length[Select[$VBundles,!TangentBundleQ[#]&]]>0,Print["** ResetSession: Some vector bundles could not be undefined: ",Select[$VBundles,!TangentBundleQ[#]&],". Try running ResetSession again.\ "]];

	UndefManifold/@$Manifolds//Quiet;
	UndefManifold/@$Manifolds;
	If[Length[$Manifolds]>0,Print["** ResetSession: Some manifolds could not be undefined: ",$Manifolds,". Try running ResetSession again.\ "]];

	listOfNonxActSymbolsWithOwnValues=Select[listOfNonxActSymbolsWithOwnValues,OwnValues[#]=!={}&];
	brokenSymbols=Select[listOfNonxActSymbolsWithOwnValues,StringContainsQ[ToString[OwnValues[#]],"Removed["]&]; (* Remove objects defined in terms of undefined elements *)
	Remove@@brokenSymbols;

	$UndefInfoQ=originalUndefInfo;
]

Protect[ResetSession];


(* ::Subsection:: *)
(*2.5.5. MakePattern*)


(* Function to make patterns for symbol matching *)
Off[RuleDelayed::rhs]
MakePattern[a_Symbol] := a_Symbol;       (* Match a positive symbol *)
MakePattern[-a_Symbol] := -a_Symbol;     (* Match a negative symbol *)
On[RuleDelayed::rhs]


(* ::Subsection:: *)
(*2.5.6. TensorWithIndices*)


ChristoffelAUX[PD]:=Zero
ChristoffelAUX[PD,PD]:=Zero
AChristoffelAUX[PD]:=Zero
AChristoffelAUX[PD,PD]:=Zero


(* Fills the slots of tensors with the correct indices *)
Attributes[TensorWithIndices]={HoldFirst};

TensorWithIndices::errorUnkown="No indices can be placed on `1`.";
TensorWithIndices::errorCovds="`1` and `2` are not compatible CovDs.";
TensorWithIndices::errordelta="No indices can be placed on \[Delta] as it is defined on every bundle (identity morphism).";
TensorWithIndices::errorGdelta="No indices can be placed on G\[Delta] as it is defined on every bundle.";

TensorWithIndices[delta]:=Throw@Message[TensorWithIndices::errordelta]
TensorWithIndices[Gdelta]:=Throw@Message[TensorWithIndices::errorGdelta]

(* Christoffel and AChristoffel are not evaluated unless some indices are included *)
TensorWithIndices[Christoffel[der1_?CovDQ,PD]]:=TensorWithIndices[ChristoffelAUX[der1]]
TensorWithIndices[AChristoffel[der1_?CovDQ,PD]]:=TensorWithIndices[AChristoffelAUX[der1]]
TensorWithIndices[Christoffel[PD,der2_?CovDQ]]:=-TensorWithIndices[ChristoffelAUX[der2]]
TensorWithIndices[AChristoffel[PD,der2_?CovDQ]]:=-TensorWithIndices[AChristoffelAUX[der2]]
TensorWithIndices[Christoffel[der_,der_?CovDQ]]:=0
TensorWithIndices[AChristoffel[der_,der_?CovDQ]]:=0

TensorWithIndices[Christoffel[der_?CovDQ]]:=TensorWithIndices[ToExpression["Christoffel"<>ToString[der]]]
TensorWithIndices[AChristoffel[der_?CovDQ]]:=TensorWithIndices[ToExpression["AChristoffel"<>ToString[der]]]

TensorWithIndices[Christoffel[der1_?CovDQ,der2_?CovDQ]]:=With[{expr=ToExpression["Christoffel"<>ToString[der1]<>ToString[der2]]},
	If[xTensorQ[expr],
        TensorWithIndices@expr,
	    If[xAct`xTensor`Private`CompatibleCovDsQ[der1,der2],
           With[{inds=GetIndicesOfVBundle[Tangent@ManifoldOfCovD@der1,3]},Christoffel[der1,der2][inds[[1]],-inds[[2]],-inds[[3]]]],
		   Throw@Message[TensorWithIndices::errorCovds,der1,der2]
        ]
    ]
]

TensorWithIndices[AChristoffel[der1_?CovDQ,der2_?CovDQ]]:=With[{expr=ToExpression["AChristoffel"<>ToString[der1]<>ToString[der2]]},
	If[xTensorQ[expr],
          TensorWithIndices@expr,
		  If[xAct`xTensor`Private`CompatibleCovDsQ[der1,der2],
             With[{ind=GetIndicesOfVBundle[Tangent@ManifoldOfCovD@der1,1][[1]],
				 inds=GetIndicesOfVBundle[VBundlesOfCovD[der1][[2]],2]},
				 AChristoffel[der1,der2][inds[[1]],-ind,-inds[[2]]]],
		     Throw@Message[TensorWithIndices::errorCovds,der1,der2]
          ]
    ]
]

TensorWithIndices[Zero]:=0
TensorWithIndices[metric_Inv?MetricQ]:=metric@@(-1*(DummyIn/@SlotsOfTensor[metric]))
TensorWithIndices[tensor_?xTensorQ]:=tensor@@(DummyIn/@SlotsOfTensor[tensor])
TensorWithIndices[tensor_?xTensorQ[inds___]]:=TensorWithIndices[tensor] (* If the tensor has indices, TensorWithIndices removes them and puts new one (in case they were not in the right place) *)
TensorWithIndices[None]:={} 
TensorWithIndices[{}]:={} 

(* Separate definition that forces evaluation for MultiplyHead *)
TensorWithIndices[expr_]:=Catch@With[{evaluated=expr},
							If[MatchQ[evaluated,MultiplyHead[integer_,tensor_]],
								evaluated[[1]]*TensorWithIndices[evaluated[[2]]],
								Throw@Message[TensorWithIndices::errorUnkown,evaluated]]]

Protect[TensorWithIndices];


(* ::Subsection:: *)
(*2.5.7. DeleteDuplicatesTensors*)


(* This function checks the Heads and removes repeated ones (regardless of the indices *)
DeleteDuplicatesTensors[list_List]:=DeleteDuplicates[list,Head[#1]===Head[#2]&]

Protect[DeleteDuplicatesTensors];


(* ::Subsection:: *)
(*2.5.8. HeadOfTensor2*)


(* Extracts the head of a tensor even if it is zero *)
HeadOfTensor2[0]:=0
HeadOfTensor2[Zero]:=Zero
HeadOfTensor2[tensor_?xTensorQ]:=With[{t=TensorWithIndices[tensor]},If[t===0,Zero,HeadOfTensor[t,List@@t]]] (* This forces the evaluation of certain expressions that are only evaluated with indices like Inv[frozenmetric] *)
HeadOfTensor2[tensor_?xTensorQ[indices___]]:=HeadOfTensor2[tensor]

Protect[HeadOfTensor2];


(* ::Subsection:: *)
(*2.5.9. CountCovD*)


(* Counts the highest number of nested derivatives in a monomial *)
CountCovD[der_?CovDQ][der_[a_][rest_]]:=CountCovD[der][rest]+1;
CountCovD[der_?CovDQ][HoldPattern[WWedge[expr1_,expr2_]]]:=Max[CountCovD[der][expr1],CountCovD[der][expr2]];
CountCovD[der_?CovDQ][expr1_ expr2_]:=Max[CountCovD[der][expr1],CountCovD[der][expr2]];
CountCovD[der_?CovDQ][expr_]/;FreeQ[expr,Plus]:=0


(* ::Subsection:: *)
(*2.5.10. splitList*)


(* splitList takes the list LIST of elements and a list of orders ORDERS (one for each element of the list, they can mean anything), and reorders LIST such that the factor with the highest order appears last.
   Everytime two elements are exchanged in the list, they supercommute. splitList Adjusts the overall sign according to the vertical degree of factors. *)
   
(* If all the orders are zero and ReturnZeroOrError is set to Zero, it returns zero *)
splitList[list_List,{0..},ReturnZeroOrError->Zero]:={0,{0,0}}

splitList[list_List,orders_List,option:OptionsPattern[Options[DivisionWWedge]]]:=Module[
	{
	currentList=list,
	sign=1,
	n=Length[list],
	maxIndex=First@Ordering[orders,-1],
	targetElement,
	currentPos
	},
	
	targetElement=list[[maxIndex]];
	
	(*Move the element to the end*)
	While[(Position[currentList,targetElement,1][[1,1]]<n),
		(*Current element's position*)
		currentPos=Position[currentList,targetElement,1][[1,1]];
		(*Swap with the next element*)
		With[
			{
			nextPos=currentPos+1,
			swapped1=currentList[[currentPos]],
			swapped2=currentList[[currentPos+1]]
			},
			(*Update sign using the given rule*)
			sign*=(-1)^(VertDeg[swapped1]*VertDeg[swapped2]);
			
			(*Swap elements*)
			currentList=ReplacePart[currentList,{currentPos->swapped2,nextPos->swapped1}]
		];
	];
		
	(*Return the modified list and the sign*)
	{orders,{sign*WWedge@@Most@currentList,list[[maxIndex]]}}
]


(* ::Subsection:: *)
(*2.5.11. splitFactors*)


(* Converts a product of WWedge and Times into a list *) 
splitFactors[expr_]:=If[Head[expr]===WWedge||Head[expr]===Times,splitFactors/@List@@expr,{expr}]//Flatten


(* ::Subsection:: *)
(*2.5.12. PositionOfElement*)


PositionOfElement::WrongDivision="Wrong WWedge division";

(* position takes a list={Subscript[elem, 1],Subscript[elem, 2],Subscript[elem, 3]...} and an elem and returns a list of 0's and 1's {0,0,0,1,0..}. The 1 is placed in the j-th position, where Subscript[elem, j]=elem (equality with indices included)*)
PositionOfElement[list_List,elem_,option:OptionsPattern[Options[DivisionWWedge]]]:=Module[{positionvector=Map[Boole[#===elem]&,list]},
	If[Total@positionvector=!=1&&OptionValue[ReturnZeroOrError]===Error,Throw[Message[PositionOfElement::WrongDivision,"Wrong WWedge division"]]];
	positionvector]


(* ::Subsection:: *)
(*2.5.13. adding*)


(* Allows to sum the elements of a list. If the expression is not a list but a single element, it returns the element. Notice that Total[v1[a,b]] doesn't work as expected. *)
adding[x_List]:=Total[Flatten[{x}]]


(* ::Subsection:: *)
(*2.5.14. SumToList*)


(* Converts a sum into a list (if there is only one term, it turns it into a list as well) *)
SumToList[x_Plus]:=List@@x
SumToList[x_]:=Flatten[{x}]


(* ::Subsection:: *)
(*2.5.15. DivisionWWedge*)


Options[DivisionWWedge]:={ReturnZeroOrError->Error};

(* This is the normal division (used in IndexCoefficient). elem must be an element of expr (with the same indices!) *)
DivisionWWedge[expr_,elem_?ZeroVertDegQ]:=expr/elem

(* This is the division whenever elem is inside a WWedge within expr. DivisionWWedge first turn the product into a list, then moves elem to the end of the list (division by the right), and then removes it *)
(* If the element is not there, it returns an error unless ReturnZeroOrError->Zero, which returns 0 *)
DivisionWWedge[expr_,elem_,option:OptionsPattern[Options[DivisionWWedge]]]:=With[{list=splitFactors[expr]},
	splitList[list,PositionOfElement[list,elem,option],option][[2]][[1]]]


(* ::Subsection:: *)
(*2.5.16. Generate tensors' names*)


(* ::Subsubsection:: *)
(*2.5.16.1. GenerateDiffName*)


(* This function generates the names and PrintAs of dltensors *) 

Options[GenerateDiffName]={PrintAs->None,PrintInverse->False};

GenerateDiffName[form_,opts:OptionsPattern[Options[GenerateDiffName]]]:=Module[{TensorName,prefix,printName,print},
	TensorName=ToString[form,InputForm];
	printName=RemoveOuterParanthesis[OptionValue[PrintAs]/. {None->TensorName}];
	
	(*Adjust the prefix for the inverse case if not overridden*)
	prefix=If[OptionValue[PrintInverse],$NameVerticalExteriorDerivative<>"Inv",$NameVerticalExteriorDerivative];
	
	print=If[OptionValue[PrintInverse],
	        StringJoin[$SymbolVerticalExteriorDerivative,"(",ToString[Superscript[printName,"-1"],StandardForm],")"],
			StringJoin["(",$SymbolVerticalExteriorDerivative,printName,")"]
			];
	
	{ToExpression[StringJoin[prefix,TensorName]],print}
];


(* ::Subsubsection:: *)
(*2.5.16.2. GenerateVariationalName*)


$RemoveParenthesesPrintAs:=True;

RemoveOuterParanthesis[string_]/;$RemoveParenthesesPrintAs:=Module[
{s=StringReplace[StringReplace[string,  "TeXAssistantTemplate\"]\))" :> "TeXAssistantTemplate\"]\)"],StartOfString ~~ "\!\((" :> "\!\("]},
	If[StringMatchQ[s,"("~~___~~")"],StringTake[s,{2,-2}],
	(s=StringReplace[s,StartOfString~~"(":>""];
	s=StringReplace[s,")"~~EndOfString:>""];
	s)]
]

RemoveOuterParanthesis[string_]/;!$RemoveParenthesesPrintAs:=string


Options[GenerateVariationalName]={PrintAs->None,PrintInverse->False};

(* This function generates the names and PrintAs of VariationalVectortensors *) 
GenerateVariationalName[tensor_,opts:OptionsPattern[Options[GenerateVariationalName]]]:=Module[{TensorName,printName},

    TensorName = If[OptionValue[PrintInverse],"VariationalVectorInv" <> ToString@tensor,"VariationalVector" <> ToString@tensor];
    printName = If[OptionValue[PrintInverse],"(\!\(\*FractionBox[\(\[Delta]\), \(\[Delta]" <> ToString[Superscript[RemoveOuterParanthesis@PrintAs@tensor,"-1"],StandardForm] <> "\)]\))","(\!\(\*FractionBox[\(\[Delta]\), \(\[Delta]" <> RemoveOuterParanthesis@PrintAs@tensor <> "\)]\))"];
	
	{ToExpression[TensorName],printName}
]


(* ::Subsubsection:: *)
(*2.5.16.3. GeneratePartialPartialName*)


ConsecutiveCounts[list_]:={#[[1]],Length[#]}&/@Split[Flatten[{list}]]
SuperScriptPartial[1]:="\[PartialD]"
SuperScriptPartial[n_?IntegerQ]:=ToString[Superscript["\[PartialD]",ToString@n],StandardForm]
RepeatedPartial[1]:=""
RepeatedPartial[n_?IntegerQ]:=ToString[n]

(* This function generates the names and PrintAs of PartialPartial *) 
Options[GeneratePartialPartialName]={PrintAs->None,PrintInverse->False};

InvName[tensor_,opts:OptionsPattern[Options[GeneratePartialPartialName]]]:=If[OptionValue[PrintInverse]&&MetricQ[tensor], "Inv"  ,""]<>ToString[tensor/.{MultiplyHead[_,name_]:>name}]
ConcatenatePartialTensorsName[list_,opts:OptionsPattern[Options[GeneratePartialPartialName]]]:=
{StringJoin@@("Partial" <> RepeatedPartial[#[[2]]]<> InvName[#[[1]],opts]&/@ConsecutiveCounts[list]),
StringJoin@@(SuperScriptPartial[#[[2]]]  <>If[OptionValue[PrintInverse]&&MetricQ[#[[1]]], ToString[Superscript[RemoveOuterParanthesis[PrintAs[Evaluate[#[[1]]]]],"-1"],StandardForm] , RemoveOuterParanthesis@PrintAs[#[[1]]//Evaluate] ]&/@ConsecutiveCounts[list])}

GeneratePartialPartialName[function_,tensors_,opts:OptionsPattern[Options[GeneratePartialPartialName]]]:=Module[{aux,length,TensorName,printName},

	length=Length@tensors;
	aux=ConcatenatePartialTensorsName[tensors,opts];
    TensorName = "Partial" <> If[length>1,ToString@length,""] <> ToString@function <>  aux[[1]];
    printName = "(\!\(\*FractionBox[\("<> SuperScriptPartial[length]<>"\!\(\*StyleBox[\"\[NegativeVeryThinSpace]\", \"Text\"]\)"<>PrintAs@function<>"\), \(" <> aux[[2]] <> "\)]\))";
    
	{ToExpression[TensorName],printName}
]


(* ::Subsection:: *)
(*2.5.17. MakeVertRule*)


(* This function creates a rule and the corresponding rule for their dltensors (linearized rule) *)
Options[MakeVertRule]:=Options[MakeRule]~Join~Options[ExpandVertDiff];

MakeVertRule[{lhs_,rhs_},opts:OptionsPattern[Options[MakeVertRule]]]:=With[
	{
	opts1=FilterRules[{opts},Options[MakeRule]],
	opts2=FilterRules[{opts},Options[ExpandVertDiff]]
	},
	MakeRule[{lhs,rhs},opts1]~Join~MakeRule[{Evaluate[VertDiff[lhs]],Evaluate[VertDiff[rhs]//ExpandVertDiff[opts2]]},opts1]
]

Protect[MakeVertRule];


(* ::Subsection:: *)
(*2.5.18. FilterVertExpand*)


(* FilterVertExpand takes a dltensor, the formula to expand dltensor and some options that indicates if it should expand it, leave it as is, or set to zero *)

(* If no expanded formula is provided, then the expanded formula is set to be dltensor itself *)
FilterVertExpand[dltensor_,options___][]:=FilterVertExpand[dltensor,options][dltensor];

(* Trivial case *)
FilterVertExpand[0,___][___]:=0;

(* This function parses the options to FilterVertExpandParsedOptions as variables. FilterVertExpandParsedOptions decides the result based on the options *)
FilterVertExpand[dltensor_,options:OptionsPattern[Options[ExpandVertDiff]]][expandedformula_]:=
	FilterVertExpandParsedOptions[
		dltensor,
		Flatten[{OptionValue[HoldExpandVertDiff]/. CheckOptions[options] /. Options[ExpandVertDiff]}], (* To allow to input lists or single elements *)
		Flatten[{OptionValue[ConstantTensors]/. CheckOptions[options] /. Options[ExpandVertDiff]}],
		Flatten[{OptionValue[NonConstantTensors]/. CheckOptions[options] /. Options[ExpandVertDiff]}]
	][expandedformula]; 

(* If tensor is defined as constant, then it returns 0 *)
FilterVertExpandParsedOptions[dltensor_[inds___],HoldExpandVertDiff_,constantList_,nonConstantList_][___]/;VariationallyConstantQ[constantList,nonConstantList][dltensor[inds]]:=0;

(* If tensor is not defined as constant but it is required to be held, then it returns dltensor[inds] *)
FilterVertExpandParsedOptions[dltensor_[inds___],HoldExpandVertDiff_,constantList_,nonConstantList_][___]/;
(!VariationallyConstantQ[constantList,nonConstantList][dltensor[inds]]&&VertDiffOfTensorToHoldQ[HoldExpandVertDiff][dltensor[inds]]):=dltensor[inds];

(* If tensor is not defined as constant and it is not required to be held, then it returns the expanded function *)
FilterVertExpandParsedOptions[dltensor_[inds___],Holdlist_,constantList_,nonConstantList_][dltensor_[inds___]]/;              (* Case base: expandedformula=dltensor[inds] *)
(!VariationallyConstantQ[constantList,nonConstantList][dltensor[inds]]&&!VertDiffOfTensorToHoldQ[Holdlist][dltensor[inds]]):=dltensor[inds]

FilterVertExpandParsedOptions[dltensor_[inds___],Holdlist_,constantList_,nonConstantList_][expandedformula_]/;             (* We reexpand *)
(!VariationallyConstantQ[constantList,nonConstantList][dltensor[inds]]&&!VertDiffOfTensorToHoldQ[Holdlist][dltensor[inds]]):=(expandedformula//ExpandVertDiff[HoldExpandVertDiff->Holdlist,ConstantTensors->constantList,NonConstantTensors->nonConstantList]);


(* ::Chapter:: *)
(*3. Variational relations*)


(* ::Section:: *)
(*3.1. Initial graph functions (independent of xAct)*)


(* ::Subsection:: *)
(*3.1.1. nonConstantVertexQ*)


(* Given a graph and edges, it checks if any of the edges is not in the graph *)
(* The use of this function is: provided a subgraph \[Subset] graph and a list {edges} \[Subset] graph that are incoming to a vertex \[Element] subgraph, check if {edges}\[Subset] subgraph *)   
nonConstantVertexQ[subgraph_,IncomingEdges_]:=AnyTrue[IncomingEdges,!EdgeQ[subgraph,#]&]


(* ::Subsection:: *)
(*3.1.2. Propagation of constant through the graph*)


(* ::Subsubsection:: *)
(*3.1.2.1. GenerateConstantGraph*)


(* This function creates the graph formed by constant nodes and edges from  the constant relation (unless the constantedges are not in graph) *)
(* VertexOutComponentGraph[graph,listOfInitialConstantVertices] generates the subgraph of all the vertices affected by the initial constant vertices *)
GenerateConstantGraph[graph_, listOfInitialConstantVertices_List] := 
 Module[{validVertices=Intersection[VertexList[graph], listOfInitialConstantVertices]},
  If[validVertices === {},
   Graph[{},{}],
   BackwardDiscardConstants[graph, VertexOutComponentGraph[graph, validVertices], validVertices, {}]
  ]
 ]


(* ::Subsubsection:: *)
(*3.1.2.2. BackwardDiscardConstants*)


(* This function propagates the constant relations to the graph *)
(* The idea is to assume that the contant vertices forward-propagate to all edges and vertices. This represents the largest possible set of constants. However, some of them might not be truly constant. *)
(* We find those which are fake-constant i.e., those with incoming edges which are not constant. We remove those. *)
(* Since new non-constant edges are created, we check recursively until no vertices are removed. *)
 
BackwardDiscardConstants[graph_,OldCandidateToConstantSubgraph_,listOfInitialConstantVertices_,OldDiscardedVertices_]:=Module[
	{newConstantVertices,incomingEdgesOfNewConstantVertices,NonConstantVertices,NewCandidateToConstantSubgraph,NewDiscardedVertices},
	
	(* Finds the vertices of the OldCandidateToConstantSubgraph removing the listOfInitialConstantVertices and the OldDiscardedVertices *)
	newConstantVertices=Complement[VertexList[OldCandidateToConstantSubgraph],listOfInitialConstantVertices~Join~OldDiscardedVertices];
	
	(* Generates a list of edges for each vertex\[Element]newConstantVertices to generate: {{Subscript[x, 11]->Subscript[v, 1]},{Subscript[x, 12]->Subscript[v, 1]},..{Subscript[x, 1Subscript[n, 1]]->Subscript[v, 1]}},..,{{Subscript[x, m1]->Subscript[v, m]},..,{Subscript[x, Subscript[mn, m]]->Subscript[v, m]}}} *)
	incomingEdgesOfNewConstantVertices=EdgeList[graph,_\[DirectedEdge]#]&/@newConstantVertices;
	
	(* We select the vertices of newConstantVertices that are not trule constant becuase they have an incoming edge not contained in OldCandidateToConstantSubgraph. *)
	(* Last/@Last/@ extracts the vertices. *)
	NonConstantVertices=Last/@Last/@Select[incomingEdgesOfNewConstantVertices,nonConstantVertexQ[OldCandidateToConstantSubgraph,#]&];
	
	(* We update the discarded vertices *)
	NewDiscardedVertices=OldDiscardedVertices~Join~NonConstantVertices;
	
	(* If no vertices have been discarded, we are done. Otherwise, we have to proceed recursively *)
	If[NonConstantVertices=={},
		OldCandidateToConstantSubgraph,
		(
		(* We forward propagate the initial constant in the initial graph minus the discarded vertices *)
		NewCandidateToConstantSubgraph=VertexOutComponentGraph[VertexDelete[graph,NewDiscardedVertices],listOfInitialConstantVertices];
		
		(* We backwards check that in the new graph all the vertices are indeed constant *)
		NewCandidateToConstantSubgraph=BackwardDiscardConstants[graph,NewCandidateToConstantSubgraph,listOfInitialConstantVertices,NewDiscardedVertices];
		NewCandidateToConstantSubgraph
		)
	]
]


(* ::Subsection:: *)
(*3.1.3. Highlight of constant through the graph*)


(* This function shows the graph with the constant edges and nodes derived from initialConstantVertices *) 
HighlightConstantRelations[graph_,initialConstantVertices_]:=Module[{
	ConstantGraph=GenerateConstantGraph[$VariationalGraph,initialConstantVertices],
	allVertices=VertexList[graph],
	NewConstantVertices,nonConstantVertices,ConstantEdges,highlightedGraph},

	(* Get Constant nodes and edges from the Constant subgraph *)
	NewConstantVertices=Complement[VertexList[ConstantGraph],initialConstantVertices];
	ConstantEdges=EdgeList[ConstantGraph];

	(*Get non-constant nodes*)
	nonConstantVertices=Complement[allVertices,initialConstantVertices~Join~NewConstantVertices];

	(*Create the highlighted graph*)
	highlightedGraph=Graph[graph,
							VertexStyle->(#->If[MemberQ[nonConstantVertices,#],Blue,Red]&/@allVertices),
							VertexShapeFunction->(#->If[MemberQ[nonConstantVertices,#],"Circle","Square"]&/@allVertices),
							EdgeStyle->(#->If[MemberQ[ConstantEdges,#],Red,Gray]&/@EdgeList[graph]),
							VertexLabels->"Name"];
	highlightedGraph
]


(* ::Subsection:: *)
(*3.1.4. Subgraphs*)


SubGraphRelations::missing= "One of the tensors in `1` is not included in the Variational Graph `2`.";

ListOut={"Up","up","In","in","Inward","inward","Inwards","inwards"};
ListIn={"Down","down","Out","out","Outward","outward","Outwards","Outwards"};
ListBoth={"Both","both","xAct`xCPS`Private`Both"};

(* Creates the subgraph of influence of a vertex *)
SubGraphRelations[graph_,vertices_List]:=SubGraphRelations[graph,vertices,"Both"]
SubGraphRelations[graph_,{All},_]:=graph;
SubGraphRelations[graph_,vertices_List,direction_]:=Module[
	{incomingVertices,outgoingVertices},
	
	If[AnyTrue[vertices,!VertexQ[graph,#]&],Throw@Message[SubGraphRelations::missing,vertices,graph]];
	
	incomingVertices=If[MemberQ[ListOut~Join~ListBoth,direction],VertexInComponent[graph,vertices],{}];(* Vertices that influence the vertices *)
	outgoingVertices=If[MemberQ[ListIn~Join~ListBoth,direction],VertexOutComponent[graph,vertices],{}];(* Vertices influenced by the vertices*)
	
	Subgraph[graph,Union[incomingVertices,outgoingVertices]]
]


(* ::Subsection:: *)
(*3.1.5. AddRelationToGraph*)


(* Add dependency to graph *)
AddRelationToGraph[master_->dependent_,graph_]:=Graph[VertexList[graph],EdgeList[graph]~Join~{master->dependent}]


(* ::Subsection:: *)
(*3.1.6. FindCyclicVariationalRelations*)


Options[FindCyclicVariationalRelations]:={ShowGraph->True};

(* Returns cyclic variational relations (and prints them) *)
FindCyclicVariationalRelations[graph_,option:OptionsPattern[Options[FindCyclicVariationalRelations]]]:=Module[{possiblecycles},

	possiblecycles=FindCycle[graph,Infinity,All];
	
	If[Length@possiblecycles>0&&OptionValue[ShowGraph],
		(
		Print["** FindCyclicVariationalRelations: Some cyclic relations found:"];
		Print[Annotate[HighlightGraph[graph,possiblecycles],{VertexLabels->"Name"}]];
		),
		If[OptionValue[ShowGraph],Print["** FindCyclicVariationalRelations: No cyclic relations found"]];
	];
	possiblecycles
]

Protect[FindCyclicVariationalRelations];


(* ::Section:: *)
(*3.2. Variational relations (xAct related)*)


(* ::Subsection:: *)
(*3.2.1. Handle VariationalRelations (vertices)*)


(* ::Subsubsection:: *)
(*3.2.1.1. AddVariationalRelation*)


(*Adds variational dependencies to $VariationalGraph*)
AddVariationalRelation::zerotensor= "`1` cannot depend variationally on the Zero tensor.";

AddVariationalRelation[masterTensor_?xTensorQ->dependentTensorList_List]:=AddVariationalRelation[masterTensor->#]&/@dependentTensorList
AddVariationalRelation[masterTensorList_List->dependentTensor_?xTensorQ]:=AddVariationalRelation[#->dependentTensor]&/@masterTensorList
AddVariationalRelation[_->Zero]:=Null
AddVariationalRelation[Zero->dependentTensor_]:=Throw@Message[AddVariationalRelation::zerotensor,dependentTensor]

(* Adds a variational relation *)
AddVariationalRelation[mastertensor_?xTensorQ->dependentTensor_?xTensorQ]:=
	(
	If[!EdgeQ[$VariationalGraph,mastertensor->dependentTensor],
		(
		$VariationalGraph=AddRelationToGraph[mastertensor->dependentTensor,$VariationalGraph];
		If[$printAddVariationalRelation&&$DefInfoQ,Print["** AddVariationalRelation: Variational relation created ",mastertensor,"\[Rule]",dependentTensor,"."]];
		)
	];
	If[(Dagger[mastertensor]=!=mastertensor||Dagger[dependentTensor]=!=dependentTensor)&&$AddVariationalRelationDagger,
		(
		$AddVariationalRelationDagger=False;
		AddVariationalRelation[Dagger[mastertensor]->Dagger[dependentTensor]];
		$AddVariationalRelationDagger=True;
		)
	];
	)

(* Recursive case: multiple chained tensors *)
AddVariationalRelation[mastertensor_?xTensorQ -> dependentTensor_?xTensorQ -> rest___] := 
  (
    AddVariationalRelation[mastertensor -> dependentTensor];
    AddVariationalRelation[dependentTensor -> rest]
  )


(* ::Subsubsection:: *)
(*3.2.1.2. RemoveVariationalRelation*)


(* Removes a variational relation *)
RemoveVariationalRelation[mastertensor_?xTensorQ->dependentTensor_?xTensorQ]:=
	(
	If[EdgeQ[$VariationalGraph,mastertensor->dependentTensor],
		(
		$VariationalGraph=EdgeDelete[$VariationalGraph,mastertensor->dependentTensor];
		If[$printAddVariationalRelation&&$UndefInfoQ,Print["** RemoveVariationalRelation: Variational relation removed ",mastertensor,"\[Rule]",dependentTensor]];
		)
	];
	If[(Dagger[mastertensor]=!=mastertensor||Dagger[dependentTensor]=!=dependentTensor)&&$RemoveVariationalRelation,
		(
		$RemoveVariationalRelation=False;
		RemoveVariationalRelation[Dagger[mastertensor]->Dagger[dependentTensor]];
		$RemoveVariationalRelation=True;
		)
	];
	)


(* ::Subsubsection:: *)
(*3.2.1.3. VertexDeleteAndUpdate*)


VertexDeleteAndUpdate[tensor_]:=(If[VertexQ[$VariationalGraph,tensor],$VariationalGraph=VertexDelete[$VariationalGraph,tensor]];
									$VariationalGraph)


(* ::Subsubsection:: *)
(*3.2.1.4. VertexAddAndUpdate*)


VertexAddAndUpdate[tensor_]:=($VariationalGraph=VertexAdd[$VariationalGraph,tensor];
							  $VariationalGraph)


(* ::Subsection:: *)
(*3.2.2. Extract variational relations*)


(* ::Subsubsection:: *)
(*3.2.2.1. VariationalRelationsOf*)


(* Shows all variational relations *) 
Options[VariationalRelationsOf]={ConstantTensors->None,Directed->Both,HideTrivialRelations -> True}; 

VariationalRelationsOf[tensors_, opts : OptionsPattern[Options[VariationalRelationsOf]]] := 
  Module[{graph, verticesToRemove},
    graph = HighlightConstantRelations[
              SubGraphRelations[$VariationalGraph, Flatten@{tensors}, ToString@OptionValue@Directed],
              Flatten@{OptionValue@ConstantTensors}
    ];
    
    If[OptionValue[HideTrivialRelations], graph = VertexDelete[graph, Select[VertexList[graph], VertExactHeadQ[#]||VariationalVectorQ[#]&]]; ];
    
    Annotate[graph,
      {VertexStyle -> {Alternatives @@ Flatten[{tensors}] -> Green}, 
       VertexShapeFunction -> {Alternatives @@ Flatten[{tensors}] -> "Triangle"}}
    ]
  ]


(* ::Subsubsection:: *)
(*3.2.2.2. ListVariationalRelationsOf*)


Options[ListVariationalRelationsOf]={Directed->Both,HideTrivialRelations->False};
ListVariationalRelationsOf[tensor_?xTensorQ,opt : OptionsPattern[Options[ListVariationalRelationsOf]]]:=VertexList[VariationalRelationsOf[tensor,Directed->OptionValue[Directed],HideTrivialRelations->OptionValue[HideTrivialRelations]]]


(* ::Subsubsection:: *)
(*3.2.2.3. ListOfVariationalConstantsOf*)


ListOfVariationalConstantsOf[tensor_]:=VertexOutComponent[GenerateConstantGraph[$VariationalGraph,Flatten[{tensor}]],tensor]

Protect[VariationalRelationsOf,ListVariationalRelationsOf,ListOfVariationalConstantsOf];


(* ::Subsection:: *)
(*3.2.3. VariationallyConstantQ*)


(* VariationallyConstantQ takes a ListConstantFields, a ListNonConstantFields, and a tensor, and checks if tensor \[Element] VertDiff[ListConstantFields] or tensor \[NotElement] VertDiff[ListNonConstantFields] *)
VariationallyConstantQ[ListConstantFields_,ListNonConstantFields_][tensor_?xTensorQ[inds___]]:=VariationallyConstantQ[ListConstantFields,ListNonConstantFields][tensor]
VariationallyConstantQ[{},{}][tensor_]:=False

VariationallyConstantQ[ListConstantFields_,{}][tensor_]:=With[
		{list=ListOfVariationalConstantsOf[{ListConstantFields}~Join~{Select[MasterOf/@ListConstantFields,xTensorQ]}//Flatten]}, (* We create the list of tensors that are related to the constants. Those are the only ones that are not constant (we have to add their VertDiff) *)
		MemberQ[list,tensor]
		] 

VariationallyConstantQ[{},ListNonConstantFields_][tensor_]:=With[
		{list=ListVariationalRelationsOf/@({ListNonConstantFields}~Join~{Select[MasterOf/@ListNonConstantFields,xTensorQ]}//Flatten)//Flatten}, (* We create the list of tensors that are related to the constants. Those are the only ones that are not constant (we have to add their VertDiff) *)
		!MemberQ[list~Join~DeleteElements[VertDiff/@list ,{Zero}]//DeleteDuplicates,tensor]
		] 

ExpandVertDiff::unknown="The options ConstantTensors and NonConstantTensors are incompatible.";
VariationallyConstantQ[ListConstantFields_,ListNonConstantFields_][tensor_]:=Throw@Message[ExpandVertDiff::unknown];

Protect[VariationallyConstantQ];


(* ::Subsection:: *)
(*3.2.4. VertDiffOfTensorToHoldQ*)


(* VertDiffOfTensorToHoldQ takes a ListTensorsToHold and checks if dltensor \[Element] ListConstantFields or dltensor \[Element] VertDiff[ListNonConstantFields] *)
VertDiffOfTensorToHoldQ[{}][dltensor_]:=False

VertDiffOfTensorToHoldQ[ListTensorsToHold_][dltensor_[inds___]]:=MemberQ[Flatten[{ListTensorsToHold, VertDiff /@ Flatten[{ListTensorsToHold}]}], dltensor]


(* ::Section:: *)
(*3.3. Imploded tensors*)


(* ::Subsection:: *)
(*3.3.1. VariationalRelationsOfImploded*)


VariationalRelationsOfImploded[tensor_Symbol]:=DeleteDuplicates[VariationalRelationsOfImploded[tensor,TensorID[tensor]]]

(* If it is not der of a scalar, it must depend on the christoffels and the VariationalDependendiesOfImploded of its master. *)
VariationalRelationsOfImploded[tensor_,{CovD,covd_,linds_Integer,intensor_}]:=Module[
	{
	tangentIndicesQ=Length@Select[HostsOf@intensor,VBundleQ[#]&&Tangent@BaseOfVBundle[#]===#&]>0, (* HostsOf@Scalar={manifold}, HostsOf@Tensor={manifold,all VBundles it has indices on *)
	innerVBindicesQ=Length@Select[HostsOf@intensor,VBundleQ[#]&&Tangent@BaseOfVBundle[#]=!=#&]>0,
	listOfChristoffels
	},
	listOfChristoffels=Join[If[tangentIndicesQ,ChristoffelAUX@covd,{}],If[innerVBindicesQ,AChristoffelAUX@covd,{}]];
	Join[listOfChristoffels,VariationalRelationsOfImploded@intensor]
]

(* This are simply PD, so it only depends on the VariationalDependendiesOfImploded of its master *)
VariationalRelationsOfImploded[tensor_,{ParamD,ps__,intensor_}]:=VariationalRelationsOfImploded@intensor;
VariationalRelationsOfImploded[tensor_,{OverDot,intensor_}]:=VariationalRelationsOfImploded@intensor;

(* LieD depends on the vector and the VariationalDependendiesOfImploded of its master *)
VariationalRelationsOfImploded[tensor_,{LieD,v_,intensor_}]:=Join[{v},VariationalRelationsOfImploded@intensor]; 

(* Case base *)
VariationalRelationsOfImploded[tensor_?xTensorQ,_]:={tensor}


(* ::Chapter:: *)
(*4. Definition of vertical forms and how to expand them*)


(* ::Section:: *)
(*4.1. Vertical forms*)


(* ::Subsection:: *)
(*4.1.1. Modification DefTensor*)


(* DefTensor defines, apart from the tensor, its vertical exterior derivative and its variational vector *)

Unprotect[DefTensor];
Options[DefTensor]=Options[DefTensor]~Join~{DefineExactFormAfterDefTensor->True,DefineVariationalVectorAfterDefTensor->True,VariationallyConstantQ->False,VertDeg->0};
Options[DefAdditionalTensors]=Options[DefTensor]~Join~{DefInverseMetric->False,PrintInverse->False};
Protect[DefTensor];

xTension["xAct`xTensor`", DefTensor, "End"]:=DefAdditionalTensors

OptionsToBeRemoved={GradeOfTensor->_,DefInfo->_,DefInfo:>_,Master->_,PrintAs->_,FrobeniusQ->_,OrthogonalTo->_,OrthogonalTo:>_,ProjectedWith->_,ProjectedWith:>_,ProtectNewSymbol->_,TensorID->_,VariationallyConstantQ->_,WeightOfTensor->_,DefineExactFormAfterDefTensor->_,DefineVariationalVectorAfterDefTensor->_};
DefAdditionalTensors[tensor_[indices___],dependencies_,sym_,options:OptionsPattern[Options[DefAdditionalTensors]]]:=
	(
	If[OptionValue[VertDeg]=!=0,tensor/:GradeOfTensor[tensor,WWedge]:=OptionValue[VertDeg]]; (* This allows the option GradeOfTensor->{WWedge->integer} *)
	
	If[!OptionValue[DefInverseMetric],
		(
		VertexAddAndUpdate[tensor];
		PartialPartialsOfTensor[tensor]^={};
		)
	];
	
	If[OptionValue[DefineExactFormAfterDefTensor],DefExactVerticalForm[tensor[indices],dependencies,sym,DeleteCases[Flatten[{options}]/. {VariationallyConstantQ -> VanishingQ},Alternatives@@OptionsToBeRemoved,2]]]; (* We remove the PrintAs of the tensor (not of the dltensor) and change VariationallyConstantQ with VanishingQ *)
	If[OptionValue[DefineVariationalVectorAfterDefTensor],DefVariationalVector[tensor[indices],dependencies,SymmetryGroupOfTensor[tensor],DeleteCases[Flatten[{options}],Alternatives@@OptionsToBeRemoved,2]]]; (* We remove the PrintAs of the tensor (not of the VariationalVector) and VariationallyConstantQ *)

    (*************************)
    (***** COMPLEX TENSORS *****)
    (*************************)

    (* Complex tensor with dagger in its name (to avoid doing it twice) *)	
    If[HasDaggerCharacterQ[tensor],
		(
		$printAddVariationalRelation=False;
		If[!VertExactHeadQ[tensor]&&!VariationalVectorQ[tensor],
			AddVariationalRelation[tensor->Dagger@tensor->tensor]; (* Only the master tensors are variationally related to avoid cluttering too much the graph (the dl tensors and variational vectors are effectively related through their masters *) 
		];
		
	    If[VertExactHeadQ[tensor], (* Complex dltensor *)
			(
			PrintAs[Evaluate[tensor]]^=Evaluate[(GenerateDiffName[MasterOfCPSTensor@tensor,PrintAs->PrintAs[Evaluate[MasterOfCPSTensor@tensor]]])[[2]]];(* This improves the PrintAs of Dagger@VertDiff *)
			AddVariationalRelation[MasterOfCPSTensor@tensor->tensor];
			)
		];

		If[VariationalVectorQ[tensor], (* Complex VariationalVectortensor *)	
			(
			PrintAs[Evaluate[tensor]]^=Evaluate[(GenerateVariationalName[MasterOfCPSTensor@tensor,PrintAs->PrintAs[Evaluate[MasterOfCPSTensor@tensor]]])[[2]]];(* This improves the PrintAs of Dagger@VariationalVector *)
			AddVariationalRelation[MasterOfCPSTensor@tensor->tensor];
            If[$DefInfoQ&&!CovDQ[MasterOf@MasterOfCPSTensor@Dagger@tensor],Print["** AddVariationalRelation: Variational relations created for conjugated tensors"]]; (* This is printed at the end *)
			)
		];
		 
		$printAddVariationalRelation=True;
		)
	];
	
    (**************************)
    (***** IMPLODED TENSORS *****)
    (**************************)
	
	(* When dealing with Imploded tensors, we only want to create the variational relations of the Master (not the VertDiff or VariationalVector) *)
	If[OptionValue[DefineExactFormAfterDefTensor]&&OptionValue[DefineVariationalVectorAfterDefTensor]&&xAct`xTensor`Private`ImplodedQ[tensor],
		(
		$printAddVariationalRelation=$ImplodeInfoQ;
		AddVariationalRelation[#->tensor]&/@DeleteElements[VariationalRelationsOfImploded[tensor],{Zero}];
		$printAddVariationalRelation=True;
		)
	];
	
    (****************************)
    (***** DOUBLE CHRISTOFFEL *****)
    (****************************)
    
	If[Length@TensorID@tensor===3&&TensorID[tensor][[1]]===Christoffel&&CovDQ[TensorID[tensor][[2]]]&&CovDQ[TensorID[tensor][[3]]]&&TensorID[tensor][[3]]=!=PD,
		(
		ChristoffelAUX[TensorID[tensor][[2]],TensorID[tensor][[3]]]=tensor; (* For TensorWithIndices *)
		ChristoffelAUX[TensorID[tensor][[3]],TensorID[tensor][[2]]]=MultiplyHead[-1,tensor]; (* For TensorWithIndices *)
		Unprotect[VertDiff,Christoffel];
		Christoffel/: VertDiff[Christoffel[TensorID[tensor][[2]],TensorID[tensor][[3]]]]=VertDiff@tensor;
		Christoffel/: VertDiff[Christoffel[TensorID[tensor][[3]],TensorID[tensor][[2]]]]=MultiplyHead[-1,VertDiff@tensor];
		Protect[VertDiff,Christoffel];
		AddVariationalRelation[HeadOfTensor2@TensorWithIndices@Christoffel[TensorID[tensor][[2]]]->tensor];
		AddVariationalRelation[HeadOfTensor2@TensorWithIndices@Christoffel[TensorID[tensor][[3]]]->tensor];
		GenerateExpandVertDiffRule[{VertDiff[tensor[indices]],VertDiff[Christoffel[TensorID[tensor][[2]]][indices]]-VertDiff[Christoffel[TensorID[tensor][[3]]][indices]]}];
		ProtectVertDiffRule[VertDiff[tensor]];
		)];
		
	If[Length@TensorID@tensor===3&&TensorID[tensor][[1]]===AChristoffel&&CovDQ[TensorID[tensor][[2]]]&&CovDQ[TensorID[tensor][[3]]]&&TensorID[tensor][[3]]=!=PD&&(!HasDaggerCharacterQ[tensor]),
		(
		AChristoffelAUX[TensorID[tensor][[2]],TensorID[tensor][[3]]]=tensor; (* For TensorWithIndices *)
		AChristoffelAUX[TensorID[tensor][[3]],TensorID[tensor][[2]]]=MultiplyHead[-1,tensor]; (* For TensorWithIndices *)
		Unprotect[VertDiff];
		Unprotect[VertDiff,AChristoffel];
		AChristoffel/: VertDiff[AChristoffel[TensorID[tensor][[2]],TensorID[tensor][[3]]]]=VertDiff@tensor;
		AChristoffel/: VertDiff[AChristoffel[TensorID[tensor][[3]],TensorID[tensor][[2]]]]=MultiplyHead[-1,VertDiff@tensor];
		Protect[VertDiff,Christoffel];
		AddVariationalRelation[HeadOfTensor2@TensorWithIndices@AChristoffel[TensorID[tensor][[2]]]->tensor];
		AddVariationalRelation[HeadOfTensor2@TensorWithIndices@AChristoffel[TensorID[tensor][[3]]]->tensor];
		GenerateExpandVertDiffRule[{VertDiff[tensor[indices]],VertDiff[AChristoffel[TensorID[tensor][[2]]][indices]]-VertDiff[AChristoffel[TensorID[tensor][[3]]][indices]]}];
		ProtectVertDiffRule[VertDiff[tensor]];
		)
	];	
	)


MasterOfCPSTensor[tensor_?HasDaggerCharacterQ]/;!PartialPartialQ[tensor]:=Dagger@MasterOfCPSTensor[Dagger@tensor/.{MultiplyHead[_,name_]:>name}]


(* ::Subsection:: *)
(*4.1.2. Modification UndefTensor*)


xTension["xAct`xTensor`", UndefTensor, "Beginning"] :=Module[{dagger=Dagger@#/.{MultiplyHead[_,name_]:>name}},
	If[DaggerQ[#]&&!xTensorQ[MasterOfCPSTensor[#]]&&!HasDaggerCharacterQ[#]&&dagger=!=#,ServantsOf[dagger]^={}]; (* This is needed to avoid double Undef of some tensors *)
	VertexDeleteAndUpdate[#];
	If[PartialPartialQ[#],RemovePartialPartialUpvalues[#]];	
	If[!PartialPartialQ[#]&&!VariationalVectorQ[#]&&!VertExactHeadQ[#],(UndefTensor/@removeDaggerPairs[PartialPartialsOfTensor[#]];)];
	]&

RemovePartialPartialUpvalues[PPtensor_?PartialPartialQ]:=Module[
	{
	tensors=DeleteDuplicates@TensorsOfPartialPartial@PPtensor,
	function=FunctionOfPartialPartial@PPtensor
	},
	Scan[(PartialPartialsOfTensor[#] ^= DeleteCases[PartialPartialsOfTensor[#],PPtensor]) &, tensors];
	PartialPartialsOfFunction[function]^=DeleteCases[PartialPartialsOfFunction[function],PPtensor];
]


(* ::Subsection:: *)
(*4.1.3. DefExactVerticalForm*)


PartialPartialsOfTensor[_]:={} (*This is necessary for some tensor that are defined as vanishing*) 
Options[DefExactVerticalForm]:=Options[DefAdditionalTensors];
DefExactVerticalForm::unknown="VertDiff[`1`] is already defined as `2`.";

DefExactVerticalForm[tensor_Symbol?xTensorQ[inds___],dependencies_,sym_,options:OptionsPattern[Options[DefExactVerticalForm]]]:=Module[
	{diffForm=GenerateDiffName[tensor,PrintAs->PrintAs[tensor],FilterRules[{options},Options[GenerateDiffName]]],prot,pinds=MakePattern/@{inds},opts,dltensor},
	
	MakexTensions[DefExactVerticalForm,"Beginning",tensor[inds],dependencies,sym,options];
	
	If[xTensorQ[VertDiff[tensor[inds]]//Head]&&!OptionValue[PrintInverse],Throw@Message[DefExactVerticalForm::unknown,tensor,VertDiff[tensor]]];
	
	opts=FilterRules[{WeightOfTensor->WeightOfTensor[tensor],VertDeg->VertDeg[tensor[inds]] + 1}~Join~{options},Options[DefTensor]]; (* Remove the old GradeOfTensor and add the new one *)
	opts=If[xAct`xTensor`Private`ImplodedQ[tensor],opts~Join~{DefInfo:>If[$ImplodeInfoQ,{"tensor",""},False]},opts];

	dltensor=Evaluate[ToExpression[Evaluate[diffForm[[1]]]]];
	VertExactHeadQ[dltensor]^=True;
	MasterOfCPSTensor[dltensor]^=tensor;
	
	(*Define the exact tensor with the given indices,symmetries, options, and arguments. Its VertDeg is one more than the tensor.*)
	If[!StringEndsQ[ToString@tensor,"\[Dagger]"],DefTensor[diffForm[[1]][inds],dependencies,sym,PrintAs->diffForm[[2]],opts,DefineExactFormAfterDefTensor->False,DefineVariationalVectorAfterDefTensor->False]];
	
	
	(* Establish relationships between the exact tensor and the original tensor *)
	xAct`xTensor`Private`SymbolRelations[dltensor,tensor,{tensor}];
	$printAddVariationalRelation=False;
	AddVariationalRelation[tensor->dltensor];
	$printAddVariationalRelation=True;
	
	(*Define the vertical derivative of the tensor to be the exact tensor*)
	If[OptionValue[PrintInverse],
		(
		VertDiff[ToExpression["Inv"<>ToString[tensor]]]=dltensor;
		dlInvMetricQ[dltensor]^=True;
		If[DaggerQ[dltensor],
			VertDiff[ToExpression["Inv"<>ToString[tensor]<>"\[Dagger]"]]=Dagger@dltensor;
			dlInvMetricQ[Dagger@dltensor]^=True;
			VertExactHeadQ[Dagger@dltensor]^=True
		];
		),
		(
		tensor/:VertDiff[tensor]=dltensor;
		PrintAs[Evaluate[ToExpression[VertDiff[tensor]]]]^=diffForm[[2]];(*This prevents the densities to have two sets of tildes,only the ones coming from the tensor remained*)
		)
	];
	
	tensor /: VertDiff[tensor @@ pinds]:= dltensor[inds];
	
	(*Define rules for ExpandVertDiffRules.This is the base case,that it leaves it as it is*)
	ExpandVertDiffRules[dltensor@@(MakePattern/@{inds}),opts:OptionsPattern[Options[ExpandVertDiff]]]:=FilterVertExpand[dltensor[inds],opts][];
		
	MakexTensions[DefExactVerticalForm,"End",tensor[inds],dependencies,sym,options];
];


(* ::Subsection:: *)
(*4.1.4. GenerateExpandVertDiffRule and RemoveExpandVertDiffRule - Customizable rules for the user*)


Off[RuleDelayed::rhs]
Options[GenerateExpandVertDiffRule]:={CheckGenerateExpandVertDiffRule->True};
GenerateExpandVertDiffRule::missmatch="VertDeg missmatch: the VertDeg of `1` is `2`\.08, which does not match the VertDeg of the expansion.";
GenerateExpandVertDiffRule::wrongindices="The indices of `1` are not the natural ones.";

(* Function for the user to generate their own expansion rules *)
GenerateExpandVertDiffRule[{dltensor_?VertExactHeadQ[inds___],expansion_},option:OptionsPattern[Options[GenerateExpandVertDiffRule]]]:=GenerateExpandVertDiffRuleAUX[dltensor[inds],expansion,option]

GenerateExpandVertDiffRuleAUX[dltensor_[inds___],expansion_,option:OptionsPattern[Options[GenerateExpandVertDiffRule]]]:=Module[{rule,oldexpansion,redefinitionQ,removalQ,newdefinitionQ},
  
   If[OptionValue[CheckGenerateExpandVertDiffRule]&&!VertDeg[dltensor]===VertDeg[expansion],Throw@Message[GenerateExpandVertDiffRule::missmatch,dltensor,VertDeg[dltensor]]];
   If[OptionValue[CheckGenerateExpandVertDiffRule]&&!SlotsOfTensor[dltensor]===xAct`xTensor`Private`SignedVBundleOfIndex/@{inds},Throw@Message[GenerateExpandVertDiffRule::wrongindices,dltensor]];

   rule=MakeRule[{dltensor[inds],expansion}];
   oldexpansion=dltensor[inds]//ExpandVertDiff[HoldExpandVertDiff->DeleteElements[ListVariationalRelationsOf[dltensor,Directed->In],{dltensor,MasterOf@dltensor}]];

   newdefinitionQ=dltensor[inds]=!=expansion&&ToCanonical[expansion-oldexpansion//ExpandVertDiff[]]=!=0; (* It has a rule now and is different from the previous one *)
   redefinitionQ=dltensor[inds]=!=oldexpansion&&ToCanonical[expansion-oldexpansion//ExpandVertDiff[]]=!=0;  (* It had a rule before and now it has a new one *)
   removalQ=dltensor[inds]=!=oldexpansion&&dltensor[inds]===expansion; (* It had an old rule and now it does not *)
     
   If[redefinitionQ,RemoveExpandVertDiffRule[dltensor[inds]]]; (* If the expansion is not the same as before, we first remove the previous Variational Relations *)
   If[removalQ&&$UndefInfoQ,(Print["** RemoveExpandVertDiffRule: Expansion formula removed for ",dltensor,"."]; If[Dagger@dltensor=!=dltensor,Print["** RemoveExpandVertDiffRule: Expansion formula removed for ",Dagger@dltensor,"."]];)];
   If[newdefinitionQ&&$DefInfoQ,Print["** GenerateExpandVertDiffRule: Expansion formula generated for ",dltensor,"."]];
 
 (* Add variational relations *)
   If[newdefinitionQ,
      If[VertDeg[dltensor]==1&&BasicVertical1FormQ[expansion],
         Module[{fieldsToAdd=DeleteDuplicates[MasterOfCPSTensor/@HeadOfTensor2/@FindAllOfType[expansion,VertDiffExact]],fieldsToRemove},
             fieldsToRemove=Complement[VertexInComponent[$VariationalGraph,MasterOfCPSTensor@dltensor,{1}],fieldsToAdd~Join~{Dagger@MasterOfCPSTensor@dltensor}];             
             RemoveVariationalRelation[#->MasterOfCPSTensor[dltensor]]&/@fieldsToRemove;
             AddVariationalRelation[#->MasterOfCPSTensor[dltensor]]&/@fieldsToAdd;
         ],
         If[VertDeg[dltensor]=!=1,
            Print["** GenerateExpandVertDiffRule: No variational relation added because ",dltensor," has VertDeg\[NotEqual]1. If necessary, they can be added manually using AddVariationalRelation."],
            Print["** GenerateExpandVertDiffRule: No variational relation added because the RHS has non-exact terms of VertDeg=1. If necessary, they can be added manually using AddVariationalRelation."]
	     ]
	  ]
	];
	
 (* Remove variational relations *)
	If[removalQ,
      If[VertDeg[dltensor]==1&&BasicVertical1FormQ[expansion],
         With[{fields=DeleteDuplicates[MasterOf/@HeadOfTensor2/@FindAllOfType[dltensor[inds]//ExpandVertDiff[HoldExpandVertDiff->DeleteElements[ListVariationalRelationsOf[dltensor,Directed->In],{dltensor,MasterOf@dltensor}]],VertDiffExact]]},
         RemoveVariationalRelation[#->MasterOfCPSTensor[dltensor]]&/@fields]
	  ]
   ]; 

   ExpandVertDiffRules[dltensor@@(MakePattern /@ {inds}),options:OptionsPattern[Options[ExpandVertDiff]]]:=FilterVertExpand[dltensor[inds],options][dltensor[inds]/.rule];
   
   If[Dagger@dltensor=!=dltensor&&$GenerateExpandVertDiffRuleDagger, (* To generate the Dagger version of the rule. Notice that no check is performed (e.g. if dltensor is real and the expansion is not or the opposite) *)
      (
      $GenerateExpandVertDiffRuleDagger=False;
      GenerateExpandVertDiffRule[{Dagger[dltensor[inds]],Dagger[expansion]},option];
      $GenerateExpandVertDiffRuleDagger=True;
      )
   ];
]

(* To remove the rule *)
RemoveExpandVertDiffRule[dltensor_?VertExactHeadQ]:=With[{dltensorwithindices=TensorWithIndices[dltensor]},GenerateExpandVertDiffRule[{dltensorwithindices,dltensorwithindices}]]
RemoveExpandVertDiffRule[dltensor_?VertExactHeadQ[inds___]]:=RemoveExpandVertDiffRule[dltensor]

Protect[GenerateExpandVertDiffRule,RemoveExpandVertDiffRule];

On[RuleDelayed::rhs]


(* ::Section:: *)
(*4.2. Rules on how to expand the VertDiff of differential operators*)


(* ::Subsection:: *)
(*4.2.1. ExpandVertDiff*)


Options[ExpandVertDiff] := {SeparateMetric->True,Explode->True,ExpandVertDiffCovD->True,ExpandVertDiffLieD->True,ExpandVertDiffBracket->True,ExpandVertDiffTotalDerivative->True,ExpandVertDiffScalarFunction->True,ConstantTensors->{},NonConstantTensors->{},HoldExpandVertDiff->None}; 
Options[ExpandVertDiffRules]:=Options[ExpandVertDiff] 
 
(* ExpandVertDiff applies ExpandVertDiffRules to all the terms with the head VertDiff or all the terms that are exact (they always have a rule for ExpandVertDiffRules). *)

ExpandVertDiff[options:OptionsPattern[Options[ExpandVertDiff]]][expr_] :=Module[{tmp},
  tmp=If[OptionValue[SeparateMetric],
    If[(expr//SeparateMetric[]//SeparateMetric[])=!=expr,ExpandVertDiff[options][expr//SeparateMetric[]//SeparateMetric[]],expr//SeparateMetric[]],
    expr];
    
  tmp=If[OptionValue[Explode],
    If[(tmp//Explode)=!=tmp,ExpandVertDiff[options][tmp//Explode],tmp],
    tmp];
    
  tmp /. {
    expr1_VertDiff :> ExpandVertDiffRules[expr1, options], (* This expands Covariant Derivaties, Lie Derivatives, and Lie Brackets. *)
    expr2_?VertExactQ :> ExpandVertDiffRules[expr2, options] (* This expands vertical exact tensors in terms of formulas previously defined *)
  }
]
  
Protect[ExpandVertDiff];


(* ::Subsection:: *)
(*4.2.2. ExpandVertDiffRules*)


 (* ExpandVertDiffRules does the heavy lift and is based on ExpandPerturbation of xPert *)
 
 (* Handles scalar expressions *)
 ExpandVertDiffRules[VertDiff[expr_Scalar],options___]:=Scalar[ExpandVertDiff[options][VertDiff[expr//NoScalar]]] 
 
 ExpandVertDiffRules[VertDiff[expr_], options:OptionsPattern[Options[ExpandVertDiff]]] := 
  Module[{sepmetric, expcov,expLie,expBracket,expScalar,exptotder,tmp,totderterms},
  {sepmetric,expcov,expLie,expBracket,expScalar,exptotder} = {SeparateMetric,ExpandVertDiffCovD,ExpandVertDiffLieD,ExpandVertDiffBracket,ExpandVertDiffScalarFunction,ExpandVertDiffTotalDerivative} /. CheckOptions[options] /. Options[ExpandVertDiff];
   
   tmp = VertDiff[If[sepmetric, SeparateMetric[][expr], expr]];
   
 (* Expanding covd is optional *)
   If[expcov, tmp = tmp /. expr1 : HoldPattern[VertDiff[_Symbol?CovDQ[_][_]]] :> ExpandVertDiffCovDFunction[expr1]];
   If[expcov, tmp = tmp /. expr1 : HoldPattern[VertDiff[ParamD[__][_]]] :> ExpandVertDiffParamDFunction[expr1]];
   If[expcov, tmp = tmp /. expr1 : HoldPattern[VertDiff[OverDot[_]]] :> ExpandVertDiffOverDotFunction[expr1]];
      
 (* Expanding Lie is optional *)
   If[expLie, tmp = tmp /. expr1 : HoldPattern[VertDiff[LieD[_][_]]] :> ExpandVertDiffLieDFunction[expr1]];
   
 (* Expanding Bracket is optional *)
   If[expBracket,tmp = tmp /. expr1 : HoldPattern[VertDiff[Bracket[_, _][_]]] :> ExpandVertDiffBracketFunction[expr1]];
   
 (* Expanding TotalDerivative is optional *)
   If[exptotder, tmp = tmp /. expr1 : HoldPattern[VertDiff[expr2_?TotalDerivativeDivergenceQ]] :> ExpandVertDiffTotalDerivativeFunction[expr1]];
   
 (* Expanding scalar functions is optional *)
   If[expScalar,tmp = tmp /. expr1 : HoldPattern[VertDiff[function_?ScalarFunctionDefinedQ[__]]] :> ExpandVertDiffScalarFunctionFunction[expr1]];
   
 (* Expanding scalar functions is optional *)
   If[expScalar,tmp = tmp /. expr1 : HoldPattern[VertDiff[partialpartial_?PartialPartialQ[___]]] :> ExpandVertDiffScalarFunctionFunction[expr1]];

   (* Reexpand *)
   
   totderterms=OnlyTotalDerivative[tmp]; (* To prevent loops with TotalDerivative terms *)
   tmp=DiscardTotalDerivative[tmp];
   
   If[tmp =!= VertDiff[expr], tmp = ExpandVertDiff[options][tmp]];
   (* Return result *)
   tmp+totderterms
   ];


(* ::Subsection:: *)
(*4.2.3. ExpandVertDiffCovDFunction*)


(* Auxiliary function extractIndices *)
extractIndices[expr_] := 
  Identity @@ 
    xAct`xTensor`Private`selecton[IndexList@#, VBundleOfIndex[#]] & /@
    Select[FindFreeIndices[expr], AIndexQ];


(* ExpandVertDiffDer uses the formula \[DifferentialD]\!\(
\*SubscriptBox[\(\[Del]\), \(a\)]
\*SubscriptBox[
SuperscriptBox[\(T\), \(b\)], \(c\)]\)=\!\(
\*SubscriptBox[\(\[Del]\), \(a\)]\(\[DifferentialD]
\*SubscriptBox[
SuperscriptBox[\(T\), \(b\)], \(c\)]\)\)+Subscript[\[CapitalGamma]^b, ad]Subscript[T^d, c]-Subscript[\[CapitalGamma]^d, ac]Subscript[T^b, d]+weight Subscript[\[CapitalGamma]^d, ad]Subscript[T^b, c]  *)
(* ExpandVertDiffDer is a faster version of: tensor -> ChangeCovD[tensor,cd,PD] -> VertDiff[%] // ChangeCovd[%,PD,cd] because \[DifferentialD]@PD=PD@\[DifferentialD] *)
ExpandVertDiffCovDFunction[VertDiff[cd_Symbol?CovDQ[-a_][expr_]]] :=Module[
	{
	freeindices= extractIndices[expr],
	result
	}, 
	result = cd[-a][VertDiff[expr]] + Plus @@ (Map[xAct`xTensor`Private`addChristoffel[expr, -a], freeindices] /. expr1_ xAct`xTensor`Private`CHR[indices__] :>  WWedge[VertDiff[Christoffel[cd][indices]],expr1]);
	  
	result + Module[{basis = WeightedWithBasis[cd], dummy = DummyIn@VBundleOfIndex[a], weight}, 
	    weight = WeightOf[expr, basis];
	    If[weight === 0, 
	       0, 
	       weight With[{chr = Christoffel[cd, xAct`xCoba`PDOfBasis[basis]][dummy, -a, -dummy]}, 
	        -WWedge[VertDiff[chr],expr] 
	      ]
	    ]
	  ]
	]


(* ::Subsection:: *)
(*4.2.4. ExpandVertDiffParamDFunction*)


ExpandVertDiffParamDFunction[HoldPattern[VertDiff[ParamD[parameter__][expr_]]]] :=ParamD[parameter][VertDiff[expr]]


(* ::Subsection:: *)
(*4.2.5. ExpandVertDiffOverDotFunction*)


ExpandVertDiffOverDotFunction[HoldPattern[VertDiff[OverDot[expr_]]]] :=OverDot[VertDiff[expr]]


(* ::Subsection:: *)
(*4.2.6. ExpandVertDiffLieDFunction*)


(* \[DifferentialD](Subscript[L, V](exp))=Subscript[L, \[DifferentialD]V](expr)+L(\[DifferentialD]expr) *)
ExpandVertDiffLieDFunction[VertDiff[LieD[v_][expr_]]] :=LieD[VertDiff@v][expr]+(-1)^VertDeg[v] LieD[v][VertDiff@expr]


(* ::Subsection:: *)
(*4.2.7. ExpandVertDiffBracketFunction*)


ExpandVertDiffBracketFunction[VertDiff[Bracket[v1_, v2_][a_Symbol]]] := Bracket[VertDiff@v1, v2][a]+(-1)^VertDeg[v1] Bracket[v1,VertDiff@v2][a]


(* ::Subsection:: *)
(*4.2.8. ExpandVertDiffTotalDerivativeFunction*)


ExpandVertDiffTotalDerivativeFunction[HoldPattern[VertDiff[expr_?TotalDerivativeDivergenceQ]]] :=Module[{info=TotalDerivativeDivergenceInfo[expr],freeindex},freeindex=-1*List@@FindFreeIndices[Evaluate[info[[4]]]];info[[3]][info[[2]]@@freeindex(VertDiff[ info[[4]]])]]


(* ::Subsection:: *)
(*4.2.9. ExpandVertDiffScalarFunction*)


ExpandVertDiffScalarFunctionFunction[HoldPattern[VertDiff[f_?ScalarFunctionDefinedQ[fields__]]]]:=Module[
	{ListOfSummands}, (* This ensures that the argument can be also PD@tensor *)
	
	(* This ensures that the PartialPartial[function,tensor] is defined *)
	(If[!PartialPartialQ[PartialPartial[f][{HeadOfTensor2@#}] ],DefPartialPartial[f,{HeadOfTensor2@#}]]&)/@Flatten[{fields}];
	
	ListOfSummands=(TensorWithIndices/@DeleteDuplicates@Flatten[{fields}]) /.{head_?xTensorQ[inds___]:>{(PartialPartial[f][head] @@(Times[#,-1]&/@IndexList[inds]))~WWedge~VertDiff[head[inds]]}};
	ListOfSummands//Flatten//adding
]


OrderOfPartialPartial[_]:=0

ExpandVertDiffScalarFunctionFunction[HoldPattern[VertDiff[tensor_?PartialPartialQ[indices___]]]]:=Module[
	{
	order=OrderOfPartialPartial[tensor],
	list,
	function=FunctionOfPartialPartial[tensor],
	tensors=TensorsOfPartialPartial[tensor],
	dependenciesofscalar,
	ListOfSummands
	},
	dependenciesofscalar=DependenciesOfScalar[function];
	dependenciesofscalar=Union[tensors~Join~If[ListQ[dependenciesofscalar],dependenciesofscalar,{}]];
	list=Prepend[{tensors},#]&/@dependenciesofscalar; (* list of tensors wrt which we take the partial derivative of function *)

	(* If the required tensors are not defined, we define them here *)
	(If[!PartialPartialQ[PartialPartial[function][Flatten[{HeadOfTensor2/@Flatten[#]}]] ],DefPartialPartial[function,Flatten[{HeadOfTensor2/@Flatten[#]}]]]&)/@list;

	ListOfSummands=(TensorWithIndices/@dependenciesofscalar) /.{head_?xTensorQ[inds___]:>
			Module[{partialtensor,extendedindices,position},
			
				partialtensor=PartialPartial[function][Flatten[{head,tensors}]];
				
				 (* {tensors appearing in the denominator of the new tensor , Placement of the contracted indices } *)
				position=Transpose[{TensorsOfPartialPartial[partialtensor],PlacementOfIndices[partialtensor]}];
				
				extendedindices=Insert[{indices},-1*{inds},Last@SelectFirst[position,#[[1]]===head &]]//Flatten;
				
				{(partialtensor@@extendedindices)~WWedge~VertDiff[head[inds]]}
			]
		}; (* Chain rule *)
		
	ListOfSummands//Flatten//adding
]


(* ::Section:: *)
(*4.3. Rules on how to expand the VertInt of differential operators*)


(* ::Subsection:: *)
(*4.3.1. ExpandVertInt*)


(* Similar to ExpandVertDiff but it expands VertInt. Some formulas are similar, but then we have the specific values defined by vvf *)

Options[ExpandVertInt] :=Options[ExpandVertDiff]~Join~{ExpandVertIntCovD->True,ExpandVertIntLieD->True,ExpandVertIntBracket->True,ExpandVertIntTotalDerivative->True,HoldExpandVertInt->None}; 
Options[ExpandVertDiffRules]:=Options[ExpandVertInt] 

ExtractComponentsVertInt[expr_]:=Join[
	ComponentsOfVVF/@(Cases[expr,VertInt[_?VVFQ],{0,DirectedInfinity[1]},Heads->True]/.{VertInt[vvf_]:>vvf}),
	ComponentsOfGeneralizedVVF/@(Cases[expr,VertInt[_?GeneralizedVVFQ],{0,DirectedInfinity[1]},Heads->True]/.{VertInt[vvf_]:>vvf})
	]//Flatten

ExpandVertInt[options:OptionsPattern[Options[ExpandVertInt]]][expr_] :=Module[
    {
	components=ExtractComponentsVertInt[expr],
	HoldExpand,opts
	},
	
	HoldExpand=Flatten[{OptionValue[HoldExpandVertDiff]}~Join~components];
	opts=DeleteCases[FilterRules[{options},Options[ExpandVertInt]],HoldExpandVertDiff->_]~Join~{HoldExpandVertDiff->HoldExpand}//Flatten;
	
	(expr//ExpandVertDiff[FilterRules[{opts},Options[ExpandVertDiff]]]//ExpandAll) /. {
		expr1: HoldPattern[VertInt[vvf1_][arg1_Bracket[inds1__]]] :> (ExpandVertIntRules@@opts)[VertInt[vvf1][arg1[inds1]]],
		expr2: HoldPattern[VertInt[vvf2_][arg2_?xTensorQ[inds2___]]] :> (ExpandVertIntReplace@@opts)[VertInt[vvf2][arg2[inds2]]],
		expr3: HoldPattern[VertInt[vvf3_][arg3_]] :> (ExpandVertIntRules@@opts)[VertInt[vvf3][arg3]]
		}
	]
 
Protect[ExpandVertInt];


(* ::Subsection:: *)
(*4.3.2. ExpandVertIntRules*)


 (* ExpandVertDiffRules does the heavy lift. Similar to ExpandVertDiffRules *)
 
 (* Handles scalar expressions *)
 ExpandVertIntRules[options:OptionsPattern[Options[ExpandVertInt]]][HoldPattern[VertInt[v_][expr_Scalar]]]:=Scalar[ExpandVertInt[options][VertInt[v][expr//NoScalar]]] 

 ExpandVertIntRules[options:OptionsPattern[Options[ExpandVertInt]]][HoldPattern[VertInt[v_][expr_]]] := 
  Module[{sepmetric, expcov,exptotder,expLie,expBracket,tmp},
  {sepmetric,expcov,exptotder,expLie,expBracket} = {SeparateMetric,ExpandVertIntCovD,ExpandVertIntTotalDerivative,ExpandVertIntLieD,ExpandVertIntBracket} /. CheckOptions[options] /. Options[ExpandVertInt];
  tmp = VertInt[v][If[sepmetric, SeparateMetric[][expr], expr]];

 (* Expanding covd is optional *)
   If[expcov, tmp = tmp /. expr1 : HoldPattern[VertInt[_][_Symbol?CovDQ[_][_]]] :> ExpandVertIntCovDFunction[expr1]];
   If[expcov, tmp = tmp /. expr1 : HoldPattern[VertInt[_][ParamD[__][_]]] :> ExpandVertIntParamDFunction[expr1]];
   If[expcov, tmp = tmp /. expr1 : HoldPattern[VertInt[_][OverDot[_]]] :> ExpandVertIntOverDotFunction[expr1]];

 (* Expanding TotalDerivative is optional *)
   If[exptotder, tmp = tmp /. expr1 : HoldPattern[VertInt[_][_Symbol?TotalDerivativeQ[_]]] :> ExpandVertIntTotalDerivativeFunction[expr1]];
   
 (* Expanding Lie is optional *)
   If[expLie, tmp = tmp /. expr1 : HoldPattern[VertInt[_][LieD[_][_]]] :> ExpandVertIntLieDFunction[expr1]];
   
 (* Expanding Bracket is optional *)
   If[expBracket,tmp = tmp /. expr1 : HoldPattern[VertInt[_][Bracket[_, _][_]]] :> ExpandVertIntBracketFunction[expr1]];
      
   (* Reexpand *)
   If[tmp =!= VertInt[v][expr], tmp = ExpandVertInt[options][tmp]];
  
   (* Return result *)
   tmp
   ];


(* ::Subsection:: *)
(*4.3.3. ExpandVertIntCovDFunction*)


ExpandVertIntCovDFunction[HoldPattern[VertInt[vv_][cd_Symbol?CovDQ[-a_][expr_]]]] :=cd[-a][VertInt[vv][expr]]


(* ::Subsection:: *)
(*4.3.4. ExpandVertIntParamDFunction*)


ExpandVertIntParamDFunction[HoldPattern[VertInt[vv_][ParamD[parameter__][expr_]]]] :=ParamD[parameter][VertInt[vv][expr]]


(* ::Subsection:: *)
(*4.3.5. ExpandVertIntOverDotFunction*)


ExpandVertIntOverDotFunction[HoldPattern[VertInt[vv_][OverDot[expr_]]]] :=OverDot[VertInt[vv][expr]]


(* ::Subsection:: *)
(*4.3.6. ExpandVertIntTotalDerivativeFunction*)


ExpandVertIntTotalDerivativeFunction[HoldPattern[VertInt[vv_][totder_Symbol?TotalDerivativeQ[expr_]]]] :=totder[VertInt[vv][expr]]


(* ::Subsection:: *)
(*4.3.7. ExpandVertIntLieDFunction*)


(* \[DifferentialD](Subscript[L, V](exp))=Subscript[L, \[DifferentialD]V](expr)+L(\[DifferentialD]expr) *)
ExpandVertIntLieDFunction[HoldPattern[VertInt[vv_][LieD[v_][expr_]]]] :=LieD[VertInt[vv][v]][expr]+(-1)^(VertDeg[VertInt[vv]]VertDeg[v]) LieD[v][VertInt[vv][expr]]


(* ::Subsection:: *)
(*4.3.8. ExpandVertIntBracketFunction*)


ExpandVertIntBracketFunction[HoldPattern[VertInt[vv_][Bracket[v1_, v2_][a_Symbol]]]] := Bracket[VertInt[vv][v1], v2][a]+(-1)^(VertDeg[VertInt[vv]]VertDeg[v1]) Bracket[v1,VertInt[vv][v2]][a]


(* ::Chapter:: *)
(*5. Definition of variational vectors and variational vector fields (VVF)*)


(* ::Section:: *)
(*5.1. Variational vector*)


(* ::Subsection:: *)
(*5.1.1. VariationalVector Head*)


DefInertHead[VariationalVector,LinearQ->True,ContractThrough->{delta},PrintAs->"(\!\(\*FractionBox[\(\[Delta]\), \(\[Delta](\[CenterDot])\)]\))",DefInfo->Null]


(* ::Subsection:: *)
(*5.1.2. VariationalVectorQ*)


(* If a tensor has been defined as a variational vector and has the right indices *)
VariationalVectorQ[tensor_?VariationalVectorQ[inds___]]:=(SlotsOfTensor[tensor]===xAct`xTensor`Private`SignedVBundleOfIndex/@{inds})
VariationalVectorQ[_]:=False

Protect[VariationalVectorQ];


(* ::Subsection:: *)
(*5.1.3. Definition of variational vector*)


(* DefVariationalVector[tensor] defines a tensor with the opposite indices of the tensor. The metric is handled separtely as the VariationalVector of the inverse needs to be defined *) 

Options[DefVariationalVector]:= Options[DefExactVerticalForm]

DefVariationalVector[tensor_Symbol?xTensorQ[inds___],dependencies_,sym_,options:OptionsPattern[Options[DefVariationalVector]]] := Module[
	{
	opts={WeightOfTensor->-WeightOfTensor[tensor],VertDeg->-VertDeg[tensor[inds]]}~Join~{options},
	nameVar=GenerateVariationalName[tensor,FilterRules[{options},Options[GenerateDiffName]]],
    flippedindices = -1*{inds}
    },
        
    MakexTensions[DefVariationalVector,"Beginning",tensor[inds],dependencies,sym,options]; 
	opts=If[xAct`xTensor`Private`ImplodedQ[tensor],opts~Join~{DefInfo:>If[$ImplodeInfoQ,{"tensor",""},False]},opts];
    
    Unprotect[VariationalVector];
    
    VariationalVectorQ[ToExpression[nameVar[[1]]]] ^= True;
    MasterOfCPSTensor[ToExpression[nameVar[[1]]]]^=tensor;
       
    If[!StringEndsQ[ToString@tensor,"\[Dagger]"],
        DefTensor[ToExpression[nameVar[[1]]] @@ flippedindices, dependencies, sym, PrintAs -> nameVar[[2]],FilterRules[opts,Options[DefTensor]],DefineExactFormAfterDefTensor -> False,DefineVariationalVectorAfterDefTensor->False];
        ];
        
	If[OptionValue[PrintInverse],
	    (
	    VariationalVector[ToExpression["Inv"<>ToString[tensor]]]= ToExpression[nameVar[[1]]];
	    If[DaggerQ[tensor],
			VariationalVector[ToExpression["Inv"<>ToString[tensor]<>"\[Dagger]"]]=Dagger@ToExpression[nameVar[[1]]];
			VariationalVectorQ[Dagger@ToExpression[nameVar[[1]]]]^=True;
		];
	    ),
		(
		VariationalVector[tensor] ^= ToExpression[nameVar[[1]]];
		PrintAs[Evaluate[ToExpression[VariationalVector[tensor]]]]^=nameVar[[2]];(* This prevents the densities to have two sets of tildes, only the ones coming from the tensor remained *) 
		)
    ];

          
    (* Establish relationships between the exact tensor and the original tensor *)
    xAct`xTensor`Private`SymbolRelations[ToExpression[nameVar[[1]]],tensor,{tensor}];
	
    $printAddVariationalRelation=False;
    AddVariationalRelation[tensor -> ToExpression[nameVar[[1]]]];
    $printAddVariationalRelation=True;
    
    Protect[VariationalVector];
    
    MakexTensions[DefVariationalVector,"End",tensor[inds],dependencies,sym,options];
]


(* ::Subsection:: *)
(*5.1.4. PartialPartialQ*)


(* If a tensor has been defined as a variational vector and has the right indices *)
PartialPartialQ[tensor_?PartialPartialQ[inds___]]:=(SlotsOfTensor[tensor]===xAct`xTensor`Private`SignedVBundleOfIndex/@{inds})
PartialPartialQ[_]:=False

Protect[PartialPartialQ];


(* ::Subsection:: *)
(*5.1.5. Definition of PartialPartial*)


(* DefPartialPartial[tensor] defines a tensor with the opposite indices of the tensor. The metric is handled separtely as the PartialPartial of the inverse needs to be defined *) 

IndexRanges[list:{_List..}]:=Map[Function[{pair},Range[pair[[1]],pair[[1]]+pair[[2]]-1]],list]
applyShifts[zs_List,shifts_List]:=MapThread[#1[[2]]/.{x_?IntegerQ/;x>0:>x+#2}&,{zs,shifts}]
JoinGenSet[list_]:=list/.{GenSet->List}//Flatten
GenerateCycles[{firstlist_List,rest__}]:=Inner[List,firstlist,#,Cycles]&/@{rest}


DefPartialPartial[function_?ScalarFunctionQ,fields__]:=Module[{partialinvMetric,partialMetric,booleanlist,tensors,sign,flippedindices,daggerpartialinvMetric,daggerpartialMetric,fullList,numberOfImaginaryTensors,numberOfComplex,numberOfHermitian,numberOfAntihermitian,dag}, 

    MakexTensions[DefPartialPartial,"Beginning",function,fields]; 
    
    tensors=Sort@Flatten[{fields}]; (* fields sorted by alphabetical order *)
    fullList=tensors~Join~{function};
    
    numberOfImaginaryTensors=Count[Dagger/@fullList,MultiplyHead[_,_]];
    numberOfComplex=Count[fullList,_?((Dagger[#]/. {MultiplyHead[_,tensor_]:>tensor})=!=#&)];
	numberOfHermitian=Count[tensors,_?HermitianQ];
	numberOfAntihermitian=Count[tensors,_?AntihermitianQ];
	
	dag = Which[
		  numberOfComplex == 0 && numberOfImaginaryTensors == 0, Real,
		  numberOfComplex == 0 && EvenQ[numberOfImaginaryTensors],Real,
		  numberOfComplex == 0 && OddQ[numberOfImaginaryTensors],Imaginary,
		  numberOfComplex =!= 0 && numberOfComplex =!= numberOfHermitian + numberOfAntihermitian,Complex,
		  numberOfComplex =!= 0 && numberOfComplex === numberOfHermitian + numberOfAntihermitian && EvenQ[numberOfAntihermitian],Hermitian,
		  numberOfComplex =!= 0 && numberOfComplex === numberOfHermitian + numberOfAntihermitian && OddQ[numberOfAntihermitian],Antihermitian
	];
	
	booleanlist=(MetricQ[#]&&Inv[#]===#)&/@tensors; (* Some main metric *)
	flippedindices=DummyIn/@((-1*SlotsOfTensor/@tensors)//Flatten);
	If[!Or@@booleanlist,
		DefPartialPartialAUX[function,flippedindices,tensors,PrintInverse -> False,Dagger->dag], (* No main metric involved *) 
		(
		sign=(-1)^(Select[booleanlist, # == True &] // Length);
		DefPartialPartialAUX[function,flippedindices,tensors,PrintInverse -> False,Dagger->dag];
		
		flippedindices=DummyIn/@((If[MetricQ[#]&&Inv[#]===#,1,-1]*SlotsOfTensor[#]&/@tensors)//Flatten); (* Flips all indices except the corresponding to main metrics (equivalent to flipping the indices of the INVERSE metric) *) 
		DefPartialPartialAUX[function,flippedindices,tensors,PrintInverse -> True,Dagger->dag];
		
		partialinvMetric=PartialPartial[function][ToExpression[InvName[#,PrintInverse -> True]&/@tensors]];
		partialMetric=PartialPartial[function][tensors];
		daggerpartialinvMetric=PartialPartial[Dagger@function/.{MultiplyHead[_,name_]:>name}][Dagger/@ToExpression[InvName[#,PrintInverse -> True]&/@tensors]/.{MultiplyHead[_,name_]:>name}];
		daggerpartialMetric=PartialPartial[Dagger@function/.{MultiplyHead[_,name_]:>name}][Dagger/@tensors/.{MultiplyHead[_,name_]:>name}];

		(* Generate automatic rules to turn PartialFunctionPartialInvg into  PartialFunctionPartialg (or vice-versa) *)
		With[{partialinvMetricSym = partialinvMetric, partialMetricSym = partialMetric,daggerpartialinvMetricSym=daggerpartialinvMetric,daggerpartialMetricSym = daggerpartialMetric},
			partialinvMetricSym/: partialinvMetricSym[inds___] /; $UseInverseMetric == False :=sign partialMetricSym[inds];
			partialMetricSym/: partialMetricSym[inds___] /; $UseInverseMetric == True :=sign partialinvMetricSym[inds];
			daggerpartialinvMetricSym/: daggerpartialinvMetricSym[inds___] /; $UseInverseMetric == False :=sign daggerpartialMetricSym[inds];
			daggerpartialMetricSym/: daggerpartialMetricSym[inds___] /; $UseInverseMetric == True :=sign daggerpartialinvMetricSym[inds];
		];
		

		)
	];
		
	With[{tensorsdependencies=TensorsOfPartialPartial[PartialPartial[function][fields]]},

		If[numberofcomplex===numberofhermitian+numberofantihermitian&&EvenQ[numberofantihermitian],
			HermitianQ[]]
	];
	
    MakexTensions[DefPartialPartial,"End",function,fields]; 
	
]

Protect[DefPartialPartial];


Options[DefPartialPartialAUX]:= Options[DefTensor]~Join~{PrintInverse -> False}

DefPartialPartialAUX[function_?ScalarFunctionQ,flippedindices_,fields_List,options:OptionsPattern[Options[DefPartialPartialAUX]]] := Module[
	{finaltensor,daggerfinaltensor,daggerfunction,daggerfields,NameVar,NameVarDagger,Dependencies,Grade,Weight,NumberOfIndices,NumberOfAccumulatedIndices,symmetries,tensorindices,aux,repeatedtensorswithindices,exchangerepeated,NumberOfImaginaryTensors},     
    
    MakexTensions[DefPartialPartialAUX,"Beginning",function,fields,options]; 
    
    daggerfunction=Dagger@function/.{MultiplyHead[_,name_]:>name};
    daggerfields=Sort[Dagger/@fields/.{MultiplyHead[_,name_]:>name}];
	NameVar=GeneratePartialPartialName[function,fields,FilterRules[{options},Options[GeneratePartialPartialName]]];
	NameVarDagger=GeneratePartialPartialName[daggerfunction,daggerfields,FilterRules[{options},Options[GeneratePartialPartialName]]];
	
	Dependencies=Flatten[DependenciesOfTensor/@fields]//DeleteDuplicates;
	Grade=VertDeg[function]-VertDeg/@fields // adding;
	Weight=WeightOf[function]-WeightOfTensor/@fields // adding;
	
	NumberOfIndices=Length/@SlotsOfTensor/@fields;
	NumberOfAccumulatedIndices=Most@FoldList[Plus,0,NumberOfIndices];
	
	repeatedtensorswithindices=GatherBy[Transpose[{NumberOfAccumulatedIndices+1,NumberOfIndices,fields}],Last]; (* We keep track of repeated tensors since they can be exchanged, notice that they are always consecutive sin fields is ordered *)
	repeatedtensorswithindices=Map[Most /@ # &, repeatedtensorswithindices]; (* We remove the tensor that is stored in the last coordinate *)
    repeatedtensorswithindices=Select[repeatedtensorswithindices,Length[#]>1&&First[#][[2]]>0&];
    exchangerepeated=GenerateCycles/@IndexRanges/@repeatedtensorswithindices//Flatten;
     
	symmetries=GenSet@@(exchangerepeated~Join~JoinGenSet@applyShifts[SymmetryGroupOfTensor/@fields,NumberOfAccumulatedIndices]);  (* We add the symmetries of each tensor and also the exchange of repeated tensors *)
	symmetries=StrongGenSet[Array[Identity,Last@NumberOfAccumulatedIndices+Length@SlotsOfTensor@Last@fields],symmetries];
		
       
   xTensorQ[aux]^=True; (* Little trick to force nice indices in DefInfoQ. I am sure there must be a better way *)
   SlotsOfTensor[aux]^=VBundleOfIndex/@flippedindices;
   tensorindices=ToExpression[NameVar[[1]]] @@( Evaluate[aux @@ (flippedindices)//ScreenDollarIndices]/.{aux->IndexList});
   Clear[aux];
   
	
   If[OptionValue[PrintInverse],
	 (
	 PartialPartial[function][ToExpression/@(InvName[#,PrintInverse -> True]&/@fields)]= ToExpression[NameVar[[1]]];
	 PartialPartial[daggerfunction][ToExpression/@(InvName[#,PrintInverse -> True]&/@daggerfields)]= ToExpression[NameVarDagger[[1]]];
	 ),
	 (
	 PartialPartial[function][fields]= ToExpression[NameVar[[1]]];
	 PartialPartial[daggerfunction][daggerfields]= ToExpression[NameVarDagger[[1]]];
	 )
   ];
   
   (* These two definitions are needed before DefTensor for Complex tensors *)
   NameOfDaggerPartialPartial[ToExpression[NameVar[[1]]]] ^=ToExpression[NameVarDagger[[1]]];
   PartialPartialQ[ToExpression[NameVar[[1]]]] ^= True;
   PartialPartialQ[ToExpression[NameVarDagger[[1]]]] ^= True;

   DefTensor[tensorindices,Dependencies,symmetries,FilterRules[{options},Options[DefTensor]], PrintAs -> NameVar[[2]], VertDeg -> Grade, WeightOfTensor -> Weight,DefineExactFormAfterDefTensor->False,DefineVariationalVectorAfterDefTensor->False];

   NumberOfImaginaryTensors=Count[Dagger/@Join[fields,{function}],MultiplyHead[_,_]];
   NumberOfImaginaryOfPartialPartial[ToExpression[NameVar[[1]]]] ^= NumberOfImaginaryTensors;
   NumberOfImaginaryOfPartialPartial[ToExpression[NameVarDagger[[1]]]] ^= NumberOfImaginaryTensors;
      
   OrderOfPartialPartial[ToExpression[NameVar[[1]]]] ^= Length@fields;
   OrderOfPartialPartial[ToExpression[NameVarDagger[[1]]]] ^= Length@fields;
      
   FunctionOfPartialPartial[ToExpression[NameVar[[1]]]] ^= function;
   FunctionOfPartialPartial[ToExpression[NameVarDagger[[1]]]] ^= daggerfunction;
      
   PartialPartialsOfFunction[function]^=PartialPartialsOfFunction[function]~Union~{ToExpression[NameVar[[1]]]};
   PartialPartialsOfFunction[daggerfunction]^=PartialPartialsOfFunction[daggerfunction]~Union~{ToExpression[NameVarDagger[[1]]]};
      
   Scan[(PartialPartialsOfTensor[#] ^= PartialPartialsOfTensor[#]~Union~{ToExpression[NameVar[[1]]]}) &, fields];
   Scan[(PartialPartialsOfTensor[#] ^= PartialPartialsOfTensor[#]~Union~{ToExpression[NameVarDagger[[1]]]}) &, daggerfields];
      
   PlacementOfIndices[ToExpression[NameVar[[1]]]] ^= NumberOfAccumulatedIndices+1;
   PlacementOfIndices[ToExpression[NameVarDagger[[1]]]] ^= NumberOfAccumulatedIndices+1;
      
   TensorsOfPartialPartial[ToExpression[NameVar[[1]]]] ^= fields;
   TensorsOfPartialPartial[ToExpression[NameVarDagger[[1]]]] ^= daggerfields;
      
   PrintAs[Evaluate[ToExpression[NameVar[[1]]]]]^=NameVar[[2]]; (* This prevents the densities to have two sets of tildes, only the ones coming from the tensor remained *) 
   PrintAs[Evaluate[ToExpression[NameVarDagger[[1]]]]]^=NameVarDagger[[2]]; (* This prevents the densities to have two sets of tildes, only the ones coming from the tensor remained *) 
      
   finaltensor=PartialPartial[function][fields];
   daggerfinaltensor=PartialPartial[daggerfunction][daggerfields];
   If[OddQ[NumberOfImaginaryTensors],
      With[{finaltensorSym = finaltensor, daggerfinaltensorSym = daggerfinaltensor},
	    finaltensorSym/: Dagger[finaltensorSym] :=MultiplyHead[-1,daggerfinaltensorSym];
	    daggerfinaltensorSym/: Dagger[daggerfinaltensorSym] :=MultiplyHead[-1,finaltensorSym];
	  ]
    ];
       
    MakexTensions[DefPartialPartialAUX,"End",function,fields,options]; 
]


PartialPartial[function_][fields__]/;!(fields===Flatten[{fields}]):=PartialPartial[function][Sort[{fields}]]
PartialPartial[function_][fields_List]/;Sort[fields]=!=fields:=PartialPartial[function][Sort[fields]]


(* ::Section:: *)
(*5.2. Variational vector fields (VVF)*)


(* ::Subsection:: *)
(*5.2.1. VVFQ*)


VVFQ[expr_]:=VVFQaux[expr//ExpandAll]

VVFQaux[expr_Scalar]:=VVFQaux[NoScalar[expr]]
VVFQaux[expr_]/; !ScalarQ[expr] :=False
VVFQaux[expr_Plus]:=And@@(VVFQaux/@List@@expr)
VVFQaux[summand_] /; (Length@FindAllOfType[summand, VariationalVector] === 1 &&
  FreeQ[summand,(CovD[_][x_?VariationalVectorQ]|LieD[_][x_?VariationalVectorQ]|VertDiff[x_?VariationalVectorQ]|VertInt[_][x_?VariationalVectorQ]|VertLie[_][x_?VariationalVectorQ]|InertHead[x_?VariationalVectorQ])]) := True
VVFQaux[_]:=False

Protect[VVFQ];


(* ::Subsection:: *)
(*5.2.2. InfoFromVVF*)


(* Turns a VariationalVectorField into three lists:
	- the first one gathers the components of VVF written as  pairs: {{Subscript[VariationalVectorTensor, 1][Subscript[inds, 1]],Subscript[Coefficient, 1][-Subscript[inds, 1]]},...,{Subscript[VariationalVectorTensor, k][Subscript[inds, k]],Subscript[Coefficient, k][-Subscript[inds, k]]}}
	- the second one is the same but exchanging Subscript[VariationalVectorTensor, i] by Subscript[dlTensor, i]: {{Subscript[dlTensor, 1][Subscript[inds, 1]],Subscript[Coefficient, 1][-Subscript[inds, 1]]},...,{Subscript[dlTensor, k][Subscript[inds, k]],Subscript[Coefficient, k][-Subscript[inds, k]]}}
    - the third one gathers the Heads of the components: {Subscript[Tensor, 1],...,Subscript[Tensor, k]} *)

InfoFromVVF::unknown="`1` is not a variational vector field.";

InfoFromVVF[vvf_]/;!VVFQ[vvf]:=Throw@Message[InfoFromVVF::unknown,vvf]

InfoFromVVF[vvf_?VVFQ]:=Module[
	{
	listcomponents=FindAllOfType[vvf//ToCanonical[#,UseMetricOnVBundle->None]&//Simplify,VariationalVector], (* ToCanonical is included to quickly check if some of the components actually vanished *)
	listcomponentsNoDuplicate,
	listofcoefficients,
	listwithvariationalvectors,
	listtensors,
	listwithdltensors
	},
	listcomponentsNoDuplicate=(TensorWithIndices/@Head/@DeleteDuplicatesTensors[listcomponents])/.{-vv_?VariationalVectorQ[inds___]:>vv[inds]}; (* To handle $UseInverseMetric *)(* To ensure the correct position of indices *)
	listofcoefficients=(IndexCoefficient[vvf,#]&)/@listcomponentsNoDuplicate; 
	listtensors=MasterOf/@Head/@listcomponentsNoDuplicate;
	listwithvariationalvectors=Transpose[{listcomponentsNoDuplicate,listofcoefficients}];
	listwithdltensors=listwithvariationalvectors/. head_?VariationalVectorQ[inds___]:>(VertDiff[MasterOfCPSTensor[head]@@(Times[#, -1] & /@{ inds})])/.{-vv_?VertExactHeadQ[indices___]:>vv[indices]}(* To handle $UseInverseMetric *);
	{listwithvariationalvectors,listwithdltensors,listtensors}
]


CoefficientsOfVVF::unknown=" `1`is not a variational vector field.";
ComponentsOfVVF::unknown="`1` is not a variational vector field.";

(* CoefficientsOfVVF extracts the first list of InfoFromVVF. ComponentsOfVVF extracts the last list of InfoFromVVF. *)
CoefficientsOfVVF[vvf_?VVFQ]:=InfoFromVVF[vvf][[1]]
ComponentsOfVVF[vvf_?VVFQ]:=InfoFromVVF[vvf][[3]]

CoefficientsOfVVF[vvf_]/;!VVFQ[vvf]:=Throw@Message[CoefficientsOfVVF::unknown,vvf]
ComponentsOfVVF[vvf_]/;!VVFQ[vvf]:=Throw@Message[ComponentsOfVVF::unknown,vvf]
 
Protect[CoefficientsOfVVF,ComponentsOfVVF];


(* ::Subsection:: *)
(*5.2.3. VVFFromList*)


VVFFromList::notvvf="The list does not generate a variational vector field";

(* VVFFromList is the "Inverse" of CoefficientsOfVVF. It generates the VVF from a list {Subscript[VariationalVectorTensor, 1][Subscript[inds, 1]],Subscript[Coefficient, 1][-Subscript[inds, 1]]},...,{Subscript[VariationalVectorTensor, k][Subscript[inds, k]],Subscript[Coefficient, k][-Subscript[inds, k]]} by WWedge multiplying each pair and adding them all *)
VVFFromList[list_]:=Module[{vvf=Total[WWedge@@@list]},
	If[VVFQ[vvf],vvf,Throw@Message[VVFFromList::notvvf]]]

Protect[VVFFromList];


(* ::Subsection:: *)
(*5.2.4. Definition of a concrete VVF (the one given by the Lie derivative of a fixed vector field)*)


(* VVFFromLieD[vector][list] defines the VVF Subscript[L, vector[ind]]Subscript[Tensor, 1][Subscript[inds, 1]]}~WWedge~Subscript[VariationalVectorTensor, 1][-Subscript[inds, 1]]+...+Subscript[L, vector[ind]]Subscript[Tensor, k][Subscript[inds, k]]}~WWedge~Subscript[VariationalVectorTensor, k][-Subscript[inds, k]] *)
VVFFromLieD[v_?xTensorQ][wrt__]:=VVFFromLieD[TensorWithIndices@v][wrt]

VVFFromLieD[vector_][wrt__]:=Module[
	{
	fields=Flatten[{wrt}],
	ListOfSummands
	},
	
	ListOfSummands=(TensorWithIndices/@fields) /.{head_?xTensorQ[inds___]:>{LieD[vector][head[inds]]~WWedge~(VariationalVector[head]@@(Times[#,-1]&/@IndexList[inds]))}};
	ListOfSummands//Flatten//adding
]

Protect[VVFFromLieD];


(* ::Section:: *)
(*5.3. Generalized variational vector fields (GVVF)*)


PossibleRuleQ[{tensor1_?xTensorQ[inds___],expr_}]:=List@@xAct`xTensor`Private`TakeEIndices[FindFreeIndices[tensor1[inds]]]===List@@xAct`xTensor`Private`TakeEIndices[FindFreeIndices[expr]]


(* DefGeneralizedVVF creates a VVF as a list of replacements for k-vertical-forms *)

Options[DefGeneralizedVVF]={VanishOverOtherForms->True,ProtectNewSymbol:>$ProtectNewSymbols,DefInfo->{"the generalized vector field",""},Validate->True};
DefGeneralizedVVF::badinput = "Each element must be a two-element pair: `1`.";
DefGeneralizedVVF::ZeroVertDeg = "No VertInt rule can be defined on forms with ZeroVertDeg: `1`.";
DefGeneralizedVVF::IndicesMissmatch = "Indices missmatch: `1`.";
DefGeneralizedVVF::RepeatedElements = "Repeated elements: `1`.";

DefGeneralizedVVF[name_Symbol,options:OptionsPattern[Options[DefGeneralizedVVF]]][pairs_List?MatrixQ]:=DefGeneralizedVVF[name,options]@@pairs

DefGeneralizedVVF[name_Symbol,options:OptionsPattern[Options[DefGeneralizedVVF]]][pairs__]:=Catch@Module[{failed1,failed2,failed3,failed4,list={pairs},intersection,pns,val,info},
	{pns,info,val}=OptionValue[{ProtectNewSymbol,DefInfo,Validate}];
	
    MakexTensions[DefGeneralizedVVF,"Beginning",name,pairs,options]; 
	If[val,
		ValidateSymbol[name];
		ValidateSymbolInSession[name]
	];
	
	If[!And@@(ListQ[#]&&Length[#]===2&/@list),Throw@Message[DefGeneralizedVVF::badinput,list]];
	
	failed2=Select[list,ZeroVertDegQ[#[[1]]]&]; 
	If[failed2=!={},Throw[Message[DefGeneralizedVVF::ZeroVertDeg,failed2]];];
	
	failed3=Select[GatherBy[list,Head@First[#]&],Length[#]>1&]; 
	If[failed3=!={},Throw[Message[DefGeneralizedVVF::RepeatedElements,failed3]];];
	
	failed1=Select[list,Not[PossibleRuleQ[#]]&]; 
	If[failed1=!={},Throw[Message[DefGeneralizedVVF::IndicesMissmatch,failed1[[1]]]];];
	
	GeneralizedVVFQ[name]^=True;
	ComponentsOfGeneralizedVVF[name]^=Head/@First/@list;
	RulesOfGeneralizedVVFQ[name]^=MakeRule/@list;
	VanishOverOtherForms[name]^=OptionValue[VanishOverOtherForms];
	DefInfo[name]^=info;
	AppendToUnevaluated[$GeneralizedVVF,name];
	
	xAct`xTensor`Private`MakeDefInfo[DefGeneralizedVVF,name,info];
	
	intersection=ComponentsOfGeneralizedVVF[name]\[Intersection](VertDiff/@ComponentsOfGeneralizedVVF[name]);
	If[intersection=!={},Print["** DefGeneralizedVVF: ",name," defined for ",MasterOf/@intersection," and ",intersection,". Make sure the definitions are compatible."]];
	If[pns,Protect[name]];
    MakexTensions[DefGeneralizedVVF,"End",name,pairs,options]; 
]

ComponentsOfGeneralizedVVF[expr_]/;!GeneralizedVVFQ[expr]:=Throw@Message[ComponentsOfGeneralizedVVF::invalid,expr]


Unprotect[Undef];
Undef[symbol_Symbol?GeneralizedVVFQ]:=UndefGeneralizedVVF[symbol]
Protect[Undef];

UndefGeneralizedVVF[list:{___?GeneralizedVVFQ}]:=Scan[UndefGeneralizedVVF,list];
UndefGeneralizedVVF[gvvf_]:=Catch@With[{servants=ServantsOf[gvvf]},
	If[!GeneralizedVVFQ[gvvf],Throw[Message[UndefGeneralizedVVF::unknown,"generalized variational vector field",gvvf]]];
	xAct`xTensor`Private`CheckRemoveSymbol[gvvf];
	
	MakexTensions[UndefGeneralizedVVF,"Beginning",gvvf];
	xUpSet[ServantsOf[gvvf],{}];
	xAct`xTensor`Private`DropFromHosts[gvvf];
	Undef/@Reverse[servants];
	$GeneralizedVVF=DeleteCases[$GeneralizedVVF,gvvf];
	MakexTensions[UndefGeneralizedVVF,"End",gvvf];
	
	xAct`xTensor`Private`MakeUndefInfo[UndefGeneralizedVVF,gvvf];
	xAct`xTensor`Private`RemoveSymbol[gvvf];
];


Unprotect[Equal,SameQ];
gvvf1_?GeneralizedVVFQ==gvvf2_?GeneralizedVVFQ:=(Sort@RulesOfGeneralizedVVFQ[gvvf1]==Sort@RulesOfGeneralizedVVFQ[gvvf2])&&(VanishOverOtherForms[gvvf1]==VanishOverOtherForms[gvvf2])
gvvf1_?GeneralizedVVFQ===gvvf2_?GeneralizedVVFQ:=(Sort[RulesOfGeneralizedVVFQ[gvvf1]]===Sort[RulesOfGeneralizedVVFQ[gvvf2]])&&(VanishOverOtherForms[gvvf1]===VanishOverOtherForms[gvvf2])
Protect[Equal,SameQ,DefGeneralizedVVF,UndefGeneralizedVVF];


(* ::Section:: *)
(*5.4. ExpandVertIntReplace*)


(* ::Subsection:: *)
(*5.4.1. ExpandVertIntReplace of VVF*)


CheckHoldExpandVertInt[options:OptionsPattern[Options[ExpandVertInt]]][dltensor_]:=With[{listtohold=Flatten[{OptionValue[HoldExpandVertInt]}]},MemberQ[Join[listtohold,VertDiff/@listtohold],dltensor]]

ExpandVertIntReplace[options___][HoldPattern[VertInt[vvf_?VVFQ][dltensor_?VertExactHeadQ[inds___]]]]/;FreeQ[vvf,Plus]&&ComponentsOfVVF[vvf][[1]]=!=MasterOfCPSTensor[dltensor]:=0
ExpandVertIntReplace[options:OptionsPattern[Options[ExpandVertInt]]][HoldPattern[VertInt[vvf_?VVFQ][dltensor_?VertExactHeadQ[inds___]]]]/;FreeQ[vvf,Plus]&&ComponentsOfVVF[vvf][[1]]===MasterOfCPSTensor[dltensor]&&CheckHoldExpandVertInt[options][dltensor]:=VertInt[vvf][dltensor[inds]]

ExpandVertIntReplace[options___][HoldPattern[VertInt[vvf_?VVFQ][dltensor_?VertExactHeadQ[inds___]]]]/;FreeQ[vvf,Plus]:=Module[
	{
	vvfSeparateInfo=InfoFromVVF[vvf],
	headtensor=MasterOfCPSTensor[dltensor],
	position,
	rule,
	result
	},
	(* Check if the headtensor is part of the components of vvf *)	
	position=Position[vvfSeparateInfo[[3]],headtensor,1];
	
	(* This is a safety measure. If headtensor is not part of the components of vvf, returns 0, otherwise we create a rule where dltensor is replaced by the coefficient of vvf associated to headtensor*)
	If[position==={},
		result=0,
		(
		If[MetricQ[headtensor]&&headtensor==Inv[headtensor],
				rule=MakeRule[vvfSeparateInfo[[2,position[[1,1]]]]//Simplification//Expand//ContractMetric//Evaluate,MetricOn->All],
				rule=MakeRule[vvfSeparateInfo[[2,position[[1,1]]]]//Evaluate]
				];
	 	result=((dltensor[inds]//SeparateMetric[])/.rule);
	 	)
	];
	
	(* This is a safety measure. It checks if the rule has been applied, otherwise it leaves the expression VertInt[vvf][dltensor[inds]] unchanged *)
	If[result===(dltensor[inds]//SeparateMetric[]),VertInt[vvf][dltensor[inds]],result]
]

(* It vanishes over other vertical forms *)
ExpandVertIntReplace[options___][HoldPattern[VertInt[vvf_?VVFQ][tensor_]]]:=0;


(* ::Subsection:: *)
(*5.4.2. ExpandVertIntReplace of GVVF*)


ExpandVertIntReplace[options___][HoldPattern[VertInt[gvvf_?GeneralizedVVFQ][tensor_?xTensorQ[inds___]]]]/;FreeQ[gvvf,Plus]&&!MemberQ[ComponentsOfGeneralizedVVF[gvvf]~Join~(VertDiff/@ComponentsOfGeneralizedVVF[gvvf]),tensor]&&VanishOverOtherForms[gvvf]:=0
ExpandVertIntReplace[options___][HoldPattern[VertInt[gvvf_?GeneralizedVVFQ][tensor_?VertExactHeadQ[inds___]]]]/;FreeQ[gvvf,Plus]&&MemberQ[VertDiff/@ComponentsOfGeneralizedVVF[gvvf],tensor]&&!MemberQ[ComponentsOfGeneralizedVVF[gvvf],tensor]&&VanishOverOtherForms[gvvf]:=VertInt[gvvf][tensor[inds]]

ExpandVertIntReplace[options___][HoldPattern[VertInt[gvvf_?GeneralizedVVFQ][tensor_?xTensorQ[inds___]]]]:=Module[
	{position=Position[ComponentsOfGeneralizedVVF[gvvf],tensor,1],result},

	result=If[position==={},
				If[VanishOverOtherForms[gvvf],0,VertInt[gvvf][tensor[inds]]],
				(tensor[inds]//SeparateMetric[])/.RulesOfGeneralizedVVFQ[gvvf][[position[[1,1]]]]
			];
		
	(* This is a safety measure. It checks if the rule has been applied, otherwise it leaves the expression VertInt[vvf][dltensor[inds]] unchanged *)
	If[result===(tensor[inds]//SeparateMetric[]),VertInt[gvvf][tensor[inds]],result]
]


(* ::Subsection:: *)
(*5.4.3. ExpandVertIntReplace of abstract VVF*)


(* Case base, it leaves the expression as is *)
ExpandVertIntReplace[options___][HoldPattern[VertInt[vvf_][tensor_]]]:=VertInt[vvf][tensor];


(* ::Chapter:: *)
(*6. Modifications to other definitions*)


(* ::Section:: *)
(*6.1. Auxiliary functions*)


(* ::Subsection:: *)
(*6.1.1. DefTotalDerivative for PD*)


TotalDerivativeQ[_]:=False
NormalOfCovDQ[_]:=False
dlNormalOfCovDQ[_]:=False
NormalOfCovDQ[tensor_?xTensorQ[inds___]]:=NormalOfCovDQ[tensor];
dlNormalOfCovDQ[tensor_?xTensorQ[inds___]]:=dlNormalOfCovDQ[tensor];

Options[DefTotalDerivative]={ProtectNewSymbol \[RightArrow] $ProtectNewSymbols};

DefTotalDerivative[PD,manifold_?ManifoldQ]:=With[{head=ToExpression["TotalDerivativeOfPDOf"<>ToString[manifold]]},
    
	MakexTensions[DefTotalDerivative,"Beginning",manifold];
	
	DefInertHead[head,LinearQ->False,ContractThrough->{delta}];
	xAct`xTensor`Private`SymbolRelations[head,manifold,{manifold}];
		
	head[0]:=0;
	head/: head[a_]+head[b_]:=head[a+b];
	head/: Times[a_?ConstantQ,head[b_]]:=head[Times[a,b]];
	
	Unprotect[WeightOf,ToCanonical,Simplification];
	
	TotalDerivativeQ[head]^=True;
	TotalDerivativeQ[head[_]]^=True;
	TotalDerivativeOfManifold[manifold]^=head;
	CovDOfTotalDerivative[head]^=PD;
	CovDOfTotalDerivative[head[_]]^=PD;
	NormalOfTotalDerivative[head]^=NormalOfManifold[manifold];
	NormalOfTotalDerivative[head[_]]^=NormalOfManifold[manifold];
(*	TotalDerivativeOfCovD[PD]^:=(Print["TotalDerivativeOfCovD[PD] has been used."];{});*)
	
	ToCanonical[expr_head]:=head[ToCanonical@@expr];
	ToCanonical[HoldPattern[head[expr_]]]:=head[ToCanonical[expr]];
	Simplification[HoldPattern[head[expr_]]]:=head[Simplification[expr]];
	xAct`xTensor`Private`ContractMetric0[args__][HoldPattern[head[expr_]]]:=head[xAct`xTensor`Private`ContractMetric0[args][expr]];
	    
    WeightOf[ih_?InertHeadQ[__]]=.;
    WeightOf[HoldPattern[head[expr_]]]:=WeightOf[expr];
    WeightOf[ih_?InertHeadQ[__]]:=Throw[Message[WeightOf::error,"WeightOf is generically undefined on inert heads."]];
     
	Protect[WeightOf,ToCanonical,Simplification];
	
	MakexTensions[DefTotalDerivative,"End",manifold];

];


(* ::Subsection:: *)
(*6.1.2. DefTotalDerivative for generic CovD*)


DefTotalDerivative[der_?CovDQ]:=With[{head=ToExpression["TotalDerivativeOf"<>ToString[der]],metric=MetricOfCovD[der]},

	MakexTensions[DefTotalDerivative,"Beginning",der];
	
	If[xTensorQ[metric], (*  Unless I am missing something, it only makes sense for metrics. But I allow for non-metrics just in case. *)
		(
		DefInertHead[head,LinearQ->False,ContractThrough->{delta,metric}];
		xAct`xTensor`Private`SymbolRelations[head,metric,{metric}];
		),
		(
		DefInertHead[head,LinearQ->False,ContractThrough->{delta}];
		xAct`xTensor`Private`SymbolRelations[head,der,{der}];(* I expected this to work for both but it doesn't. *)
		)
	];
		
	head[0]:=0;
	head/: head[a_]+head[b_]:=head[a+b];
	head/: Times[a_?ConstantQ,head[b_]]:=head[Times[a,b]];
	
	Unprotect[WeightOf,ToCanonical,Simplification];
	
	TotalDerivativeQ[head]^=True;
	TotalDerivativeQ[head[_]]^=True;
	TotalDerivativeOfCovD[der]^=head;
	CovDOfTotalDerivative[head]^=der;
	CovDOfTotalDerivative[head[_]]^=der;
	NormalOfTotalDerivative[head]^=NormalOfCovD[der];
	NormalOfTotalDerivative[head[_]]^=NormalOfCovD[der];

	ToCanonical[expr_head]:=head[ToCanonical@@expr];
	ToCanonical[HoldPattern[head[expr_]]]:=head[ToCanonical[expr]];
	Simplification[HoldPattern[head[expr_]]]:=head[Simplification[expr]];
	xAct`xTensor`Private`ContractMetric0[args__][HoldPattern[head[expr_]]]:=head[xAct`xTensor`Private`ContractMetric0[args][expr]];
        
    WeightOf[ih_?InertHeadQ[__]]=.;
    WeightOf[HoldPattern[head[expr_]]]:=WeightOf[expr];
    WeightOf[ih_?InertHeadQ[__]]:=Throw[Message[WeightOf::error,"WeightOf is generically undefined on inert heads."]];
     
	Protect[WeightOf,ToCanonical,Simplification];
	
	MakexTensions[DefTotalDerivative,"End",der];

];


(* ::Subsection:: *)
(*6.1.3. DefNormalOfCovD for PD*)


Options[DefNormalOfCovD]={ProtectNewSymbol \[RightArrow] $ProtectNewSymbols};

DefNormalOfCovD[PD,ind_,manifold_?ManifoldQ]:=Module[
	{
	nameNormal=ToExpression@StringJoin["NormalOfPDOf",ToString[manifold]],
	printAs="(\*SubscriptBox[n,\("<>ToString@PrintAs@manifold<>"\)])"
	},

	MakexTensions[DefNormalOfCovD,"Beginning",ind,manifold];
	
	DefTensor[nameNormal[-ind],manifold,PrintAs->printAs,VariationallyConstantQ->True];
			
    NormalOfManifold[manifold]^=nameNormal;
	CovDOfNormal[nameNormal]^=PD;
	TotalDerivativeOfNormal[nameNormal]^=TotalDerivativeOfManifold[manifold];
    NormalOfCovDQ[nameNormal]^=True;
    NormalOfPDQ[nameNormal]^=True;
    dlNormalOfCovDQ[VertDiff@nameNormal]^=True;
    dlNormalOfPDQ[VertDiff@nameNormal]^=True;
    
	MakexTensions[DefNormalOfCovD,"End",ind,manifold];
]


(* ::Subsection:: *)
(*6.1.4. DefNormalOfCovD for generic CovD*)


DefNormalOfCovD[covd_,ind_,manifold_?ManifoldQ]:=Module[
	{
	nameNormal=ToExpression@StringJoin["NormalOf",ToString[covd]],
	printAs="(\*SubscriptBox[n,\("<>SymbolOfCovD[covd][[2]]<>"\)])",
	christoffel=Christoffel[covd],
	inds=GetIndicesOfVBundle[Tangent[manifold],3]
	},

	MakexTensions[DefTotalDerivative,"Beginning",covd,ind,manifold];
	
	christoffel=HeadOfTensor2[christoffel[inds[[1]],-inds[[2]],-inds[[3]]]];
	DefTensor[nameNormal[-ind],manifold,PrintAs->printAs];		
    $printAddVariationalRelation=False;
	AddVariationalRelation[christoffel->nameNormal];		
    $printAddVariationalRelation=True;

	xAct`xTensor`Private`SymbolRelations[nameNormal,covd,{covd}];
		
	NormalOfCovD[covd]^=nameNormal;
	CovDOfNormal[nameNormal]^=covd;
	TotalDerivativeOfNormal[nameNormal]^=TotalDerivativeOfCovD[covd];
    NormalOfCovDQ[nameNormal]^=True;
    dlNormalOfCovDQ[VertDiff@nameNormal]^=True;
    
	MakexTensions[DefTotalDerivative,"End",covd,ind,manifold];
]


(* ::Subsection:: *)
(*6.1.5. NormalOfCovD and TotalDerivative for PD*)


NormalOfCovD[PD]:=If[Length@$Manifolds>1,Catch@Throw@Message[NormalOfCovD::error,"PD is defined over every manifold. Use NormalOfCovD[PD, manifold] or NormalOfCovD[PD, index] instead."],NormalOfManifold[$Manifolds[[1]]]]
NormalOfCovD[PD,ind_?AIndexQ]:=NormalOfManifold[BaseOfVBundle@VBundleOfIndex@ind]
NormalOfCovD[PD,manifold_?ManifoldQ]:=NormalOfManifold[manifold]
NormalOfCovD[der_,ind_]/;der=!=PD :=NormalOfCovD[der]

TotalDerivativeOfCovD[PD]:=If[Length@$Manifolds>1,Catch@Throw@Message[TotalDerivativeOfCovD::error,"PD is defined over every manifold. Use TotalDerivativeOfCovD[PD, manifold] or TotalDerivativeOfCovD[PD, index] instead."],TotalDerivativeOfManifold[$Manifolds[[1]]]]
TotalDerivativeOfCovD[PD,ind_?AIndexQ]:=TotalDerivativeOfManifold[BaseOfVBundle@VBundleOfIndex@ind]
TotalDerivativeOfCovD[PD,manifold_?ManifoldQ]:=TotalDerivativeOfManifold[manifold]
TotalDerivativeOfCovD[der_,ind_]/;der=!=PD :=TotalDerivativeOfCovD[der]


(* ::Subsection:: *)
(*6.1.6. TotalDerivativeDivergenceQ and TotalDerivativeDivergenceInfo*)


TotalDerivativeDivergenceQ[_]:=False
TotalDerivativeDivergenceQ[expr_?TotalDerivativeQ]:=TotalDerivativeDivergenceInfo[expr][[1]]

TotalDerivativeDivergenceInfo[expr_]:=If[TotalDerivativeQ[expr],expr/.{HoldPattern[totder_?TotalDerivativeQ[pot_]]:>Module[{covd=CovDOfTotalDerivative[totder],normals=FindAllOfType[pot,NormalOfCovD],boolean,coeff=0},
	boolean=(Length@normals==1)&&CovDOfNormal[HeadOfTensor2[normals[[1]]]]===covd&&ScalarQ[pot]&&WeightOf[pot]/AIndex==1;
	If[boolean,coeff=IndexCoefficient[pot,normals[[1]]]];
	{boolean,HeadOfTensor2[normals[[1]]],totder,coeff}]}]

Protect[TotalDerivativeDivergenceQ];


(* ::Section:: *)
(*6.2. Modifications of DefScalarFunction*)


(* ::Subsection:: *)
(*6.2.1. Auxiliary functions*)


(* This function prints the dependence of a scalar function after definition *) 
FieldsToText[list_List]:=FieldsToText[list,Length@list]
FieldsToText[list_List,n_]:=ToString[list[[1]]]<>", "<>FieldsToText[Drop[list,1],n-1]
FieldsToText[list_List,2]:=ToString[list[[1]]]<>" and "<>FieldsToText[Drop[list,1],1]
FieldsToText[list_List,1]:=ToString[list[[1]]]


(* These Q-functions distinguish between xAct scalar functions and functions like Sqrt[x] *)
ScalarFunctionDefinedQ[_]:=False
ScalarFunctionButNotDefinedQ[x_]:=ScalarFunctionQ[x]&&!ScalarFunctionDefinedQ[x]


(* ::Subsection:: *)
(*6.2.2. DefScalarFunction*)


Unprotect@DefScalarFunction;

DefScalarFunction[sf_,tensor_,options:OptionsPattern[Options[DefScalarFunction]]]:=DefScalarFunction[sf,{tensor},options] (* With two arguments *)
DefScalarFunction[sf_,tensors_?ListQ,opts:OptionsPattern[Options[DefScalarFunction]]]:=Catch@Module[{fields=Sort@DeleteDuplicates@tensors,options=DeleteCases[Flatten[{opts}],Dagger->_]},

	If[!xTensorQ[#],Throw@Message[DefScalarFunction::unknown,"tensor",#]]&/@fields;
	
	DefScalarFunction[sf,DefInfo->{"scalar function","It depends on "<>FieldsToText@fields},options];
	
	Switch[OptionValue[Dagger],
		Complex,xAct`xTensor`Private`SetDaggerPair[sf,MakeDaggerSymbol[sf]];DefScalarFunction[Dagger[sf],Dagger/@tensors/.{MultiplyHead[_,name_]:>name},Dagger->Conjugate,Master->sf,options],
		Conjugate,xAct`xTensor`Private`SetPrintAs[sf,If[OptionValue[PrintAs]===Identity,PrintAs[sf],OptionValue[PrintAs]<>"\!\(\*StyleBox[\"\[NegativeVeryThinSpace]\", \"Text\"]\)"<>$DaggerCharacter]], (* This is needed because the second call to DefScalarFunction has dagger in the name, which is then removed for the PrintAs (usually it is added because there is none). Also, this seems better than AddDaggerCharacter since it leaves a gigantic space *)
		Imaginary,Dagger[sf]^=MultiplyHead[-1,sf],
		Real,Dagger[sf]^=sf,
		_,Throw@Message[DefScalarFunction::unknown,"Dagger value",OptionValue[Dagger]]
		];

	DependenciesOfScalar[sf]^=fields;
	]
	
Protect@DefScalarFunction;


(* ::Subsection:: *)
(*6.2.3. ScalarModificationsCPSEnd*)


Options[ScalarModificationsCPSEnd]:=Options[DefScalarFunction];

xTension["xAct`xTensor`",DefScalarFunction, "End"] := ScalarModificationsCPSEnd;

ScalarModificationsCPSEnd[sf_,options:OptionsPattern[DefScalarFunction]]:=(
	ScalarFunctionDefinedQ[sf]^=True; (* This is used to handle these functions differently than, for instance, Sqrt[x] when handling VertDiff and ExpandVertDiff *)
	Unprotect[WeightOf];
	
	sf/: WeightOf[sf]=0;
	sf/: WeightOf[sf[___]]:=0;
	PartialPartialsOfFunction[sf]^={};
	DependenciesOfScalar[sf]^={};
	
	Protect[WeightOf];
	)


(* ::Section:: *)
(*6.3. Modifications of UndefScalarFunction*)


(* This functions removes one element from a pair (a,b) such that Dagger@a=b. Thus,  Intersection[list,removeDaggerPairs@list]={}. This avoids repeated undefinition *)
removeDaggerPairs[list_List]:=Module[{seen=<||>,result={}},
	Do[
		If[!KeyExistsQ[seen,Dagger[elem]/.{MultiplyHead[_,tensor_]:>tensor}]&&!KeyExistsQ[seen,elem],
			AppendTo[result,elem];
			seen[elem]=True;],{elem,list}];
			result
	]


xTension["xAct`xTensor`",UndefScalarFunction, "Beginning"] :=  UndefTensor/@removeDaggerPairs[PartialPartialsOfFunction[#]] & (* To avoid the repeated undefinitions *) 


(* ::Section:: *)
(*6.4. Modifications of DefManifold*)


(* ::Subsection:: *)
(*6.4.1. ManifoldModificationsCPS*)


Options[ManifoldModificationsCPS]:=Options[DefManifold];
xTension["xAct`xTensor`",DefManifold, "End"] := DefManifoldModificationsCPS;

xTension["xAct`xTensor`",UndefManifold, "Beginning"] := UndefManifoldModificationsCPS;


(* Creates a FiducialNormal associated with the manifold *)

DefManifoldModificationsCPS[manifold_,dim_,indices_List,options___]:=Module[{dlnormal},
	Unprotect[PD];
	
	DefNormalOfCovD[PD,indices[[1]],manifold];	
	DefTotalDerivative[PD,manifold];
		
	Protect[PD];
]

(* Removes the FiducialNormal associated with the manifold *)
UndefManifoldModificationsCPS[manifold_]:=UndefTensor[NormalOfManifold[manifold]];


(* ::Section:: *)
(*6.5. Modifications of DefCovD*)


(* ::Subsection:: *)
(*6.5.1. CovDModificationsCPS*)


Options[CovDModificationsCPS]:=Options[DefCovD];
xTension["xAct`xTensor`", DefCovD, "End"] := CovDModificationsCPS;


(* Additional changes to addapt CovD to the WWedge. It defines the rules to expand the Riemann, Torsion and Ricci *)

Off[RuleDelayed::rhs]

CovDModificationsCPS[covd_[ind_],vbundle_,additionalvariables___]:=Module[
	{tangentbundle,manifold,indices,innerindices,christoffel,dlChristoffel,Achristoffel,dlAChristoffel,riemann,dlriemann,Friemann,ricci,torsion,normal},
			
	tangentbundle=VBundleOfIndex[ind];  
	manifold=BaseOfVBundle[tangentbundle];
	indices=GetIndicesOfVBundle[tangentbundle,4];
	
	(* Impose WWedge-Leibniz rule of the covariant derivative *)
	covd[MakePattern[-ind]][expr_WWedge]:=Sum[MapAt[covd[-ind][#]&,expr,i],{i,1,Length[expr]}];
	covd[MakePattern[ind]][expr_WWedge]:=Sum[MapAt[covd[ind][#]&,expr,i],{i,1,Length[expr]}];
		
	(* Definition of tensors to be used below *)
	christoffel=HeadOfTensor[Christoffel[covd][indices[[1]],-indices[[2]],-indices[[3]]],{indices[[1]],-indices[[2]],-indices[[3]]}]; (* Christoffel[covd] remains unevaluated, hence we need to include the indices  *)
	ChristoffelAUX[covd]=christoffel;
	ChristoffelAUX[covd,PD]=christoffel;
	ChristoffelAUX[PD,covd]=MultiplyHead[-1,christoffel];
	dlChristoffel=VertDiff[christoffel];
	torsion=HeadOfTensor2[Torsion[covd]];
	riemann=HeadOfTensor2[Riemann[covd]];
	ricci=HeadOfTensor2[Ricci[covd]]; 
	
	(* Create variational relations *)		
    $printAddVariationalRelation=False;	
	AddVariationalRelation[christoffel->riemann->ricci];
	AddVariationalRelation[christoffel->torsion];
	
	(* Definition of tensors to be used below if there are inner indices *)
	If[tangentbundle=!=vbundle,
		(
		innerindices=GetIndicesOfVBundle[vbundle,4]; 
		Achristoffel=HeadOfTensor[AChristoffel[covd][innerindices[[1]],-indices[[2]],-innerindices[[3]]],{innerindices[[1]],-indices[[2]],-innerindices[[3]]}]; (* AChristoffel[covd] remains unevaluated, hence we need to include the indices  *)
		AChristoffelAUX[covd]=Achristoffel;
		AChristoffelAUX[covd,PD]=Achristoffel;
	    AChristoffelAUX[PD,covd]=MultiplyHead[-1,Achristoffel];
		dlAChristoffel=VertDiff[Achristoffel];
		
		Unprotect[VertDiff,AChristoffel];
		VertDiff[AChristoffel[covd]]^=Head@dlAChristoffel[indices[[1]],-indices[[2]],-indices[[3]]];
		VertDiff[AChristoffel[covd,PD]]^=Head@dlAChristoffel[indices[[1]],-indices[[2]],-indices[[3]]];
		VertDiff[AChristoffel[PD,covd]]^=MultiplyHead[-1,Head@dlAChristoffel[indices[[1]],-indices[[2]],-indices[[3]]]];
		Protect[VertDiff,AChristoffel];
	
		Friemann=HeadOfTensor2[FRiemann[covd]];
		AddVariationalRelation[Achristoffel->Friemann];
		AddVariationalRelation[Dagger@Achristoffel->Dagger@Friemann];
		GenerateExpandVertDiffRuleRiemann[covd,Achristoffel,dlAChristoffel,Friemann[-indices[[1]],-indices[[2]],-innerindices[[3]],innerindices[[4]]],VertDiff[Friemann],torsion];
		),
		(
		AChristoffelAUX[covd]=Zero;
		)
	];
	
	(* Define rules for ExpandVertDiffRules *)
	Unprotect[VertDiff,Christoffel];
	
	GenerateExpandVertDiffRuleRiemann[covd,christoffel,dlChristoffel,riemann[-indices[[1]],-indices[[2]],-indices[[3]],indices[[4]]],VertDiff[riemann],torsion];
	GenerateExpandVertDiffRuleTorsion[dlChristoffel,torsion[indices[[1]],-indices[[2]],-indices[[3]]],VertDiff[torsion]]; 
	GenerateExpandVertDiffRuleRicci[ricci[-indices[[1]],-indices[[2]]],VertDiff[ricci],riemann];
		
	VertDiff[Christoffel[covd]]^=Head@dlChristoffel[indices[[1]],-indices[[2]],-indices[[3]]];
	VertDiff[Christoffel[covd,PD]]^=Head@dlChristoffel[indices[[1]],-indices[[2]],-indices[[3]]];
	VertDiff[Christoffel[PD,covd]]^=MultiplyHead[-1,Head@dlChristoffel[indices[[1]],-indices[[2]],-indices[[3]]]];
	Protect[VertDiff,Christoffel];
	
	DefTotalDerivative[covd];
	DefNormalOfCovD[covd,indices[[1]],manifold];
	
	If[$DefInfoQ,Print["** AddVariationalRelation: Variational relations created for ",covd,"."]];
    $printAddVariationalRelation=True;
]


(* ::Subsection:: *)
(*6.5.2. Rules for ExpandVertDiffRules*)


(* ::Input:: *)
(*(* I am pretty sure that I could simply use ExpandVertDiffRules, but that function was added later and it will require some work and it just works fine like that *)*)


RemoveExpandVertDiffRule::ProtectedRules1 = "`1` has predefined rules and cannot be removed. Use MakeVertRule instead.";
GenerateExpandVertDiffRule::ProtectedRules2 = "`1` has predefined rules and cannot be overridden. Use MakeVertRule instead.";

ProtectVertDiffRule[dltensor_]:=(
    ProtectedVertDiffRuleQ[dltensor]^=True;
	dltensor /: RemoveExpandVertDiffRule[dltensor]:=Throw@Message[RemoveExpandVertDiffRule::ProtectedRules1,dltensor];
	dltensor /: GenerateExpandVertDiffRuleAUX[dltensor[indices___],_] :=Throw[Message[GenerateExpandVertDiffRule::ProtectedRules2,dltensor]];)


UnprotectVertDiffRule[dltensor_]:=If[ProtectedVertDiffRuleQ[dltensor],
	(
	If[$DefInfoQ,Print["** UnprotectVertDiffRule: VertDiffRule for ",dltensor," has been unprotected."]];
	ProtectedVertDiffRuleQ[dltensor]^=False;
	dltensor /: RemoveExpandVertDiffRule[dltensor]=.;
	dltensor /: GenerateExpandVertDiffRuleAUX[dltensor[indices___],_]=.;)]


GenerateExpandVertDiffRuleRiemann[covd_,Chris_, dlChris_, riemann_[-a_, -b_, -c_, d_], dlRiemann_, torsion_]:=(

	ExpandVertDiffRules[dlRiemann[-a_Symbol,-b_Symbol,-c_Symbol,b_Symbol],options:OptionsPattern[Options[ExpandVertDiff]]]:=
			FilterVertExpand[dlRiemann[-a, -b, -c, b],options][VertDiff[$RicciSign Ricci[covd][-a,-c]]]; (* This definitions allow to use HoldExpandVertDiff *)
		
	ExpandVertDiffRules[dlRiemann[-a_Symbol,-b_Symbol,-c_Symbol,a_Symbol],options:OptionsPattern[Options[ExpandVertDiff]]]:=
			FilterVertExpand[dlRiemann[-a, -b, -c, a],options][VertDiff[-$RicciSign Ricci[covd][-b,-c]]];
		
	ExpandVertDiffRules[dlRiemann[-a_Symbol,-b_Symbol,-c_Symbol,d_Symbol],options:OptionsPattern[Options[ExpandVertDiff]]]:= (* c and d might be internal indices *)
		With[{dummy=DummyAs[a]},
			FilterVertExpand[dlRiemann[-a, -b, -c, d],options]
							[-$RiemannSign (covd[-a][dlChris[d, -b, -c]] - covd[-b][dlChris[d, -a, -c]] + $TorsionSign  torsion[dummy, -a, -b]  dlChris[d, -dummy, -c])]
		];
  
	ProtectVertDiffRule[dlRiemann];
);


GenerateExpandVertDiffRuleTorsion[dlChris_,torsion_[c_, -a_, -b_],dltorsion_] := (
	ExpandVertDiffRules[dltorsion[c_Symbol,-a_Symbol,-b_Symbol],options:OptionsPattern[Options[ExpandVertDiff]]]:=
		FilterVertExpand[dltorsion[c, -a, -b],options][$TorsionSign(dlChris[c, -a, -b] - dlChris[c, -b, -a])];
		
	ProtectVertDiffRule[dltorsion];
);


GenerateExpandVertDiffRuleRicci[ricci_[-a_Symbol,-c_Symbol], dlricci_, riemann_]:=(
  (* Expansion rule of dlRicci *)
  ExpandVertDiffRules[dlricci[-a_Symbol, -c_Symbol], options:OptionsPattern[Options[ExpandVertDiff]]]:=
    Module[{dummy1 = DummyAs[c], dummy2 = DummyAs[c]},
       FilterVertExpand[dlricci[-a, -c],options][$RicciSign delta[-dummy2, dummy1] ExpandVertDiff[options][VertDiff[riemann[-a, -dummy1, -c, dummy2]]]] (* We need to include ExpadnVertDiff or else it turns into dlRicci *)
    ];

	ProtectVertDiffRule[dlricci];
);

On[RuleDelayed::rhs]


(* ::Section:: *)
(*6.6. Modifications of DefMetric*)


(* ::Subsection:: *)
(*6.6.1. MetricModificationsCPS*)


Options[MetricModificationsCPS]:=Options[DefMetric];
xTension["xAct`xTensor`",DefMetric, "End"] := MetricModificationsCPS;


(* Additional changes to addapt a VBundle Metric (with no CovD) to the WWedge *)

Off[RuleDelayed::rhs]

MetricModificationsCPS[_,metric_[ind1_?DownIndexQ,ind2_?DownIndexQ],None,additionalvariables___]:=Module[
	{vbundle,indices,NameInv,invMetric,dlinvMetric,variationalinvMetric,dlmetric,mdet,metricepsilon,dimen,dagger},
	
	If[!MetricQ[metric],Throw@Message[MetricModificationsCPS::unknown,"metric",metric]];
	
	vbundle=VBundleOfMetric[metric];
	indices=GetIndicesOfVBundle[vbundle,2];  
	
	dlmetric=VertDiff[metric];
	invMetric=Inv[metric];
	mdet = Determinant[metric,AIndex];
	metricepsilon = epsilon[metric];
	dimen=DimOfVBundle[VBundleOfIndex[ind1]];
	dagger=If[Dagger[metric]=!=metric,Complex,Real];
	
	(* Create variational relations *)		
    $printAddVariationalRelation=False;
	AddVariationalRelation[metric->{mdet,Tetra[metric]}];
	AddVariationalRelation[mdet->metricepsilon->mdet];
		
	Unprotect[VertDiff];

	(* Define rules for ExpandVertDiffRules *)
	
	With[{a=indices[[1]],b=indices[[2]]},
		
		(* It allows to contract the metric through the Innert Head VertInt *)
		Unprotect[VertInt];
		VertInt/:ContractThroughQ[VertInt[v_],metric]=True;
		Protect[VertInt];
		If[invMetric=!=metric,
			(
			dlinvMetric=VertDiff[invMetric];
			AddVariationalRelation[metric->invMetric];
			AddVariationalRelation[invMetric->metric];
			GenerateExpandVertDiffRuleInvFrozenMetric[metric[-a,-b],invMetric,dlmetric,dlinvMetric];
			),
			(
			
			DefAdditionalTensors[metric[a,b],DependenciesOf[metric[-a,-b]],Symmetric[{1,2}],PrintInverse->True,Dagger->dagger,DefInverseMetric->True];
			
			dlinvMetric=VertDiff[ToExpression["Inv"<>ToString[metric]]];
			With[{dlinvMetricSym = dlinvMetric, dlmetricSym = dlmetric},
				dlinvMetricSym/: dlinvMetricSym[PatternIndex[a,AIndex],PatternIndex[b,AIndex]] /; $UseInverseMetric == False :=With[{index1=DummyAs[a],index2=DummyAs[a]},-metric[a,index1] metric[b,index2] dlmetricSym[-index1,-index2]];
				dlmetricSym/: dlmetricSym[PatternIndex[a,AIndex],PatternIndex[b,AIndex]] /; $UseInverseMetric == True :=With[{index1=DummyAs[a],index2=DummyAs[a]},-metric[a,-index1] metric[b,-index2] dlinvMetricSym[index1,index2]];
			];
			
			(* Generate automatic rules to turn VariationalVectorInvmetric into  VariationalVectormetric (or vice-versa) *)
			variationalinvMetric=VariationalVector[ToExpression["Inv"<>ToString[metric]]];
			With[{VVinvMetricSym = variationalinvMetric, VVmetricSym = VariationalVector[metric]},
				VVinvMetricSym/: VVinvMetricSym[PatternIndex[a,AIndex],PatternIndex[b,AIndex]] /; $UseInverseMetric == False :=With[{index1=DummyAs[a],index2=DummyAs[a]},-metric[a,index1] metric[b,index2] VVmetricSym[-index1,-index2]];
				VVmetricSym/: VVmetricSym[PatternIndex[a,AIndex],PatternIndex[b,AIndex]] /; $UseInverseMetric == True :=With[{index1=DummyAs[a],index2=DummyAs[a]},-metric[a,-index1] metric[b,-index2] VVinvMetricSym[index1,index2]];
			];
			
			(* Since dlInvmetric\[Dagger] and VariationalInvMetric\[Dagger] are defined separately, we have to do this: I am not sure why this is needed, I thought it was automatic *)
			If[DaggerQ[metric],With[{daggervarInv=Dagger@variationalinvMetric,daggerdlInv=Dagger@dlinvMetric},
				$printAddVariationalRelation=False;
				RemoveVariationalRelation[daggervarInv->variationalinvMetric]; (* To follow the convention of not relating the dltensors with their Dagger *)
				RemoveVariationalRelation[variationalinvMetric->daggervarInv];
				RemoveVariationalRelation[daggerdlInv->dlinvMetric];
				RemoveVariationalRelation[dlinvMetric->daggerdlInv];
				
				PrintAs[Evaluate[daggerdlInv]]^=Evaluate[(GenerateDiffName[MasterOfCPSTensor@daggerdlInv,PrintInverse->True,PrintAs->PrintAs[Evaluate[MasterOfCPSTensor@daggerdlInv]]])[[2]]];(* This improves the PrintAs of Dagger@VertDiff *)
				PrintAs[Evaluate[daggervarInv]]^=Evaluate[(GenerateVariationalName[MasterOfCPSTensor@daggervarInv,PrintInverse->True,PrintAs->PrintAs[Evaluate[MasterOfCPSTensor@daggervarInv]]])[[2]]];(* This improves the PrintAs of Dagger@VertDiff *)
							
				With[{dlinvMetricSym = daggerdlInv, dlmetricSym = Dagger@dlmetric},
					dlinvMetricSym/: dlinvMetricSym[PatternIndex[a,AIndex],PatternIndex[b,AIndex]] /; $UseInverseMetric == False :=With[{index1=DummyAs[a],index2=DummyAs[a]},-Dagger[metric][a,index1] Dagger[metric][b,index2] dlmetricSym[-index1,-index2]];
					dlmetricSym/: dlmetricSym[PatternIndex[a,AIndex],PatternIndex[b,AIndex]] /; $UseInverseMetric == True :=With[{index1=DummyAs[a],index2=DummyAs[a]},-Dagger[metric][a,-index1] Dagger[metric][b,-index2] dlinvMetricSym[index1,index2]];
				];
			
				With[{VVinvMetricSym = daggervarInv, VVmetricSym = Dagger@VariationalVector@metric},
					VVinvMetricSym/: VVinvMetricSym[PatternIndex[a,AIndex],PatternIndex[b,AIndex]] /; $UseInverseMetric == False :=With[{index1=DummyAs[a],index2=DummyAs[a]},-Dagger[metric][a,index1] Dagger[metric][b,index2] VVmetricSym[-index1,-index2]];
					VVmetricSym/: VVmetricSym[PatternIndex[a,AIndex],PatternIndex[b,AIndex]] /; $UseInverseMetric == True :=With[{index1=DummyAs[a],index2=DummyAs[a]},-Dagger[metric][a,-index1] Dagger[metric][b,-index2] VVinvMetricSym[index1,index2]];
				];
				]
			];
			VertDiff[delta[a_,-b_]]:=0;
			)
		];
		
		GenerateExpandVertDiffRuleDet[vbundle,metric,dlmetric,TensorWithIndices[metricepsilon],VertDiff[metricepsilon],mdet,VertDiff[mdet]];
	];

	Protect[VertDiff];
		
	If[$DefInfoQ,Print["** AddVariationalRelation: Variational relations created for ",metric,"."]];
	$printAddVariationalRelation=True;
]


(* Additional changes to addapt Metric to the WWedge. It defines the rules to expand the RicciScalar, Kreshtchman, Wyel, Einstein... *)

MetricModificationsCPS[_,metric_[ind1_?DownIndexQ,ind2_?DownIndexQ],covd_,additionalvariables___]:=Module[
	{vbundle,indices,NameInv,invMetric,dlinvMetric,variationalinvMetric,dlmetric,
	christoffel,dlChristoffel,torsion,
	riemann,riemanndown,ricci,ricciscalar,einstein,weyl,tfricci,kretschmann,mdet,metricepsilon,dimen},
	
	If[!MetricQ[metric],Throw@Message[MetricModificationsCPS::unknown,"metric",metric]];
	
	vbundle=VBundleOfMetric[metric];
	indices=GetIndicesOfVBundle[vbundle,4];  
	
	dlmetric=VertDiff[metric];
	invMetric=Inv[metric];
	mdet = Determinant[metric,AIndex];
	metricepsilon = epsilon[metric];
	christoffel=Head@Christoffel[covd][indices[[1]],-indices[[2]],-indices[[3]]];(* We need the indices to force its evaluation in case it is zero. ?L: check what happens if is zero, probably we have to handle that option differently *)
	dlChristoffel=VertDiff[christoffel]; 
	torsion=Torsion[covd]; 
	riemann=Riemann[covd];
	riemanndown=RiemannDown[covd];
	ricci=Ricci[covd];
	ricciscalar=RicciScalar[covd];
	einstein=Einstein[covd];
	weyl=Weyl[covd];
	tfricci=TFRicci[covd];
	kretschmann=Kretschmann[covd];
	dimen=DimOfManifold[ManifoldOfCovD[covd]];
	
	(* Create variational relations *)		
    $printAddVariationalRelation=False;
	AddVariationalRelation[metric->{mdet,Tetra[metric],christoffel}];
	If[DaggerQ[metric],AddVariationalRelation[metric->{Dagger[Tetra[metric]]}]];
	AddVariationalRelation[mdet->metricepsilon->mdet];

	(* Some relations are already defined when the Levi-Civita CovD is defined *)
	AddVariationalRelation[{christoffel,riemann}->ricci];
	AddVariationalRelation[{metric,christoffel,riemann,ricci}->tfricci];
	AddVariationalRelation[{metric,christoffel,riemann,ricci}->ricciscalar];
	AddVariationalRelation[{metric,christoffel,riemann,ricci}->einstein];
	AddVariationalRelation[{metric,christoffel,riemann,ricci,ricciscalar}->weyl];
	AddVariationalRelation[{metric,christoffel,riemann}->kretschmann];
	If[dimen=!=2,AddVariationalRelation[einstein->ricci],AddVariationalRelation[einstein->tfricci]];
		
	Unprotect[VertDiff];

	(* Define rules for ExpandVertDiffRules *)
	
	With[{a=indices[[1]],b=indices[[2]],c=indices[[3]],d=indices[[4]]},
		
		(* Generates the rule to transform the Kretschmann scalar into the square of the Riemann *)
		KretschmannToRiemannAUX[kretschmann,riemanndown[-a,-b,-c,-d],invMetric];
		
		(* It allows to contract the metric through the Innert Head VertInt *)
		Unprotect[VertInt];
		VertInt/:ContractThroughQ[VertInt[v_],metric]=True;
		Protect[VertInt];
		If[invMetric=!=metric,
			(
			dlinvMetric=VertDiff[invMetric];
			AddVariationalRelation[metric->invMetric];
			AddVariationalRelation[invMetric->metric];
			AddVariationalRelation[{metric,riemann}->riemanndown];
			AddVariationalRelation[riemanndown->riemann];
			GenerateExpandVertDiffRuleInvFrozenMetric[metric[-a,-b],invMetric,dlmetric,dlinvMetric];
			GenerateExpandVertDiffRuleRiemannDown[riemanndown[-a,-b,-c,-d],VertDiff[riemanndown]];
			),
			(
			DefExactVerticalForm[metric[a,b],DependenciesOf[metric[a,b]],Symmetric[{1,2}],PrintInverse->True];
			dlinvMetric=VertDiff[ToExpression["Inv"<>ToString[metric]]];
			Unprotect[Dagger];
			Dagger[ToExpression["Inv"<>ToString[metric]]]:=ToExpression["Inv"<>ToString[metric]]; (* There is no dagger metric, so it just returns the normal one *)
			Protect[Dagger];
			AddVariationalRelation[metric->dlinvMetric];
			(* Generate automatic rules to turn dlInvmetric into  dlmetric (or vice-versa) *)
			With[{dlinvMetricSym = dlinvMetric, dlmetricSym = dlmetric},
				dlinvMetricSym/: dlinvMetricSym[PatternIndex[a,AIndex],PatternIndex[b,AIndex]] /; $UseInverseMetric == False :=With[{index1=DummyAs[a],index2=DummyAs[a]},-metric[a,index1] metric[b,index2] dlmetricSym[-index1,-index2]];
				dlmetricSym/: dlmetricSym[PatternIndex[a,AIndex],PatternIndex[b,AIndex]] /; $UseInverseMetric == True :=With[{index1=DummyAs[a],index2=DummyAs[a]},-metric[a,-index1] metric[b,-index2] dlinvMetricSym[index1,index2]];
			];
			
			DefVariationalVector[metric[a,b],DependenciesOf[metric[a,b]],Symmetric[{1,2}],PrintInverse->True];
			variationalinvMetric=VariationalVector[ToExpression["Inv"<>ToString[metric]]];
			AddVariationalRelation[metric->variationalinvMetric];
			(* Generate automatic rules to turn VariationalVectorInvmetric into  VariationalVectormetric (or vice-versa) *)
			With[{VVinvMetricSym = variationalinvMetric, VVmetricSym = VariationalVector[metric]},
				VVinvMetricSym/: VVinvMetricSym[PatternIndex[a,AIndex],PatternIndex[b,AIndex]] /; $UseInverseMetric == False :=With[{index1=DummyAs[a],index2=DummyAs[a]},-metric[a,index1] metric[b,index2] VVmetricSym[-index1,-index2]];
				VVmetricSym/: VVmetricSym[PatternIndex[a,AIndex],PatternIndex[b,AIndex]] /; $UseInverseMetric == True :=With[{index1=DummyAs[a],index2=DummyAs[a]},-metric[a,-index1] metric[b,-index2] VVinvMetricSym[index1,index2]];
			];
			
			VertDiff[delta[a_,-b_]]:=0;		
			)
		];
		
		GenerateExpandVertDiffRuleDet[vbundle,metric,dlmetric,TensorWithIndices[metricepsilon],VertDiff[metricepsilon],mdet,VertDiff[mdet]];
		GenerateExpandVertDiffRuleChristoffel[covd,metric,dlmetric,christoffel[a,-b,-c],dlChristoffel];
		GenerateExpandVertDiffRuleRicci[ricci[-a, -b],VertDiff[ricci],riemann];
		GenerateExpandVertDiffRuleRiemannMetric[covd,dlChristoffel, riemann[-a,-b,-c,-d],VertDiff[riemann],dlmetric,Torsion[covd]];
		GenerateExpandVertDiffRuleRicciScalar[vbundle, metric, ricci,VertDiff[ricci], ricciscalar,VertDiff[ricciscalar]];
		GenerateExpandVertDiffRuleEinstein[metric, ricci, ricciscalar, einstein[-a, -b],VertDiff[einstein]];
		GenerateExpandVertDiffRuleWeyl[weyl[-a, -b, -c, -d],VertDiff[weyl]];
		GenerateExpandVertDiffRuleTFRicci[tfricci[-a, -b],VertDiff[tfricci],ricci,metric,ricciscalar,dimen];
		
		GenerateExpandVertDiffRuleKretschmann[vbundle, metric, riemann, kretschmann,VertDiff[kretschmann]];
	];

	Protect[VertDiff];
		
	If[$DefInfoQ,Print["** AddVariationalRelation: Variational relations created for ",metric,"."]];
	$printAddVariationalRelation=True;
]


(* ::Subsection:: *)
(*6.6.2. Modification UndefMetric*)


xTension["xAct`xTensor`",UndefMetric, "Beginning"] := UndefMetricModificationsCPSBeg;
xTension["xAct`xTensor`",UndefMetric, "End"] := UndefMetricModificationsCPSEnd;


UndefMetricModificationsCPSBeg[metric_]:=If[DaggerQ[metric]&&!xTensorQ[MasterOfCPSTensor[metric]]&&!HasDaggerCharacterQ[metric],ServantsOf[Dagger@metric]^={}]; (* This is needed to avoid double Undef of some tensors *)


UndefMetricModificationsCPSEnd[metric_]:=(
	Unprotect[VertDiff,VariationalVector];
	
	If[!DaggerQ[metric]||(DaggerQ[metric]&&!HasDaggerCharacterQ[metric]),VertDiff[delta[a_,-b_]]=.];
	If[Inv[metric]===metric,
		(
		VertDiff[ToExpression["Inv"<>ToString[metric]]]=.;
		VariationalVector[ToExpression["Inv"<>ToString[metric]]]=.;
		)
	];
	Protect[VertDiff,VariationalVector];)


(* ::Subsection:: *)
(*6.6.3. KretschmannToRiemann*)


KretschmannToRiemannAUX[kretschmann_,riemanndown_[-a_Symbol,-b_Symbol,-c_Symbol,-d_Symbol],invMetric_]:=
	KretschmannToRiemannSingle[kretschmann[]]:=With[
		{
		ind1=DummyAs[a],ind2=DummyAs[a],
		ind3=DummyAs[a],ind4=DummyAs[a]
		},
		Scalar[riemanndown[-a,-b,-c,-d]riemanndown[-ind1,-ind2,-ind3,-ind4]invMetric[a,ind1]invMetric[b,ind2]invMetric[c,ind3]invMetric[d,ind4]]
		]

Unprotect[KretschmannToRiemann]; (* Similar to xTras *)

KretschmannToRiemann[expr_,der_?CovDQ]/;xTensorQ[MetricOfCovD[der]] :=expr /. Kretschmann[der][] :> KretschmannToRiemannSingle[Kretschmann[der][]]
KretschmannToRiemann[expr_, _] := expr;
KretschmannToRiemann[expr_] := Fold[KretschmannToRiemann, expr, $CovDs];

Protect[KretschmannToRiemann];


(* ::Subsection:: *)
(*6.6.4. Rules for ExpandVertDiffRules*)


(* We have to be careful that all objects are defined with their indices in their natural positions and using Inv[metric][a,b] instead of metric[a,b]. Otherwise it would be wrong for Frozen metrics *)

GenerateExpandVertDiffRuleInvFrozenMetric[metric_[-a_Symbol,-b_Symbol],invMetric_,dlmetric_,dlinvMetric_] := (
 
  ExpandVertDiffRules[dlinvMetric[a_Symbol, b_Symbol], options:OptionsPattern[Options[ExpandVertDiff]]]:=
    FilterVertExpand[dlinvMetric[a, b],options][With[{dummy1 = DummyAs[a], dummy2 = DummyAs[b]},
        -invMetric[a, dummy1] invMetric[b, dummy2] dlmetric[-dummy1, -dummy2]]
    ];

	ProtectVertDiffRule[dlinvMetric];
 )


GenerateExpandVertDiffRuleDet[vbundle_, metric_,dlmetric_,metricepsilon_[inds___],dlmetricepsilon_, mdet_, dlmdet_] := (
  (* Expansion rule of dlmdet *)
  
  ExpandVertDiffRules[dlmdet[], options:OptionsPattern[Options[ExpandVertDiff]]]:=
    FilterVertExpand[dlmdet[],options][
      With[
        {
        dummy1 = DummyIn[vbundle],dummy2 = DummyIn[vbundle]
        },
        Inv[metric][dummy1, dummy2] dlmetric[-dummy1,-dummy2] mdet[]
      ]
    ];

  (* Expansion rule of dlmetricepsilon *)
  ExpandVertDiffRules[dlmetricepsilon@@(MakePattern /@ {inds}),options:OptionsPattern[Options[ExpandVertDiff]]] :=
    FilterVertExpand[dlmetricepsilon[inds],options][
      With[
        {
        dummy1 = DummyIn[vbundle],dummy2 = DummyIn[vbundle]
        },
        Inv[metric][dummy1, dummy2] dlmetric[-dummy1, -dummy2] metricepsilon[inds]/2
      ]
    ];
    
	ProtectVertDiffRule[dlmdet];
	ProtectVertDiffRule[dlmetricepsilon];
)


GenerateExpandVertDiffRuleChristoffel[covd_, metric_, dlmetric_, Christoffel_[a_, -b_, -c_],dlChristoffel_] := (
  ExpandVertDiffRules[dlChristoffel[a_Symbol, -b_Symbol, -c_Symbol], options:OptionsPattern[Options[ExpandVertDiff]]] :=
    FilterVertExpand[dlChristoffel[a, -b, -c],options][With[{dummy = DummyAs[a]},
        1/2 Inv[metric][a, dummy] (covd[-b]@dlmetric[-dummy,-c] + covd[-c]@dlmetric[-b, -dummy] - covd[-dummy]@dlmetric[-b, -c])]
    ];
 
	ProtectVertDiffRule[dlChristoffel];
)


(* Notice that we include the middle steps instead of the final answer to allow HoldVertDiff to work *) 

GenerateExpandVertDiffRuleRiemannMetric[covd_, dlChris_, riemann_[-a_, -b_, -c_,-d_], dlRiemann_,dlmetric_,torsion_]:=(
  ExpandVertDiffRules[dlRiemann[-a_Symbol,-b_Symbol,-c_Symbol,-d_Symbol],options:OptionsPattern[Options[ExpandVertDiff]]]:=With[{dummy=DummyAs[c]},
    FilterVertExpand[dlRiemann[-a, -b, -c, -d],options]
    [-(dlmetric[-c,-dummy]riemann[-a,-b,-d,dummy])+$RiemannSign (covd[-a][dlChris[-c,-b,-d]]-covd[-b][dlChris[-c,-a,-d]]+ $TorsionSign  torsion[dummy, -a, -b]  dlChris[-d, -dummy, -c])]
  ];
  
	ProtectVertDiffRule[dlRiemann];
)


GenerateExpandVertDiffRuleRiemannDown[riemanndown_[-a_, -b_, -c_,-d_],dlRiemanndown_]:=(

  ExpandVertDiffRules[dlRiemanndown[-a_Symbol,-b_Symbol,-c_Symbol,-d_Symbol],options:OptionsPattern[Options[ExpandVertDiff]]]:=With[{dummy=DummyAs[c]},
    FilterVertExpand[dlRiemanndown[-a, -b, -c, -d],options][VertDiff[riemanndown[-a,-b,-c,-d]//RiemannDownToRiemann]]];
  
	ProtectVertDiffRule[dlRiemanndown]; 
)


 GenerateExpandVertDiffRuleRicciScalar[vbundle_, metric_, ricci_,dlricci_, ricciscalar_, dlricciscalar_] := (

  ExpandVertDiffRules[dlricciscalar[], options:OptionsPattern[Options[ExpandVertDiff]]] :=
    FilterVertExpand[dlricciscalar[],options][Module[{dummy1 = DummyIn[vbundle], dummy2 = DummyIn[vbundle]},
        ricci[-dummy1, -dummy2] VertDiff[Inv[metric][dummy1, dummy2]]+dlricci[-dummy1, -dummy2] Inv[metric][dummy1, dummy2]
      ]
    ];

	ProtectVertDiffRule[dlricciscalar];     
)


GenerateExpandVertDiffRuleEinstein[metric_, ricci_, ricciscalar_, einstein_[-a_, -b_], dleinstein_] := (

  ExpandVertDiffRules[dleinstein[-a_Symbol, -b_Symbol], options:OptionsPattern[Options[ExpandVertDiff]]] :=
    FilterVertExpand[dleinstein[-a, -b],options]
      [VertDiff[ricci[-a, -b]] - 1/2 metric[-a, -b]VertDiff[ricciscalar[]] - 1/2 VertDiff[metric[-a, -b]] ricciscalar[]
    ];

	ProtectVertDiffRule[dleinstein];           
)


GenerateExpandVertDiffRuleWeyl[weyl_[-a_, -b_, -c_, -d_], dlweyl_] := (

  ExpandVertDiffRules[dlweyl[-a_Symbol, -b_Symbol, -c_Symbol, -d_Symbol], options:OptionsPattern[Options[ExpandVertDiff]]] :=
    FilterVertExpand[dlweyl[-a, -b, -c, -d],options][VertDiff[WeylToRiemann[weyl[-a, -b, -c, -d]]]];

	ProtectVertDiffRule[dlweyl];      
)


GenerateExpandVertDiffRuleTFRicci[tfricci_[-a_, -b_], dltfricci_,ricci_,metric_,ricciscalar_,dimen_] := (

  ExpandVertDiffRules[dltfricci[-a_Symbol, -b_Symbol], options:OptionsPattern[Options[ExpandVertDiff]]] :=
    FilterVertExpand[dltfricci[-a, -b],options][VertDiff[ricci[-a, -b]-metric[-a,-b]ricciscalar[]/dimen]];(* TFRicciToRicci doesn't work for frozen metrics *)
    
	ProtectVertDiffRule[dltfricci];      
)


GenerateExpandVertDiffRuleKretschmann[vbundle_, metric_, riemann_, kretschmann_,dlkretschmann_] := (

  ExpandVertDiffRules[dlkretschmann[],options:OptionsPattern[Options[ExpandVertDiff]]] := 
    FilterVertExpand[dlkretschmann[],options][VertDiff[KretschmannToRiemann[kretschmann[]]]];

	ProtectVertDiffRule[dlkretschmann];    
)

On[RuleDelayed::rhs]


(* ::Chapter:: *)
(*7. Variational calculus*)


(* ::Section:: *)
(*7.1. Total derivative term*)


(* ::Subsection:: *)
(*7.1.1. Total derivative term*)


DiscardTotalDerivative[expr_]:=expr/.{term_?TotalDerivativeQ:>0,covd_?CovDQ[a_][Keep[_]]:>0}
OnlyTotalDerivative[expr_]:=expr-(expr//DiscardTotalDerivative)
TotalDerivativePotential[expr_]:=TotalDerivativePotential2[OnlyTotalDerivative[expr]]
TotalDerivativePotential2[number_?ConstantQ expr_]:=number TotalDerivativePotential2[expr]
TotalDerivativePotential2[expr_]:=(Identity@@expr)/.(Keep->Identity)

Protect[DiscardTotalDerivative,TotalDerivativePotential,OnlyTotalDerivative];


(* ::Subsection:: *)
(*7.1.2. Exchange between divergence and TotalDerivative*)


TotalDerivativeToCovD[expr_]:=expr/.{term_?TotalDerivativeQ:>Module[{der=CovDOfTotalDerivative[term],normal,index},
	normal=NormalOfTotalDerivative@term;
	index=(DummyIn/@SlotsOfTensor[normal])[[1]];
	der[index][Keep[IndexCoefficient[TotalDerivativePotential[term],normal[index]]]]
]}


CovDToTotalDerivative[expr_]:=expr//. {der_?CovDQ[ind_][Keep[term_]]:>TotalDerivativeOfCovD[der,ind][NormalOfTotalDerivative[TotalDerivativeOfCovD[der,ind]][ind] term]}


NormalOfCovDToCovD[expr_]:=SeparateMetric[][expr]//.
	{HoldPattern[before___ tensor_?NormalOfCovDQ[ind_] after___]:>CovDOfNormal[tensor][ind][before~WWedge~after]}

dlNormalOfCovDToCovD[expr_]:=(expr//Expand//SeparateMetric[])//. {HoldPattern[before___ WWedge[rest1___, dlnormal_?dlNormalOfCovDQ[ind_?DownIndexQ], rest2___]after___]:>VertDiff[CovDOfNormal[MasterOfCPSTensor[dlnormal]][ind][WWedge@@{before,rest1,rest2,after}]]-CovDOfNormal[MasterOfCPSTensor[dlnormal]][ind][VertDiff[WWedge@@{before,rest1,rest2,after}]]}//.{before___  dlnormal_?dlNormalOfCovDQ[ind_?DownIndexQ]after___:>VertDiff[CovDOfNormal[MasterOfCPSTensor[dlnormal]][ind][WWedge@@{before,after}]]-CovDOfNormal[MasterOfCPSTensor[dlnormal]][ind][VertDiff[WWedge@@{before,after}]]}

Protect[TotalDerivativeToCovD,CovDToTotalDerivative,NormalOfCovDToCovD,dlNormalOfCovDToCovD];


(* ::Section:: *)
(*7.2. Auxiliary functions*)


(* ::Subsection:: *)
(*7.2.1. Apply optional functions to an expression*)


(* Define a helper to apply optional functions *)
applyOpts[ex_,optionalfunctions___] := Fold[#2[#1] &, ex, Flatten[{optionalfunctions}]];


(* ::Subsection:: *)
(*7.2.2. Handle fields*)


(* LieDToCovDNonZeroVertDeg applies LieDToCovD whenever the directional vector has VertDeg>0 *)
(* BracketToCovDNonZeroVertDeg applies BracketToCovD whenever any of the vectors in the bracket has VertDeg>0 *)
(* ToCovDNonZeroVertDeg applies both LieDToCovDNonZeroVertDeg and BracketToCovDNonZeroVertDeg *)

LieDToCovDNonZeroVertDeg[der_?CovDQ][expr_]:=expr//.{LieD[vector_?NonZeroVertDegQ][expr2_]:>LieDToCovD[LieD[vector][expr2],der]}
BracketToCovDNonZeroVertDeg[der_?CovDQ][expr_]:=expr//.{
											Bracket[expr1_?NonZeroVertDegQ,expr2_][ind_]:>BracketToCovD[Bracket[expr1,expr2][ind],der],
											Bracket[expr1_,expr2_?NonZeroVertDegQ][ind_]:>BracketToCovD[Bracket[expr1,expr2][ind],der]}

ToCovDNonZeroVertDeg[der_?CovDQ][expr_]:=LieDToCovDNonZeroVertDeg[der][BracketToCovDNonZeroVertDeg[der][expr]]


SplitByVertDeg[expr_]:=Module[{result},
  result=SplitByVertDegAux[expr//Expand];
  If[MatchQ[result,{_List..}], result, {result}] (* We ensure that it is a list of 3-elements-lists *)
]  

(* Converts a sum into a List and applies the function to each element *)
SplitByVertDegAux[expr_Plus]:=SplitByVertDegAux/@(List@@expr)

(* For each summand we split the factors into ({dltensor}, \[Del]\[CenterDot]\[CenterDot]\[CenterDot]\[Del]dltensor,rest) where rest contains only elements of VertDeg=0 while the middle term contains all elements with VertDeg>0. We need to separate the metrics to ensure that no metric term (which has VertDeg=0) ends up in the bucket of VertDeg>0  *)

SplitByVertDegAux[expr_]:=(expr//SeparateMetric[])/.{ZeroVertDegTerms_?ZeroVertDegQ HigherVertDegTerms_:>{HigherVertDegTerms,ZeroVertDegTerms}}
SplitByVertDegAux[expr_]/;Length[expr]===1:=(expr//SeparateMetric[])/.{HigherVertDegTerms_?NonZeroVertDegQ :>{HigherVertDegTerms,1}}


FilterByField[tensor_[___]][{expr_,_}]:= MatchQ[Head/@FindAllOfType[expr,VertDiffExact],{tensor}]
FilterByField[-tensor_[___]][{expr_,_}]:= MatchQ[Head/@FindAllOfType[expr,VertDiffExact],{tensor}]


FindFieldsFromdlExpr[dlexpr_]:=TensorWithIndices/@MasterOf/@(Head/@FindAllOfType[dlexpr//SeparateMetric[],VertDiffExact]//DeleteDuplicates)


FindCovDFromdlExpr::unknown= "Several CovDs detected: `1`. Specify which one should be used to integrate by parts or use ChangeCovD.";

FindCovDFromdlExpr[expr_,function_]:=Module[
	{covds=FindAllOfType[expr,CovD]/.{der_?CovDQ[ind_][expr2_]:>der}//DeleteDuplicates},
		If[Length@covds>1,Throw@Message[FindCovDFromdlExpr::unknown,covds]];
		If[Length@covds==0,PD,covds[[1]]]
	]


(* ::Subsection:: *)
(*7.2.3. Handle CovDs*)


(* Takes an expression EXPR and converts it into a list where each element is {{NUMBER1,NUMER2},{rest,term with NUMBER CovD}} where NUMBER1 is the highest number of CovD's that appear derive a single tensor and NUMBER2 the second *)  
SplitHighestCovD[der_][expr_]:=Module[
	{
	ExprWithOneCovD=(ChangeCovD[expr,$CovDs,der]//ExpandAll),
	splitlist,orders,max
	},
	splitlist=splitFactors[ExprWithOneCovD];
	orders=CountCovD[der]/@splitlist;
	max=Max@orders;
	{{max,Max@DeleteCases[orders,max,1,1]},splitList[splitlist,orders][[2]]}
]


(* CovDOfSquareQ takes a pair {expr,der[ind][tensor[inds]]} and returns True if tensor[-inds] appears in expr and False otherwise. Furthermore, in the former case, it returns the division and the two inidivial terms. In the later it returns the original expression and the two terms *)

CovDOfSquareQ[der_,True][expr_,der_[ind1_][der_[ind2_][scalarfield_?ScalarQ[]]]]:=Module[{oppexpr1,oppexpr2,SquareQ1,SquareQ2,listSymmetries},
	oppexpr1=der[ind1][scalarfield[]]/.{x_?ABIndexQ:>-x};
	SquareQ1=!FreeQ[expr,oppexpr1];
	oppexpr2=der[ind2][scalarfield[]]/.{x_?ABIndexQ:>-x};
	SquareQ2=!FreeQ[expr,oppexpr2];
	Which[
	SquareQ1,{True,DivisionWWedge[expr,oppexpr1,ReturnZeroOrError->Zero],oppexpr1,der[ind2][der[ind1][scalarfield[]]]},
	SquareQ2,{True,DivisionWWedge[expr,oppexpr2,ReturnZeroOrError->Zero],oppexpr2,der[ind1][der[ind2][scalarfield[]]]},
	True,{False,expr,der[ind1][der[ind2][scalarfield[]]]}]
]

CovDOfSquareQ[der_,True][expr1_,der_[ind_][expr2_]]:=Module[{oppexpr2=expr2/.{x_?ABIndexQ:>-x},SquareQ,listSymmetries},
	listSymmetries=xAct`xTensor`Private`SymmetryEquivalentsOf[oppexpr2];
	SquareQ=!And@@(FreeQ[expr1,#]&/@listSymmetries);
	If[SquareQ,
		{True,adding@(DivisionWWedge[expr1,#,ReturnZeroOrError->Zero]&/@listSymmetries),oppexpr2,der[ind][expr2]},
		{False,expr1,der[ind][expr2]}
	]
]
	
CovDOfSquareQ[der_,False][expr_,der_[ind_][tensor_[inds___]]]:={False,expr,der[ind][tensor[inds]]}

CovDOfSquareQ[der_,_][expr1_,expr2_]:={False,expr1,expr2};


(* ::Subsection:: *)
(*7.2.4. LagrangianQ*)


LagrangianQ[expr_]:=ScalarQ[expr] && ZeroVertDegQ[expr] && (Length@FindAllOfType[expr,Tensor]>0) 

Protect[LagrangianQ];


(* ::Subsection:: *)
(*7.2.5. Exact1FormQ*)


BasicVertical1FormQ[expr_]:=BasicVertical1FormQAUX[expr//ExpandVertDiff[]//Expand]
BasicVertical1FormQAUX[expr_Plus]:=And@@(BasicVertical1FormQAUX/@List@@expr)
BasicVertical1FormQAUX[expr_]:= VertDeg[expr]==1 && (Length@FindAllOfType[expr,VertDiffExact]==1) 

Protect[BasicVertical1FormQAUX];


(* ::Subsection:: *)
(*7.2.6. dlLagrangianQ*)


dlLagrangianQ[expr_]:=ScalarQ[expr] && BasicVertical1FormQ[expr] 

Protect[dlLagrangianQ];


(* ::Subsection:: *)
(*7.2.7. Leibniz rules*)


(* We apply Leibniz "backwards" just once (to find the potential) *)
LeibnizOnce[der_][False,rest_,der_[a_][expr_]]:={NormalOfCovD[der,a][a]WWedge[rest,expr] ,-WWedge[der[a][rest],expr]}
LeibnizOnce[der_][False,rest_,0]:={0,0}

(* We apply Leibniz "backwards" for squares (analogous to f'f=1/2(f^2)') *)
LeibnizSquare[der_][True,expr1_,der_[a_][expr2_]]:={1/2 NormalOfCovD[der,a][a]WWedge[expr1,expr2],0}
LeibnizSquare[der_][True,rest_,expr1_,der_[a_][expr2_]]:={1/2 NormalOfCovD[der,a][a]WWedge[rest,expr1,expr2] ,-(1/2)WWedge[der[a][rest],expr1,expr2]}


(* Application of the Leibniz Rule ("integration by parts") with CovD *)
Options[LeibnizRule]:={KeepDivergence->True};

LeibnizRule[dlfield_,der_?CovDQ,option:OptionsPattern[Options[LeibnizRule]]][der_[ind_][expr_],rest_]:=
	-LeibnizRule[dlfield,der,option][expr,der[ind][rest]]+
	If[OptionValue[KeepDivergence],TotalDerivativeOfCovD[der,ind][(NormalOfCovD[der,ind][ind] rest//ReplaceDummies)(expr//ReplaceDummies)],0]

(* Application of the Leibniz Rule ("integration by parts") with LieD (the weight has to be taken into account) *)
LeibnizRule[dlfield_,der_?CovDQ,option:OptionsPattern[Options[LeibnizRule]]][LieD[vector_][expr_],rest_]/;(VertDeg[vector]==0):=Module[
	{
	ind=-1*FindFreeIndices[vector][[1]],
	weight=WeightOf[LieD[vector][expr]rest]/AIndex
	},
	-LeibnizRule[dlfield,der,option][expr,LieD[vector][rest]+(1- weight)rest (der[ind][vector]//ReplaceDummies)]+
	If[OptionValue[KeepDivergence],TotalDerivativeOfCovD[der,ind][(NormalOfCovD[der,ind][ind] vector//ReplaceDummies)( expr rest//ReplaceDummies)],0]
]
(* (Subscript[L, v]expr)rest=Subscript[L, v](expr rest)-expr Subscript[L, v]rest=v^a\!\(
\*SubscriptBox[\(\[Del]\), \(a\)]\((expr\ rest)\)\)+Weight expr rest \!\(
\*SubscriptBox[\(\[Del]\), \(a\)]
\*SuperscriptBox[\(V\), \(a\)]\)-expr Subscript[L, v]rest=\!\(
\*SubscriptBox[\(\[Del]\), \(a\)]\((
\*SuperscriptBox[\(v\), \(a\)]\ expr\ rest)\)\)-(\!\(
\*SubscriptBox[\(\[Del]\), \(a\)]
\*SuperscriptBox[\(v\), \(a\)]\))expr rest+Weight expr rest \!\(
\*SubscriptBox[\(\[Del]\), \(a\)]
\*SuperscriptBox[\(V\), \(a\)]\)-expr Subscript[L, v]rest=\!\(
\*SubscriptBox[\(\[Del]\), \(a\)]\((
\*SuperscriptBox[\(v\), \(a\)]\ expr\ rest)\)\)-expr((1-Weight)(\!\(
\*SubscriptBox[\(\[Del]\), \(a\)]
\*SuperscriptBox[\(v\), \(a\)]\))rest+Subscript[L, v]rest) *)

(* Final case when no more "integration by parts" are needed since dlfield2 has no more CovD in front of it  *)
LeibnizRule[dlfield_[inds1___],der_?CovDQ,option:OptionsPattern[Options[LeibnizRule]]][dlfield_[inds2___],rest_]:=rest dlfield[inds2]
LeibnizRule[-dlfield_[inds1___],der_?CovDQ,option:OptionsPattern[Options[LeibnizRule]]][dlfield_[inds2___],rest_]:=-rest dlfield[inds2]

(* If we reach a constant using Lebniz Rule, dlfield is not present in the initial expression. We never use this case but it is defined just in case. *)
LeibnizRule[dlfield1_,der_?CovDQ,option:OptionsPattern[Options[LeibnizRule]]][x_?ConstantQ,rest_]:=0

(* If we reach a different field at the end, it returns zero. This is a safeguard as this case should never be applied. *)
LeibnizRule[dlfield_?VertExactHeadQ[inds1___],der_?CovDQ,option:OptionsPattern[Options[LeibnizRule]]][dlfield2_?VertExactHeadQ[inds2___],rest_]:=0

(* Different connection: ChangeCovD *)
LeibnizRule[dlfield_,der1_?CovDQ,option:OptionsPattern[Options[LeibnizRule]]][expr:der2_?CovDQ[_][_],rest_]:=LeibnizRule[dlfield,der1,option][ChangeCovD[expr,der2,der1],rest]/;xAct`xTensor`Private`CompatibleCovDsQ[der1,der2]
LeibnizRule[dlfield_,der_?CovDQ,option:OptionsPattern[Options[LeibnizRule]]][expr_Plus,rest_]:=LeibnizRule[dlfield,der,option]@@SplitByVertDeg[# rest]&/@expr


(* ::Section:: *)
(*7.3. Physical information from the Lagrangian*)


(* ::Subsection:: *)
(*7.3.1. First variation (variation of the Lagrangian)*)


Options[FirstVariationOf1Form]:={Simplification->True,ContractMetric->True};
Options[FirstVariation]:=Options[FirstVariationOf1Form];
Options[FirstVariationOf1FormAUX]:=Options[FirstVariationOf1Form];

FirstVariationOf1Form::vertdeg="Incorrect VertDeg of the Lagrangian.";

FirstVariationOf1Form[][dlLagrangian_]:=FirstVariationOf1Form[All,None][dlLagrangian]
FirstVariationOf1Form[der:(_?CovDQ|None):None,options:OptionsPattern[Options[FirstVariationOf1Form]]][dlLagrangian_]:=FirstVariationOf1Form[All,der,options][dlLagrangian]
FirstVariationOf1Form[All,der:(_?CovDQ|None):None,options:OptionsPattern[Options[FirstVariationOf1Form]]][dlLagrangian_]:=FirstVariationOf1Form[FindFieldsFromdlExpr[dlLagrangian//ExpandVertDiff[]],der,options][dlLagrangian]

FirstVariationOf1Form[wrt_,der:(_?CovDQ|None):None,options:OptionsPattern[Options[FirstVariationOf1Form]]][TotalDerivative_?TotalDerivativeQ+dlLagrangian_.]:=TotalDerivative+FirstVariationOf1Form[wrt,der,options][dlLagrangian]
FirstVariationOf1Form[wrt_,der:(_?CovDQ|None):None,options:OptionsPattern[Options[FirstVariationOf1Form]]][dlLagrangian_?ConstantQ]:=0
FirstVariationOf1Form[wrt_,der:(_?CovDQ|None):None,options:OptionsPattern[Options[FirstVariationOf1Form]]][dlLagrangian_]:=Module[{derAUX,fieldList,dlLagrangianPrepared},
  (* We put to zero the exterior derivative of the constant fields. We then create a list of pairs {Subscript[expr, i],Subscript[zeroform, i]} such that Subscript[expr, i] is 
  a factor with Subscript[exactform, i] in it (usually hidden behind CovD) such that \!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i = 1\), \(k\)]\(
\*SubscriptBox[\(zeroform\), \(i\)]\ 
\*SubscriptBox[\(expr\), \(i\)]\)\)=dlLagrangian  *)
     
    If[VertDeg@dlLagrangian=!=1,Throw@Message[FirstVariationOf1Form::vertdeg]];
    
    fieldList=DeleteDuplicatesTensors@(TensorWithIndices/@Flatten[{wrt}]);
	dlLagrangianPrepared = dlLagrangian // ExpandVertDiff[NonConstantTensors->Head/@fieldList,HoldExpandVertDiff->Head/@fieldList];
	derAUX=If[der===None,FindCovDFromdlExpr[dlLagrangianPrepared,FirstVariationOf1Form],der];	
	dlLagrangianPrepared=dlLagrangianPrepared  // ToCovDNonZeroVertDeg[derAUX]// SplitByVertDeg;  
	If[WeightOf[dlLagrangian]/AIndex=!=1,Print["** Warning: WeightOf is not 1, which means that ",TotalDerivativeOfCovD[derAUX]," might not be a total divergence."]];

  (* We compute the first variation with respecto to each Subscript[field, i] \[Element] fieldList. For that, we take the pairs (expr1,expr2) \[Element] dlLagrangianPrepared such that Subscript[dlfield, i] appears in expr1 *)
  Sum[FirstVariationOf1FormAUX[VertDiff[fieldList[[i]]],derAUX,options][Select[dlLagrangianPrepared,FilterByField[VertDiff[fieldList[[i]]]]]],{i,Length[fieldList]}] 
];


FirstVariationOf1FormAUX[dlfield_,der_,options:OptionsPattern[Options[FirstVariationOf1FormAUX]]][dlLagrangian_]:=Module[{result},

  (* For each Subscript[field, j] we take the pairs {Subscript[expr, i],Subscript[zeroform, i],} such that Subscript[field, j]\[Element]Subscript[expr, i]. We apply the LeibnizRule to those terms and then we add them *)
  result=Plus@@LeibnizRule[dlfield,der]@@@(dlLagrangian);
  
  result = Which[
      OptionValue[Simplification] && OptionValue[ContractMetric], (Simplification[IndexCoefficient[result // DiscardTotalDerivative, dlfield]]//ContractMetric)dlfield + (OnlyTotalDerivative[result]// Simplification//ContractMetric),
      OptionValue[Simplification] && Not[OptionValue[ContractMetric]], Simplification[IndexCoefficient[result // DiscardTotalDerivative, dlfield]] dlfield + (OnlyTotalDerivative[result]// Simplification),
      Not[OptionValue[Simplification]] && OptionValue[ContractMetric], ContractMetric[IndexCoefficient[result // DiscardTotalDerivative, dlfield]] dlfield + (OnlyTotalDerivative[result]// ContractMetric),
      Not[OptionValue[Simplification]] && Not[OptionValue[ContractMetric]], result
	];

  result
  ];
     
(* To extract the first variation from the Lagrangian wrt to a field *) 
FirstVariation[][Lagrangian_?LagrangianQ]:=FirstVariation[None][Lagrangian]
FirstVariation[der:(_?CovDQ|None):None,options:OptionsPattern[Options[FirstVariation]]][Lagrangian_?LagrangianQ]:=FirstVariationOf1Form[All,der,options][VertDiff@Lagrangian]
FirstVariation[wrt_,der:(_?CovDQ|None):None,options:OptionsPattern[Options[FirstVariation]]][Lagrangian_?LagrangianQ]:=FirstVariationOf1Form[wrt,der,options][VertDiff@Lagrangian]
  
Protect[FirstVariationOf1Form,FirstVariation];


(* ::Subsection:: *)
(*7.3.2. Equations of motion*)


(* Auxiliary function to deal with the sign that appears when $UseInverseMetric=True due to the fact that dlg[-a,-b]=-dlInvg[-a,-b] *)
HandleInverse[expr___ dlmetric_?dlInvMetricQ[indices___]]:=TensorWithIndices@dlmetric
HandleInverse[expr_]:=expr

EOMOf1Form::vertdeg="Incorrect VertDeg of the Lagrangian.";
EOMOf1Form::wrongindices="Incorrect placement of indices `1`. Place them correctly or write simply the head of the tensor `2`.";

(* To extract the EOM from the variation of a Lagrangian wrt to a field *)
EOMOf1Form[wrt_?xTensorQ,der:(_?CovDQ|None):None][dlLagrangian_]:=EOMOf1Form[TensorWithIndices@wrt,der][dlLagrangian]
EOMOf1Form[wrt_?xTensorQ[inds___],der:(_?CovDQ|None):None][dlLagrangian_]:=Module[{derAUX,dlfield=Head@HandleInverse@VertDiff[TensorWithIndices@wrt],dlLagrangianPrepared,aux},  

    If[VertDeg@dlLagrangian=!=1,Throw@Message[EOMOf1Form::vertdeg]];
    If[xAct`xTensor`Private`SignedVBundleOfIndex/@{inds}=!=SlotsOfTensor@dlfield, (* Checks that the indices are correctly places (except for the inverse metric) *)
       If[!($UseInverseMetric&&xAct`xTensor`Private`SignedVBundleOfIndex/@{inds}==-SlotsOfTensor@dlfield),Throw@Message[EOMOf1Form::wrongindices,{inds},MasterOf@dlfield]]];
    
	dlLagrangianPrepared = ReplaceDummies[dlLagrangian] // ExpandVertDiff[NonConstantTensors->MasterOf@dlfield,HoldExpandVertDiff->MasterOf@dlfield];
	derAUX=If[der===None,FindCovDFromdlExpr[dlLagrangianPrepared,EOMOf1Form],der];	
	dlLagrangianPrepared=dlLagrangianPrepared  // ToCovDNonZeroVertDeg[derAUX]// SplitByVertDeg; 

    If[WeightOf[dlLagrangian]/AIndex=!=1,Print["** Warning: WeightOf is not 1, thus ",TotalDerivativeOfCovD[derAUX]," might not be a total divergence."]];

	aux=Plus@@LeibnizRule[dlfield@@{inds},derAUX,KeepDivergence->False]@@@dlLagrangianPrepared//Evaluate;	(* The provided indices are only used at the end *)
	IndexCoefficient[aux,dlfield@@{inds}]
  ];
  
(* To extract the EOM from the Lagrangian wrt to a field *) 
EOM[wrt_?xTensorQ,der:(_?CovDQ|None):None][Lagrangian_?LagrangianQ]:=EOMOf1Form[TensorWithIndices@wrt,der][VertDiff@Lagrangian]
EOM[wrt_?xTensorQ[inds___],der:(_?CovDQ|None):None][Lagrangian_?LagrangianQ]:=EOMOf1Form[wrt[inds],der][VertDiff@Lagrangian]
  
Protect[EOMOf1Form,EOM];


(* ::Subsection:: *)
(*7.3.3. SymplecticPotential*)


(* To extract the symplectic potential from the variation of a Lagrangian wrt to several fields *) 
SymplecticPotentialOf1Form[][dlLagrangian_]:=SymplecticPotentialOf1Form[All,None][dlLagrangian]
SymplecticPotentialOf1Form[der:(_?CovDQ|None):None][dlLagrangian_]:=SymplecticPotentialOf1Form[All,der][dlLagrangian]
SymplecticPotentialOf1Form[wrt_,der:(_?CovDQ|None):None][dlLagrangian_]:=dlLagrangian // FirstVariationOf1Form[wrt,der,Simplification->False,ContractMetric->False] // TotalDerivativePotential

(* To extract the symplectic potential from the Lagrangian wrt to several fields *) 
SymplecticPotential[][Lagrangian_?LagrangianQ]:=SymplecticPotential[All,None][Lagrangian]
SymplecticPotential[der:(_?CovDQ|None):None][Lagrangian_?LagrangianQ]:=SymplecticPotentialOf1Form[All,der][VertDiff@Lagrangian]
SymplecticPotential[wrt_,der:(_?CovDQ|None):None][Lagrangian_?LagrangianQ]:=SymplecticPotentialOf1Form[wrt,der][VertDiff@Lagrangian]


(* ::Subsection:: *)
(*7.3.4. SymplecticCurrent*)


Options[SymplecticCurrentOf1Form]:={HoldExpandVertDiff->None};
Options[SymplecticCurrent]:=Options[SymplecticCurrentOf1Form];

(* Symplectic current from the variation of the Lagrangian *)
SymplecticCurrentOf1Form[option:OptionsPattern[Options[SymplecticCurrentOf1Form]]][dlLagrangian_]:=SymplecticCurrentOf1Form[All,None][dlLagrangian]
SymplecticCurrentOf1Form[der:(_?CovDQ|None):None,option:OptionsPattern[Options[SymplecticCurrentOf1Form]]][dlLagrangian_]:=SymplecticCurrentOf1Form[All,der,option][dlLagrangian]
SymplecticCurrentOf1Form[All,der:(_?CovDQ|None):None,option:OptionsPattern[Options[SymplecticCurrentOf1Form]]][dlLagrangian_]:=$SymplecticCurrentSign(VertDiff[SymplecticPotentialOf1Form[All,der][dlLagrangian]]//ExpandVertDiff[ConstantTensors->$NormalsOfCovD,HoldExpandVertDiff->OptionValue[HoldExpandVertDiff]])   (* This is not necessary because they turn out to not depend on it, but it is faster *)
SymplecticCurrentOf1Form[wrt_,der:(_?CovDQ|None):None,option:OptionsPattern[Options[SymplecticCurrentOf1Form]]][dlLagrangian_]:=Module[{fields=Head/@DeleteDuplicatesTensors@(TensorWithIndices/@Flatten[{wrt}~Join~{OptionValue[HoldExpandVertDiff]}])},$SymplecticCurrentSign VertDiff[SymplecticPotentialOf1Form[wrt,der][dlLagrangian]]//ExpandVertDiff[NonConstantTensors->fields,HoldExpandVertDiff->fields]]/.{VertDiff[NormalOfCovD[der]][_]->0}

(* Symplectic current from Lagrangian *)
SymplecticCurrent[option:OptionsPattern[Options[SymplecticCurrent]]][Lagrangian_?LagrangianQ]:=SymplecticCurrentOf1Form[All,None][VertDiff@Lagrangian]
SymplecticCurrent[der:(_?CovDQ|None):None,option:OptionsPattern[Options[SymplecticCurrent]]][Lagrangian_?LagrangianQ]:=SymplecticCurrentOf1Form[All,der,option][VertDiff@Lagrangian]
SymplecticCurrent[wrt_,der:(_?CovDQ|None):None,option:OptionsPattern[Options[SymplecticCurrent]]][Lagrangian_?LagrangianQ]:=SymplecticCurrentOf1Form[wrt,der,option][VertDiff@Lagrangian]

Protect[SymplecticCurrentOf1Form,SymplecticPotentialOf1Form,SymplecticPotential,SymplecticCurrent];


(* ::Subsection:: *)
(*7.3.5. EnergyMomentum*)


EnergyMomentum::unknown = "No metric found.";
EnergyMomentum[Lagrangian_?LagrangianQ]:=Module[{metric,metrics,LCmetric,sign=If[$UseInverseMetric,-1,1]},
  
  metrics=FindAllOfType[TensorWithIndices/@DeleteDuplicates@Flatten@(ListVariationalRelationsOf[#,Directed->In]&/@Head/@FindAllOfType[Lagrangian//SeparateMetric[],Tensor]),Metric];

  If[Length[metrics]===0,Throw@Message[EnergyMomentum::unknown]];
  metrics=DeleteDuplicatesTensors[metrics]; (* Removes repeated metrics *)
  
  If[Length[metrics]>1,
    Print["** EnergyMomentum: Frozen metric detected, Energy-momentum tensor computed with non-frozen metric."];
    metrics=Select[metrics,Inv[Head[#]]===Head[#]&]]; (* Selects the non-frozen metric *) 
    
  metric=Head[metrics[[1]]];
  LCmetric=CovDOfMetric[metric];
  
  If[WeightOf[Lagrangian]/AIndex=!=1,Print["** EnergyMomentum: WeightOf[Lagrangian]\[NotEqual]1. Make sure ",Sqrt[SignDetOfMetric[metric]Determinant[metric][]]," is not missing."]];
    
  -sign*2*EOM[metric,LCmetric][Lagrangian]/Sqrt[SignDetOfMetric[metric]Determinant[metric][]]
]
	
Protect[EnergyMomentum];


(* ::Subsection:: *)
(*7.3.6. CurrentFromVector*)


CurrentFromVector[vector_?xTensorQ][expr1__][expr2__]:=CurrentFromVector[TensorWithIndices@vector][expr1][expr2]

CurrentFromVector[vector_][der:(_?CovDQ|None):None][Lagrangian_?LagrangianQ]:=CurrentFromVector[vector][All,der][Lagrangian]

CurrentFromVector[vector_][All,der:(_?CovDQ|None):None][Lagrangian_?LagrangianQ]:=CurrentFromVector[vector][FindFieldsFromdlExpr[VertDiff[Lagrangian]//ExpandVertDiff[]],der][Lagrangian]

CurrentFromVector[vector_][wrt_,der:(_?CovDQ|None):None][Lagrangian_?LagrangianQ]:=Module[{derAux,vvfLie,normal,fields,index=xAct`xTensor`Private`UltraindexOf[vector]},
  fields=DeleteDuplicatesTensors@(TensorWithIndices/@Flatten[{wrt}]);
  vvfLie=VVFFromLieD[vector][Head/@fields];
  derAux=If[der===None,FindCovDFromdlExpr[VertDiff@Lagrangian//ExpandVertDiff[],FirstVariationOf1Form],der];	
	
  normal=NormalOfCovD[derAux,index];
  $NoetherCurrentSign(normal[-index]vector //ReplaceDummies)Lagrangian-VertInt[vvfLie][SymplecticPotential[fields,derAux][Lagrangian]]//ExpandVertInt[]
]

Protect[CurrentFromVector];


(* ::Subsection:: *)
(*7.3.7. DivergenceQ*)


Options[DivergenceQ]:={CheckZero->False}
DivergenceQ::invalidCovDs=" `1` is defined over `2` while `3` is defined over `4`. Define an auxiliary CovD over `2` with the option ExtendedFrom->`3`";

(* If only a metric CovD is provided, it finds its metric *)
DivergenceQ[der_?CovDQ,option:OptionsPattern[Options[DivergenceQ]]][expr_?ScalarQ,optionalfunctions___]/;MetricQ@MetricOfCovD@der:=DivergenceQ[{MetricOfCovD@der,der},der,option][expr,optionalfunctions]
DivergenceQ[der_?CovDQ,option:OptionsPattern[Options[DivergenceQ]]][expr_?ScalarQ,optionalfunctions___]/;(ExtendedFrom@der=!=Null&&MetricQ@MetricOfCovD@ExtendedFrom@der):=DivergenceQ[{MetricOfCovD@ExtendedFrom@der,der},der,option][expr,optionalfunctions]
DivergenceQ[der_?CovDQ,der2_?CovDQ,option:OptionsPattern[Options[DivergenceQ]]][expr_?ScalarQ,optionalfunctions___]/;MetricQ@MetricOfCovD@der:=DivergenceQ[{MetricOfCovD@der,der},der2,option][expr,optionalfunctions]
DivergenceQ[der_?CovDQ,der2_?CovDQ,option:OptionsPattern[Options[DivergenceQ]]][expr_?ScalarQ,optionalfunctions___]/;(ExtendedFrom@der=!=Null&&MetricQ@MetricOfCovD@ExtendedFrom@der):=DivergenceQ[{MetricOfCovD@ExtendedFrom@der,der},der2,option][expr,optionalfunctions]

(* If only the metric is provided, it uses the metric CovD *)
DivergenceQ[metric_?MetricQ,option:OptionsPattern[Options[DivergenceQ]]][expr_?ScalarQ,optionalfunctions___]:=DivergenceQ[{metric,CovDOfMetric[metric]},CovDOfMetric[metric],option][expr,optionalfunctions]
DivergenceQ[metric_?MetricQ,der_?CovDQ,option:OptionsPattern[Options[DivergenceQ]]][expr_?ScalarQ,optionalfunctions___]:=DivergenceQ[{metric,CovDOfMetric[metric]},der,option][expr,optionalfunctions]

(* TotalDerivative is already a divergence (with the appropriate CovD) *)
DivergenceQ[{metric_?MetricQ,LCg_?CovDQ},der_?CovDQ,option:OptionsPattern[Options[DivergenceQ]]][number_?ConstantQ expr_,optionalfunctions___]:=DivergenceQ[{metric,LCg},der,option][expr,optionalfunctions]
DivergenceQ[{metric_?MetricQ,LCg_?CovDQ},der_?CovDQ,option:OptionsPattern[Options[DivergenceQ]]][totder_?TotalDerivativeQ[expr1_?ScalarQ],optionalfunctions___]/;TotalDerivativeOfCovD[der]==totder:=True;
DivergenceQ[{metric_?MetricQ,LCg_?CovDQ},der_?CovDQ,option:OptionsPattern[Options[DivergenceQ]]][totder_?TotalDerivativeQ[expr1_?ScalarQ]+expr2_?ScalarQ,optionalfunctions___]/;TotalDerivativeOfCovD[der]==totder:=DivergenceQ[{metric,LCg},der,option][expr2,optionalfunctions];

DivergenceQ[{metric_?MetricQ,LCg_?CovDQ},der_?CovDQ,option:OptionsPattern[Options[DivergenceQ]]][expr_?ScalarQ,optionalfunctions___]:=Module[
	{
	equalityFunc=If[OptionValue[CheckZero],SameQ,Equal],
	weight=WeightOf[expr]/AIndex,
	lagrangian,
	dlLagrangian,
	FieldsExp,
	SquareRootDetMetric=Sqrt[SignDetOfMetric[metric]Determinant[metric][]],
	eomCheck,
	optionUseMetricOnVBundle=OptionValue[ToCanonical,UseMetricOnVBundle],
	metricCovDQ=xTensorQ[MetricOfCovD[der]]
	},
	If[VBundlesOfCovD[der]=!=VBundlesOfCovD[LCg],Throw@Message[DivergenceQ::invalidCovDs,der,VBundlesOfCovD[der],LCg,VBundlesOfCovD[LCg]]];
	 
	If[!metricCovDQ,SetOptions[ToCanonical,UseMetricOnVBundle->None]];
	
	(*Compute Lagrangian and its variation*)
	lagrangian=SquareRootDetMetric^(1-weight) If[LCg===der,
		BracketToCovD[LieDToCovD[ChangeCovD[expr,$CovDs,LCg],LCg],LCg],
		BracketToCovD[LieDToCovD[ChangeCovD[expr,$CovDs,der],der],der]/.{der->LCg}]; (* Write everything in terms of der, and then change it for the metric CovD *)
					
	dlLagrangian=VertDiff[lagrangian//ContractMetric//Simplification]//ExpandVertDiff[]//Simplification;
	FieldsExp=FindFieldsFromdlExpr[dlLagrangian];
	
	(* Check equations of motion (EOMs) after applying optional functions to both sides *)
	eomCheck = equalityFunc[applyOpts[1/SquareRootDetMetric EOMOf1Form[Head[#],LCg][dlLagrangian],{optionalfunctions, {SeparateMetric[],Simplification,ContractMetric, Simplification}}],0] & /@ FieldsExp;
  
  SetOptions[ToCanonical, UseMetricOnVBundle -> optionUseMetricOnVBundle];
  
  And @@ (ScreenDollarIndices /@ eomCheck)
]

Protect[DivergenceQ];


(* ::Subsection:: *)
(*7.3.8. FindPotentialDivergence*)


(* If the metric is provided, it uses the metric CovD *)
FindPotentialDivergence[metric_?MetricQ][expr_,optionalfunctions___]:=FindPotentialDivergence[CovDOfMetric[metric]][expr,optionalfunctions]
FindPotentialDivergence[metric_?MetricQ,iteration_][expr_,optionalfunctions___]:=FindPotentialDivergence[CovDOfMetric[metric],iteration][expr,optionalfunctions]

(* If no number of iterations is included, it takes it to be infinite *)
FindPotentialDivergence[derORmetric_:(_?CovDQ|_?MetricQ)][expr_,optionalfunctions___]:=FindPotentialDivergence[derORmetric,Infinity][expr,optionalfunctions]

(* TotalDerivative is already a potential, so we only need to extract it *)
FindPotentialDivergence[der_?CovDQ,iteration_][totder_?TotalDerivativeQ[expr1_],optionalfunctions___]/;TotalDerivativeOfCovD[der]==totder:=expr1
FindPotentialDivergence[der_?CovDQ,iteration_][totder_?TotalDerivativeQ[expr1_]+expr2_,optionalfunctions___]/;TotalDerivativeOfCovD[der]==totder:=expr1+FindPotentialDivergence[der,iteration][expr2,optionalfunctions]

FindPotentialDivergence[der_?CovDQ,iteration_][expr_,optionalfunctions___]:=Module[{result=FindPotentialDivergenceAUX[der,iteration][expr,optionalfunctions]},
			If[der=!=PD,
				Module[
					{manifold=ManifoldOfCovD[der],normal},
					normal=NormalOfCovD[der][-DummyIn[Tangent[manifold]]]; (* This simplifies and removes the terms that are not truly divergence (when the iteration is not \[Infinity]) *)
					(IndexCoefficient[result,normal]//ContractMetric//Simplification)normal
					],
				Module[(* With PD we will need more information to know what is the normal (it depends on the manifold) *)
					{normal=FindAllOfType[result,NormalOfPD][[1]]},
					(IndexCoefficient[result,normal]//ContractMetric//ToCanonical[#,UseMetricOnVBundle->None]&//Simplify)normal
					]
				]
			]

FindPotentialDivergenceAUX[__][0,optionalfunctions___]:=0;
FindPotentialDivergenceAUX[der_?CovDQ,0][expr_,optionalfunctions___]:=expr
FindPotentialDivergenceAUX[der_?CovDQ,iteration_][expr_,optionalfunctions___]:=Module[
	{
	splitlist,
	OrderedByNumberOfCovD,
	covdofsquareQ,
	LeibnizOfMostCovDs,
	resultafterLeibniz,
	optionUseMetricOnVBundle=OptionValue[ToCanonical,UseMetricOnVBundle],
	metricCovDQ=xTensorQ[MetricOfCovD[der]],
	aux
	},
	If[!metricCovDQ,SetOptions[ToCanonical,UseMetricOnVBundle->None]];
	
	aux=applyOpts[BracketToCovD[LieDToCovD[ChangeCovD[expr,$CovDs,der],der],der],optionalfunctions];(* LieD cannot be used to integrate by parts unless it is a scalar *)

	splitlist=SplitHighestCovD[der]/@(SumToList@(aux//ExpandAll));
	OrderedByNumberOfCovD=SortBy[splitlist,First]//ContractMetric;
	
	(* If der is metric, we check if the the term with most CovD's is a square i.e. v^a\!\(
\*SubscriptBox[\(\[Del]\), \(b\)]
\*SubscriptBox[\(v\), \(a\)]\)=1/2\!\(
\*SubscriptBox[\(\[Del]\), \(b\)]\((
\*SuperscriptBox[\(v\), \(a\)]
\*SubscriptBox[\(v\), \(a\)])\)\) (otherwise CovDOfSquareQ does nothing)*)
	covdofsquareQ=CovDOfSquareQ[der,metricCovDQ]@@OrderedByNumberOfCovD[[-1,2]];

	LeibnizOfMostCovDs=If[covdofsquareQ[[1]],
		LeibnizSquare[der]@@covdofsquareQ,
		LeibnizOnce[der]@@covdofsquareQ
	];
	
	resultafterLeibniz=adding[(WWedge@@#&)/@((OrderedByNumberOfCovD//Most)[[All,2]])]+LeibnizOfMostCovDs[[2]];
	resultafterLeibniz=If[metricCovDQ,resultafterLeibniz//SeparateMetric[]//ContractMetric,resultafterLeibniz]//Simplification; (* SeparateMetric is necessary to ensure that all the monomials are contracted equally (also for the non-metric CovD) *) 
			
	SetOptions[ToCanonical,UseMetricOnVBundle->optionUseMetricOnVBundle];
	
	(* We iterate the process keeping the potentials and adding them at the end *)
	adding@Flatten@{FindPotentialDivergenceAUX[der,iteration-1][resultafterLeibniz,optionalfunctions],LeibnizOfMostCovDs[[1]]}
		
]

Protect[FindPotentialDivergence];


(* ::Subsection:: *)
(*7.3.9. NoetherSymmetryQ*)


NoetherSymmetryQ[vvf_][metric_?MetricQ,option:OptionsPattern[Options[DivergenceQ]]][Lagrangian_?LagrangianQ,options___]/;(VVFQ[vvf]||GeneralizedVVFQ[vvf]):=
	DivergenceQ[metric,option][VertInt[vvf][VertDiff@Lagrangian]//ExpandVertInt[],options]

Protect[NoetherSymmetryQ];


(* ::Subsection:: *)
(*7.3.10. NoetherPotential*)


NoetherPotential[vvf_][metric_?MetricQ][Lagrangian_?LagrangianQ,optionalfunctions___]/;(VVFQ[vvf]||GeneralizedVVFQ[vvf]):=NoetherPotential[vvf][CovDOfMetric[metric]][Lagrangian,optionalfunctions]
NoetherPotential[vvf_][der_?CovDQ][Lagrangian_?LagrangianQ,optionalfunctions___]/;(VVFQ[vvf]||GeneralizedVVFQ[vvf]):=FindPotentialDivergence[der][VertInt[vvf][VertDiff@Lagrangian]//ExpandVertInt[],optionalfunctions]

NoetherPotential[vvf_][metric_?MetricQ,iteration_][Lagrangian_?LagrangianQ,optionalfunctions___]/;(VVFQ[vvf]||GeneralizedVVFQ[vvf]):=NoetherPotential[vvf][CovDOfMetric[metric],iteration][Lagrangian,optionalfunctions]
NoetherPotential[vvf_][der_?CovDQ,iteration_][Lagrangian_?LagrangianQ,optionalfunctions___]/;(VVFQ[vvf]||GeneralizedVVFQ[vvf]):=FindPotentialDivergence[der,iteration][VertInt[vvf][VertDiff@Lagrangian]//ExpandVertInt[],optionalfunctions]

Protect[NoetherPotential];


(* ::Subsection:: *)
(*7.3.11. NoetherCurrent*)


NoetherCurrent[vvf_][metric_?MetricQ][Lagrangian_?LagrangianQ,optionalfunctions___]/;(VVFQ[vvf]||GeneralizedVVFQ[vvf]):=NoetherCurrent[vvf][CovDOfMetric[metric]][Lagrangian,optionalfunctions]
NoetherCurrent[vvf_][der_?CovDQ][Lagrangian_?LagrangianQ,optionalfunctions___]/;(VVFQ[vvf]||GeneralizedVVFQ[vvf]):=$NoetherCurrentSign(NoetherPotential[vvf][der][Lagrangian,optionalfunctions]-(VertInt[vvf][SymplecticPotential[ComponentsOfVVF[vvf],der][Lagrangian]]//ExpandVertInt[]))

NoetherCurrent[vvf_][metric_?MetricQ,iteration_][Lagrangian_?LagrangianQ,optionalfunctions___]/;(VVFQ[vvf]||GeneralizedVVFQ[vvf]):=NoetherCurrent[vvf][CovDOfMetric[metric],iteration][Lagrangian,optionalfunctions]
NoetherCurrent[vvf_][der_?CovDQ,iteration_][Lagrangian_?LagrangianQ,optionalfunctions___]/;(VVFQ[vvf]||GeneralizedVVFQ[vvf]):=$NoetherCurrentSign(NoetherPotential[vvf][der,iteration][Lagrangian,optionalfunctions]-(VertInt[vvf][SymplecticPotential[ComponentsOfVVF[vvf],der][Lagrangian]]//ExpandVertInt[]))

Protect[NoetherCurrent];


(* ::Chapter:: *)
(*8. End private and package*)


(* ::Input::Initialization:: *)
NamesxCPS=DeleteElements[Names["xAct`xCPS`*"],{"Disclaimer"}];

FunctionNamesxCPS=Select[NamesxCPS,(DownValues[#]=!={})||(UpValues[#]=!={})||(SubValues[#]=!={})||StringEndsQ[ToString[#],"Q"]||(StringContainsQ[#,"Of"]&&!StringContainsQ[#,"$"])&  ];
FunctionNamesxCPS=DeleteElements[FunctionNamesxCPS,{"NormalOfPD"}];

FunctionsxCPS=RegularExpression["^("<>StringRiffle[FunctionNamesxCPS,"|"]<>")$"];

ConstantNamesxCPS=Select[NamesxCPS,StringStartsQ[ToString[#],"$"]& ];
ConstantNamesxCPS=DeleteElements[ConstantNamesxCPS,{"$Version","$xTensorVersionExpected"}];
ConstantsxCPS=RegularExpression["^("<>StringRiffle[("\\"<>#)&/@ConstantNamesxCPS,"|"]<>")$"];

RestNamesxCPS=Complement[NamesxCPS,FunctionNamesxCPS,ConstantNamesxCPS];
RestxCPS=RegularExpression["^("<>StringRiffle[RestNamesxCPS,"|"]<>")$"];

End[]
EndPackage[]
