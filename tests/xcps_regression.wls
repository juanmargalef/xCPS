(* ::Package:: *)
(* xcps_regression.wls -- regression tests for xCPS (PASS/FAIL per check).

   Usage (wolfram -script): prepend parameter assignments, e.g.
     PKG = "xCPS.m";            (* package file to test; default: installed xAct`xCPS` *)
     REFDIR = "refs";           (* directory with reference results res_<REFTAG>_<THEORY>.m *)
     REFTAG = "v112";           (* tag of the reference results *)
     REFTHEORIES = {"EH","Maxwell","CS","R2"};  (* theories to compare against references; {} to skip *)
     STEPTIME = 900;
   All comparisons are symbolic: the difference is reduced with ContractMetric/ToCanonical
   (and SortCovDs/Simplification when needed) and must vanish identically.

   Check classes:
     PASS/FAIL   : must hold (analytic identities, reference equality, fixed bugs).
     XPASS/XFAIL : known open issues that are documented but not fixed (XFAIL is expected).
*)

If[!ValueQ[PKG], PKG = None];
If[!ValueQ[REFDIR], REFDIR = If[StringQ[$InputFileName] && $InputFileName =!= "", FileNameJoin[{DirectoryName[$InputFileName], "refs"}], "."]];
If[!ValueQ[REFTAG], REFTAG = "v112"];
If[!ValueQ[REFTHEORIES], REFTHEORIES = {"EH", "Maxwell", "CS", "R2"}];
If[!ValueQ[STEPTIME], STEPTIME = 900];

(* The package is loaded before any helper is defined, so that xAct symbols (ToCanonical, ContractMetric, ...) are not shadowed by Global` symbols. *)
tload = First@AbsoluteTiming[Block[{Print}, If[PKG === None, Needs["xAct`xCPS`"], Get[PKG]]]];
out[s__] := WriteString["stdout", s, "\n"];
out["xCPS ", ToString[xAct`xCPS`$Version], "  xTensor ", ToString[xAct`xTensor`$Version], "  Wolfram ", $VersionNumber, "  load ", tload, " s"];
npass = 0; nfail = 0; nxfail = 0; nxpass = 0;
SetAttributes[{check, xcheck, run}, HoldRest];
run[test_] := Block[{$MessageList = {}},
   TimeConstrained[MemoryConstrained[test, 1500000000, "MEMLIMIT"], STEPTIME, "TIMEOUT"]];
check[name_String, test_] := Module[{t, r},
   {t, r} = AbsoluteTiming[run[test]];
   If[r === True, npass++; out["PASS  ", name, "  (", ToString[Round[t, 0.01]], " s)"],
    nfail++; out["FAIL  ", name, "  (", ToString[Round[t, 0.01]], " s)  got: ", ToString[Short[r, 4], InputForm]]]];
xcheck[name_String, test_] := Module[{t, r},
   {t, r} = AbsoluteTiming[run[test]];
   If[r === True, nxpass++; out["XPASS ", name, "  (known issue now passes)"],
    nxfail++; out["XFAIL ", name, "  (known open issue)"]]];

zeroQ[x_] := Module[{y = x, cv},
   (* Lie derivatives are rewritten with the Levi-Civita connection of the manifold they act on *)
   If[!FreeQ[y, LieD], cv = If[FreeQ[y, h | cdh | B | xi3 | VertDiff[B] | VertDiff[h]], cd, cdh]; y = LieDToCovD[y, cv]];
   y = Quiet[y // Expand // ContractMetric // ToCanonical, {ToCanonical::cmods}];
   If[y === 0, True,
    (* lower all indices (derivatives included), commute derivatives, contract metrics through derivatives *)
    y = Quiet[y // SeparateMetric[] // Expand // ToCanonical // SortCovDs // ToCanonical // ContractMetric[#, OverDerivatives -> True] & // ToCanonical // Simplification, {ToCanonical::cmods}];
    y === 0]];


$DefInfoQ = False; $HistoryLength = 0;

(* Same definitions as profile.wls (4D theories on M, Chern-Simons on N3) *)
DefManifold[M, 4, {a, b, c, d, e, f, a1, b1, c1, d1}];
DefMetric[-1, g[-a, -b], cd];
DefTensor[xi[a], M, VariationallyConstantQ -> True];
DefTensor[A[-a], M];
DefManifold[N3, 3, {p, q, r, s, u, w, p1, q1, r1, s1}];
DefMetric[1, h[-p, -q], cdh];
DefTensor[B[-p], N3];
DefTensor[xi3[p], N3, VariationallyConstantQ -> True];
(* auxiliary tensors *)
DefTensor[T[-a, -b], M, Symmetric[{1, 2}]];
DefTensor[S[-a, -b], M, Symmetric[{1, 2}]];
DefTensor[V[a], M];
DefTensor[phi[], M];
DefTensor[F[-a, -b], M, Antisymmetric[{1, 2}]];

theory["EH"] := {Sqrt[-Detg[]] RicciScalarcd[], {g}, cd, xi[a], g[-a, -b]};
theory["R2"] := {Sqrt[-Detg[]] RicciScalarcd[]^2, {g}, cd, xi[a], g[-a, -b]};
theory["Maxwell"] := {-1/4 Sqrt[-Detg[]] g[a, c] g[b, d] (cd[-a][A[-b]] - cd[-b][A[-a]]) (cd[-c][A[-d]] - cd[-d][A[-c]]), {g, A}, cd, xi[a], g[-a, -b]};
theory["CS"] := {Sqrt[Deth[]] epsilonh[p, q, r] B[-p] cdh[-q][B[-r]], {B}, cdh, xi3[p], B[-p]};
nrm = NormalOfCovD[cd]; nrmh = NormalOfCovD[cdh];
LEH = theory["EH"][[1]]; LMax = theory["Maxwell"][[1]]; LCS = theory["CS"][[1]];

out["--- A. Analytic identities ---"];
check["A01 d(T^a_a) = g^ab dT_ab - dg^ab T_ab",
  zeroQ[ExpandVertDiff[][VertDiff[T[a, -a]]] - (g[a, b] VertDiff[T[-a, -b]] - g[a, c] g[b, d] VertDiff[g[-c, -d]] T[-a, -b])]];
check["A02 d(T^ab T_ab)",
  zeroQ[ExpandVertDiff[][VertDiff[T[a, b] T[-a, -b]]] -
    (2 g[a, c] g[b, d] T[-c, -d] VertDiff[T[-a, -b]] - 2 g[a, c] g[b, e] g[d, f] T[-a, -b] T[-e, -f] VertDiff[g[-c, -d]])]];
check["A03 dR = -R^ab dg_ab + D^a D^b dg_ab - Box(g^ab dg_ab)",
  zeroQ[ExpandVertDiff[][VertDiff[RicciScalarcd[]]] -
    (-Riccicd[a, b] VertDiff[g[-a, -b]] + cd[a][cd[b][VertDiff[g[-a, -b]]]] - g[a, b] cd[c][cd[-c][VertDiff[g[-a, -b]]]])]];
check["A04 d(det g) = det g g^ab dg_ab",
  zeroQ[ExpandVertDiff[][VertDiff[Detg[]]] - Detg[] g[a, b] VertDiff[g[-a, -b]]]];
check["A05 d^2 = 0", VertDiff[VertDiff[T[-a, -b]]] === 0];
check["A06 EH: EOM_g = -sqrt(-g) G^ab",
  zeroQ[EOM[g[-a, -b], cd][LEH] + Sqrt[-Detg[]] (Riccicd[a, b] - 1/2 g[a, b] RicciScalarcd[])]];
check["A07 EH: Theta = sqrt(-g) n_a (D_b dg^ab - D^a dg^b_b)",
  zeroQ[SymplecticPotential[{g}, cd][LEH] - Sqrt[-Detg[]] nrm[-a] (cd[-b][VertDiff[g[-c, -d]]] g[a, c] g[b, d] - g[a, e] cd[-e][VertDiff[g[-c, -d]]] g[c, d])]];
check["A08 Maxwell: EOM_A = sqrt(-g) D_a F^ab",
  zeroQ[EOM[A[-b], cd][LMax] - Sqrt[-Detg[]] g[a, c] g[b, d] cd[-a][cd[-c][A[-d]] - cd[-d][A[-c]]]]];
check["A09 Maxwell: Theta_A = -sqrt(-g) n_a F^ab dA_b",
  zeroQ[SymplecticPotential[{A}, cd][LMax] + Sqrt[-Detg[]] nrm[-a] g[a, c] g[b, d] (cd[-c][A[-d]] - cd[-d][A[-c]]) VertDiff[A[-b]]]];
check["A10 Maxwell: omega_A = -sqrt(-g) n_a dF^ab ^ dA_b",
  zeroQ[SymplecticCurrent[{A}, cd][LMax] + $SymplecticCurrentSign Sqrt[-Detg[]] nrm[-a] g[a, c] g[b, d] WWedge[cd[-c][VertDiff[A[-d]]] - cd[-d][VertDiff[A[-c]]], VertDiff[A[-b]]]]];
check["A11 CS: Theta = sqrt(h) eps^pqr n_q B_p dB_r",
  zeroQ[SymplecticPotential[{B}, cdh][LCS] - Sqrt[Deth[]] epsilonh[p, q, r] nrmh[-q] B[-p] VertDiff[B[-r]]]];
check["A12 i_{L_xi} dg = L_xi g",
  zeroQ[LieDToCovD[ExpandVertInt[][VertInt[VVFFromLieD[xi[c]][{g}]][VertDiff[g[-a, -b]]]], cd] - LieDToCovD[LieD[xi[c]][g[-a, -b]], cd]]];
check["A13 ExpandVertDiff of an equation", Module[{eq = ExpandVertDiff[][VertDiff[Detg[]] == 0]}, Head[eq] === Equal && zeroQ[eq[[1]] - Detg[] g[a, b] VertDiff[g[-a, -b]]]]];

out["--- B. Bug fixes (expected to FAIL on unpatched 1.1.2) ---"];
check["B01 ExpandVertDiff[SeparateMetric->False][dR] returns g^ab dR_ab - R^ab dg_ab (not Null)",
  Module[{r0 = ExpandVertDiff[SeparateMetric -> False][VertDiff[RicciScalarcd[]]]},
   r0 =!= Null && zeroQ[r0 - (g[a, b] VertDiff[Riccicd[-a, -b]] - Riccicd[a, b] VertDiff[g[-a, -b]])]]];
check["B02 Grade is Protected after loading", MemberQ[Attributes[Grade], Protected]];
check["B03 DefCovD is Protected after loading", MemberQ[Attributes[DefCovD], Protected]];
check["B04 ContractMetric g_ab WWedge[dphi, PD_c dV^a] is well formed and unchanged",
  Module[{x = g[-a, -b] WWedge[VertDiff[phi[]], PD[-c][VertDiff[V[a]]]], y},
   y = ContractMetric[x]; FreeQ[y, xAct`xTensor`Private`ContractMetric1] && ToCanonical[y - x] === 0]];
check["B05 AddVariationalRelation[dlT -> dlS] adds the edge T -> S",
  (AddVariationalRelation[VertDiff[T] -> VertDiff[S]]; EdgeQ[$VariationalGraph, T -> S])];
check["B06 CurrentFromVector carries the overall sign $NoetherCurrentSign",
  Module[{Lphi = -1/2 Sqrt[-Detg[]] g[a, b] cd[-a][phi[]] cd[-b][phi[]], cp, cm},
   cp = CurrentFromVector[xi[a]][{phi}, cd][Lphi];
   cm = Block[{$NoetherCurrentSign = -1}, CurrentFromVector[xi[a]][{phi}, cd][Lphi]];
   zeroQ[cp + cm]]];
check["B07 ExpandVertDiff options propagate into user expansion rules",
  (GenerateExpandVertDiffRule[{VertDiff[F[-a, -b]], VertDiff[cd[-a][A[-b]] - cd[-b][A[-a]]]}];
   !FreeQ[ExpandVertDiff[ExpandVertDiffCovD -> False][VertDiff[F[-a, -b]]], HoldPattern[VertDiff[cd[_][_]]]])];
check["B08 Directed -> \"outwards\" is recognised", MemberQ[xAct`xCPS`Private`ListOut, "outwards"]];
check["B09 delta[Dir[..], _] defined once (2 rules)", Count[DownValues[delta], _?(!FreeQ[#, Dir] &)] === 2];
check["B10 Dir contraction with delta still works", delta[Dir[V[a]], -b] === V[-b] && delta[-b, Dir[V[a]]] === V[-b]];
check["B11 no xCPS DownValues on System`SameQ / System`Equal", DownValues[SameQ] === {} && DownValues[Equal] === {}];
check["B12 GVVF equality (==, ===) still works",
  (DefGeneralizedVVF[gv1][{VertDiff[A[-a]], cd[-a][phi[]]}];
   DefGeneralizedVVF[gv2][{VertDiff[A[-a]], cd[-a][phi[]]}];
   DefGeneralizedVVF[gv3][{VertDiff[A[-a]], 2 cd[-a][phi[]]}];
   TrueQ[gv1 == gv2] && TrueQ[gv1 === gv2] && !TrueQ[gv1 === gv3] && !TrueQ[gv1 == gv3])];
check["B13 no xCPS DownValues on System`Expand", DownValues[Expand] === {}];
check["B14 Expand acts inside total derivatives",
  Module[{td = TotalDerivativeOfCovD[cd], x},
   x = td[(phi[] + 1) (phi[] + 2) nrm[-a] xi[a]];
   Expand[x] === td[Expand[(phi[] + 1) (phi[] + 2) nrm[-a] xi[a]]]]];

check["B15 Lagrangian written with PD, integrated with cd: same Theta and EOM as the cd Lagrangian",
  Module[{LA = -1/4 Sqrt[-Detg[]] g[a, c] g[b, d] (PD[-a][A[-b]] - PD[-b][A[-a]]) (PD[-c][A[-d]] - PD[-d][A[-c]])},
   zeroQ[ChangeCovD[SymplecticPotential[{A}, cd][LA], PD, cd] - SymplecticPotential[{A}, cd][LMax]] &&
   zeroQ[ChangeCovD[EOM[A[-b], cd][LA], PD, cd] - EOM[A[-b], cd][LMax]]]];

out["--- C. Comparison with reference results ('", REFTAG, ") ---"];
Do[
 Module[{file = FileNameJoin[{REFDIR, "res_" <> REFTAG <> "_" <> th <> ".m"}], ref, L, flds, cv, vec, eomf, vvf, new, t},
  If[!FileExistsQ[file], out["SKIP  C ", th, " (no reference file ", file, ")"],
   ref = Get[file];
   {L, flds, cv, vec, eomf} = theory[th];
   vvf = VVFFromLieD[vec][flds];
   Do[
    Module[{expr},
     expr = Switch[k,
       "dlRicciScalar", Hold[ExpandVertDiff[][VertDiff[RicciScalarcd[]]]],
       "ExpandVertDiff", Hold[ExpandVertDiff[][VertDiff[L]]],
       "EOM_first", Hold[EOM[eomf, cv][L]],
       "CurrentFromVector", Hold[CurrentFromVector[vec][flds, cv][L]],
       "SymplecticPotential", Hold[SymplecticPotential[flds, cv][L]],
       "SymplecticCurrent", Hold[SymplecticCurrent[flds, cv][L]],
       "NoetherCurrent", Hold[NoetherCurrent[vvf][cv][L]],
       _, Hold[$Failed]];
     If[StringQ[ref[k]] || ref[k] === $Failed,
      out["SKIP  C ", th, " ", k, " (reference is ", ToString[ref[k]], ")"],
      check["C " <> th <> " " <> k <> " equals reference", Catch@Module[{n = ReleaseHold[expr]}, n === ref[k] || zeroQ[n - ref[k]]]]]],
    {k, Keys[ref]}]]],
 {th, REFTHEORIES}];

out["--- X. Known open issues (documented, not fixed in this branch) ---"];
xcheck["X01 dl-tensor of a tensor from another context lives in that context",
  (DefTensor[Foo`hh[-a, -b], M, Symmetric[{1, 2}]]; Context @@ {Head[VertDiff[Foo`hh[-a, -b]]]} === "Foo`")];
xcheck["X04 NoetherCurrent[diffeomorphism VVF] agrees with CurrentFromVector (EH)",
  zeroQ[NoetherCurrent[VVFFromLieD[xi[a]][{g}]][cd][LEH] - CurrentFromVector[xi[a]][{g}, cd][LEH]]];
Block[{Print}, Needs["xAct`xCoba`"]];
DefChart[cart, M, {0, 1, 2, 3}, {tt[], xx[], yy[], zz[]}];
xcheck["X02 VertDiff of a tensor with basis indices evaluates (xCoba)", VertDiff[V[{a, cart}]] =!= HoldForm[VertDiff[V[{a, cart}]]] && Head[VertDiff[V[{a, cart}]]] =!= VertDiff];
xcheck["X03 chart Christoffels are variationally constant (xCoba)",
  FreeQ[ExpandVertDiff[][VertDiff[PDcart[-a][V[b]]]], _?(xAct`xCPS`VertExactHeadQ[#] && StringContainsQ[SymbolName[#], "Christoffel"] &)]];

out["SUMMARY pass=", npass, " fail=", nfail, " xfail=", nxfail, " xpass=", nxpass];
out["REGRESSION DONE"];
