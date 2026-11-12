(* profile.wls : timings of the main xCPS functions for one theory and one package version.
   Prepend: PKG = "xCPS_112.m"; THEORY = "EH"|"R2"|"Maxwell"|"CS"; TAG = "v112"; REF = "" or "res_v111_EH.m"; STEPTIME = 900; *)
SetDirectory["C:/Users/juanm/cs-wolfram/audit"];
If[!ValueQ[STEPTIME], STEPTIME = 900];
If[!ValueQ[REF], REF = ""];
out[s__] := WriteString["stdout", s, "\n"];
tload = First@AbsoluteTiming[Block[{Print}, Get[PKG]]];
out["VERSION ", ToString[xAct`xCPS`$Version], " load_s ", tload, " theory ", THEORY];
$DefInfoQ = False;
$HistoryLength = 0;

Which[
 THEORY === "CS",
  DefManifold[N3, 3, {p, q, r, s, u, w, p1, q1, r1, s1}];
  DefMetric[1, h[-p, -q], cdh];
  DefTensor[B[-p], N3];
  DefTensor[xi3[p], N3, VariationallyConstantQ -> True];
  L = Sqrt[Deth[]] epsilonh[p, q, r] B[-p] cdh[-q][B[-r]];
  fields = {B}; cov = cdh; vec = xi3[p]; eomf = B[-p],
 True,
  DefManifold[M, 4, {a, b, c, d, e, f, a1, b1, c1, d1}];
  DefMetric[-1, g[-a, -b], cd];
  DefTensor[xi[a], M, VariationallyConstantQ -> True];
  cov = cd; vec = xi[a]; eomf = g[-a, -b];
  Which[
   THEORY === "EH", L = Sqrt[-Detg[]] RicciScalarcd[]; fields = {g},
   THEORY === "R2", L = Sqrt[-Detg[]] RicciScalarcd[]^2; fields = {g},
   THEORY === "Ric2", L = Sqrt[-Detg[]] Riccicd[-a, -b] Riccicd[a, b]; fields = {g},
   THEORY === "Riem2", L = Sqrt[-Detg[]] Riemanncd[-a, -b, -c, -d] Riemanncd[a, b, c, d]; fields = {g},
   THEORY === "GB", L = Sqrt[-Detg[]] (RicciScalarcd[]^2 - 4 Riccicd[-a, -b] Riccicd[a, b] + Riemanncd[-a, -b, -c, -d] Riemanncd[a, b, c, d]); fields = {g},
   THEORY === "Maxwell",
    DefTensor[A[-a], M];
    L = -1/4 Sqrt[-Detg[]] g[a, c] g[b, d] (cd[-a][A[-b]] - cd[-b][A[-a]]) (cd[-c][A[-d]] - cd[-d][A[-c]]);
    fields = {g, A}
  ]
];
vvf = VVFFromLieD[vec][fields];

SetAttributes[step, HoldRest];
results = <||>;
step[name_, expr_] := Module[{t, r, mem0 = MemoryInUse[]},
  {t, r} = AbsoluteTiming[Block[{$MessageList = {}}, rr = TimeConstrained[MemoryConstrained[expr, 1500000000, "MEMLIMIT"], STEPTIME, "TIMEOUT"]; msgs = $MessageList; rr]];
  results[name] = r;
  out["STEP ", name, " time_s ", NumberForm[t, {8, 2}], " leaves ", LeafCount[r], " mem_MB ", Round[(MaxMemoryUsed[] )/10.^6], " msgs ", ToString[DeleteDuplicates[msgs]]];
  r];

If[MemberQ[{"EH", "R2", "GB", "Ric2", "Riem2"}, THEORY], step["dlRicciScalar", ExpandVertDiff[][VertDiff[RicciScalarcd[]]]]];
step["ExpandVertDiff", ExpandVertDiff[][VertDiff[L]]];
step["EOM_first", EOM[eomf, cov][L]];
step["SymplecticPotential", SymplecticPotential[fields, cov][L]];
step["SymplecticCurrent", SymplecticCurrent[fields, cov][L]];
step["NoetherCurrent", NoetherCurrent[vvf][cov][L]];
step["CurrentFromVector", CurrentFromVector[vec][fields, cov][L]];
ncq = Catch[Quiet[TimeConstrained[(LieDToCovD[results["NoetherCurrent"] - results["CurrentFromVector"], cov] // ContractMetric // ToCanonical // Simplification) === 0, STEPTIME, "TIMEOUT"]]];
out["CHECK NoetherCurrent == CurrentFromVector : ", ToString[ncq]];

Put[results, "res_" <> TAG <> "_" <> THEORY <> ".m"];

(* comparison with a reference results file *)
If[REF =!= "" && FileExistsQ[REF],
 ref = Get[REF];
 Do[
  If[KeyExistsQ[ref, k],
   Module[{d, s},
    s = Catch@Quiet[TimeConstrained[
        d = ref[k] - results[k];
        Which[d === 0, "IDENTICAL",
         (LieDToCovD[d, cov] // ContractMetric // ToCanonical // Simplification) === 0, "EQUAL",
         True, "DIFFERENT"], STEPTIME, "CMP_TIMEOUT"]];
    out["CMP ", k, " ", s, " leaves_ref ", LeafCount[ref[k]], " leaves_new ", LeafCount[results[k]]]]],
  {k, Keys[results]}]
];
out["PROFILE DONE"];
