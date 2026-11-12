# xCPS regression and profiling scripts

* `xcps_regression.wls` prints `PASS`/`FAIL` (and `XFAIL`/`XPASS` for documented open issues) per check, followed by a `SUMMARY` line. All comparisons are symbolic (ContractMetric/ToCanonical/SortCovDs/Simplification).
* `profile_xcps.wls` times ExpandVertDiff, EOM, SymplecticPotential, SymplecticCurrent, NoetherCurrent and CurrentFromVector (diffeomorphism VVF) for one theory (`EH`, `Maxwell`, `CS`, `R2`, `Ric2`, `Riem2`, `GB`). It writes `res_<TAG>_<THEORY>.m` and can compare against a reference file.
* `refs/res_v112_*.m` hold reference results produced by xCPS 1.1.2 with xTensor 1.3.0 on Wolfram 15.0.

The scripts take their parameters as assignments prepended to the file, since `$ScriptCommandLine` is empty under `-script`:

```
(* run_regression.wls *)
PKG = "xCPS.m";          (* or omit to use Needs["xAct`xCPS`"] *)
REFDIR = "tests/refs"; REFTAG = "v112";
Get["tests/xcps_regression.wls"];
```

Run it with `wolfram -noprompt -script run_regression.wls`.

Expected on branch `audit/xcps-fixes`: `SUMMARY pass=54 fail=0 xfail=4 xpass=0`. On unpatched 1.1.2: `pass=42 fail=12 xfail=4`, and the 12 failures are exactly the bug checks B01-B09, B11, B13 and B15.
