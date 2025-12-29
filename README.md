# xCPS: Covariant Phase Space for xAct

[![License: GPL](https://img.shields.io/badge/License-GPL-blue.svg)](LICENSE)

[![xAct](https://img.shields.io/badge/xAct-compatible-green.svg)](http://www.xact.es/)

**xCPS** is a [Mathematica](https://www.wolfram.com/mathematica/) package that implements the **Covariant Phase Space (CPS)** formalism for field theories within the [xAct](http://www.xact.es/) tensor algebra suite.

## Features

For a given a Lagrangian, xCPS can compute:

- **Equations of Motion (EOM)**

- **Symplectic currents**

- **Symmetry verification** for arbitrary field transformations

- **Noether charges** associated with symmetries

The package handles generic Lagrangians of the form $L(\text{tensors})$, including higher-derivative theories such as $f(\text{Riemann})$ gravity and beyond.

### Additional Utilities

Two powerful tools for general tensor calculations might be useful for the general public:

- **Divergence detection**: Determine whether an expression can be written as $\nabla_a V^a$

- **Potential extraction**: Extract the "potential" tensor $T^{bc}$ from expressions of the form $\nabla_a T^{bc}$

## Installation

**xCPS is included by default in xAct 1.3 and later versions.** For prior versions, please refer to **Section A.1** of xCPS_doc.nb for manual installation instructions.

## Documentation

A complete tutorial with theoretical background and detailed examples is included in the documentation notebook [*xCPS_doc.nb*](xCPS_doc.nb) which includes, among other things, the following examples:

  - Scalar field theories (Klein-Gordon and generic Lagrangians that depend on the scalar field and its derivatives)

  - Electromagnetism (Maxwell theory)

  - General Relativity and $f(R)$ gravity

  - Boundary terms and corner contributions

## Citation

If xCPS contributes to your research, please cite:

### The Package

> J. Margalef-Bentabol, "xCPS: an xAct package for covariant phase space, Noether symmetries and Noether charges." arXiv preprint (forthcoming). [GitHub: juanmargalef/xCPS](https://github.com/juanmargalef/xCPS)

### Theoretical Foundation

> J. Margalef-Bentabol and E. J. S. Villaseñor, "Geometric formulation of the covariant phase space methods with boundaries," [*Phys. Rev. D* **103**, 025011 (2021)](http://dx.doi.org/10.1103/PhysRevD.103.025011). [arXiv:2008.01842](https://arxiv.org/abs/2008.01842)

## Quick Example

```mathematica
(* Load the package *)
<< xAcxCPS
$PrePrint = ScreenDollarIndices;

(* Setup manifold and metric *)
DefManifold[M, 4, {a, b, c, d, e, f, i}];
DefMetric[-1, g[-a, -b], LCDer];

(*********************************)
(* Example 1: General Relativity *)
(*********************************)

(* Einstein-Hilbert Lagrangian *)
LGR = RicciScalarLCDer[] Sqrt[-Determinant[g][]];

(* Equations of Motion → Einstein equations *)
EOM[g, LCDer][LGR]

(* Symplectic Current *)
SymplecticCurrent[g, LCDer][LGR]

(******************************************)
(* Example 2: Generic Scalar Field Theory *)
(******************************************)

(* Define scalar field and its covariant derivative (as an independent tensor) *)
DefTensor[phi[], M, PrintAs -> "\[Phi]"];
Implode[LCDer[-a]@phi[]]

(* Generic scalar function depending on φ and ∇φ *)
DefScalarFunction[LScalar1, {LCDer@phi}, PrintAs -> "\!\(\*SubscriptBox[\(L\), \(1\)]\)"];

(* Generic scalar Lagrangian *)
L = Sqrt[-Detg[]] LScalar1[LCDer@phi];

(* First variation with respect to φ *)
FirstVariation[phi, LCDer][L]

(* Symplectic Current *)
SymplecticCurrent[phi, LCDer][L]
```

## Support

For questions, bug reports, or feature requests:

- **GitHub Issues**: Open an [issue](../../issues) for bug reports or feature requests

- **xAct Community**: Join discussions on the [xAct Google Group](https://groups.google.com/g/xAct)

- Send me an email (info in the [xCPS_doc.nb](xCPS_doc.nb) file)

## License

This project is licensed under the **GNU General Public License v2.0**. See the [LICENSE](LICENSE.txt) file for full details.

---

**Keywords:** Covariant Phase Space, Field Theory, General Relativity, Symplectic Geometry, Noether Charges, xAct, Mathematica, Tensor Calculus

