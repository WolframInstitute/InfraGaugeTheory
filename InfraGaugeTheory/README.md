> ⚠️ **Actively developed, experimental research code.** It undergoes frequent cleanings and refactors, and the API may change without notice.

# 🌐 InfraGaugeTheory

Discrete gauge theory on graphs at the infra-scale — using only the connectivity structure, with no labels. The paclet detects and describes fiber-bundle structure in graphs and provides core gauge theory on the discrete substrate: fibrations, sections, connections, parallel transport, holonomy, the graph tangent bundle, and the metric Levi-Civita connection.

## ✨ Usage

Install from the Wolfram Cloud:

```wolfram
PacletInstall["https://www.wolframcloud.com/obj/hajek_pavel/InfraGaugeTheory.paclet", ForceVersionInstall -> True]
Needs["WolframInstitute`InfraGaugeTheory`"]
```

Explore the paclet in the **[LLM-generated presentation notebook](https://www.wolframcloud.com/obj/hajek_pavel/InfraGaugeTheory/Presentation.nb)** (runs on the Wolfram Cloud).

Ready-made example substrates and ambient styles for the analyses: **[LLM-generated example-graphs notebook](https://www.wolframcloud.com/obj/hajek_pavel/ExampleGraphs.nb)** (runs on the Wolfram Cloud).

## 📓 Research Notebooks

| Notebook | Description | Link | Revision By |
|---|---|---|---|
| The canonical 1-form and discrete symplectic geometry | The tautological 1-form as a section of the double cotangent graph, its shadow cochain and the symplectic cochain ω = dθ, perfect matchings as the isotropic 1-forms, winding numbers as actions on subdivided circles, the intersection form as the tessellation-independent symplectic form of the torus, and the non-backtracking geodesic flow with its Ihara zeta orbit census | [Wolfram Cloud](https://www.wolframcloud.com/obj/hajek_pavel/InfraGaugeTheory/SymplecticGeometry.nb) | — |
| Fibered graphs | The definition of a graph fibration, the three drawing methods, sections, connections, holonomy, and the five bundle predicates separated by an eight-object battery in which each row fails exactly one clause | [Wolfram Cloud](https://www.wolframcloud.com/obj/hajek_pavel/InfraGaugeTheory/FiberedGraphs.nb) | — |
| Causal coordinatization and slice fibrations | Observer chains, radar coordinatization, causal foliations, and slice fibrations detected over the coordinate grid on causal graphs | [Wolfram Cloud](https://www.wolframcloud.com/obj/hajek_pavel/InfraGaugeTheory/CausalGaugePipeline.nb) | — |

> ⚠️ **LLM-generated from the code base.** These notebooks are written by an LLM directly from the source, as a demonstration of a mode of access to computational mathematics in which the source of truth for the formalism is the code and not the prose. They are **not revised by a human** by default. Where a human revision exists it is linked in the *Revision By* column, under the name of its author; an em dash means no revised version exists yet.
