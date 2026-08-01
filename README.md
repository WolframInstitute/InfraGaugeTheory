> ⚠️ **Actively developed, experimental research code.** It undergoes frequent cleanups and refactors, and the API may change without notice.

# 🌐 InfraGaugeTheory

This experimental research project develops a computational theory of fibered graphs, with the goal of applying it as a formalism for [Infrageometry](https://p135246.github.io/wolfram/2026/05/30/infrageometry-manifesto.html) and as a foundation for gauge theory in the [Wolfram Physics Project](https://www.wolframphysics.org).

The goal is to explore what meaningful structure can be defined at different levels of information — unlabeled graphs, labeled graphs, ribbon graphs, hypergraphs, etc. — and what notions aggregate in an observer's scaling hierarchy.

## 🎯 Goals

* Fiber bundles, sections, connections, and tangent and cotangent bundles on graphs
* Hypergraphs and higher gauge theory
* Gauge theory, curvature, holonomy, and Wilson loops
* Relation to lattice gauge theory as a coarse-grained theory, and renormalization
* Natural clustering of graphs into fibers
* Obtaining fibered graphs from hypergraph rewriting
* Dynamics of gauge fields in the Wolfram Physics Project
* Factorization algebras

## ⚡ Usage

Install from the Wolfram Cloud:

```wolfram
PacletInstall["https://www.wolframcloud.com/obj/hajek_pavel/InfraGaugeTheory.paclet", ForceVersionInstall -> True]
Needs["WolframInstitute`InfraGaugeTheory`"]
```

Test on example graphs.

## 📓 Research Notebooks — "Math from code"

> ⚠️ LLM versions generated directly from the codebase via [ClaudePluginComputationalResearch](https://github.com/WolframInstitute/ClaudePluginComputationalResearch) with no warranty of correctness. Humans are welcome to publish their own versions alongside.

| Notebook | Description | Versions |
|----------|-------------|----------|
| Tautological 1-form | Tautological section of the double cotangent graph | [LLM](https://www.wolframcloud.com/obj/hajek_pavel/InfraGaugeTheory/CotangentBundle.nb) |
| Levi-Civita connection |  |  |
| Twisted fiber bundles |  |  |
| Wilson loops |  |  |
| WPP Coordinatization Pipeline | Gauge theory from causal graphs |  |

## 📚 Main References

* [WSS25: *An Investigation of Discrete SU(2) Gauge Theory through the Hopf Fibration and Wilson Loops* — Ioana-Alexandra Milea](https://community.wolfram.com/groups/-/m/t/3497643)

## 📄 License

- **Code**: [MIT](https://opensource.org/license/mit)
- **Research notebooks and ideas**: [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/)

