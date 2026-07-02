# 🌐 InfraGaugeTheory

> ⚠️ **Actively developed, experimental research code.** It undergoes frequent cleanings and refactors, and the API may change without notice.

Discrete gauge theory on graphs at the infra-scale — using only the connectivity structure, with no labels. The paclet detects and describes fiber-bundle structure in graphs and provides core gauge theory on the discrete substrate: fibrations, sections, connections, parallel transport, holonomy, the graph tangent bundle, and the metric Levi-Civita connection.

## ⚡ Install

```wolfram
PacletInstall["https://www.wolframcloud.com/obj/hajek_pavel/InfraGaugeTheory.paclet", ForceVersionInstall -> True]
Needs["WolframInstitute`InfraGaugeTheory`"]
{total, proj} = RandomFiberedGraph[CycleGraph[5], "VerticalVertices" -> 3, "IsomorphicFibers" -> True]
```

## 🧪 Dev notebook (Pavel)

A full tour of the paclet — a reference card of every exported function, then worked examples: random fibrations, the torus bundle, the three visualizers, sections (smooth vs not), connections / parallel transport / holonomy (cylinder vs Möbius), the tangent bundle at radius r, and the metric Levi-Civita connection (curved octahedron vs flat grid). Pictures are embedded as bitmaps so it renders without re-evaluation:

**[Dev notebook — Pavel](https://www.wolframcloud.com/obj/hajek_pavel/InfraGaugeTheory/DevTest-Pavel.nb)** (runs on the Wolfram Cloud). Rebuild/redeploy with `Scripts/build_devtest_notebook.wls`.
