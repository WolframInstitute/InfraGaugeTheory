# 🛜 InfraGaugeTheory

> ⚠️ **Actively developed, experimental research code.** It undergoes frequent cleanings and refactors, and the API may change without notice.

Discrete gauge theory on graphs at the infra-scale — using only the connectivity structure, with no labels — as it arises from hypergraph rewriting in the [Wolfram Physics Project](https://www.wolframphysics.org).

This project detects and describes fiber-bundle structure in graphs and develops core elements of gauge theory on the discrete substrate: sections, connections, parallel transport, and Wilson loops. We run computational experiments as the underlying graph is refined by rewriting, studying the evolution of gauge-theoretic observables. A central question is what kind of limiting gauge theory emerges, which we probe, for example, through the behavior of Wilson loops.

We also clarify the relation to lattice gauge theory and to "mesoscale" models that use labeled graphs. Such models can be viewed as coarse-grained versions of the infra-scale and support renormalization.

**Our goals include:**

- detecting and describing fiber-bundle structure in graphs
- developing sections, connections, parallel transport, and Wilson loops on the discrete substrate
- performing ruliology, clustering rewriting rules by their gauge-theoretic properties
- observing limiting behavior, branching, and persistence as the graph is refined
- clarifying the relation to lattice gauge theory and to mesoscale labeled-graph models
- exploring dynamics of connections induced by causal graphs
- extracting weights on the moduli space of connections from properties of the rewriting system
- computing quantum corrections via path integration
- investigating whether hyperedges and n-ary relations play a natural role
- lifting the substrate to a **fibered graph** with a discrete connection, defining a combinatorial curvature 2-form via face holonomy, discretizing the Yang-Mills equation and the Lorentz-force trajectory of a charged particle, and recovering electromagnetism (U(1) gauge theory) and Levi-Civita curvature in the appropriate special cases

## ✨ Usage

Install from the Wolfram Cloud:

```wolfram
PacletInstall["https://www.wolframcloud.com/obj/hajek_pavel/InfraGaugeTheory.paclet", ForceVersionInstall -> True]
Needs["WolframInstitute`InfraGaugeTheory`"]
```

## 🧪 Dev notebook

A full tour of the paclet — a reference card of every exported function, then worked examples: random fibrations, the torus bundle, the three visualizers, sections (smooth vs not), connections / parallel transport / holonomy (cylinder vs Möbius), the tangent bundle, and the metric Levi-Civita connection (curved octahedron vs flat grid). Pictures are embedded as bitmaps so it renders without re-evaluation:

**[Dev notebook — Pavel](https://www.wolframcloud.com/obj/hajek_pavel/InfraGaugeTheory/DevTest-Pavel.nb)** (runs on the Wolfram Cloud).

## ∴ License

MIT
