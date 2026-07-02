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

## ✨ Usage

Install from the Wolfram Cloud:

```wolfram
PacletInstall["https://www.wolframcloud.com/obj/hajek_pavel/InfraGaugeTheory.paclet", ForceVersionInstall -> True]
Needs["WolframInstitute`InfraGaugeTheory`"]
```

Explore the paclet in the **[LLM-generated presentation notebook](https://www.wolframcloud.com/obj/hajek_pavel/InfraGaugeTheory/Presentation.nb)** (runs on the Wolfram Cloud).

## ⚖️ License

MIT
