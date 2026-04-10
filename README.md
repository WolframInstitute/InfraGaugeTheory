# InfraGaugeTheory

Discrete gauge theory on graphs at the infra-scale -- using only the connectivity structure, with no labels -- as it arises from hypergraph rewriting in the [Wolfram Physics Project](https://www.wolframphysics.org).

This project detects and describes fiber-bundle structure in graphs and develops core elements of gauge theory on the discrete substrate: sections, connections, parallel transport, and Wilson loops. We run computational experiments as the underlying graph is refined by rewriting, studying the evolution of gauge-theoretic observables. We perform ruliology, clustering rewriting rules by their gauge-theoretic properties, and observe limiting behavior, branching, and persistence. A central question is what kind of limiting gauge theory emerges, which we probe, for example, through the behavior of Wilson loops.

We also clarify the relation to lattice gauge theory and to "mesoscale" models that use labeled graphs. Such models can be viewed as coarse-grained versions of the infra-scale and support renormalization. We make these connections explicit.

We further explore dynamics of connections induced by causal graphs, extract weights on the moduli space of connections from properties of the rewriting system, and compute quantum corrections via path integration.

Finally, we investigate whether hyperedges and n-ary relations may play a natural role in this framework.

## Quick start

### Install from Wolfram Cloud

```wolfram
PacletInstall["https://www.wolframcloud.com/obj/hajek_pavel/InfraGaugeTheory.paclet"]
Needs["WolframInstitute`InfraGaugeTheory`"]
{total, proj} = RandomGraphFibration[CycleGraph[5]]
```

### Install from source

```bash
git clone git@github.com:WolframInstitute/InfraGaugeTheory.git
```

```wolfram
PacletDirectoryLoad["/path/to/InfraGaugeTheory"]
Needs["WolframInstitute`InfraGaugeTheory`"]
```

## Build and publish

Build the paclet archive and install locally:

```bash
./Scripts/build_paclet.wls
```

Build, install, and upload to Wolfram Cloud:

```bash
./Scripts/publish_paclet.wls
```

## License

MIT
