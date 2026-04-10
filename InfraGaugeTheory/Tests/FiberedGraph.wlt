(* ========================= RandomFiberedGraph ========================= *)

VerificationTest[
    {total, proj} = RandomFiberedGraph[CycleGraph[5]];
    {GraphQ[total], AssociationQ[proj]},
    {True, True},
    TestID -> "RandomFiberedGraph-BasicOutput"
]

VerificationTest[
    {total, proj} = RandomFiberedGraph[CycleGraph[5], "VerticalVertices" -> 3];
    VertexCount[total],
    15,
    TestID -> "RandomFiberedGraph-FiberSize"
]

VerificationTest[
    {total, proj} = RandomFiberedGraph[CycleGraph[5], "VerticalVertices" -> 3];
    Sort[DeleteDuplicates[Values[proj]]] === Sort[VertexList[CycleGraph[5]]],
    True,
    TestID -> "RandomFiberedGraph-ProjectionSurjective"
]

VerificationTest[
    {total, proj} = RandomFiberedGraph[CycleGraph[5], "VerticalVertices" -> 3];
    Values[Length /@ GroupBy[Normal @ proj, Last -> First]],
    {3, 3, 3, 3, 3},
    TestID -> "RandomFiberedGraph-UniformFibers"
]

VerificationTest[
    SeedRandom[42];
    {total, proj} = RandomFiberedGraph[CycleGraph[5], "VerticalVertices" -> {1, 4}];
    AllTrue[Values[Length /@ GroupBy[Normal @ proj, Last -> First]], Between[{1, 4}]],
    True,
    TestID -> "RandomFiberedGraph-RandomFiberSizes"
]

VerificationTest[
    {total, proj} = RandomFiberedGraph[CycleGraph[5], "VerticalVertices" -> 3, "VerticalEdges" -> 2];
    EdgeCount[total] > 15,
    True,
    TestID -> "RandomFiberedGraph-VerticalEdges"
]

VerificationTest[
    {total, proj} = RandomFiberedGraph[CycleGraph[5], "VerticalVertices" -> 3, "IsomorphicFibers" -> True];
    IsomorphicFibersQ[total, proj, "CheckIsomorphism" -> False],
    True,
    TestID -> "RandomFiberedGraph-IsomorphicFibers-SameSize"
]

VerificationTest[
    {total, proj} = RandomFiberedGraph[CycleGraph[5], "VerticalVertices" -> 3, "VerticalEdges" -> 2, "IsomorphicFibers" -> True];
    IsomorphicFibersQ[total, proj],
    True,
    TestID -> "RandomFiberedGraph-IsomorphicFibers-IsomorphicGraphs"
]

(* ========================= ReconstructBaseGraph ========================= *)

VerificationTest[
    {total, proj} = RandomFiberedGraph[CycleGraph[5], "VerticalVertices" -> 3];
    base = ReconstructBaseGraph[total, proj];
    IsomorphicGraphQ[base, CycleGraph[5]],
    True,
    TestID -> "ReconstructBaseGraph-CycleGraph"
]

VerificationTest[
    {total, proj} = RandomFiberedGraph[GridGraph[{3, 3}], "VerticalVertices" -> 2];
    base = ReconstructBaseGraph[total, proj];
    IsomorphicGraphQ[base, GridGraph[{3, 3}]],
    True,
    TestID -> "ReconstructBaseGraph-GridGraph"
]

VerificationTest[
    {total, proj} = RandomFiberedGraph[CompleteGraph[4], "VerticalVertices" -> 2];
    base = ReconstructBaseGraph[total, proj];
    IsomorphicGraphQ[base, CompleteGraph[4]],
    True,
    TestID -> "ReconstructBaseGraph-CompleteGraph"
]

(* ========================= IsomorphicFibersQ ========================= *)

VerificationTest[
    {total, proj} = RandomFiberedGraph[CycleGraph[4], "VerticalVertices" -> 3, "VerticalEdges" -> 0];
    IsomorphicFibersQ[total, proj],
    True,
    TestID -> "IsomorphicFibersQ-UniformEmptyFibers"
]

VerificationTest[
    total = Graph[{1, 2, 3, 4}, {UndirectedEdge[1, 2], UndirectedEdge[3, 4]}];
    proj = Association[1 -> "a", 2 -> "a", 3 -> "b", 4 -> "b"];
    IsomorphicFibersQ[total, proj],
    True,
    TestID -> "IsomorphicFibersQ-IsomorphicEdgeFibers"
]

VerificationTest[
    total = Graph[{1, 2, 3, 4}, {UndirectedEdge[1, 2]}];
    proj = Association[1 -> "a", 2 -> "a", 3 -> "b", 4 -> "b"];
    IsomorphicFibersQ[total, proj],
    False,
    TestID -> "IsomorphicFibersQ-NonIsomorphicFibers"
]

VerificationTest[
    total = Graph[{1, 2, 3, 4}, {UndirectedEdge[1, 2]}];
    proj = Association[1 -> "a", 2 -> "a", 3 -> "b", 4 -> "b"];
    IsomorphicFibersQ[total, proj, "CheckIsomorphism" -> False],
    True,
    TestID -> "IsomorphicFibersQ-SameVertexCountOnly"
]

(* ========================= EdgeLiftingPropertyQ ========================= *)

VerificationTest[
    total = Graph[{1, 2, 3, 4}, {UndirectedEdge[1, 3], UndirectedEdge[1, 4], UndirectedEdge[2, 3], UndirectedEdge[2, 4]}];
    proj = Association[1 -> "u", 2 -> "u", 3 -> "v", 4 -> "v"];
    EdgeLiftingPropertyQ[total, proj],
    True,
    TestID -> "EdgeLiftingPropertyQ-CompleteBipartiteFibers"
]

VerificationTest[
    total = Graph[{1, 2, 3}, {UndirectedEdge[1, 3]}];
    proj = Association[1 -> "u", 2 -> "u", 3 -> "v"];
    EdgeLiftingPropertyQ[total, proj],
    False,
    TestID -> "EdgeLiftingPropertyQ-MissingLift"
]

VerificationTest[
    {total, proj} = RandomFiberedGraph[CycleGraph[4], "VerticalVertices" -> 2, "HorizontalEdgesDensity" -> 1];
    EdgeLiftingPropertyQ[total, proj],
    True,
    TestID -> "EdgeLiftingPropertyQ-DiagonalFiberedGraph"
]

(* ========================= LocallyTrivialQ ========================= *)

VerificationTest[
    total = GraphProduct[PathGraph[{1, 2}], CompleteGraph[3], "Cartesian"];
    proj = Association @ Map[v |-> v -> v[[2]], VertexList[total]];
    LocallyTrivialQ[total, proj],
    True,
    TestID -> "LocallyTrivialQ-ProductBundle"
]

VerificationTest[
    verts = Flatten[Table[{i, j}, {i, 3}, {j, 2}], 1];
    edges = {UndirectedEdge[{1, 1}, {2, 1}], UndirectedEdge[{1, 2}, {2, 2}],
      UndirectedEdge[{2, 1}, {3, 1}], UndirectedEdge[{2, 2}, {3, 2}],
      UndirectedEdge[{3, 1}, {1, 2}], UndirectedEdge[{3, 2}, {1, 1}]};
    total = Graph[verts, edges];
    proj = Association @ Map[v |-> v -> v[[1]], verts];
    LocallyTrivialQ[total, proj],
    False,
    TestID -> "LocallyTrivialQ-TwistedTriangle"
]

(* ========================= FiberBundleQ ========================= *)

VerificationTest[
    total = GraphProduct[CompleteGraph[3], CycleGraph[5], "Cartesian"];
    proj = Association @ Map[v |-> v -> v[[2]], VertexList[total]];
    FiberBundleQ[total, proj],
    True,
    TestID -> "FiberBundleQ-ProductBundle"
]

VerificationTest[
    verts = Flatten[Table[{i, j}, {i, 5}, {j, 2}], 1];
    edges = Join[
      Flatten @ Table[UndirectedEdge[{i, j}, {Mod[i, 5] + 1, j}], {i, 4}, {j, 2}],
      {UndirectedEdge[{5, 1}, {1, 2}], UndirectedEdge[{5, 2}, {1, 1}]}];
    total = Graph[verts, edges];
    proj = Association @ Map[v |-> v -> v[[1]], verts];
    FiberBundleQ[total, proj],
    True,
    TestID -> "FiberBundleQ-MobiusBundle"
]
