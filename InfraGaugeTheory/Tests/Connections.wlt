(* ========================= RandomConnection ========================= *)

VerificationTest[
    SeedRandom[42];
    {total, proj} = RandomFiberedGraph[CycleGraph[5], "VerticalVertices" -> 3, "IsomorphicFibers" -> True];
    conn = RandomConnection[total, proj];
    GraphQ[conn],
    True,
    TestID -> "RandomConnection-ReturnsGraph"
]

VerificationTest[
    SeedRandom[42];
    {total, proj} = RandomFiberedGraph[CycleGraph[5], "VerticalVertices" -> 3, "IsomorphicFibers" -> True];
    conn = RandomConnection[total, proj];
    SubsetQ[EdgeList[total], EdgeList[conn]],
    True,
    TestID -> "RandomConnection-SubgraphOfTotal"
]

(* ========================= ConnectionQ ========================= *)

VerificationTest[
    SeedRandom[42];
    {total, proj} = RandomFiberedGraph[CycleGraph[5], "VerticalVertices" -> 3, "IsomorphicFibers" -> True];
    conn = RandomConnection[total, proj];
    ConnectionQ[total, proj, conn],
    True,
    TestID -> "ConnectionQ-RandomConnectionIsValid"
]

VerificationTest[
    SeedRandom[42];
    {total, proj} = RandomFiberedGraph[CompleteGraph[4], "VerticalVertices" -> 2, "IsomorphicFibers" -> True];
    conn = RandomConnection[total, proj];
    ConnectionQ[total, proj, conn],
    True,
    TestID -> "ConnectionQ-CompleteBase"
]

VerificationTest[
    {total, proj} = RandomFiberedGraph[CycleGraph[5], "VerticalVertices" -> 3, "IsomorphicFibers" -> True];
    ConnectionQ[total, proj, Graph[VertexList[total], {}]],
    False,
    TestID -> "ConnectionQ-EmptyGraphIsNotConnection"
]

(* ========================= AtMostOneEdgeLiftQ ========================= *)

VerificationTest[
    SeedRandom[42];
    {total, proj} = RandomFiberedGraph[CycleGraph[5], "VerticalVertices" -> 3, "IsomorphicFibers" -> True];
    conn = RandomConnection[total, proj];
    AtMostOneEdgeLiftQ[conn, proj],
    True,
    TestID -> "AtMostOneEdgeLiftQ-ConnectionHasUniqueLift"
]

(* ========================= FindHorizontalLift ========================= *)

VerificationTest[
    SeedRandom[42];
    {total, proj} = RandomFiberedGraph[CycleGraph[5], "VerticalVertices" -> 3, "IsomorphicFibers" -> True];
    conn = RandomConnection[total, proj];
    fibers = GroupBy[Normal @ proj, Last -> First];
    basePath = {1, 2, 3};
    startVertex = First @ fibers[1];
    curve = FindHorizontalLift[total, proj, conn, startVertex, basePath];
    proj /@ curve === basePath,
    True,
    TestID -> "FindHorizontalLift-ProjectsToBasePath"
]

VerificationTest[
    SeedRandom[42];
    {total, proj} = RandomFiberedGraph[CycleGraph[5], "VerticalVertices" -> 3, "IsomorphicFibers" -> True];
    conn = RandomConnection[total, proj];
    fibers = GroupBy[Normal @ proj, Last -> First];
    curve = FindHorizontalLift[total, proj, conn, First @ fibers[1], {1}];
    curve,
    {First @ fibers[1]},
    TestID -> "FindHorizontalLift-SingleVertex"
]

(* ========================= ParallelTransport ========================= *)

VerificationTest[
    SeedRandom[42];
    {total, proj} = RandomFiberedGraph[CycleGraph[5], "VerticalVertices" -> 3, "IsomorphicFibers" -> True];
    conn = RandomConnection[total, proj];
    transport = ParallelTransport[total, proj, conn, {1, 2, 3}];
    AssociationQ[transport] && AllTrue[Values[transport], proj[#] === 3 &],
    True,
    TestID -> "ParallelTransport-MapsToEndFiber"
]

VerificationTest[
    SeedRandom[42];
    {total, proj} = RandomFiberedGraph[CycleGraph[5], "VerticalVertices" -> 3, "IsomorphicFibers" -> True];
    conn = RandomConnection[total, proj];
    transport = ParallelTransport[total, proj, conn, {1}];
    transport,
    <||>,
    TestID -> "ParallelTransport-TrivialPath"
]

(* ========================= HolonomyMatrix ========================= *)

VerificationTest[
    SeedRandom[42];
    {total, proj} = RandomFiberedGraph[CycleGraph[5], "VerticalVertices" -> 3, "IsomorphicFibers" -> True];
    conn = RandomConnection[total, proj];
    mat = HolonomyMatrix[total, proj, conn, {1, 2, 3, 4, 5, 1}];
    Dimensions[mat] === {3, 3},
    True,
    TestID -> "HolonomyMatrix-CorrectDimensions"
]

VerificationTest[
    total = GraphProduct[CompleteGraph[3], CycleGraph[4], "Cartesian"];
    proj = Association @ Map[v |-> v -> v[[2]], VertexList[total]];
    conn = RandomConnection[total, proj];
    mat = HolonomyMatrix[total, proj, conn, {1, 2, 3, 4, 1}];
    Normal[mat] === IdentityMatrix[3],
    True,
    TestID -> "HolonomyMatrix-TrivialBundleIsIdentity"
]

(* ========================= HolonomyMatrices ========================= *)

VerificationTest[
    SeedRandom[42];
    {total, proj} = RandomFiberedGraph[CycleGraph[5], "VerticalVertices" -> 3, "IsomorphicFibers" -> True];
    conn = RandomConnection[total, proj];
    hols = HolonomyMatrices[total, proj, conn, 1];
    AssociationQ[hols] && AllTrue[Values[hols], MatrixQ],
    True,
    TestID -> "HolonomyMatrices-ReturnsAssociationOfMatrices"
]

VerificationTest[
    {total, proj} = RandomFiberedGraph[PathGraph[Range[4]], "VerticalVertices" -> 2, "IsomorphicFibers" -> True];
    conn = RandomConnection[total, proj];
    hols = HolonomyMatrices[total, proj, conn, 1];
    hols,
    <||>,
    TestID -> "HolonomyMatrices-TreeHasNoCycles"
]

VerificationTest[
    SeedRandom[42];
    {total, proj} = RandomFiberedGraph[CycleGraph[5], "VerticalVertices" -> 3, "IsomorphicFibers" -> True];
    conn = RandomConnection[total, proj];
    hols = HolonomyMatrices[total, proj, conn, 1];
    AllTrue[Values[hols], Dimensions[#] === {3, 3} &],
    True,
    TestID -> "HolonomyMatrices-CorrectDimensions"
]

VerificationTest[
    SeedRandom[42];
    {total, proj} = RandomFiberedGraph[CycleGraph[5], "VerticalVertices" -> 3, "IsomorphicFibers" -> True];
    conn = RandomConnection[total, proj];
    hols = HolonomyMatrices[total, proj, conn, 1];
    AllTrue[Keys[hols], First[#] === Last[#] === 1 &],
    True,
    TestID -> "HolonomyMatrices-LoopsStartAndEndAtBasepoint"
]

VerificationTest[
    verts = Flatten[Table[{i, j}, {i, 4}, {j, 2}], 1];
    edges = {
      UndirectedEdge[{1, 1}, {2, 1}], UndirectedEdge[{1, 2}, {2, 2}],
      UndirectedEdge[{2, 1}, {3, 1}], UndirectedEdge[{2, 2}, {3, 2}],
      UndirectedEdge[{3, 1}, {4, 1}], UndirectedEdge[{3, 2}, {4, 2}],
      UndirectedEdge[{4, 1}, {1, 2}], UndirectedEdge[{4, 2}, {1, 1}]};
    total = Graph[verts, edges];
    proj = Association @ Map[v |-> v -> v[[1]], verts];
    conn = RandomConnection[total, proj];
    hols = HolonomyMatrices[total, proj, conn, 1];
    AnyTrue[Values[hols], # =!= IdentityMatrix[2] &],
    True,
    TestID -> "HolonomyMatrices-MobiusHasNontrivialHolonomy"
]

(* ========================= FlatConnectionQ ========================= *)

VerificationTest[
    total = GraphProduct[CompleteGraph[3], CycleGraph[4], "Cartesian"];
    proj = Association @ Map[v |-> v -> v[[2]], VertexList[total]];
    conn = RandomConnection[total, proj];
    FlatConnectionQ[total, proj, conn],
    True,
    TestID -> "FlatConnectionQ-TrivialBundleIsFlat"
]

VerificationTest[
    verts = Flatten[Table[{i, j}, {i, 4}, {j, 2}], 1];
    edges = {
      UndirectedEdge[{1, 1}, {2, 1}], UndirectedEdge[{1, 2}, {2, 2}],
      UndirectedEdge[{2, 1}, {3, 1}], UndirectedEdge[{2, 2}, {3, 2}],
      UndirectedEdge[{3, 1}, {4, 1}], UndirectedEdge[{3, 2}, {4, 2}],
      UndirectedEdge[{4, 1}, {1, 2}], UndirectedEdge[{4, 2}, {1, 1}]};
    total = Graph[verts, edges];
    proj = Association @ Map[v |-> v -> v[[1]], verts];
    conn = RandomConnection[total, proj];
    FlatConnectionQ[total, proj, conn],
    False,
    TestID -> "FlatConnectionQ-MobiusIsNotFlat"
]

VerificationTest[
    {total, proj} = RandomFiberedGraph[PathGraph[Range[4]], "VerticalVertices" -> 3, "IsomorphicFibers" -> True];
    conn = RandomConnection[total, proj];
    FlatConnectionQ[total, proj, conn],
    True,
    TestID -> "FlatConnectionQ-TreeIsAlwaysFlat"
]

(* ========================= FindHorizontalLeaf ========================= *)

VerificationTest[
    total = GraphProduct[CompleteGraph[3], CycleGraph[4], "Cartesian"];
    proj = Association @ Map[v |-> v -> v[[2]], VertexList[total]];
    conn = RandomConnection[total, proj];
    startV = First @ Select[VertexList[total], proj[#] === 1 &];
    leaf = FindHorizontalLeaf[conn, proj, startV];
    AssociationQ[leaf] && Sort[Keys[leaf]] === Sort[VertexList[ReconstructBaseGraph[total, proj]]],
    True,
    TestID -> "FindHorizontalLeaf-TrivialBundleCoversBase"
]

VerificationTest[
    total = GraphProduct[CompleteGraph[3], CycleGraph[4], "Cartesian"];
    proj = Association @ Map[v |-> v -> v[[2]], VertexList[total]];
    conn = RandomConnection[total, proj];
    fiberOver1 = Select[VertexList[total], proj[#] === 1 &];
    AllTrue[fiberOver1, AssociationQ @ FindHorizontalLeaf[conn, proj, #] &],
    True,
    TestID -> "FindHorizontalLeaf-TrivialBundleAllLeavesExist"
]

VerificationTest[
    verts = Flatten[Table[{i, j}, {i, 4}, {j, 2}], 1];
    edges = {
      UndirectedEdge[{1, 1}, {2, 1}], UndirectedEdge[{1, 2}, {2, 2}],
      UndirectedEdge[{2, 1}, {3, 1}], UndirectedEdge[{2, 2}, {3, 2}],
      UndirectedEdge[{3, 1}, {4, 1}], UndirectedEdge[{3, 2}, {4, 2}],
      UndirectedEdge[{4, 1}, {1, 2}], UndirectedEdge[{4, 2}, {1, 1}]};
    total = Graph[verts, edges];
    proj = Association @ Map[v |-> v -> v[[1]], verts];
    conn = RandomConnection[total, proj];
    FindHorizontalLeaf[conn, proj, {1, 1}],
    $Failed,
    TestID -> "FindHorizontalLeaf-MobiusNoLeaves"
]

VerificationTest[
    verts = Flatten[Table[{i, j}, {i, 4}, {j, 3}], 1];
    edges = Join[
      Flatten @ Table[UndirectedEdge[{i, j}, {i + 1, j}], {i, 3}, {j, 3}],
      {UndirectedEdge[{4, 1}, {1, 2}], UndirectedEdge[{4, 2}, {1, 1}],
       UndirectedEdge[{4, 3}, {1, 3}]}];
    total = Graph[verts, edges];
    proj = Association @ Map[v |-> v -> v[[1]], verts];
    conn = RandomConnection[total, proj];
    {FindHorizontalLeaf[conn, proj, {1, 1}],
     FindHorizontalLeaf[conn, proj, {1, 2}],
     AssociationQ @ FindHorizontalLeaf[conn, proj, {1, 3}]},
    {$Failed, $Failed, True},
    TestID -> "FindHorizontalLeaf-PartialTwistOnlyFixedPointSucceeds"
]
