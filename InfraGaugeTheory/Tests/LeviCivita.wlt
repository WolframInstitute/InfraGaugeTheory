(* ========================= InfraParallelTransport ========================= *)

(* Geodesics are auto-parallel: transporting along a path sends the germ toward q to *)
(* its straight continuation (3 -> 4 carries the germ 4 to 5, and 2 to 3). *)
VerificationTest[
    InfraParallelTransport[PathGraph[Range[6]], 3, 4, 1],
    {<|2 -> 3, 4 -> 5|>},
    TestID -> "InfraParallelTransport-RadialAutoParallel"
]

(* Inverse edge undoes the transport. *)
VerificationTest[
    InfraParallelTransport[PathGraph[Range[6]], 4, 3, 1],
    {<|3 -> 2, 5 -> 4|>},
    TestID -> "InfraParallelTransport-Invertible"
]

(* Auto-parallel at scale r = 2: the radius-2 germs slide one step along the path. *)
VerificationTest[
    InfraParallelTransport[PathGraph[Range[9]], 4, 5, 2],
    {<|2 -> 3, 6 -> 7|>},
    TestID -> "InfraParallelTransport-RadialAutoParallelScale2"
]

(* Both angle structures agree on the path graph. *)
VerificationTest[
    InfraParallelTransport[PathGraph[Range[6]], 3, 4, 1, "Angle" -> "Alexandrov"],
    {<|2 -> 3, 4 -> 5|>},
    TestID -> "InfraParallelTransport-AlexandrovAngle"
]

(* Triangular lattice: the arc metric distinguishes 120 from 180 degrees (the chordal *)
(* Alexandrov metric collapses them), so the bond germ maps to the unique straight *)
(* continuation and the fiber symmetry shrinks to the dihedral stabilizer {id, flip}. *)
VerificationTest[
    tri = Graph[Flatten[Table[{i, j}, {i, 0, 4}, {j, 0, 4}], 1],
      Flatten[Table[{
         UndirectedEdge[{i, j}, {Mod[i + 1, 5], j}],
         UndirectedEdge[{i, j}, {i, Mod[j + 1, 5]}],
         UndirectedEdge[{i, j}, {Mod[i + 1, 5], Mod[j + 1, 5]}]}, {i, 0, 4}, {j, 0, 4}]]];
    {Length @ InfraParallelTransport[tri, {0, 0}, {1, 0}, 1],
     Lookup[First @ InfraParallelTransport[tri, {0, 0}, {1, 0}, 1], Key[{1, 0}]]},
    {2, {2, 0}},
    TestID -> "InfraParallelTransport-ArcResolvesTriangularPin"
]

(* ========================= InfraLeviCivitaDefect ========================= *)

(* Exact metric isometry on a vertex-transitive substrate: zero defect. *)
VerificationTest[
    oct = EdgeDelete[CompleteGraph[6], {1 \[UndirectedEdge] 4, 2 \[UndirectedEdge] 5, 3 \[UndirectedEdge] 6}];
    {InfraLeviCivitaDefect[oct, 1, 2, 1], InfraLeviCivitaDefect[oct, 1, 2, 1, "Angle" -> "Alexandrov"]},
    {0, 0},
    TestID -> "InfraLeviCivitaDefect-VertexTransitiveIsExact"
]

(* ========================= InfraHolonomyAngle ========================= *)

(* Flat substrate: a contractible square has trivial holonomy. *)
VerificationTest[
    torus = GraphProduct[CycleGraph[4], CycleGraph[4], "Cartesian"];
    InfraHolonomyAngle[torus, {{1, 1}, {1, 2}, {2, 2}, {2, 1}, {1, 1}}, 1],
    0,
    TestID -> "InfraHolonomyAngle-FlatIsTrivial"
]

(* Flat triangular lattice: a face of the tessellation bounds a flat region. *)
VerificationTest[
    tri = Graph[Flatten[Table[{i, j}, {i, 0, 4}, {j, 0, 4}], 1],
      Flatten[Table[{
         UndirectedEdge[{i, j}, {Mod[i + 1, 5], j}],
         UndirectedEdge[{i, j}, {i, Mod[j + 1, 5]}],
         UndirectedEdge[{i, j}, {Mod[i + 1, 5], Mod[j + 1, 5]}]}, {i, 0, 4}, {j, 0, 4}]]];
    InfraHolonomyAngle[tri, {{0, 0}, {1, 0}, {1, 1}, {0, 0}}, 1],
    0,
    TestID -> "InfraHolonomyAngle-FlatTriangularFaceIsTrivial"
]

(* Positively curved substrate: an octahedron triangle rotates the direction sphere by *)
(* one step of its 4-cycle link -- a quarter turn, 1 arc radian. Gauss-Bonnet is exact: *)
(* 8 faces x (1/4 turn) = 2 full turns = 4 Pi, the total curvature of the sphere. *)
VerificationTest[
    oct = EdgeDelete[CompleteGraph[6], {1 \[UndirectedEdge] 4, 2 \[UndirectedEdge] 5, 3 \[UndirectedEdge] 6}];
    Abs[InfraHolonomyAngle[oct, {1, 2, 3, 1}, 1] - 1] < 10^-6,
    True,
    TestID -> "InfraHolonomyAngle-CurvedIsNonzero"
]

(* The same holonomy read through the Alexandrov comparison angle is Pi/3. *)
VerificationTest[
    oct = EdgeDelete[CompleteGraph[6], {1 \[UndirectedEdge] 4, 2 \[UndirectedEdge] 5, 3 \[UndirectedEdge] 6}];
    Abs[InfraHolonomyAngle[oct, {1, 2, 3, 1}, 1, "Angle" -> "Alexandrov"] - Pi/3] < 10^-6,
    True,
    TestID -> "InfraHolonomyAngle-AlexandrovComparisonAngle"
]

(* ========================= InfraConnectionCurvature ========================= *)

(* Face curvature = holonomy around the closed boundary; matches the triangle angle. *)
VerificationTest[
    oct = EdgeDelete[CompleteGraph[6], {1 \[UndirectedEdge] 4, 2 \[UndirectedEdge] 5, 3 \[UndirectedEdge] 6}];
    Abs[InfraConnectionCurvature[oct, {1, 2, 3}, 1] - 1] < 10^-6,
    True,
    TestID -> "InfraConnectionCurvature-FaceHolonomy"
]

(* ========================= InfraHolonomy ========================= *)

VerificationTest[
    oct = EdgeDelete[CompleteGraph[6], {1 \[UndirectedEdge] 4, 2 \[UndirectedEdge] 5, 3 \[UndirectedEdge] 6}];
    AssociationQ @ InfraHolonomy[oct, {1, 2, 3, 1}, 1],
    True,
    TestID -> "InfraHolonomy-ReturnsSelfMap"
]

(* ========================= FindInfraParallelFrame ========================= *)

(* Auto-parallel frame: the tangent germ toward 4 transports to the straight continuation. *)
VerificationTest[
    FindInfraParallelFrame[PathGraph[Range[6]], {2, 3, 4}, {3}, 1],
    {{5}},
    TestID -> "FindInfraParallelFrame-AutoParallel"
]

(* ========================= FindInfraLeviCivitaConnection ========================= *)

(* The connection is returned as a fibration triple feedable to the rest of the paclet. *)
VerificationTest[
    oct = EdgeDelete[CompleteGraph[6], {1 \[UndirectedEdge] 4, 2 \[UndirectedEdge] 5, 3 \[UndirectedEdge] 6}];
    lc = FindInfraLeviCivitaConnection[oct, 1];
    Sort @ Keys @ lc,
    {"Connection", "Projection", "TotalGraph"},
    TestID -> "FindInfraLeviCivitaConnection-ReturnsTriple"
]

(* Compatibility: the triple is a valid connection and feeds HolonomyMatrix. *)
VerificationTest[
    oct = EdgeDelete[CompleteGraph[6], {1 \[UndirectedEdge] 4, 2 \[UndirectedEdge] 5, 3 \[UndirectedEdge] 6}];
    lc = FindInfraLeviCivitaConnection[oct, 1];
    ConnectionQ[lc["TotalGraph"], lc["Projection"], lc["Connection"]] &&
      Dimensions[HolonomyMatrix[lc["TotalGraph"], lc["Projection"], lc["Connection"], {1, 2, 3, 1}]] === {4, 4},
    True,
    TestID -> "FindInfraLeviCivitaConnection-FeedsHolonomyMatrix"
]

(* Only the metric method is implemented. *)
VerificationTest[
    FindInfraLeviCivitaConnection[CycleGraph[4], 1, Method -> "Ribbon"],
    $Failed,
    {FindInfraLeviCivitaConnection::badmethod},
    TestID -> "FindInfraLeviCivitaConnection-BadMethod"
]

(* ========================= InfraCanonicalOneForm ========================= *)

(* Vertical germs vanish; the geodesic spray evaluates to r d(x, y). *)
VerificationTest[
    {InfraCanonicalOneForm[PathGraph[Range[9]], {4, 6}, {4, 7}],
     InfraCanonicalOneForm[PathGraph[Range[9]], {4, 6}, {5, 7}],
     InfraCanonicalOneForm[PathGraph[Range[9]], {4, 6}, {3, 5}]},
    {0, 2, -2},
    TestID -> "InfraCanonicalOneForm-VerticalSprayAntiSpray"
]
