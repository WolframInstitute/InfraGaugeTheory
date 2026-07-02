(* ========================= InfraParallelTransport ========================= *)

(* Geodesics are auto-parallel: transporting along a path sends the germ toward q to *)
(* its straight continuation (3 -> 4 carries the germ 4 to 5, and 2 to 3). *)
VerificationTest[
    InfraParallelTransport[PathGraph[Range[6]], 3, 4],
    {<|2 -> 3, 4 -> 5|>},
    TestID -> "InfraParallelTransport-RadialAutoParallel"
]

(* Inverse edge undoes the transport. *)
VerificationTest[
    InfraParallelTransport[PathGraph[Range[6]], 4, 3],
    {<|3 -> 2, 5 -> 4|>},
    TestID -> "InfraParallelTransport-Invertible"
]

(* ========================= InfraLeviCivitaDefect ========================= *)

(* Exact metric isometry on a vertex-transitive substrate: zero defect. *)
VerificationTest[
    oct = EdgeDelete[CompleteGraph[6], {1 \[UndirectedEdge] 4, 2 \[UndirectedEdge] 5, 3 \[UndirectedEdge] 6}];
    InfraLeviCivitaDefect[oct, 1, 2],
    0,
    TestID -> "InfraLeviCivitaDefect-VertexTransitiveIsExact"
]

(* ========================= InfraHolonomyAngle ========================= *)

(* Flat substrate: a contractible square has trivial holonomy. *)
VerificationTest[
    torus = GraphProduct[CycleGraph[4], CycleGraph[4], "Cartesian"];
    InfraHolonomyAngle[torus, {{1, 1}, {1, 2}, {2, 2}, {2, 1}, {1, 1}}],
    0,
    TestID -> "InfraHolonomyAngle-FlatIsTrivial"
]

(* Positively curved substrate: an octahedron triangle holonomy is Pi/3. *)
VerificationTest[
    oct = EdgeDelete[CompleteGraph[6], {1 \[UndirectedEdge] 4, 2 \[UndirectedEdge] 5, 3 \[UndirectedEdge] 6}];
    Abs[InfraHolonomyAngle[oct, {1, 2, 3, 1}] - Pi/3] < 10^-6,
    True,
    TestID -> "InfraHolonomyAngle-CurvedIsNonzero"
]

(* ========================= InfraConnectionCurvature ========================= *)

(* Face curvature = holonomy around the closed boundary; matches the triangle angle. *)
VerificationTest[
    oct = EdgeDelete[CompleteGraph[6], {1 \[UndirectedEdge] 4, 2 \[UndirectedEdge] 5, 3 \[UndirectedEdge] 6}];
    Abs[InfraConnectionCurvature[oct, {1, 2, 3}] - Pi/3] < 10^-6,
    True,
    TestID -> "InfraConnectionCurvature-FaceHolonomy"
]

(* ========================= InfraHolonomy ========================= *)

VerificationTest[
    oct = EdgeDelete[CompleteGraph[6], {1 \[UndirectedEdge] 4, 2 \[UndirectedEdge] 5, 3 \[UndirectedEdge] 6}];
    AssociationQ @ InfraHolonomy[oct, {1, 2, 3, 1}],
    True,
    TestID -> "InfraHolonomy-ReturnsSelfMap"
]

(* ========================= FindInfraParallelFrame ========================= *)

(* Auto-parallel frame: the tangent germ toward 4 transports to the straight continuation. *)
VerificationTest[
    FindInfraParallelFrame[PathGraph[Range[6]], {2, 3, 4}, {3}],
    {{5}},
    TestID -> "FindInfraParallelFrame-AutoParallel"
]

(* ========================= FindInfraLeviCivitaConnection ========================= *)

(* The connection is returned as a fibration triple feedable to the rest of the paclet. *)
VerificationTest[
    oct = EdgeDelete[CompleteGraph[6], {1 \[UndirectedEdge] 4, 2 \[UndirectedEdge] 5, 3 \[UndirectedEdge] 6}];
    lc = FindInfraLeviCivitaConnection[oct];
    Sort @ Keys @ lc,
    {"Connection", "Projection", "TotalGraph"},
    TestID -> "FindInfraLeviCivitaConnection-ReturnsTriple"
]

(* Compatibility: the triple is a valid connection and feeds HolonomyMatrix. *)
VerificationTest[
    oct = EdgeDelete[CompleteGraph[6], {1 \[UndirectedEdge] 4, 2 \[UndirectedEdge] 5, 3 \[UndirectedEdge] 6}];
    lc = FindInfraLeviCivitaConnection[oct];
    ConnectionQ[lc["TotalGraph"], lc["Projection"], lc["Connection"]] &&
      Dimensions[HolonomyMatrix[lc["TotalGraph"], lc["Projection"], lc["Connection"], {1, 2, 3, 1}]] === {4, 4},
    True,
    TestID -> "FindInfraLeviCivitaConnection-FeedsHolonomyMatrix"
]

(* Only the metric method is implemented. *)
VerificationTest[
    FindInfraLeviCivitaConnection[CycleGraph[4], Method -> "Ribbon"],
    $Failed,
    {FindInfraLeviCivitaConnection::badmethod},
    TestID -> "FindInfraLeviCivitaConnection-BadMethod"
]
