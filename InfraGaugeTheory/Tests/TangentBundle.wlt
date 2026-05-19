(* ========================= GraphTangentBundle ========================= *)

VerificationTest[
    {tg, proj} = GraphTangentBundle[ CycleGraph[ 5 ] ];
    { GraphQ[ tg ], AssociationQ[ proj ] },
    { True, True },
    TestID -> "GraphTangentBundle-BasicOutput"
]

VerificationTest[
    {tg, proj} = GraphTangentBundle[ CycleGraph[ 5 ] ];
    Sort @ DeleteDuplicates @ Values @ proj === Sort @ VertexList @ CycleGraph[ 5 ],
    True,
    TestID -> "GraphTangentBundle-ProjectionSurjective"
]

VerificationTest[
    {tg, proj} = GraphTangentBundle[ CompleteGraph[ 4 ] ];
    VertexCount[ tg ] === 4 * 4,
    True,
    TestID -> "GraphTangentBundle-CompleteGraph-VertexCount"
]

(* --- List-valued vertices: iterated tangent bundle --- *)

VerificationTest[
    {tg, proj} = GraphTangentBundle[ CycleGraph[ 5 ] ];
    {ttg, proj2} = GraphTangentBundle[ tg ];
    { GraphQ[ ttg ], AssociationQ[ proj2 ] },
    { True, True },
    TestID -> "GraphTangentBundle-Iterated-BasicOutput"
]

VerificationTest[
    (* Cross horizontal edges must exist when vertices are lists.
       For TTG vertex {v, w} with v =!= w, Reverse {w, v} must be a TTG neighbor. *)
    {tg, proj} = GraphTangentBundle[ CycleGraph[ 6 ] ];
    {ttg, proj2} = GraphTangentBundle[ tg ];
    sample = Select[ VertexList[ ttg ], v |-> v[[ 1 ]] =!= v[[ 2 ]] ];
    AllTrue[ sample, v |-> MemberQ[ AdjacencyList[ ttg, v ], Reverse[ v ] ] ],
    True,
    TestID -> "GraphTangentBundle-Iterated-CrossEdges"
]

VerificationTest[
    (* TTG must have strictly more edges than just shared horizontal + vertical *)
    {tg, proj} = GraphTangentBundle[ CycleGraph[ 6 ] ];
    {ttg, proj2} = GraphTangentBundle[ tg ];
    EdgeCount[ ttg ] > VertexCount[ ttg ],
    True,
    TestID -> "GraphTangentBundle-Iterated-EdgeCount"
]

VerificationTest[
    (* Triple tangent bundle — deeply nested list vertices *)
    {tg, proj} = GraphTangentBundle[ PathGraph[ { 1, 2, 3 } ] ];
    {ttg, proj2} = GraphTangentBundle[ tg ];
    {tttg, proj3} = GraphTangentBundle[ ttg ];
    { GraphQ[ tttg ], VertexCount[ tttg ] > 0 },
    { True, True },
    TestID -> "GraphTangentBundle-TripleIterated-Builds"
]

VerificationTest[
    (* Cross edges survive at depth 3 *)
    {tg, proj} = GraphTangentBundle[ PathGraph[ { 1, 2, 3 } ] ];
    {ttg, proj2} = GraphTangentBundle[ tg ];
    {tttg, proj3} = GraphTangentBundle[ ttg ];
    sample = Select[ VertexList[ tttg ], v |-> v[[ 1 ]] =!= v[[ 2 ]] ];
    AllTrue[ sample, v |-> MemberQ[ AdjacencyList[ tttg, v ], Reverse[ v ] ] ],
    True,
    TestID -> "GraphTangentBundle-TripleIterated-CrossEdges"
]

(* ========================= TangentFiberedGraph ========================= *)

VerificationTest[
    {tg, proj} = GraphTangentBundle[ CycleGraph[ 5 ] ];
    {ttg, tProj} = TangentFiberedGraph[ tg, proj ];
    { GraphQ[ ttg ], AssociationQ[ tProj ] },
    { True, True },
    TestID -> "TangentFiberedGraph-BasicOutput"
]

VerificationTest[
    (* TangentFiberedGraph on TG projects to base, not to TG *)
    {tg, proj} = GraphTangentBundle[ CycleGraph[ 5 ] ];
    {ttg, tProj} = TangentFiberedGraph[ tg, proj ];
    Sort @ DeleteDuplicates @ Values @ tProj === Sort @ VertexList @ CycleGraph[ 5 ],
    True,
    TestID -> "TangentFiberedGraph-ProjectionToBase"
]

(* --- List-valued vertices: iterated TangentFiberedGraph --- *)

VerificationTest[
    {tg, proj} = GraphTangentBundle[ CycleGraph[ 6 ] ];
    {ttg, tProj} = TangentFiberedGraph[ tg, proj ];
    {tttg, ttProj} = TangentFiberedGraph[ ttg, tProj ];
    { GraphQ[ tttg ], AssociationQ[ ttProj ] },
    { True, True },
    TestID -> "TangentFiberedGraph-Iterated-BasicOutput"
]

VerificationTest[
    (* Cross horizontal edges in iterated TangentFiberedGraph *)
    {tg, proj} = GraphTangentBundle[ CycleGraph[ 6 ] ];
    {ttg, tProj} = TangentFiberedGraph[ tg, proj ];
    sample = Select[ VertexList[ ttg ], v |-> v[[ 1 ]] =!= v[[ 2 ]] ];
    AllTrue[ sample, v |-> MemberQ[ AdjacencyList[ ttg, v ], Reverse[ v ] ] ],
    True,
    TestID -> "TangentFiberedGraph-Iterated-CrossEdges"
]
