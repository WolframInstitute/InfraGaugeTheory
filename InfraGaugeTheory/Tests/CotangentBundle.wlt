(* ========================= GraphTangentBundle Method ========================= *)

VerificationTest[
    {tg, proj} = GraphTangentBundle[ CycleGraph[ 6 ], Method -> "Complete" ];
    { GraphQ[ tg ], AssociationQ[ proj ], VertexCount[ tg ] === 18 },
    { True, True, True },
    TestID -> "GraphTangentBundle-Complete-BasicOutput"
]

VerificationTest[
    (* the three scaffolds carry one vertex set *)
    verts = Sort @ VertexList @ First @ GraphTangentBundle[ CycleGraph[ 6 ], Method -> # ] & /@
      { "Diagonal", "Metric", "Complete" };
    SameQ @@ verts,
    True,
    TestID -> "GraphTangentBundle-Methods-SameVertices"
]

VerificationTest[
    (* Diagonal subset of Metric subset of Complete *)
    es = Sort /@ ( List @@@ EdgeList @ First @ GraphTangentBundle[ GridGraph[ { 3, 3 } ], Method -> # ] ) & /@
      { "Diagonal", "Metric", "Complete" };
    { SubsetQ[ es[[ 2 ]], es[[ 1 ]] ], SubsetQ[ es[[ 3 ]], es[[ 2 ]] ] },
    { True, True },
    TestID -> "GraphTangentBundle-Methods-Nesting"
]

VerificationTest[
    (* sphere model at radius 1 has one germ per directed edge *)
    VertexCount @ First @ GraphTangentBundle[ PetersenGraph[ ], "Fiber" -> "Sphere" ] ===
      2 EdgeCount @ PetersenGraph[ ],
    True,
    TestID -> "GraphTangentBundle-Sphere-GermCount"
]

VerificationTest[
    Quiet @ GraphTangentBundle[ CycleGraph[ 5 ], Method -> "Nonsense" ],
    $Failed,
    TestID -> "GraphTangentBundle-BadMethod"
]

(* ========================= CotangentFlip ========================= *)

VerificationTest[
    flip = CotangentFlip[ CycleGraph[ 6 ], "Fiber" -> "Sphere" ];
    { Length @ flip === 12, AllTrue[ Normal @ flip, Reverse @ First @ # === Last @ # & ] },
    { True, True },
    TestID -> "CotangentFlip-Involution"
]

VerificationTest[
    (* the flip is an automorphism of the metric and diagonal scaffolds, not the complete one *)
    CotangentFlipQ[ CycleGraph[ 6 ], Method -> #, "Fiber" -> "Sphere" ] & /@
      { "Diagonal", "Metric", "Complete" },
    { True, True, False },
    TestID -> "CotangentFlipQ-ByMethod"
]

(* ========================= InfraTautologicalSection ========================= *)

VerificationTest[
    section = InfraTautologicalSection[ CycleGraph[ 6 ], "Fiber" -> "Sphere" ];
    { AssociationQ @ section,
      Length @ section === 12,
      AllTrue[ Normal @ section, First @ # === First @ Last @ # & ],
      AllTrue[ Normal @ section, Reverse @ First @ # === Last @ Last @ # & ] },
    { True, True, True, True },
    TestID -> "InfraTautologicalSection-IsFlippedSection"
]

VerificationTest[
    (* the diagonal scaffold admits no tautological germ *)
    Quiet @ InfraTautologicalSection[ CycleGraph[ 6 ], Method -> "Diagonal", "Fiber" -> "Sphere" ],
    $Failed,
    TestID -> "InfraTautologicalSection-DiagonalFails"
]

VerificationTest[
    (* it is a smooth section of the double cotangent graph *)
    cot = First @ GraphCotangentBundle[ CycleGraph[ 6 ], "Fiber" -> "Sphere" ];
    { dcot, dproj } = GraphCotangentBundle[ cot, "Fiber" -> "Sphere" ];
    SmoothSectionQ[ dcot, dproj, InfraTautologicalSection[ CycleGraph[ 6 ], "Fiber" -> "Sphere" ] ],
    True,
    TestID -> "InfraTautologicalSection-Smooth"
]

VerificationTest[
    (* the pairing on the tautological germ is d(s,t)^2, so 1 at radius 1 *)
    DeleteDuplicates @ Values @ InfraTautologicalOneForm[ CycleGraph[ 6 ], "Fiber" -> "Sphere" ],
    { 1 },
    TestID -> "InfraTautologicalOneForm-UnitValue"
]

(* ========================= FindCanonicalSection ========================= *)

VerificationTest[
    (* the canonical section of the tangent graph is the tautological germ *)
    tg = First @ GraphTangentBundle[ CycleGraph[ 6 ], "Fiber" -> "Sphere" ];
    section = FindCanonicalSection[ tg, Association @ Map[ # -> First @ # &, VertexList @ tg ] ];
    { AssociationQ @ section, AllTrue[ Normal @ section, Reverse @ First @ # === Last @ Last @ # & ] },
    { True, True },
    TestID -> "FindCanonicalSection-FlippedGerm"
]
