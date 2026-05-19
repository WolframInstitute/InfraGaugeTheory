(* ========================= FindZeroSection ========================= *)

VerificationTest[
    {total, proj} = GraphTangentBundle[ CycleGraph[ 5 ] ];
    section = FindZeroSection[ total, proj ];
    AssociationQ[ section ],
    True,
    TestID -> "FindZeroSection-CycleGraph-Returns"
]

VerificationTest[
    {total, proj} = GraphTangentBundle[ CompleteGraph[ 4 ] ];
    section = FindZeroSection[ total, proj ];
    AllTrue[ Keys[ section ], v |-> section[ v ] === { v, v } ],
    True,
    TestID -> "FindZeroSection-CompleteGraph-DiagonalElements"
]

VerificationTest[
    {total, proj} = GraphTangentBundle[ PathGraph[ { 1, 2, 3 } ] ];
    section = FindZeroSection[ total, proj ];
    AllTrue[ Values[ section ], MemberQ[ VertexList[ total ], # ] & ],
    True,
    TestID -> "FindZeroSection-PathGraph-VerticesInTotal"
]

VerificationTest[
    {total, proj} = GraphTangentBundle[ CycleGraph[ 5 ] ];
    section = FindZeroSection[ total, proj ];
    AllTrue[ Keys[ section ], v |-> proj[ section[ v ] ] === v ],
    True,
    TestID -> "FindZeroSection-ProjectionProperty"
]

VerificationTest[
    {total, proj} = GraphTangentBundle[ CycleGraph[ 5 ] ];
    section = FindZeroSection[ total, proj ];
    SmoothSectionQ[ total, proj, section ],
    True,
    TestID -> "FindZeroSection-IsSmooth"
]

VerificationTest[
    total = Graph[ { "a", "b", "c" }, { UndirectedEdge[ "a", "b" ], UndirectedEdge[ "b", "c" ] } ];
    proj = Association[ "a" -> 1, "b" -> 1, "c" -> 2 ];
    FindZeroSection[ total, proj ],
    $Failed,
    TestID -> "FindZeroSection-NoZeroSection-Fails"
]

VerificationTest[
    {total, proj} = RandomFiberedGraph[ CycleGraph[ 4 ], "VerticalVertices" -> 3 ];
    FindZeroSection[ total, proj ],
    $Failed,
    TestID -> "FindZeroSection-RandomFiberedGraph-Fails"
]

(* ========================= FindCanonicalSection ========================= *)

VerificationTest[
    {total, proj} = GraphTangentBundle[ CycleGraph[ 5 ] ];
    section = FindCanonicalSection[ total, proj ];
    AssociationQ[ section ],
    True,
    TestID -> "FindCanonicalSection-CycleGraph-Returns"
]

VerificationTest[
    {total, proj} = GraphTangentBundle[ CompleteGraph[ 4 ] ];
    section = FindCanonicalSection[ total, proj ];
    AllTrue[ Keys[ section ], v |-> section[ v ] === { v, Reverse[ v ] } ],
    True,
    TestID -> "FindCanonicalSection-CompleteGraph-ReversedElements"
]

VerificationTest[
    {total, proj} = GraphTangentBundle[ CycleGraph[ 5 ] ];
    section = FindCanonicalSection[ total, proj ];
    ttg = First @ GraphTangentBundle[ total ];
    AllTrue[ Values[ section ], MemberQ[ VertexList[ ttg ], # ] & ],
    True,
    TestID -> "FindCanonicalSection-VerticesInTTG"
]

VerificationTest[
    {total, proj} = GraphTangentBundle[ PathGraph[ { 1, 2, 3 } ] ];
    section = FindCanonicalSection[ total, proj ];
    Length[ section ] === VertexCount[ total ],
    True,
    TestID -> "FindCanonicalSection-PathGraph-CorrectSize"
]

VerificationTest[
    total = Graph[ { "a", "b", "c" }, { UndirectedEdge[ "a", "b" ], UndirectedEdge[ "b", "c" ] } ];
    proj = Association[ "a" -> 1, "b" -> 1, "c" -> 2 ];
    FindCanonicalSection[ total, proj ],
    $Failed,
    TestID -> "FindCanonicalSection-NonTangentBundle-Fails"
]
