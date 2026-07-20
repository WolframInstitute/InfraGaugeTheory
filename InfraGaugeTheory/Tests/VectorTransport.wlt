(* ========================= InfraVectorTransport ========================= *)

(* Auto-parallel: the vector 4 -> 6 slides along the path to 5 -> 7. *)
VerificationTest[
    InfraVectorTransport[PathGraph[Range[9]], 6, {4, 5}],
    {7},
    TestID -> "InfraVectorTransport-AutoParallel"
]

(* The zero vector transports to the zero vector at the endpoint. *)
VerificationTest[
    InfraVectorTransport[PathGraph[Range[9]], 4, {4, 5}],
    {5},
    TestID -> "InfraVectorTransport-ZeroVector"
]

(* Vectors longer than the scale r are dropped. *)
VerificationTest[
    InfraVectorTransport[PathGraph[Range[9]], 6, {4, 5}, 1],
    {},
    TestID -> "InfraVectorTransport-ScaleRestriction"
]

(* A vector-field value is transported like the single vector. *)
VerificationTest[
    InfraVectorTransport[PathGraph[Range[9]], <|4 -> 6|>, {4, 5}],
    {7},
    TestID -> "InfraVectorTransport-FieldValue"
]

(* Scale-2 transport on the grid interior is rigid: a single parallel endpoint. *)
VerificationTest[
    InfraVectorTransport[GridGraph[{7, 7}], 20, {18, 25, 32}],
    {34},
    TestID -> "InfraVectorTransport-GridRigidScale2"
]

(* Flat substrate: around a closed plaquette walk the vector returns to itself. *)
VerificationTest[
    InfraVectorTransport[GridGraph[{7, 7}], 27, {25, 26, 33, 32, 25}],
    {27},
    TestID -> "InfraVectorTransport-FlatLoopReturns"
]

(* Curved substrate: around a buckyball pentagon a scale-2 vector comes back *)
(* rotated -- a single realisation different from the start vector. *)
VerificationTest[
    bucky = BuckyballGraph[];
    pent = First[FindCycle[bucky, {5}, 1]][[All, 1]];
    loop = Append[pent, First[pent]];
    v = First[Select[VertexList[bucky], GraphDistance[bucky, First[pent], #] == 2 &]];
    out = InfraVectorTransport[bucky, v, loop];
    {Length[out], out =!= {v}, InfraHolonomyAngle[bucky, loop, 2] > 0},
    {1, True, True},
    TestID -> "InfraVectorTransport-CurvedLoopRotates"
]

(* ========================= InfraCovariantDerivative ========================= *)

(* A parallel field has zero covariant derivative: every walk vertex maps to itself. *)
VerificationTest[
    InfraCovariantDerivative[PathGraph[Range[9]], AssociationThread[Range[8], Range[2, 9]], {3, 4, 5}],
    <|3 -> 3, 4 -> 4|>,
    TestID -> "InfraCovariantDerivative-ParallelFieldIsZero"
]

(* The same on the grid, at the reflection-ambiguous scale 1: the zero pair is *)
(* found first, so the derivative is still the zero field. *)
VerificationTest[
    InfraCovariantDerivative[GridGraph[{7, 7}], <|18 -> 19, 25 -> 26, 32 -> 33|>, {18, 25, 32}],
    <|18 -> 18, 25 -> 25|>,
    TestID -> "InfraCovariantDerivative-GridParallelField"
]

(* A field with a missing value gives Missing at the dead step. *)
VerificationTest[
    MissingQ @ InfraCovariantDerivative[PathGraph[Range[9]], <|3 -> 4|>, {3, 4}][3],
    True,
    TestID -> "InfraCovariantDerivative-DeadTransportIsMissing"
]

(* All endpoints: n -> All returns the sorted list of difference endpoints. *)
VerificationTest[
    InfraCovariantDerivative[PathGraph[Range[9]], AssociationThread[Range[8], Range[2, 9]], {3, 4, 5}, Automatic, All],
    <|3 -> {3}, 4 -> {4}|>,
    TestID -> "InfraCovariantDerivative-AllEndpoints"
]
