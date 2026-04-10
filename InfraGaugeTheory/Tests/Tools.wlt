VerificationTest[
    HausdorffDistance[GraphDistanceMatrix[CycleGraph[6]], {1, 2}, {4, 5}],
    _Integer,
    SameTest -> MatchQ,
    TestID -> "HausdorffDistance-Matrix"
]

VerificationTest[
    HausdorffDistance[CycleGraph[6], {1, 2}, {4, 5}],
    _Integer,
    SameTest -> MatchQ,
    TestID -> "HausdorffDistance-Graph"
]

VerificationTest[
    FrechetDistance[GraphDistanceMatrix[CycleGraph[6]], {1, 2, 3}, {4, 5, 6}],
    _Integer,
    SameTest -> MatchQ,
    TestID -> "FrechetDistance-Matrix"
]

VerificationTest[
    MeanFrechetDistance[GraphDistanceMatrix[CycleGraph[6]], {1, 2, 3}, {4, 5, 6}],
    _Integer,
    SameTest -> MatchQ,
    TestID -> "MeanFrechetDistance-Matrix"
]

VerificationTest[
    Separation[GraphDistanceMatrix[CycleGraph[6]], {1, 2}, {4, 5}],
    _Integer,
    SameTest -> MatchQ,
    TestID -> "Separation-Matrix"
]

VerificationTest[
    GraphQ[GeodesicSubgraph[CycleGraph[6], {{1, 4}}]],
    True,
    TestID -> "GeodesicSubgraph-Basic"
]
