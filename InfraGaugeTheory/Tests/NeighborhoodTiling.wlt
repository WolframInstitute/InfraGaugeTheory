VerificationTest[
  With[{g = GridGraph[{5, 5}]},
    SeparatingNetQ[g, FindSeparatingNet[g, 2], "Distance" -> 2]
  ],
  True,
  TestID -> "NeighborhoodTiling-net-separated"
]

VerificationTest[
  With[{g = GridGraph[{5, 5}]},
    With[{net = FindSeparatingNet[g, 2]},
      AllTrue[VertexList[g], v |-> AnyTrue[net, c |-> GraphDistance[g, v, c] <= 2]]
    ]
  ],
  True,
  TestID -> "NeighborhoodTiling-net-covers"
]

VerificationTest[
  With[{g = CycleGraph[12]},
    With[{net = FindSeparatingNet[g, 2, Method -> "Maximum"]},
      SeparatingNetQ[g, net, "Distance" -> 2] &&
        AllTrue[VertexList[g], v |-> AnyTrue[net, c |-> GraphDistance[g, v, c] <= 2]]
    ]
  ],
  True,
  TestID -> "NeighborhoodTiling-net-maximum"
]

VerificationTest[
  With[{g = GridGraph[{5, 5}]},
    With[{net = FindSeparatingNet[g, 2]},
      With[{tiling = FindNeighborhoodTiling[g, net]},
        Sort[Keys[tiling]] === Sort[VertexList[g]] &&
          AllTrue[VertexList[g],
            v |-> GraphDistance[g, v, tiling[v]] == Min[GraphDistance[g, v, #] & /@ net]]
      ]
    ]
  ],
  True,
  TestID -> "NeighborhoodTiling-tiling-nearest-center"
]

VerificationTest[
  With[{g = PathGraph[Range[7]]},
    FindNeighborhoodTiling[g, {2, 6}, Method -> "All"][4]
  ],
  {2, 6},
  TestID -> "NeighborhoodTiling-tiling-all-ties"
]

VerificationTest[
  With[{g = GridGraph[{5, 5}]},
    With[{net = FindSeparatingNet[g, 2]},
      With[{tiling = FindNeighborhoodTiling[g, net, Method -> "Balanced"]},
        AllTrue[VertexList[g],
          v |-> GraphDistance[g, v, tiling[v]] == Min[GraphDistance[g, v, #] & /@ net]]
      ]
    ]
  ],
  True,
  TestID -> "NeighborhoodTiling-tiling-balanced-still-nearest"
]

VerificationTest[
  With[{g = CycleGraph[9]},
    With[{tiling = FindNeighborhoodTiling[g, FindSeparatingNet[g, 2]]},
      VertexCount[ReconstructBaseGraph[g, tiling]] == Length[DeleteDuplicates[Values[tiling]]]
    ]
  ],
  True,
  TestID -> "NeighborhoodTiling-tiling-base-reconstruction"
]
