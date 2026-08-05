VerificationTest[
  FindDiametralPaths[PathGraph[Range[5]]],
  {{1, 2, 3, 4, 5}},
  TestID -> "GraphAxes-diametral-path-graph"
]

VerificationTest[
  With[{paths = FindDiametralPaths[CycleGraph[6], All]},
    Length[paths] == 6 && AllTrue[paths, Length[#] == 4 &]
  ],
  True,
  TestID -> "GraphAxes-diametral-cycle-all"
]

VerificationTest[
  Length[FindDiametralPaths[CycleGraph[6], 2]],
  2,
  TestID -> "GraphAxes-diametral-count"
]

VerificationTest[
  FindLongestGeodesicsThrough[PathGraph[Range[5]], 3],
  {{1, 2, 3, 4, 5}},
  TestID -> "GraphAxes-longest-geodesics-through-center"
]

VerificationTest[
  Sort[FindLongestGeodesicsThrough[CycleGraph[6], 1, All]],
  {{2, 1, 6, 5}, {3, 2, 1, 6}},
  TestID -> "GraphAxes-longest-geodesics-through-all"
]

VerificationTest[
  With[{axes = FindGraphAxes[GridGraph[{3, 3}]]},
    Length[axes] == 2 && Sort[Sort[{First[#], Last[#]}] & /@ axes] === {{1, 9}, {3, 7}}
  ],
  True,
  TestID -> "GraphAxes-axes-grid"
]

VerificationTest[
  Length[FindGraphAxes[GridGraph[{3, 3}], "MaxAxes" -> 1]],
  1,
  TestID -> "GraphAxes-axes-max-axes"
]

VerificationTest[
  With[{axes = FindGraphAxesThrough[GridGraph[{3, 3}], 5]},
    Length[axes] == 2 && AllTrue[axes, MemberQ[#, 5] && Length[#] == 5 &]
  ],
  True,
  TestID -> "GraphAxes-axes-through-center"
]
