VerificationTest[
  CoordinatizeGraph[GridGraph[{3, 3}], {Graph[{1 -> 4, 4 -> 7}], Graph[{1 -> 2, 2 -> 3}]}],
  <|1 -> {0, 0}, 2 -> {0, 1}, 3 -> {0, 2}, 4 -> {1, 0}, 5 -> {1, 1}, 6 -> {1, 2}, 7 -> {2, 0}, 8 -> {2, 1}, 9 -> {2, 2}|>,
  TestID -> "Coordinatization-grid-two-chains"
]

VerificationTest[
  CoordinatizeGraph[Graph[{1 -> 2, 2 -> 3, 3 -> 4}], {Graph[{1 -> 2, 2 -> 3, 3 -> 4}]}],
  <|1 -> {0}, 2 -> {1}, 3 -> {2}, 4 -> {3}|>,
  TestID -> "Coordinatization-directed-path-self-chain"
]

VerificationTest[
  CoordinatizeGraph[PathGraph[Range[3]], {Graph[{1 -> 3}]}],
  <|1 -> {0}, 2 -> {0}, 3 -> {1}|>,
  TestID -> "Coordinatization-tie-takes-min"
]
