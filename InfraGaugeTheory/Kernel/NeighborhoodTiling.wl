Package["WolframInstitute`InfraGaugeTheory`"]

PackageExport["FindSeparatingNet"]
PackageExport["SeparatingNetQ"]
PackageExport["FindNeighborhoodTiling"]

FindSeparatingNet::usage = "FindSeparatingNet[g, k] finds a maximal k-separated vertex set: centers pairwise at graph distance > k, with every vertex within distance k of some center. Method -> \"Greedy\" (default; deterministic scan in vertex order) | \"Maximum\" (largest such set; exhaustive, expensive on large graphs).";

Options[FindSeparatingNet] = {Method -> "Greedy"};

FindSeparatingNet[g_Graph, k_Integer : 1, OptionsPattern[]] :=
  With[{power = GraphPower[g, k]},
    Switch[OptionValue[Method],
      "Greedy",
        Fold[If[IntersectingQ[AdjacencyList[power, #2], #1], #1, Append[#1, #2]] &, {}, VertexList[g]],
      "Maximum",
        First @ FindIndependentVertexSet[power]
    ]
  ]

SeparatingNetQ::usage = "SeparatingNetQ[g, set] tests whether the vertices of set are pairwise at graph distance greater than the option \"Distance\" (default 1).";

Options[SeparatingNetQ] = {"Distance" -> 1};

SeparatingNetQ[g_Graph, set_List, OptionsPattern[]] :=
  With[{k = OptionValue["Distance"]},
    AllTrue[Subsets[set, {2}], GraphDistance[g, #[[1]], #[[2]]] > k &]
  ]

FindNeighborhoodTiling::usage = "FindNeighborhoodTiling[g, centers] assigns every vertex of g to a nearest center and returns the tiling as an Association vertex -> center, a graph fibration projection onto the centers. Method -> \"First\" (default; ties to the earliest center) | \"All\" (ties kept as a list) | \"Random\" | \"Balanced\" (ties broken toward smaller tiles).";

Options[FindNeighborhoodTiling] = {Method -> "First"};

FindNeighborhoodTiling[g_Graph, centers_List, OptionsPattern[]] :=
  With[
    {candidates = With[{rows = Transpose[GraphDistance[g, #] & /@ centers]},
       AssociationThread[VertexList[g], Pick[centers, #, Min[#]] & /@ rows]]},
    Switch[OptionValue[Method],
      "First", First /@ candidates,
      "All", candidates,
      "Random", RandomChoice /@ candidates,
      "Balanced",
        Module[{counts = AssociationThread[centers -> 0], assignments = <||>},
          Do[
            With[{best = First @ MinimalBy[candidates[v], counts]},
              assignments[v] = best;
              counts[best] += 1
            ],
            {v, SortBy[Keys[candidates], Length @* candidates]}
          ];
          KeyTake[assignments, VertexList[g]]
        ]
    ]
  ]
