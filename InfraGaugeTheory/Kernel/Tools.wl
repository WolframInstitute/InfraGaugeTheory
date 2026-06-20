Package["WolframInstitute`InfraGaugeTheory`"]

PackageExport["HausdorffDistance"]
PackageExport["FrechetDistance"]
PackageExport["MeanFrechetDistance"]
PackageExport["Separation"]
PackageExport["SymmetricRelationGraph"]
PackageExport["GeodesicSubgraph"]

HausdorffDistance::usage = "HausdorffDistance[d, setX, setY] computes the Hausdorff distance between two sets using a distance matrix or graph.";
FrechetDistance::usage = "FrechetDistance[d, setX, setY] computes the Fr\[EAcute]chet distance between two point sets.";
MeanFrechetDistance::usage = "MeanFrechetDistance[d, setX, setY] computes the mean of point-wise distances between two sets.";
Separation::usage = "Separation[d, setX, setY] finds the minimum distance between two sets.";
SymmetricRelationGraph::usage = "SymmetricRelationGraph[f, v, opts] creates a graph where vertices are connected if a relation f holds between their values.";
GeodesicSubgraph::usage = "GeodesicSubgraph[g, pairs, opts] extracts geodesic paths connecting pairs of vertices or a thickness neighborhood around them.";

HausdorffDistance[ d_List, setX_, setY_ ] :=
  With[ { distSubMatrix = d[[ setX, setY ]] },
    Max[ Max[ Min /@ distSubMatrix ], Max[ Min /@ Transpose @ distSubMatrix ] ]
  ]

HausdorffDistance[ g_Graph, setX_List, setY_List ] :=
  With[ { distSubMatrix = Outer[ GraphDistance[ g, #1, #2 ] &, setX, setY, 1 ] },
    Max[ Max[ Min /@ distSubMatrix ], Max[ Min /@ Transpose @ distSubMatrix ] ]
  ]

FrechetDistance[ d_List, setX_, setY_ ] :=
  Max[ Diagonal[ d[[ setX, setY ]] ] ]

FrechetDistance[ g_Graph, setX_List, setY_List ] :=
  Max[ Diagonal[ Outer[ GraphDistance[ g, #1, #2 ] &, setX, setY, 1 ] ] ]

MeanFrechetDistance[ d_List, setX_, setY_ ] :=
  Total[ Diagonal[ d[[ setX, setY ]] ] ]

MeanFrechetDistance[ g_Graph, setX_List, setY_List ] :=
  Total[ Diagonal[ Outer[ GraphDistance[ g, #1, #2 ] &, setX, setY, 1 ] ] ]

Separation[ d_List, setX_, setY_ ] :=
  Min[ d[[ setX, setY ]] ]

Separation[ g_Graph, setX_List, setY_List ] :=
  Min[ Outer[ GraphDistance[ g, #1, #2 ] &, setX, setY, 1 ] ]

SymmetricRelationGraph[f_, v_Association, opts___] :=
  Graph[Keys[v],
    UndirectedEdge @@@ Pick[Subsets[Keys[v], {2}],
      f @@@ Subsets[Values[v], {2}]], opts];

Options[ GeodesicSubgraph ] = { "PathThickness" -> 0, "Directed" -> True };

GeodesicSubgraph[ g_, pairs_, OptionsPattern[] ] :=
  Module[ { distMatrix, thickness, vertexToIndex, selectedPaths, directed },
    thickness = OptionValue[ "PathThickness" ];
    directed = OptionValue[ "Directed" ];
    vertexToIndex = AssociationThread[ VertexList[ g ], Range @ VertexCount[ g ] ];
    distMatrix = GraphDistanceMatrix[ g ];
    selectedPaths = Which[
      thickness === 0,
      ( First @ FindPath[ g, #1, #2, { distMatrix[[ vertexToIndex[ #1 ], vertexToIndex[ #2 ] ]] }, 1 ] & ) @@@ pairs,
      thickness === Infinity,
      Flatten[ ( FindPath[ g, #1, #2, { distMatrix[[ vertexToIndex[ #1 ], vertexToIndex[ #2 ] ]] }, All ] & ) @@@ pairs, 1 ],
      True,
      Flatten[
        ( If[ # === {}, {},
            With[ { ref = vertexToIndex /@ First[ # ] },
              Select[ #, path |-> HausdorffDistance[ distMatrix, vertexToIndex /@ path, ref ] <= thickness ]
            ]
          ] & ) /@
        ( ( FindPath[ g, #1, #2, { distMatrix[[ vertexToIndex[ #1 ], vertexToIndex[ #2 ] ]] }, All ] & ) @@@ pairs ),
        1
      ]
    ];
    GraphUnion @@ ( PathGraph[ #, DirectedEdges -> directed ] & /@ selectedPaths )
  ]
