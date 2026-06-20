Package["WolframInstitute`InfraGaugeTheory`"]

PackageExport["CoordinatizeGraph"]

CoordinatizeGraph::usage = "CoordinatizeGraph[g, chains] assigns spatial coordinates to vertices of a directed graph g based on a list of chain graphs.";

CoordinatizeGraph[g_Graph, chains : {__Graph}] :=
  With[{graphDist = GraphDistanceMatrix[g]},
    AssociationThread[
      VertexList[g],
      Thread[Map[
        Function[c,
          With[{
            chainVertices = VertexList[c],
            chainIdx = AssociationMap[VertexIndex[g, #] &, VertexList[c]],
            chainCoord = AssociationThread[
              VertexList[c],
              MapThread[Min,
                GraphDistanceMatrix[c][[
                  VertexIndex[c,
                    Select[VertexList[c], VertexInDegree[c, #] == 0 &]],
                  All]]
              ]
            ]
          },
            Map[
              Min[Lookup[chainCoord,
                MinimalBy[chainVertices, cv |-> #[[chainIdx[cv]]]]]] &,
              graphDist
            ]
          ]
        ],
        chains
      ]]
    ]
  ];
