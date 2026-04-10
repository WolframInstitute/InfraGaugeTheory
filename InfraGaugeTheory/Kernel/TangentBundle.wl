Package["WolframInstitute`InfraGaugeTheory`"]

PackageExport["GraphTangentBundle"]
PackageExport["TangentFiberedGraph"]
PackageExport["ZeroSection"]

Options[ GraphTangentBundle ] = { "Radius" -> 1 };

GraphTangentBundle[ g_Graph, opts : OptionsPattern[] ] :=
  Module[ { vertices, radius, fibers, allVertices, verticalEdges, horizontalEdges, projection },
    vertices = VertexList[ g ];
    radius = OptionValue[ "Radius" ];
    fibers = Association @ Map[
      vertex |-> vertex -> NeighborhoodGraph[ g, vertex, radius ],
      vertices
    ];
    allVertices = Flatten[ Map[ vertex |-> { vertex, # } & /@ VertexList[ fibers[ vertex ] ], vertices ], 1 ];
    verticalEdges = Flatten[
      Map[
        vertex |->
          UndirectedEdge[ { vertex, #[[ 1 ]] }, { vertex, #[[ 2 ]] } ] & /@ EdgeList[ fibers[ vertex ] ],
        vertices
      ],
      1
    ];
    horizontalEdges = Flatten[
      Map[
        baseEdge |->
          With[ { source = baseEdge[[ 1 ]], target = baseEdge[[ 2 ]],
                  sourceVerts = VertexList[ fibers[ baseEdge[[ 1 ]] ] ],
                  targetVerts = VertexList[ fibers[ baseEdge[[ 2 ]] ] ] },
            Flatten[ {
              UndirectedEdge[ { source, # }, { target, # } ] & /@
                Intersection[ sourceVerts, targetVerts ],
              Select[
                Flatten[ Outer[ UndirectedEdge[ { source, #1 }, { target, #2 } ] &, sourceVerts, targetVerts ], 1 ],
                edge |-> EdgeQ[ g, UndirectedEdge[ edge[[ 1, 2 ]], edge[[ 2, 2 ]] ] ]
              ]
            }, 1 ]
          ],
        EdgeList[ g ]
      ],
      1
    ];
    horizontalEdges = DeleteDuplicatesBy[ horizontalEdges, Sort @* List @@ # & ];
    projection = Association @ Map[ # -> #[[ 1 ]] &, allVertices ];
    { Graph[ allVertices, Join[ verticalEdges, horizontalEdges ] ], projection }
  ]

ZeroSection[ g_Graph ] :=
  Association @ Map[ vertex |-> vertex -> { vertex, vertex }, VertexList[ g ] ]

Options[ TangentFiberedGraph ] = { "Radius" -> 1 };

TangentFiberedGraph[ total_Graph, proj_Association, opts : OptionsPattern[] ] :=
  Module[ { vertices, radius, fibers, allVertices, verticalEdges, horizontalEdges, tangentProj },
    vertices = VertexList[ total ];
    radius = OptionValue[ "Radius" ];
    fibers = Association @ Map[
      vertex |-> vertex -> NeighborhoodGraph[ total, vertex, radius ],
      vertices
    ];
    allVertices = Flatten[ Map[ vertex |-> { vertex, # } & /@ VertexList[ fibers[ vertex ] ], vertices ], 1 ];
    verticalEdges = Flatten[
      Map[
        vertex |->
          UndirectedEdge[ { vertex, #[[ 1 ]] }, { vertex, #[[ 2 ]] } ] & /@ EdgeList[ fibers[ vertex ] ],
        vertices
      ],
      1
    ];
    horizontalEdges = Flatten[
      Map[
        totalEdge |->
          With[ { source = totalEdge[[ 1 ]], target = totalEdge[[ 2 ]],
                  sourceVerts = VertexList[ fibers[ totalEdge[[ 1 ]] ] ],
                  targetVerts = VertexList[ fibers[ totalEdge[[ 2 ]] ] ] },
            Flatten[ {
              UndirectedEdge[ { source, # }, { target, # } ] & /@
                Intersection[ sourceVerts, targetVerts ],
              Select[
                Flatten[ Outer[ UndirectedEdge[ { source, #1 }, { target, #2 } ] &, sourceVerts, targetVerts ], 1 ],
                edge |-> EdgeQ[ total, UndirectedEdge[ edge[[ 1, 2 ]], edge[[ 2, 2 ]] ] ]
              ]
            }, 1 ]
          ],
        EdgeList[ total ]
      ],
      1
    ];
    horizontalEdges = DeleteDuplicatesBy[ horizontalEdges, Sort @* List @@ # & ];
    tangentProj = Association @ Map[ # -> proj[ #[[ 1 ]] ] &, allVertices ];
    { Graph[ allVertices, Join[ verticalEdges, horizontalEdges ] ], tangentProj }
  ]
