Package["WolframInstitute`InfraGaugeTheory`"]

PackageExport["GraphTangentBundle"]
PackageExport["TangentFiberedGraph"]

PackageScope["tangentFiberVertices"]
PackageScope["tangentHorizontalEdges"]
PackageScope["tangentVerticalEdges"]

GraphTangentBundle::usage =
  "GraphTangentBundle[g] is the radius-r tangent graph of g as a fibered graph {total, projection}: the fiber over p is the geodesic graph at p (the subgraph induced on the radius-r ball, or on the radius-r sphere under \"Fiber\" -> \"Sphere\"), vertical edges are the fiber's own edges, and horizontal edges join the fibers over adjacent base vertices. Options \"Radius\" (default 1), Method (\"Metric\" (default) | \"Complete\" | \"Diagonal\"), \"Fiber\" (\"Ball\" (default) | \"Sphere\"). The three Methods nest: Diagonal is a subgraph of Metric is a subgraph of Complete, on one vertex set.";

TangentFiberedGraph::usage =
  "TangentFiberedGraph[total, proj] is the tangent graph of the total space of a fibered graph, projected all the way down to the original base. Same options as GraphTangentBundle.";

GraphTangentBundle::badmethod =
  "Unknown Method `1`; use \"Metric\", \"Complete\" or \"Diagonal\".";
GraphTangentBundle::badfiber =
  "Unknown \"Fiber\" `1`; use \"Ball\" or \"Sphere\".";

(* ========================= Fibers ========================= *)

(* The fiber over a vertex is the geodesic graph at that vertex: the induced
   subgraph on the radius-r ball, which carries the zero vector p itself, or, in
   the sphere model, on the vertices at distance exactly r. At r = 1 the sphere
   model makes a fiber vertex {p, v} the same datum as a directed edge p -> v. *)

tangentFiber[ g_Graph, p_, r_Integer, fiber_String ] :=
  Switch[ fiber,
    "Ball", NeighborhoodGraph[ g, p, r ],
    "Sphere", Subgraph[ g, Select[ VertexList[ g ], GraphDistance[ g, p, # ] == r & ] ],
    _, Message[ GraphTangentBundle::badfiber, fiber ]; $Failed ];

tangentFiberVertices[ g_Graph, r_Integer, fiber_String ] :=
  With[ { fibers = AssociationMap[ tangentFiber[ g, #, r, fiber ] &, VertexList[ g ] ] },
    If[ AnyTrue[ fibers, FailureQ ], $Failed, VertexList /@ fibers ] ];

tangentVerticalEdges[ g_Graph, r_Integer, fiber_String ] :=
  Flatten[
    Map[
      p |-> UndirectedEdge[ { p, #[[ 1 ]] }, { p, #[[ 2 ]] } ] & /@ EdgeList @ tangentFiber[ g, p, r, fiber ],
      VertexList[ g ] ],
    1 ];

(* ========================= Horizontal edges ========================= *)

(* A horizontal edge lies over a base edge p ~ q and joins {p, v} to {q, w}. The
   Method fixes which pairs (v, w) are admitted, and so fixes which connections
   the total graph can carry: a connection is a choice of one horizontal lift per
   fiber vertex per base edge, hence a subgraph of this scaffold.

     "Complete"  every pair. The maximal scaffold: every fiber matching is a
                 connection, and no metric input enters the construction.
     "Metric"    v == w, or v ~ w in g. The metric-compatible sub-scaffold; the
                 flip {p, v} -> {v, p} is a graph automorphism of it.
     "Diagonal"  v == w only. The flat scaffold: the identity matching alone.

   Diagonal is a subgraph of Metric is a subgraph of Complete, on one vertex set. *)

tangentHorizontalEdges[ g_Graph, baseEdges_List, fibers_Association, method_String ] :=
  Module[ { admits },
    admits = Switch[ method,
      "Complete", True &,
      "Metric", ( #1 === #2 || EdgeQ[ g, UndirectedEdge[ #1, #2 ] ] ) &,
      "Diagonal", ( #1 === #2 ) &,
      _, Message[ GraphTangentBundle::badmethod, method ]; Return[ $Failed, Module ] ];
    Flatten[
      Map[
        baseEdge |->
          With[ { source = baseEdge[[ 1 ]], target = baseEdge[[ 2 ]] },
            With[ { sourceVerts = Lookup[ fibers, Key @ source, { } ], targetVerts = Lookup[ fibers, Key @ target, { } ] },
              Select[
                Flatten[ Outer[ UndirectedEdge[ { source, #1 }, { target, #2 } ] &, sourceVerts, targetVerts, 1 ], 1 ],
                edge |-> TrueQ @ admits[ edge[[ 1, 2 ]], edge[[ 2, 2 ]] ] ] ] ],
        baseEdges ],
      1 ]
  ];

(* ========================= GraphTangentBundle ========================= *)

Options[ GraphTangentBundle ] = { "Radius" -> 1, Method -> "Metric", "Fiber" -> "Ball" };

GraphTangentBundle[ g_Graph, opts : OptionsPattern[] ] :=
  Module[ { radius, method, fiberModel, fibers, allVertices, verticalEdges, horizontalEdges, projection },
    radius = OptionValue[ "Radius" ];
    method = OptionValue[ Method ];
    fiberModel = OptionValue[ "Fiber" ];
    fibers = tangentFiberVertices[ g, radius, fiberModel ];
    If[ FailureQ @ fibers, Return[ $Failed, Module ] ];
    allVertices = Flatten[ Map[ p |-> { p, # } & /@ Lookup[ fibers, Key @ p, { } ], VertexList[ g ] ], 1 ];
    verticalEdges = tangentVerticalEdges[ g, radius, fiberModel ];
    horizontalEdges = tangentHorizontalEdges[ g, EdgeList[ g ], fibers, method ];
    If[ FailureQ @ horizontalEdges, Return[ $Failed, Module ] ];
    horizontalEdges = DeleteDuplicatesBy[ horizontalEdges, Sort @* List @@ # & ];
    projection = Association @ Map[ # -> #[[ 1 ]] &, allVertices ];
    { Graph[ allVertices, Join[ verticalEdges, horizontalEdges ] ], projection }
  ]

(* ========================= TangentFiberedGraph ========================= *)

Options[ TangentFiberedGraph ] = { "Radius" -> 1, Method -> "Metric", "Fiber" -> "Ball" };

TangentFiberedGraph[ total_Graph, proj_Association, opts : OptionsPattern[] ] :=
  Module[ { radius, method, fiberModel, fibers, allVertices, verticalEdges, horizontalEdges, tangentProj },
    radius = OptionValue[ "Radius" ];
    method = OptionValue[ Method ];
    fiberModel = OptionValue[ "Fiber" ];
    fibers = tangentFiberVertices[ total, radius, fiberModel ];
    If[ FailureQ @ fibers, Return[ $Failed, Module ] ];
    allVertices = Flatten[ Map[ p |-> { p, # } & /@ Lookup[ fibers, Key @ p, { } ], VertexList[ total ] ], 1 ];
    verticalEdges = tangentVerticalEdges[ total, radius, fiberModel ];
    horizontalEdges = tangentHorizontalEdges[ total, EdgeList[ total ], fibers, method ];
    If[ FailureQ @ horizontalEdges, Return[ $Failed, Module ] ];
    horizontalEdges = DeleteDuplicatesBy[ horizontalEdges, Sort @* List @@ # & ];
    tangentProj = Association @ Map[ # -> proj[ #[[ 1 ]] ] &, allVertices ];
    { Graph[ allVertices, Join[ verticalEdges, horizontalEdges ] ], tangentProj }
  ]
