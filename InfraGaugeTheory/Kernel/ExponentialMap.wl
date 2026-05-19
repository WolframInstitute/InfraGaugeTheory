Package["WolframInstitute`InfraGaugeTheory`"]

PackageExport["ExponentialCoordinates"]
PackageExport["InjectivityRadius"]
PackageExport["ExponentialMapViewer"]

ExponentialCoordinates::usage = "ExponentialCoordinates[g, v] returns exponential coordinates for all vertices as an Association. ExponentialCoordinates[g, v, w] returns the coordinate for a single target vertex.";
InjectivityRadius::usage = "InjectivityRadius[g, v] returns the largest radius within which the exponential map at v is injective.";
ExponentialMapViewer::usage = "ExponentialMapViewer[g, v] interactive viewer for the exponential map. Click to change basepoint, hover to see coordinate arrows. Green = injectivity ball.";

(* ========================= ExponentialCoordinates ========================= *)

Options[ExponentialCoordinates] = {
  "FormatCoordinate" -> ({dist, list} |->
    If[list === {} || Values[list] === {},
      0,
      Expand[dist / Norm[Values[list]] Plus @@ (Times @@@ list)]
    ]
  ),
  "Radius" -> Infinity
};

ExponentialCoordinates[g_, v_, opts : OptionsPattern[]] :=
  Module[{neighborhood, format, vertices, radius},
    radius = OptionValue["Radius"];
    neighborhood = NeighborhoodGraph[g, v, radius];
    format = OptionValue["FormatCoordinate"];
    vertices = VertexList[neighborhood];
    Association @ Map[
      w |-> With[{dist = GraphDistance[g, v, w]},
        w -> format[
          dist,
          KeyValueMap[
            DirectedEdge @@ #1 -> #2 &,
            CountsBy[FindPath[neighborhood, v, w, {dist}, All], Take[#, 2] &]
          ]
        ]
      ],
      vertices
    ]
  ];

ExponentialCoordinates[g_, v_, w_, opts : OptionsPattern[]] :=
  Module[{distance, edgeCounts, format},
    format = OptionValue["FormatCoordinate"];
    distance = GraphDistance[g, v, w];
    edgeCounts = KeyValueMap[
      DirectedEdge @@ #1 -> #2 &,
      CountsBy[FindPath[g, v, w, {distance}, All], Take[#, 2] &]
    ];
    format[distance, edgeCounts]
  ];

(* ========================= InjectivityRadius ========================= *)

Options[InjectivityRadius] = Options[ExponentialCoordinates];

InjectivityRadius[g_, v_, opts : OptionsPattern[]] :=
  Module[{verticesByDist, result},
    verticesByDist = Rest @ SortBy[
      Thread[VertexList[g] -> GraphDistance[g, v]],
      Last
    ];
    result = Fold[
      Function[{state, vertexDist},
        Module[{seenCoords, lastDist, collisionFlag, target, distance, coord},
          {seenCoords, lastDist, collisionFlag} = state;
          If[collisionFlag, Return[state, Module]];
          {target, distance} = Through[{First, Last}[vertexDist]];
          coord = ExponentialCoordinates[g, v, target, opts];
          If[KeyExistsQ[seenCoords, coord],
            {seenCoords, distance - 1, True},
            {Append[seenCoords, coord -> target], distance, False}
          ]
        ]
      ],
      {<||>, 0, False},
      verticesByDist
    ];
    result[[2]]
  ];

(* ========================= ExponentialMapViewer ========================= *)

Options[ExponentialMapViewer] = {ImageSize -> 500};

ExponentialMapViewer[g_Graph, v0_, opts : OptionsPattern[]] :=
  DynamicModule[{hoveredVertex = None, vertexCoords, nearestFunc, v = v0,
    graphBounds, injectivityRadiusVertices, vertexStyle, vertices, coords},

    vertices = VertexList[g];
    coords = VertexCoordinates /. AbsoluteOptions[g];
    vertexCoords = AssociationThread[vertices -> coords];
    graphBounds = CoordinateBounds[coords];
    nearestFunc = Nearest[coords -> vertices];

    Dynamic[
      injectivityRadiusVertices =
        VertexList @ NeighborhoodGraph[g, v, InjectivityRadius[g, v]];
      vertexStyle = Join[
        (# -> Green) & /@ injectivityRadiusVertices,
        {v -> Red, hoveredVertex -> Orange}
      ];

      EventHandler[
        Show[
          Graph[g,
            VertexStyle -> vertexStyle,
            ImageSize -> OptionValue[ImageSize]
          ],
          Graphics[
            If[hoveredVertex === None || hoveredVertex === v,
              {},
              Module[{dist, paths, edgeCounts, arrows},
                dist = GraphDistance[g, v, hoveredVertex];
                paths = FindPath[g, v, hoveredVertex, {dist}, All];
                edgeCounts = CountsBy[paths, Take[#, 2] &];
                arrows = KeyValueMap[
                  Function[{edge, count},
                    Module[{start, dir, weight, endPt, clippedEnd},
                      start = vertexCoords[edge[[1]]];
                      dir = vertexCoords[edge[[2]]] - start;
                      weight = dist / Norm[Values[edgeCounts]] * count;
                      endPt = start + weight * dir;
                      clippedEnd = MapThread[Clip[#1, #2] &, {endPt, graphBounds}];
                      {If[clippedEnd =!= endPt, Gray, Red], Thick,
                       Arrow[{start, clippedEnd}]}
                    ]
                  ],
                  edgeCounts
                ];
                arrows
              ]
            ]
          ]
        ],
        {
          "MouseMoved" :> Module[{mp = MousePosition["Graphics"]},
            If[mp =!= None,
              hoveredVertex = First[nearestFunc[mp], None]
            ]
          ],
          "MouseClicked" :> If[hoveredVertex =!= None, v = hoveredVertex]
        },
        PassEventsDown -> True
      ],
      TrackedSymbols :> {v, hoveredVertex}
    ]
  ];
