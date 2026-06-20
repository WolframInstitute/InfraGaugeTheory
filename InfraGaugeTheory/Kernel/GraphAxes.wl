Package["WolframInstitute`InfraGaugeTheory`"]

PackageExport["FindDiametralPaths"]
PackageExport["FindLongestGeodesicsThrough"]
PackageExport["FindGraphAxes"]
PackageExport["FindGraphAxesThrough"]

FindDiametralPaths::usage = "FindDiametralPaths[g, n, epsilon] finds paths of length close to the graph diameter.";
FindLongestGeodesicsThrough::usage = "FindLongestGeodesicsThrough[g, v, n] finds longest geodesics passing through vertex v.";
FindGraphAxes::usage = "FindGraphAxes[g, opts] finds a set of maximally separated long geodesic paths forming axes of the graph.";
FindGraphAxesThrough::usage = "FindGraphAxesThrough[g, v, opts] finds maximally separated axes passing through vertex v.";

FindDiametralPaths[ g_, n_: All, epsilon_: 0 ] :=
  Module[ { vertices, distMatrix, maxDist, pairs, numPairs, counts },
    vertices = VertexList[ g ];
    distMatrix = GraphDistanceMatrix[ g ];
    maxDist = Max[ distMatrix ];
    pairs = DeleteDuplicatesBy[ Position[ distMatrix, _?( # >= maxDist - epsilon & ) ], Sort ];
    numPairs = Length[ pairs ];
    counts = If[ n === All,
      ConstantArray[ All, numPairs ],
      RandomSample @ Table[ Quotient[ n, numPairs ] + Boole[ i <= Mod[ n, numPairs ] ], { i, numPairs } ]
    ];
    Flatten[
      Cases[
        Transpose[ { pairs, counts } ],
        { { i_, j_ }, cnt_ /; cnt =!= 0 } :> FindPath[ g, vertices[[ i ]], vertices[[ j ]], { distMatrix[[ i, j ]] }, cnt ]
      ],
      1
    ]
  ]

FindLongestGeodesicsThrough[ g_Graph, v_, n_: 1 ] :=
  Module[ { vertices, distMatrix, vertexIdx, distFromVertex, candidates, maxLen, pairs, counts },
    vertices = VertexList[ g ];
    distMatrix = GraphDistanceMatrix[ g ];
    vertexIdx = FirstPosition[ vertices, v ][[ 1 ]];
    distFromVertex = distMatrix[[ vertexIdx ]];
    candidates = Select[
      Subsets[ Range @ Length[ vertices ], { 2 } ],
      distMatrix[[ #[[1]], #[[2]] ]] == distFromVertex[[ #[[1]] ]] + distFromVertex[[ #[[2]] ]] &
    ];
    maxLen = Max[ distFromVertex[[ #[[1]] ]] + distFromVertex[[ #[[2]] ]] & /@ candidates ];
    pairs = Select[ candidates, distFromVertex[[ #[[1]] ]] + distFromVertex[[ #[[2]] ]] == maxLen & ];
    counts = If[ n === All,
      ConstantArray[ All, Length[ pairs ] ],
      With[ { l = Length[ pairs ] },
        RandomSample @ Table[ Quotient[ n, l ] + Boole[ i <= Mod[ n, l ] ], { i, l } ]
      ]
    ];
    Flatten[
      MapThread[
        { pair, cnt } |-> If[ cnt === 0, {},
          Take[
            Flatten[
              Outer[
                { p1, p2 } |-> Join[ Reverse[ p1 ], Rest[ p2 ] ],
                FindPath[ g, v, vertices[[ pair[[1]] ]], { distFromVertex[[ pair[[1]] ]] }, All ],
                FindPath[ g, v, vertices[[ pair[[2]] ]], { distFromVertex[[ pair[[2]] ]] }, All ],
                1
              ],
              1
            ],
            UpTo[ Replace[ cnt, All -> Infinity ] ]
          ]
        ],
        { pairs, counts }
      ],
      1
    ]
  ]

Options[ FindGraphAxes ] = {
  "DistanceFunction" -> "MinEndpoint",
  "MinLength" -> Automatic,
  "MinSeparation" -> Automatic,
  "AxisThickness" -> 0,
  "MaxAxes" -> Infinity,
  "RandomPick" -> False
};

FindGraphAxes[ g_Graph, OptionsPattern[] ] :=
  Module[ { paths, distMatrix, vertices, vertexIndex, minLength, minSeparation, thickness, maxAxes, distanceFunction, pick,
            axes, candidates, next, previousIndices, previousEndpoints, separation, closeAxes },
    vertices = VertexList[ g ];
    distMatrix = GraphDistanceMatrix[ g ];
    vertexIndex = AssociationThread[ vertices, Range @ Length @ vertices ];
    minLength = Replace[ OptionValue[ "MinLength" ], Automatic -> Max[ distMatrix ] ];
    paths = FindDiametralPaths[ g, All, Max[ distMatrix ] - minLength ];
    If[ paths === {}, Return[ {} ] ];
    thickness = OptionValue[ "AxisThickness" ];
    minSeparation = Replace[ OptionValue[ "MinSeparation" ], Automatic -> minLength / 2 ];
    maxAxes = OptionValue[ "MaxAxes" ];
    distanceFunction = OptionValue[ "DistanceFunction" ];
    pick = If[ OptionValue[ "RandomPick" ], RandomChoice, First ];
    axes = { pick[ paths ] };
    previousIndices = Lookup[ vertexIndex, axes[[ 1 ]] ];
    previousEndpoints = { vertexIndex[ axes[[1, 1]] ], vertexIndex[ axes[[1, -1]] ] };
    candidates = paths;
    While[ candidates =!= {} && Length[ axes ] < maxAxes,
      { next, separation } = Switch[ distanceFunction,
        "MinEndpoint",
        With[ { scores = ( Min[ distMatrix[[ vertexIndex[ #[[1]] ], previousEndpoints ]], distMatrix[[ vertexIndex[ #[[-1]] ], previousEndpoints ]] ] & ) /@ candidates },
          { pick[ candidates[[ Flatten @ Position[ scores, Max[ scores ] ] ]] ], Max[ scores ] }
        ],
        "Hausdorff",
        With[ { scores = ( p |-> HausdorffDistance[ distMatrix, Lookup[ vertexIndex, p ], previousIndices ] ) /@ candidates },
          { pick[ candidates[[ Flatten @ Position[ scores, Max[ scores ] ] ]] ], Max[ scores ] }
        ],
        "Separation",
        With[ { scores = ( p |-> Separation[ distMatrix, Lookup[ vertexIndex, p ], previousIndices ] ) /@ candidates },
          { pick[ candidates[[ Flatten @ Position[ scores, Max[ scores ] ] ]] ], Max[ scores ] }
        ]
      ];
      If[ separation < minSeparation, Break[] ];
      closeAxes = If[ thickness == 0, { next },
        Select[ candidates, HausdorffDistance[ distMatrix, Lookup[ vertexIndex, # ], Lookup[ vertexIndex, next ] ] <= thickness & ]
      ];
      axes = Join[ axes, closeAxes ];
      previousIndices = Union[ previousIndices, Flatten[ Lookup[ vertexIndex, # ] & /@ closeAxes ] ];
      previousEndpoints = Union[ previousEndpoints, Flatten[ { vertexIndex[ #[[1]] ], vertexIndex[ #[[-1]] ] } & /@ closeAxes ] ];
      candidates = Complement[ candidates, closeAxes ];
    ];
    axes
  ]

Options[ FindGraphAxesThrough ] = {
  "DistanceFunction" -> "MinEndpoint",
  "MinSeparation" -> Automatic,
  "AxisThickness" -> 0,
  "MaxAxes" -> Infinity,
  "RandomPick" -> False
};

FindGraphAxesThrough[ g_Graph, v_, OptionsPattern[] ] :=
  Module[ { geodesics, distMatrix, vertices, vertexIndex, minSeparation, thickness, maxAxes, distanceFunction, pick, pathLength,
            axes, candidates, next, previousIndices, previousEndpoints, separation, closeAxes },
    geodesics = FindLongestGeodesicsThrough[ g, v, All ];
    If[ geodesics === {}, Return[ {} ] ];
    vertices = VertexList[ g ];
    distMatrix = GraphDistanceMatrix[ g ];
    vertexIndex = AssociationThread[ vertices, Range @ Length @ vertices ];
    pathLength = Length[ First[ geodesics ] ] - 1;
    minSeparation = Replace[ OptionValue[ "MinSeparation" ], Automatic -> pathLength / 2 ];
    thickness = OptionValue[ "AxisThickness" ];
    maxAxes = OptionValue[ "MaxAxes" ];
    distanceFunction = OptionValue[ "DistanceFunction" ];
    pick = If[ OptionValue[ "RandomPick" ], RandomChoice, First ];
    axes = { pick[ geodesics ] };
    previousIndices = Lookup[ vertexIndex, axes[[ 1 ]] ];
    previousEndpoints = { vertexIndex[ axes[[1, 1]] ], vertexIndex[ axes[[1, -1]] ] };
    candidates = geodesics;
    While[ candidates =!= {} && Length[ axes ] < maxAxes,
      { next, separation } = Switch[ distanceFunction,
        "MinEndpoint",
        With[ { scores = ( Min[ distMatrix[[ vertexIndex[ #[[1]] ], previousEndpoints ]], distMatrix[[ vertexIndex[ #[[-1]] ], previousEndpoints ]] ] & ) /@ candidates },
          { pick[ candidates[[ Flatten @ Position[ scores, Max[ scores ] ] ]] ], Max[ scores ] }
        ],
        "Hausdorff",
        With[ { scores = ( p |-> HausdorffDistance[ distMatrix, Lookup[ vertexIndex, p ], previousIndices ] ) /@ candidates },
          { pick[ candidates[[ Flatten @ Position[ scores, Max[ scores ] ] ]] ], Max[ scores ] }
        ],
        "Separation",
        With[ { scores = ( p |-> Separation[ distMatrix, Lookup[ vertexIndex, p ], previousIndices ] ) /@ candidates },
          { pick[ candidates[[ Flatten @ Position[ scores, Max[ scores ] ] ]] ], Max[ scores ] }
        ]
      ];
      If[ separation < minSeparation, Break[] ];
      closeAxes = If[ thickness == 0, { next },
        Select[ candidates, HausdorffDistance[ distMatrix, Lookup[ vertexIndex, # ], Lookup[ vertexIndex, next ] ] <= thickness & ]
      ];
      axes = Join[ axes, closeAxes ];
      previousIndices = Union[ previousIndices, Flatten[ Lookup[ vertexIndex, # ] & /@ closeAxes ] ];
      previousEndpoints = Union[ previousEndpoints, Flatten[ { vertexIndex[ #[[1]] ], vertexIndex[ #[[-1]] ] } & /@ closeAxes ] ];
      candidates = Complement[ candidates, closeAxes ];
    ];
    axes
  ]
