Package["WolframInstitute`InfraGaugeTheory`"]

PackageExport["RandomFiberedGraphViewer"]
PackageExport["FiberedGraphViewer"]

RandomFiberedGraphViewer::usage = "RandomFiberedGraphViewer[g, opts] interactive viewer for randomly generated fibered graphs.";
FiberedGraphViewer::usage = "FiberedGraphViewer[total, proj] interactive viewer. FiberedGraphViewer[total, proj, section] includes section. Hover vertices to highlight fibers (red) and section values (dark blue). Hover edges to highlight connection edges (purple).";

Options[RandomFiberedGraphViewer] = {
  "TotalImageSize" -> Medium,
  "BaseImageSize" -> Medium,
  "HighlightColor" -> Red
};

RandomFiberedGraphViewer[g_Graph, opts : OptionsPattern[]] :=
  With[{
    baseCoords = AssociationThread[
      VertexList @ g, GraphEmbedding @ g],
    layouts = {
      Automatic, "CircularEmbedding", "SpringElectricalEmbedding",
      "SpringEmbedding", "SpectralEmbedding", "LayeredEmbedding",
      "RadialEmbedding", "BipartiteEmbedding", "PlanarEmbedding",
      "TutteEmbedding", "GravityEmbedding", "HighDimensionalEmbedding",
      "LinearEmbedding", "SpiralEmbedding", "StarEmbedding",
      "RandomEmbedding"}},
    Manipulate[
      DynamicModule[
        {total, proj, base, totalCoords, baseVerts, totalVerts,
          fiberOf, hoverTotal, hoverBase, highlightColor},
        hoverTotal = None;
        hoverBase = None;
        highlightColor = OptionValue["HighlightColor"];
        {total, proj} =
          RandomFiberedGraph[g, "VerticalVertices" -> verticalVertices,
            "VerticalEdges" -> verticalEdges,
            "HorizontalEdgesRadius" -> horizontalEdgesRadius,
            "HorizontalEdgesDensity" -> horizontalEdgesDensity];
        base = ReconstructBaseGraph[total, proj];
        totalCoords =
          If[totalLayout === Automatic,
            AssociationThread[VertexList @ total, GraphEmbedding @ total],
            AssociationThread[VertexList @ total,
              GraphEmbedding[total, totalLayout]]];
        baseVerts = Thread[Values[baseCoords] -> Keys[baseCoords]];
        totalVerts = Thread[Values[totalCoords] -> Keys[totalCoords]];
        fiberOf = GroupBy[Normal @ proj, Last -> First];
        Row[
          {EventHandler[
            Dynamic @ Graph[
              total,
              VertexCoordinates -> totalCoords,
              VertexStyle -> {
                If[hoverBase =!= None,
                  Sequence @@ ((# -> highlightColor) & /@ fiberOf[hoverBase]), Nothing],
                If[hoverTotal =!= None, hoverTotal -> highlightColor, Nothing]},
              ImageSize -> OptionValue["TotalImageSize"]
            ],
            {
              "MouseMoved" :>
                Module[{mp = MousePosition["Graphics"]}, If[mp =!= None,
                  hoverTotal = First @ Nearest[totalVerts, mp, 1];
                  hoverBase = None;
                ]]
            }
          ],
          Spacer[20],
          Graphics[{Arrowheads[.5], Arrow[{{0, 0}, {1, 0}}]},
            ImageSize -> 50],
          Spacer[20],
          EventHandler[
            Dynamic @ Graph[
              base,
              VertexStyle -> {
                If[hoverTotal =!= None, proj[hoverTotal] -> highlightColor,
                  Nothing],
                If[hoverBase =!= None, hoverBase -> highlightColor, Nothing]},
              VertexSize -> Large,
              VertexCoordinates -> baseCoords,
              ImageSize -> OptionValue["BaseImageSize"]
            ],
            {
              "MouseMoved" :>
                Module[{mp = MousePosition["Graphics"]}, If[mp =!= None,
                  hoverBase = First @ Nearest[baseVerts, mp, 1];
                  hoverTotal = None;
                ]]
            }
          ]
          }
        ]
      ],
      Grid[{
        {Control[{{verticalVertices, 1, "Vertical vertices"}, 1, 20, 1}],
          Control[{{verticalEdges, 0, "Vertical edges"}, 0, 20, 1}]},
        {Control[{{horizontalEdgesRadius, 1, "Horizontal edges radius"}, 1, 10, 1}],
          Control[{{horizontalEdgesDensity, 1, "Horizontal edges density"}, 1, 20, 1}]},
        {Control[{{totalLayout, Automatic, "Total space layout"}, layouts}],
          SpanFromLeft}
      }, Spacings -> {15, 1}]
    ]
  ];

(* ========================= FiberedGraphViewer ========================= *)

Options[FiberedGraphViewer] = Join[{
  "TotalImageSize" -> Medium,
  "BaseImageSize" -> Medium,
  "FiberColor" -> Red,
  "SectionColor" -> Darker[Blue]
}, Options[Graph]];

FiberedGraphViewer[total_Graph, proj_Association, opts : OptionsPattern[]] :=
  FiberedGraphViewer[total, proj, <||>, opts];

FiberedGraphViewer[total_Graph, proj_Association, section_Association, opts : OptionsPattern[]] :=
  DynamicModule[{base, baseCoords, totalCoords, baseVerts, totalVerts,
    fiberOf, sectionVerts,
    fiberColor, sectionColor, baseGraphOpts,
    hoverTotal, hoverBase,
    highlightedFiber, highlightedSection},

    fiberColor = OptionValue["FiberColor"];
    sectionColor = OptionValue["SectionColor"];
    sectionVerts = Values[section];

    baseGraphOpts = FilterRules[{opts}, Options[Graph]];
    base = ReconstructBaseGraph[total, proj, Sequence @@ baseGraphOpts];
    fiberOf = GroupBy[Normal @ proj, Last -> First];

    baseCoords = AssociationThread[
      VertexList @ base, VertexCoordinates /. AbsoluteOptions[base]];
    totalCoords = AssociationThread[
      VertexList @ total, VertexCoordinates /. AbsoluteOptions[total]];
    baseVerts = Thread[Values[baseCoords] -> Keys[baseCoords]];
    totalVerts = Thread[Values[totalCoords] -> Keys[totalCoords]];

    hoverTotal = None;
    hoverBase = None;
    highlightedFiber = {};
    highlightedSection = {};

    Row[{
      EventHandler[
        Dynamic @ Graph[total,
          VertexCoordinates -> totalCoords,
          VertexStyle -> Join[
            Map[# -> Directive[fiberColor, PointSize[0.02]] &, highlightedFiber],
            Map[# -> Directive[sectionColor, PointSize[0.025]] &, highlightedSection],
            If[hoverTotal =!= None && Not[MemberQ[highlightedFiber, hoverTotal]] &&
                Not[MemberQ[highlightedSection, hoverTotal]],
              {hoverTotal -> fiberColor}, {}]
          ],
          ImageSize -> OptionValue["TotalImageSize"]
        ],
        {"MouseMoved" :>
          Module[{mp = MousePosition["Graphics"]},
            If[mp =!= None,
              hoverTotal = First @ Nearest[totalVerts, mp, 1];
              hoverBase = None;
              With[{baseV = proj[hoverTotal]},
                highlightedFiber = fiberOf[baseV];
                highlightedSection = If[Length[section] > 0,
                  Select[sectionVerts, MemberQ[fiberOf[baseV], #] &], {}]
              ]
            ]
          ]
        }
      ],
      Spacer[20],
      Graphics[{Arrowheads[.5], Arrow[{{0, 0}, {1, 0}}]}, ImageSize -> 50],
      Spacer[20],
      EventHandler[
        Dynamic @ Graph[base,
          VertexCoordinates -> baseCoords,
          VertexSize -> Large,
          VertexStyle -> Join[
            If[hoverTotal =!= None, {proj[hoverTotal] -> fiberColor}, {}],
            If[hoverBase =!= None, {hoverBase -> fiberColor}, {}]
          ],
          ImageSize -> OptionValue["BaseImageSize"]
        ],
        {"MouseMoved" :>
          Module[{mp = MousePosition["Graphics"]},
            If[mp =!= None,
              hoverBase = First @ Nearest[baseVerts, mp, 1];
              hoverTotal = None;
              highlightedFiber = fiberOf[hoverBase];
              highlightedSection = If[Length[section] > 0,
                Select[sectionVerts, MemberQ[fiberOf[hoverBase], #] &], {}]
            ]
          ]
        }
      ]
    }]
  ];
