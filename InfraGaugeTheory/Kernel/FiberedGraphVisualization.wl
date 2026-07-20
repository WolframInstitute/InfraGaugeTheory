Package["WolframInstitute`InfraGaugeTheory`"]

PackageExport["VisualizeFiberedGraph"]

VisualizeFiberedGraph::usage = "VisualizeFiberedGraph[total, proj] visualizes a fibered graph. Optional positional arguments: a section (Association) and/or a connection (Graph), in any order, auto-detected by type. Method -> \"CircularFiber\" | \"3DFiber\" | \"HighlightFiber\". Option \"BaseVertexCoordinates\" -> Association overrides the default base embedding with explicit coordinates keyed by base vertex.";

(* ========================= VisualizeFiberedGraph ========================= *)

Options[VisualizeFiberedGraph] = {
  Method -> "CircularFiber",
  ImageSize -> Automatic,
  "FiberRadius" -> Automatic,
  "Dimension" -> 2,
  "ColorScheme" -> "Rainbow",
  "SectionStyle" -> Directive[Thick, Darker[Blue]],
  "ConnectionStyle" -> Directive[Thick, Lighter[Purple]],
  "SectionVertexStyle" -> Directive[Darker[Blue], PointSize[0.02]],
  "VerticalEdgeStyle" -> Directive[Thick, GrayLevel[0.7]],
  "HorizontalEdgeStyle" -> Automatic,
  "FiberThickness" -> 0.03,
  "FiberOpacity" -> 0.3,
  "FiberColorFunction" -> (Blend[{Pink, Red, Darker[Red]}, #] &),
  "BaseGraphLayout" -> "SpringElectricalEmbedding",
  "BaseVertexCoordinates" -> Automatic,
  "FiberWidth" -> 0.2,
  "FiberHeight" -> 1,
  "FiberVertexStyle" -> Automatic
};

VisualizeFiberedGraph[total_Graph, proj_Association,
    opts : OptionsPattern[{VisualizeFiberedGraph, Graph, Graph3D, Graphics}]] :=
  vizDispatch[total, proj, <||>, None, "Section", opts];

VisualizeFiberedGraph[total_Graph, proj_Association, section_Association,
    opts : OptionsPattern[{VisualizeFiberedGraph, Graph, Graph3D, Graphics}]] :=
  vizDispatch[total, proj, section, None, "Section", opts];

VisualizeFiberedGraph[total_Graph, proj_Association, connection_Graph,
    opts : OptionsPattern[{VisualizeFiberedGraph, Graph, Graph3D, Graphics}]] :=
  vizDispatch[total, proj, <||>, connection, "Connection", opts];

VisualizeFiberedGraph[total_Graph, proj_Association, section_Association, connection_Graph,
    opts : OptionsPattern[{VisualizeFiberedGraph, Graph, Graph3D, Graphics}]] :=
  vizDispatch[total, proj, section, connection, "Section", opts];

VisualizeFiberedGraph[total_Graph, proj_Association, connection_Graph, section_Association,
    opts : OptionsPattern[{VisualizeFiberedGraph, Graph, Graph3D, Graphics}]] :=
  vizDispatch[total, proj, section, connection, "Connection", opts];

vizDispatch[total_Graph, proj_Association, section_Association, connection_, priority_String, opts___] :=
  Switch[OptionValue[VisualizeFiberedGraph, {opts}, Method],
    "CircularFiber", circularMethod[total, proj, section, connection, priority, opts],
    "3DFiber", threeDMethod[total, proj, section, connection, priority, opts],
    "HighlightFiber", highlightMethod[total, proj, section, connection, priority, opts],
    _, circularMethod[total, proj, section, connection, priority, opts]
  ];

(* ---------- Circular method ---------- *)

circularMethod[total_Graph, proj_Association, section_Association, connection_, priority_String,
    opts : OptionsPattern[VisualizeFiberedGraph]] :=
  Module[{base, baseCoords, fibers, fiberCoords, allCoords, dim, radius,
    verticalEdges, horizontalEdges,
    sectionVerts, sectionEdges, connEdges,
    vertexStyles, edgeStyles, colorScheme,
    baseVertices, fiberColors},
    base = ReconstructBaseGraph[total, proj];
    dim = OptionValue["Dimension"];
    colorScheme = OptionValue["ColorScheme"];

    baseCoords = Replace[OptionValue["BaseVertexCoordinates"], {
      Automatic :> If[dim == 2,
        AssociationThread[VertexList[base], GraphEmbedding[base]],
        AssociationThread[VertexList[base], GraphEmbedding[base, Dimension -> 3]]
      ],
      assoc_Association :> assoc
    }];

    radius = Replace[OptionValue["FiberRadius"],
      Automatic :>
        With[{pts = Values[baseCoords]},
          If[Length[pts] >= 2,
            Min[EuclideanDistance @@@ Subsets[pts, {2}]] / 3,
            0.3]]];

    fibers = GroupBy[Normal[proj], Last -> First];
    baseVertices = Keys[fibers];

    fiberColors = AssociationThread[
      baseVertices,
      Table[
        ColorData[colorScheme][i / Length[baseVertices]],
        {i, Length[baseVertices]}
      ]
    ];

    fiberCoords = Catenate @ KeyValueMap[
      {baseVertex, fiberVertices} |->
        With[{basePos = baseCoords[baseVertex], n = Length[fiberVertices]},
          If[n == 1,
            {fiberVertices[[ 1 ]] -> basePos},
            With[{circCoords = GraphEmbedding[ Subgraph[ total, fiberVertices ], "CircularEmbedding" ]},
              MapThread[Rule, {fiberVertices,
                Map[basePos + radius # &,
                  If[dim == 2, circCoords, PadRight[#, 3] & /@ circCoords]]}]]]],
      fibers
    ];

    allCoords = Association[fiberCoords];

    verticalEdges = Select[EdgeList[total], proj[#[[1]]] === proj[#[[2]]] &];
    horizontalEdges = Select[EdgeList[total], proj[#[[1]]] =!= proj[#[[2]]] &];

    sectionVerts = Values[section];
    sectionEdges = Select[horizontalEdges,
      MemberQ[sectionVerts, #[[1]]] && MemberQ[sectionVerts, #[[2]]] &];
    connEdges = If[connection === None, {}, EdgeList[connection]];

    vertexStyles = If[Length[sectionVerts] > 0,
      Map[# -> OptionValue["SectionVertexStyle"] &, sectionVerts],
      {}
    ];

    edgeStyles = With[{
      secStyles = Map[# -> OptionValue["SectionStyle"] &, sectionEdges],
      connStyles = Map[# -> OptionValue["ConnectionStyle"] &, connEdges]},
      Join[
        If[priority === "Section", Join[secStyles, connStyles], Join[connStyles, secStyles]],
        Map[# -> OptionValue["VerticalEdgeStyle"] &, verticalEdges],
        If[OptionValue["HorizontalEdgeStyle"] =!= Automatic,
          Map[# -> OptionValue["HorizontalEdgeStyle"] &,
            Complement[horizontalEdges, Join[sectionEdges, connEdges]]],
          {}
        ]
      ]
    ];

    If[dim == 2,
      Graph[total,
        VertexCoordinates -> allCoords,
        VertexSize -> Small,
        VertexStyle -> vertexStyles,
        EdgeStyle -> edgeStyles,
        Sequence @@ FilterRules[{opts}, Options[Graph]]
      ],
      Graph3D[total,
        VertexCoordinates -> allCoords,
        VertexSize -> Small,
        VertexStyle -> vertexStyles,
        EdgeStyle -> edgeStyles,
        Sequence @@ FilterRules[{opts}, Options[Graph]]
      ]
    ]
  ];

(* ---------- 3D method ---------- *)

threeDMethod[total_Graph, proj_Association, section_Association, connection_, priority_String,
    opts : OptionsPattern[VisualizeFiberedGraph]] :=
  Module[{base, fiberWidth, fiberHeight, fibers, baseCoords, meanEdgeLen,
    vertexCoords, verticalEdges, horizontalEdges,
    sectionVerts, sectionEdges, connEdges,
    sortedSectionEdges, sortedConnEdges, otherTransverseEdges},
    base = ReconstructBaseGraph[total, proj];
    fiberWidth = OptionValue["FiberWidth"];
    fiberHeight = OptionValue["FiberHeight"];
    fibers = GroupBy[Normal@proj, Last -> First];
    sectionVerts = Values[section];
    connEdges = If[connection === None, {}, EdgeList[connection]];
    baseCoords = Replace[OptionValue["BaseVertexCoordinates"], {
      Automatic :> AssociationThread[VertexList@base,
        GraphEmbedding[base, OptionValue["BaseGraphLayout"]]],
      assoc_Association :> assoc
    }];
    meanEdgeLen = If[EdgeCount@base > 0,
      Mean @ Map[EuclideanDistance @@ (baseCoords /@ (List @@ #)) &, EdgeList@base], 1];
    vertexCoords =
      Catenate @
        KeyValueMap[{baseVert, fiberVerts} |->
          With[{fiberGraph = Subgraph[total, fiberVerts],
            baseXY = baseCoords[baseVert],
            sectionVert = Lookup[section, baseVert, None]},
            With[{rawEmb =
              If[VertexCount@fiberGraph > 1,
                GraphEmbedding[fiberGraph, "SpringElectricalEmbedding", 3],
                {{0, 0, 0}}]},
              With[{origin = If[sectionVert =!= None,
                    rawEmb[[FirstPosition[fiberVerts, sectionVert, {1}][[1]]]],
                    Mean @ rawEmb],
                  embMax = Max[1, Max@Abs@Flatten@rawEmb]},
                With[{centered = Map[# - origin &, rawEmb]},
                  MapThread[
                    Rule, {fiberVerts,
                      Map[pt |-> {baseXY[[1]] + fiberWidth * meanEdgeLen * pt[[1]] / embMax,
                        baseXY[[2]] + fiberWidth * meanEdgeLen * pt[[2]] / embMax,
                        fiberHeight * meanEdgeLen * pt[[3]] / embMax}, centered]}]]]]], fibers];
    verticalEdges =
      Select[EdgeList@total, e |-> proj[e[[1]]] === proj[e[[2]]]];
    horizontalEdges =
      Select[EdgeList@total, e |-> proj[e[[1]]] =!= proj[e[[2]]]];
    sectionEdges =
      Select[horizontalEdges,
        e |-> MemberQ[sectionVerts, e[[1]]] && MemberQ[sectionVerts, e[[2]]]];
    sortedSectionEdges = Map[Sort@*List @@ # &, sectionEdges];
    sortedConnEdges = Map[Sort@*List @@ # &, connEdges];
    otherTransverseEdges =
      Select[horizontalEdges,
        e |-> Not[MemberQ[sortedSectionEdges, Sort@{e[[1]], e[[2]]}]] &&
          Not[MemberQ[sortedConnEdges, Sort@{e[[1]], e[[2]]}]]];
    Graph3D[VertexList@total, EdgeList@total,
      VertexCoordinates -> vertexCoords,
      VertexStyle ->
        Join[
          Map[v |-> v -> OptionValue["SectionVertexStyle"], sectionVerts],
          Map[v |-> v -> OptionValue["FiberVertexStyle"],
            Complement[VertexList@total, sectionVerts]]],
      EdgeStyle ->
        With[{
          secStyles = Map[e |-> e -> OptionValue["SectionStyle"], sectionEdges],
          connStyles = Map[e |-> e -> OptionValue["ConnectionStyle"], connEdges]},
          Join[
            If[priority === "Section", Join[secStyles, connStyles], Join[connStyles, secStyles]],
            Map[e |-> e -> OptionValue["VerticalEdgeStyle"], verticalEdges],
            Map[e |-> e -> OptionValue["HorizontalEdgeStyle"], otherTransverseEdges]]],
      Sequence @@ FilterRules[{opts}, Options[Graph3D]]]
  ];

(* ---------- Highlight method ---------- *)

highlightMethod[total_Graph, proj_Association, section_Association, connection_, priority_String,
    opts : OptionsPattern[VisualizeFiberedGraph]] :=
  Module[{emb, fibers, slices, thickness, opacity, numFibers,
    colorFunc, coords, coordRange, scale},
    thickness = OptionValue["FiberThickness"];
    opacity = OptionValue["FiberOpacity"];
    colorFunc = OptionValue["FiberColorFunction"];
    emb = AssociationThread[VertexList@total, GraphEmbedding@total];
    coords = Values@emb;
    coordRange = Max[coords] - Min[coords];
    scale = coordRange * thickness;
    fibers = GroupBy[Normal@proj, Last -> First];
    numFibers = Length@fibers;
    slices =
      MapIndexed[{fiber, idx} |->
        With[{sorted = SortBy[fiber, First@emb[#] &],
          color = colorFunc[First[idx] / numFibers]},
          Which[Length@sorted > 1, {color, Line[Map[emb, sorted]]},
            Length@sorted == 1, {color,
              Rectangle[emb[First@sorted] - {scale/2, scale/2},
                emb[First@sorted] + {scale/2, scale/2}]}, True, {}]],
        Values@fibers];
    Show[total, Graphics[{Opacity[opacity], Thickness[thickness], slices}],
      Sequence @@ FilterRules[{opts}, Options[Graphics]]]
  ];
