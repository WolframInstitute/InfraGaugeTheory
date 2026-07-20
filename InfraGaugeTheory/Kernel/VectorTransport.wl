Package["WolframInstitute`InfraGaugeTheory`"]

PackageExport["InfraVectorTransport"]
PackageExport["InfraCovariantDerivative"]
PackageExport["InfraVectorTransportPlot"]
PackageExport["InfraCovariantDerivativePlot"]

InfraVectorTransport::usage =
  "InfraVectorTransport[g, v, walk] parallel-transports the tangent vector First[walk] -> v along the vertex walk: v is a point of the direction sphere at its own radius len = d(First[walk], v), and the Levi-Civita sphere matching (InfraParallelTransport) at scale len carries it to the list of possible transported endpoints at Last[walk] over all realisations (the zero vector v == First[walk] goes to {Last[walk]}; germs with no image are dropped). InfraVectorTransport[g, v, walk, r] restricts to the scale-r tangent space, the vectors of length <= r transported layer by layer: vectors longer than r give {}. InfraVectorTransport[g, y, walk, r] transports the (possibly multivalued) vector-field value y[First[walk]]. Options \"Angle\", \"MaxRealizations\".";
InfraCovariantDerivative::usage =
  "InfraCovariantDerivative[g, y, walk] is the discrete covariant derivative of the vector field y along the curve walk: for each step p -> p' the value y[p'] is parallel-transported back to p at the scale of its own length, the minus against y[p] is taken by carrying the difference arrow y[p] -> transported endpoint back to p along a shortest path at its own scale, and the endpoint nearest to p in graph distance is kept, giving the association p -> derivative vector (p itself is the zero vector, so parallel fields give the identity field; a dead transport gives Missing). InfraCovariantDerivative[g, y, walk, r] restricts the field to the scale-r tangent space (values longer than r are dropped); InfraCovariantDerivative[g, y, walk, r, n] keeps the n (or UpTo[n], or All) nearest endpoints. Options \"Angle\", \"MaxRealizations\".";

InfraVectorTransportPlot::usage =
  "InfraVectorTransportPlot[g, v, walk] draws InfraVectorTransport[g, v, walk] over a gray copy of g: the walk in blue, the start vector First[walk] -> v in red, the transported endpoints at Last[walk] in green. An optional fourth argument r and the options are passed to InfraVectorTransport.";
InfraCovariantDerivativePlot::usage =
  "InfraCovariantDerivativePlot[g, y, walk] draws InfraCovariantDerivative[g, y, walk] over a gray copy of g: the walk in blue, the field y along the walk in pink, the derivative vectors in purple (zero vectors as points). An optional fourth argument r and the options are passed to InfraCovariantDerivative.";

Options[InfraVectorTransport] = {"Angle" -> "Arclength", "MaxRealizations" -> 200000};

InfraVectorTransport[g_Graph, y_Association, walk_List, r : _Integer | Automatic : Automatic, opts : OptionsPattern[]] :=
  DeleteDuplicates @ Catenate[
    (v |-> InfraVectorTransport[g, v, walk, r, opts]) /@ fieldValueList[g, y, First[walk]]];

InfraVectorTransport[g_Graph, v_, walk_List, r : _Integer | Automatic : Automatic, OptionsPattern[]] :=
  Module[{len = GraphDistance[g, First[walk], v], composites},
    If[len == 0, Return[{Last[walk]}, Module]];
    If[! IntegerQ[len] || (IntegerQ[r] && len > r), Return[{}, Module]];
    composites = leviCivitaWalkComposites[g, walk, len, OptionValue["Angle"], OptionValue["MaxRealizations"]];
    DeleteDuplicates @ DeleteMissing @ Lookup[composites, Key[v], Missing[]]
  ];

Options[InfraCovariantDerivative] = {"Angle" -> "Arclength", "MaxRealizations" -> 200000};

InfraCovariantDerivative[g_Graph, y_Association, walk_List, r : _Integer | Automatic : Automatic,
    n : _Integer | _UpTo | All | Automatic : Automatic, OptionsPattern[]] :=
  Association @ MapThread[
    {p, pNext} |-> p -> covariantDifference[g, y, p, pNext, r, n, OptionValue["Angle"], OptionValue["MaxRealizations"]],
    {Most[walk], Rest[walk]}
  ];

covariantDifference[g_, y_, p_, pNext_, r_, n_, angle_, maxReals_] :=
  Module[{transported, groups, need, deltas = {}},
    transported = InfraVectorTransport[g, y, {pNext, p}, r, "Angle" -> angle, "MaxRealizations" -> maxReals];
    groups = SortBy[
      GatherBy[Tuples[{transported, fieldValueList[g, y, p]}], pair |-> GraphDistance[g, pair[[2]], pair[[1]]]],
      group |-> GraphDistance[g, group[[1, 2]], group[[1, 1]]]];
    need = Replace[n, {Automatic -> 1, All -> Infinity, UpTo[k_Integer] :> k}];
    Do[
      deltas = DeleteDuplicates @ Join[deltas, Catenate[pairDelta[g, p, #, angle, maxReals] & /@ group]];
      If[Length[deltas] >= need, Break[]],
      {group, groups}];
    nearestSelect[g, p, deltas, n]
  ];

pairDelta[g_, p_, {a_, b_}, angle_, maxReals_] :=
  Which[
    a === b, {p},
    b === p, {a},
    FindShortestPath[g, b, p] === {}, {},
    True, InfraVectorTransport[g, a, FindShortestPath[g, b, p], Automatic, "Angle" -> angle, "MaxRealizations" -> maxReals]
  ];

Options[InfraVectorTransportPlot] = Options[InfraVectorTransport];

InfraVectorTransportPlot[g_Graph, v_, walk_List, r : _Integer | Automatic : Automatic, opts : OptionsPattern[]] :=
  With[{pos = AssociationThread[VertexList[g], GraphEmbedding[g]]},
    Show[
      grayGraph[g],
      Graphics[{Arrowheads[0.03],
        {RGBColor[0.4, 0.6, 1], Thickness[0.008], Line[pos /@ walk]},
        {Red, Thickness[0.006], Arrow[{pos[First[walk]], pos[v]}]},
        {Darker[Green], Thickness[0.006],
          (w |-> Arrow[{pos[Last[walk]], pos[w]}]) /@ InfraVectorTransport[g, v, walk, r, opts]}}]]];

Options[InfraCovariantDerivativePlot] = Options[InfraCovariantDerivative];

InfraCovariantDerivativePlot[g_Graph, y_Association, walk_List, r : _Integer | Automatic : Automatic, opts : OptionsPattern[]] :=
  With[{pos = AssociationThread[VertexList[g], GraphEmbedding[g]]},
    Show[
      grayGraph[g],
      Graphics[{Arrowheads[0.025],
        {RGBColor[0.4, 0.6, 1], Thickness[0.008], Line[pos /@ walk]},
        {Pink, (p |-> (w |-> If[w === p, {}, Arrow[{pos[p], pos[w]}]]) /@ fieldValueList[g, y, p]) /@ walk},
        {Purple, Thickness[0.006],
          KeyValueMap[
            {p, w} |-> Which[MissingQ[w], {}, w === p, {PointSize[0.015], Point[pos[p]]}, True, Arrow[{pos[p], pos[w]}]],
            InfraCovariantDerivative[g, y, walk, r, opts]]}}]]];

fieldValueList[g_, y_Association, p_] :=
  Replace[Lookup[y, Key[p]], {_Missing -> {}, v_List /; ! VertexQ[g, v] :> v, v_ :> {v}}];

nearestSelect[g_, p_, points_, n_] :=
  With[{sorted = SortBy[DeleteDuplicates[points], AssociationThread[VertexList[g], GraphDistance[g, p]]]},
    Replace[n, {
      Automatic :> First[sorted, Missing["NotFound"]],
      All :> sorted,
      UpTo[k_Integer] | k_Integer :> Take[sorted, UpTo[k]]
    }]
  ];

grayGraph[g_] :=
  Graph[g, VertexSize -> Small, VertexStyle -> GrayLevel[0.5], EdgeStyle -> GrayLevel[0.87]];
