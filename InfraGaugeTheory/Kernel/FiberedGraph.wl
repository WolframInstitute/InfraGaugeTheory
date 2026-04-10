Package["WolframInstitute`InfraGaugeTheory`"]

PackageExport["RandomFiberedGraph"]
PackageExport["ReconstructBaseGraph"]
PackageExport["IsomorphicFibersQ"]
PackageExport["EdgeLiftingPropertyQ"]
PackageExport["AtMostOneEdgeLiftQ"]
PackageExport["LocallyTrivialQ"]
PackageExport["FiberBundleQ"]

RandomFiberedGraph::usage = "RandomFiberedGraph[g, opts] creates a random fibered graph over a base graph g. Option \"IsomorphicFibers\" -> True generates a single fiber graph and copies it to all fibers.";
ReconstructBaseGraph::usage = "ReconstructBaseGraph[total, proj, opts] reconstructs the base space of a fibered graph from total space and projection.";
IsomorphicFibersQ::usage = "IsomorphicFibersQ[total, proj, opts] tests whether all fibers are isomorphic as graphs. Option \"IsomorphismFunction\" -> f uses f[g1, g2] instead of IsomorphicGraphQ.";
EdgeLiftingPropertyQ::usage = "EdgeLiftingPropertyQ[total, proj] tests the edge lifting property: for every base edge and every fiber vertex over its source, there exists at least one lift.";
AtMostOneEdgeLiftQ::usage = "AtMostOneEdgeLiftQ[total, proj] tests that for every base edge and every fiber vertex, there is at most one lift.";
LocallyTrivialQ::usage = "LocallyTrivialQ[total, proj, opts] tests local triviality: trivial holonomy around all cycles in each vertex neighborhood. Option \"Radius\" -> 1 controls the neighborhood size.";
FiberBundleQ::usage = "FiberBundleQ[total, proj] tests whether a graph morphism is a fiber bundle: isomorphic fibers and local triviality.";

(* ========================= RandomFiberedGraph ========================= *)

Options[RandomFiberedGraph] = {
  "VerticalVertices" -> 1,
  "VerticalEdges" -> 0,
  "HorizontalEdgesRadius" -> 1,
  "HorizontalEdgesDensity" -> 1,
  "IsomorphicFibers" -> False
};

RandomFiberedGraph[g_Graph, opts : OptionsPattern[]] :=
  Module[{getVerticalVertices, getVerticalEdges, getHorizontalEdgesRadius,
    getHorizontalEdgesDensity, numberGetter, baseVertices,
    verticalVertexSym, baseToFiber, proj, totalVertices, verticalEdges,
    horizontalEdges, pairs},
    numberGetter[n_?NumericQ] := (n &);
    numberGetter[{a_, b_}] := (RandomReal[{a, b}] &);
    getVerticalVertices = numberGetter @ OptionValue["VerticalVertices"];
    getVerticalEdges = numberGetter @ OptionValue["VerticalEdges"];
    getHorizontalEdgesRadius =
      numberGetter @ OptionValue["HorizontalEdgesRadius"];
    getHorizontalEdgesDensity =
      numberGetter @ OptionValue["HorizontalEdgesDensity"];
    baseVertices = VertexList[g];
    verticalVertexSym = Unique["v"];
    baseToFiber =
      If[OptionValue["IsomorphicFibers"],
        With[{n = Round @ getVerticalVertices[]},
          AssociationMap[(b |->
            Indexed[verticalVertexSym, {b, #}] & /@ Range[n]),
            baseVertices]],
        AssociationMap[(b |->
          Indexed[verticalVertexSym, {b, #}] & /@
            Range[Round @ getVerticalVertices[]]), baseVertices]];
    proj =
      Association @
        Catenate @
          KeyValueMap[{base, fiber} |-> Thread[fiber -> base],
            baseToFiber];
    totalVertices = Keys[proj];
    verticalEdges =
      If[OptionValue["IsomorphicFibers"],
        With[{n = Length @ First @ Values @ baseToFiber},
          With[{m = Min[Max[0, Round @ getVerticalEdges[]], n (n - 1)/2]},
            If[n > 1 && m > 0,
              With[{template = EdgeList[RandomGraph[{n, m}]]},
                Catenate @ Map[
                  fiber |-> Replace[template, Thread[Range[n] -> fiber], {2}],
                  Values[baseToFiber]]],
              {}]]],
        Catenate @ Map[
          fiber |->
            With[{n = Length[fiber]}, {
              m = Min[Max[0, Round @ getVerticalEdges[]], n (n - 1)/2]},
              If[n > 1 && m > 0,
                Replace[EdgeList[RandomGraph[{n, m}]],
                  Thread[Range[n] -> fiber], {2}], {}]],
          Values[baseToFiber]]];
    horizontalEdges = Catenate @ Map[
      pair |->
        With[{f1 = baseToFiber[First[pair]],
          f2 = baseToFiber[Last[pair]]}, {n1 = Length[f1],
          n2 = Length[f2], d = getHorizontalEdgesDensity[]},
          Map[
            p |-> UndirectedEdge[f1[[First[p]]], f2[[Last[p]]]],
            Select[Tuples[{Range[n1], Range[n2]}],
              p |-> Mod[First[p] n2 - Last[p] n1, n1 n2] < d Max[n1, n2]]
          ]],
      Select[Subsets[baseVertices, {2}],
        pair |->
          GraphDistance[g, First[pair], Last[pair]] <=
            Max[0, Round @ getHorizontalEdgesRadius[]]]];
    {Graph[totalVertices, Join[verticalEdges, horizontalEdges]], proj}
  ];

(* ========================= ReconstructBaseGraph ========================= *)

ReconstructBaseGraph[total_Graph, proj_Association, opts___] :=
  Graph[DeleteDuplicates @ Values @ proj,
    DeleteDuplicates[
      DeleteCases[Map[Map[proj], EdgeList[total]],
        e_ /; First@e === Last@e], Sort[#1] === Sort[#2] &],
    opts];

(* ========================= Fibered Graph Properties ========================= *)

Options[IsomorphicFibersQ] = {"IsomorphismFunction" -> IsomorphicGraphQ};

IsomorphicFibersQ[total_Graph, proj_Association, opts : OptionsPattern[]] :=
  With[{
    fiberGraphs = Map[Subgraph[total, #] &, Values @ GroupBy[Normal @ proj, Last -> First]],
    iso = OptionValue["IsomorphismFunction"]},
    AllTrue[Rest @ fiberGraphs, iso[First @ fiberGraphs, #] &]];

EdgeLiftingPropertyQ[total_Graph, proj_Association] :=
  With[{
    base = ReconstructBaseGraph[total, proj],
    fibers = GroupBy[Normal @ proj, Last -> First]},
    AllTrue[EdgeList @ base,
      edge |->
        With[{u = First @ edge, v = Last @ edge},
          AllTrue[fibers[u],
            fu |-> AnyTrue[AdjacencyList[total, fu], fv |-> proj[fv] === v]] &&
          AllTrue[fibers[v],
            fv |-> AnyTrue[AdjacencyList[total, fv], fu |-> proj[fu] === u]]]]];

AtMostOneEdgeLiftQ[total_Graph, proj_Association] :=
  With[{
    base = ReconstructBaseGraph[total, proj],
    fibers = GroupBy[Normal @ proj, Last -> First]},
    AllTrue[EdgeList @ base,
      edge |->
        With[{u = First @ edge, v = Last @ edge},
          AllTrue[fibers[u],
            fu |-> Length @ Select[AdjacencyList[total, fu], fv |-> proj[fv] === v] <= 1] &&
          AllTrue[fibers[v],
            fv |-> Length @ Select[AdjacencyList[total, fv], fu |-> proj[fu] === u] <= 1]]]];

Options[LocallyTrivialQ] = {"Radius" -> 1};

LocallyTrivialQ[total_Graph, proj_Association, opts : OptionsPattern[]] :=
  Module[{base, fibers, lift, radius},
    base = ReconstructBaseGraph[total, proj];
    fibers = GroupBy[Normal @ proj, Last -> First];
    radius = OptionValue["Radius"];
    lift[fv_, targetBase_] :=
      First @ Select[AdjacencyList[total, fv], u |-> proj[u] === targetBase];
    AllTrue[VertexList[base],
      v |->
        With[{neighborhood = VertexList @ NeighborhoodGraph[base, v, radius]},
          With[{localBase = Subgraph[base, neighborhood]},
            With[{cycles = FindCycle[localBase, Infinity, All]},
              AllTrue[cycles,
                cycle |->
                  With[{verts = DeleteDuplicates @ Flatten[List @@@ cycle]},
                    With[{loop = Append[verts, First @ verts]},
                      AllTrue[fibers[First @ verts],
                        fv |-> Fold[lift, fv, Rest @ loop] === fv]]]]]]]]];

FiberBundleQ[total_Graph, proj_Association, opts : OptionsPattern[{LocallyTrivialQ}]] :=
  IsomorphicFibersQ[total, proj] && EdgeLiftingPropertyQ[total, proj] && AtMostOneEdgeLiftQ[total, proj] &&
    LocallyTrivialQ[total, proj, FilterRules[{opts}, Options[LocallyTrivialQ]]];
