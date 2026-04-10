Package["WolframInstitute`InfraGaugeTheory`"]

PackageExport["ConnectionQ"]
PackageExport["RandomConnection"]
PackageExport["FindHorizontalLift"]
PackageExport["ParallelTransport"]
PackageExport["HolonomyMatrix"]
PackageExport["HolonomyMatrices"]
PackageExport["FlatConnectionQ"]
PackageExport["FindHorizontalLeaf"]

ConnectionQ::usage = "ConnectionQ[total, proj, connection] verifies that a graph forms a valid connection: unique edge lifting and completeness.";
RandomConnection::usage = "RandomConnection[total, proj] generates a random connection on a fibered graph.";
FindHorizontalLift::usage = "FindHorizontalLift[total, proj, connection, startVertex, basePath] lifts a base path horizontally starting from a fiber vertex. Returns a truncated list if a lift does not exist at some step.";
ParallelTransport::usage = "ParallelTransport[total, proj, connection, path] computes parallel transport along a base path. Returns an Association mapping start fiber vertices to end fiber vertices. Fiber vertices whose lift is blocked are omitted.";
HolonomyMatrix::usage = "HolonomyMatrix[total, proj, connection, loop] returns the holonomy as a permutation matrix. Rows for fiber vertices with blocked transport are zero.";
HolonomyMatrices::usage = "HolonomyMatrices[total, proj, connection, basePoint] computes holonomy matrices for all basis cycles routed through basePoint.";
FlatConnectionQ::usage = "FlatConnectionQ[total, proj, connection] tests whether a connection has trivial holonomy around all loops at all basepoints.";
FindHorizontalLeaf::usage = "FindHorizontalLeaf[connection, proj, startVertex] computes the horizontal leaf through startVertex by BFS on the connection graph. Returns an Association mapping base vertices to total-space vertices, or $Failed if parallel transport is path-dependent at startVertex. A horizontal leaf exists iff every holonomy at the base point fixes startVertex.";

(* ========================= Connections ========================= *)

ConnectionQ[total_Graph, proj_Association, connection_Graph] :=
  AtMostOneEdgeLiftQ[connection, proj] &&
    With[{base = ReconstructBaseGraph[total, proj]},
      AllTrue[Keys @ proj,
        fv |-> AllTrue[AdjacencyList[base, proj[fv]],
          baseNeighbor |->
            With[{lifts = Select[AdjacencyList[total, fv], u |-> proj[u] === baseNeighbor]},
              Length @ lifts == 0 ||
                AnyTrue[AdjacencyList[connection, fv], u |-> proj[u] === baseNeighbor]]]]];

RandomConnection[total_Graph, proj_Association] :=
  Module[{transverseEdges, edgesByPair, connectionEdges},
    transverseEdges = Select[EdgeList[total], proj[#[[1]]] =!= proj[#[[2]]] &];
    edgesByPair = GroupBy[transverseEdges, Sort[{proj[#[[1]]], proj[#[[2]]]}] &];
    connectionEdges = Catenate @ KeyValueMap[
      {pair, edges} |->
        With[{matching = FindIndependentEdgeSet[Graph[RandomSample[edges]]]},
          If[Length[matching] > 0, matching, {First[edges]}]],
      edgesByPair];
    Graph[VertexList[total], connectionEdges]
  ];

FindHorizontalLift[total_Graph, proj_Association, connection_Graph, startVertex_, basePath_List] :=
  If[proj[startVertex] =!= First @ basePath, {},
    FoldWhileList[
      {current, nextBase} |->
        With[{lifts = Select[AdjacencyList[connection, current], proj[#] === nextBase &]},
          If[Length[lifts] > 0, RandomChoice @ lifts, Null]],
      startVertex,
      Rest @ basePath,
      # =!= Null &
    ]
  ];

ParallelTransport[total_Graph, proj_Association, connection_Graph, path_List] :=
  Module[{fibers},
    If[Length[path] < 2, Return[<||>]];
    fibers = GroupBy[Normal @ proj, Last -> First];
    Association @ Map[
      startVertex |->
        With[{curve = FindHorizontalLift[total, proj, connection, startVertex, path]},
          If[Length[curve] == Length[path] && Last[curve] =!= Null,
            startVertex -> Last[curve],
            Nothing]],
      fibers[First[path]]]
  ];

(* ========================= Holonomy ========================= *)

HolonomyMatrix[total_Graph, proj_Association, connection_Graph, loop_List] :=
  Module[{basePoint, fiber, transport, n},
    If[Length[loop] < 2 || First[loop] =!= Last[loop], Return[{}]];
    basePoint = First[loop];
    fiber = Sort @ Select[VertexList[total], proj[#] === basePoint &];
    n = Length[fiber];
    If[n == 0, Return[{}]];
    transport = ParallelTransport[total, proj, connection, loop];
    SparseArray[
      Catenate @ MapIndexed[
        {fv, idx} |->
          With[{image = Lookup[transport, Key[fv], None]},
            If[image =!= None,
              With[{j = FirstPosition[fiber, image, None]},
                If[j =!= None, {{First[idx], First[j]} -> 1}, {}]],
              {}]],
        fiber],
      {n, n}]
  ];

HolonomyMatrices[total_Graph, proj_Association, connection_Graph, basePoint_] :=
  Module[{base, cycles, loops},
    base = ReconstructBaseGraph[total, proj];
    cycles = FindCycle[base, Infinity, All];
    If[Length[cycles] == 0, Return[<||>]];
    loops = DeleteDuplicates[Map[
      cycle |->
        Module[{open, rotated, pathTo, pathFrom},
          open = Join[{cycle[[1, 1]]}, Map[Last, cycle]];
          open = Most[open];
          If[MemberQ[open, basePoint],
            With[{idx = First @ FirstPosition[open, basePoint]},
              rotated = RotateLeft[open, idx - 1];
              Append[rotated, First @ rotated]],
            pathTo = First[FindPath[base, basePoint, First[open], Infinity, 1], {basePoint}];
            pathFrom = Rest @ First[FindPath[base, Last[open], basePoint, Infinity, 1], {basePoint}];
            Join[pathTo, Rest[open], pathFrom]]],
      cycles], Sort[#1] === Sort[#2] &];
    AssociationThread[loops,
      Map[loop |-> HolonomyMatrix[total, proj, connection, loop], loops]]
  ];

FlatConnectionQ[total_Graph, proj_Association, connection_Graph] :=
  With[{base = ReconstructBaseGraph[total, proj]},
    AllTrue[VertexList[base],
      bp |-> AllTrue[
        Values @ HolonomyMatrices[total, proj, connection, bp],
        mat |-> AllTrue[Range[Length[mat]],
          i |-> mat[[i, i]] == 1 || Total[mat[[i]]] == 0]]]];

(* ========================= Horizontal Subgraph ========================= *)

FindHorizontalLeaf[connection_Graph, proj_Association, startVertex_] :=
  Module[{assignment, queue, pos, neighbors, target},
    assignment = <| proj[startVertex] -> startVertex |>;
    queue = { startVertex };
    pos = 1;
    While[pos <= Length[queue],
      neighbors = AdjacencyList[connection, queue[[ pos ]]];
      pos++;
      Do[
        With[{base = proj[target]},
          Which[
            !KeyExistsQ[assignment, base],
              AssociateTo[assignment, base -> target];
              AppendTo[queue, target],
            assignment[base] =!= target,
              Return[$Failed, Module]]],
        {target, neighbors}]];
    assignment
  ];
