Package["WolframInstitute`InfraGaugeTheory`"]

PackageExport["InfraParallelTransport"]
PackageExport["InfraLeviCivitaDefect"]
PackageExport["FindInfraLeviCivitaConnection"]
PackageExport["InfraHolonomy"]
PackageExport["InfraHolonomyAngle"]
PackageExport["InfraConnectionCurvature"]
PackageExport["FindInfraParallelFrame"]

InfraParallelTransport::usage =
  "InfraParallelTransport[g, p, q] is the metric Levi-Civita parallel transport of directions from p to q: the InfraScalarProduct-Gram isometry of the radius-r direction spheres sending the radial germ toward q to its straightest continuation. Returns the list of realisations, each an Association germ -> image (source germs with no image omitted). InfraParallelTransport[g, walk] composes along a vertex walk. Option \"Radius\" -> 1, \"MaxRealizations\".";
InfraLeviCivitaDefect::usage =
  "InfraLeviCivitaDefect[g, p, q] is the InfraScalarProduct-Gram mismatch of the best transport p -> q: 0 iff an exact angle-isometry exists (vertex-transitive case), > 0 when the fibers are incongruent. Option \"Radius\" -> 1.";
FindInfraLeviCivitaConnection::usage =
  "FindInfraLeviCivitaConnection[g] returns the metric Levi-Civita connection on the radius-r direction-sphere fibration as <|\"TotalGraph\" -> _, \"Projection\" -> _, \"Connection\" -> _|>, feedable to ParallelTransport / HolonomyMatrix / VisualizeFiberedGraph. Read the gauge-invariant curvature with InfraHolonomyAngle. Options \"Radius\" -> 1, Method -> \"Metric\".";
InfraHolonomy::usage =
  "InfraHolonomy[g, loop] is the holonomy self-map of the direction sphere over the base point of a closed walk: the minimal-rotation realisation over the per-edge transport choices. Option \"Radius\" -> 1, \"MaxRealizations\".";
InfraHolonomyAngle::usage =
  "InfraHolonomyAngle[g, loop] is the rotation angle (radians) of InfraHolonomy: 0 iff the loop bounds a flat region, > 0 measuring the enclosed curvature. Option \"Radius\" -> 1, \"MaxRealizations\".";
InfraConnectionCurvature::usage =
  "InfraConnectionCurvature[g, face] is the holonomy angle around the boundary cycle of a face (the discrete curvature 2-form). Option \"Radius\" -> 1, \"MaxRealizations\".";
FindInfraParallelFrame::usage =
  "FindInfraParallelFrame[g, walk, frame0] parallel-transports an orthogonal frame frame0 (a list of germ vertices in the radius-r sphere of the first walk vertex) along walk, returning the list of transported frames at the endpoint -- one per realisation, deduped, with frame germs that have no image dropped. Option \"Radius\" -> 1, \"MaxRealizations\".";

PackageScope["infraScalarProductValue"]
PackageScope["leviCivitaEdgeData"]
PackageScope["leviCivitaWalkComposites"]
PackageScope["holonomyAngleValue"]

InfraParallelTransport::cap =
  "The Cartesian of per-edge realisations has `1` elements, exceeding the cap `2`; each per-edge list was truncated. The returned holonomy is a bound over the truncated set, not the exact optimum. Raise \"MaxRealizations\" or use a finer radius to separate angles.";
FindInfraLeviCivitaConnection::badmethod = "Unknown Method `1`; only \"Metric\" is implemented.";

(* ===================== Metric-native Levi-Civita transport ===================== *)

(* Per-edge parallel transport tau_{p->q}: S_r(p) -> S_r(q), the scalar-product-Gram *)
(* isometry sending the radial germ toward q to its straightest continuation. *)
(* Realised by matching the InfraScalarProduct Gram matrices of the two direction *)
(* spheres (metric compatibility) with the bond germ pinned (geodesics auto-parallel); *)
(* the residual fiber symmetry is returned as multi-realisation. Output: list of *)
(* Associations u -> tau[u] (one per realisation), source vertices with no match omitted. *)

Options[InfraParallelTransport] = {"Radius" -> 1, "MaxRealizations" -> 200000};

InfraParallelTransport[g_Graph, p_, q_, OptionsPattern[]] :=
  leviCivitaEdgeData[g, p, q, OptionValue["Radius"]]["transports"];

(* Composite transport along a walk v0 -> v1 -> ... -> vk: the multi-realisation set of *)
(* maps S_r(v0) -> S_r(vk), one per choice of per-edge realisation (Cartesian, deduped). *)

InfraParallelTransport[g_Graph, walk_List, OptionsPattern[]] :=
  DeleteDuplicates @ leviCivitaWalkComposites[g, walk, OptionValue["Radius"], OptionValue["MaxRealizations"]];

(* Levi-Civita defect of the edge: the InfraScalarProduct-Gram mismatch of the best *)
(* transport; 0 iff an exact angle-isometry exists (regular/vertex-transitive case), *)
(* > 0 when the fibers are incongruent (deg p != deg q) -- the obstruction, not a crash. *)

Options[InfraLeviCivitaDefect] = {"Radius" -> 1};

InfraLeviCivitaDefect[g_Graph, p_, q_, OptionsPattern[]] :=
  leviCivitaEdgeData[g, p, q, OptionValue["Radius"]]["defect"];

(* ===================== Holonomy ===================== *)

(* Holonomy of a closed walk = self-map of S_r(base) by composing the per-edge transports *)
(* around the loop. The metric leaves a residual fiber symmetry, so the holonomy is the *)
(* minimal-rotation realisation over all per-edge choices -- the irreducible (gauge- *)
(* invariant) part: identity iff the loop bounds a flat region, a nontrivial rotation *)
(* whose angle measures the enclosed curvature. *)

Options[InfraHolonomy] = {"Radius" -> 1, "MaxRealizations" -> 200000};

InfraHolonomy[g_Graph, loop_List, OptionsPattern[]] :=
  Module[{dd = GraphDistanceMatrix[g], ix},
    ix = AssociationThread[VertexList[g], Range @ VertexCount[g]];
    First @ MinimalBy[
      leviCivitaWalkComposites[g, loop, OptionValue["Radius"], OptionValue["MaxRealizations"]],
      holonomyAngleValue[dd, ix, First[loop], #] &]
  ];

(* Rotation angle (radians) of the holonomy: the minimal mean direction-rotation over all *)
(* realisations. 0 iff trivial holonomy (flat); the discrete curvature 2-form value. *)

Options[InfraHolonomyAngle] = {"Radius" -> 1, "MaxRealizations" -> 200000};

InfraHolonomyAngle[g_Graph, loop_List, OptionsPattern[]] :=
  Module[{dd = GraphDistanceMatrix[g], ix},
    ix = AssociationThread[VertexList[g], Range @ VertexCount[g]];
    Min[holonomyAngleValue[dd, ix, First[loop], #] & /@
      leviCivitaWalkComposites[g, loop, OptionValue["Radius"], OptionValue["MaxRealizations"]]]
  ];

(* Curvature of a face = holonomy angle around its boundary cycle. *)

Options[InfraConnectionCurvature] = {"Radius" -> 1, "MaxRealizations" -> 200000};

InfraConnectionCurvature[g_Graph, face_List, opts : OptionsPattern[]] :=
  InfraHolonomyAngle[g, If[First[face] === Last[face], face, Append[face, First[face]]], opts];

(* ===================== Global connection (fibration triple) ===================== *)

(* Assemble the per-edge transports into a sphere-fibration connection feedable to *)
(* HolonomyMatrix[total, proj, connection, loop]: total/connection = the horizontal graph *)
(* on vertices {v, u}, u in S_r(v); proj{v,u} = v. One representative (First per edge); *)
(* the gauge-invariant rotation is InfraHolonomyAngle, not this connection. *)

Options[FindInfraLeviCivitaConnection] = {"Radius" -> 1, Method -> "Metric"};

FindInfraLeviCivitaConnection[g_Graph, OptionsPattern[]] :=
  Module[{r = OptionValue["Radius"], sphere, totalV, horizontal},
    If[OptionValue[Method] =!= "Metric",
      Message[FindInfraLeviCivitaConnection::badmethod, OptionValue[Method]]; Return[$Failed]];
    sphere = AssociationMap[v |-> Select[VertexList[g], GraphDistance[g, v, #] == r &], VertexList[g]];
    totalV = Catenate[(v |-> ({v, #} & /@ sphere[v])) /@ VertexList[g]];
    horizontal = Graph[totalV, Catenate[
      (e |-> KeyValueMap[{u, w} |-> UndirectedEdge[{e[[1]], u}, {e[[2]], w}],
         First @ InfraParallelTransport[g, e[[1]], e[[2]], "Radius" -> r]]) /@ EdgeList[g]]];
    <|"TotalGraph" -> horizontal, "Projection" -> AssociationThread[totalV, totalV[[All, 1]]],
      "Connection" -> horizontal|>
  ];

(* ===================== Parallel frame transport ===================== *)

(* Transport an orthogonal frame (a tuple of germ directions at the first walk vertex) *)
(* along the walk: apply each composite transport to every frame germ.  Along a geodesic *)
(* the tangent germ goes to its straight continuation (auto-parallel); around a loop the *)
(* transported frame differs from frame0 by the holonomy.  Returns one frame per per-edge *)
(* realisation (deduped); frame germs with no image are dropped. *)

Options[FindInfraParallelFrame] = {"Radius" -> 1, "MaxRealizations" -> 200000};

FindInfraParallelFrame[g_Graph, walk_List, frame0_List, OptionsPattern[]] :=
  DeleteDuplicates[
    (comp |-> DeleteMissing @ Lookup[comp, frame0]) /@
      leviCivitaWalkComposites[g, walk, OptionValue["Radius"], OptionValue["MaxRealizations"]]
  ];

(* ===================== Walk composition & angle helpers ===================== *)

(* Composite maps S_r(first walk vertex) -> S_r(last) for every per-edge realisation choice. *)
(* Composition threads each base germ through the chain; a germ with no image is dropped *)
(* (Missing). The Cartesian is capped at maxReals (truncating each per-edge list, with a *)
(* message) so high-symmetry fibers (e.g. the r=1 cubic lattice) stay tractable. *)

leviCivitaWalkComposites[g_Graph, walk_List, r_, maxReals_] :=
  Module[{sphere, taus, prod, used},
    sphere = Select[VertexList[g], GraphDistance[g, First[walk], #] == r &];
    taus = (e |-> InfraParallelTransport[g, e[[1]], e[[2]], "Radius" -> r]) /@ Partition[walk, 2, 1];
    prod = Times @@ (Length /@ taus);
    used = If[prod > maxReals,
      Message[InfraParallelTransport::cap, prod, maxReals];
      With[{perCap = Max[1, Floor[maxReals^(1/Length[taus])]]}, Take[#, UpTo[perCap]] & /@ taus],
      taus];
    Map[chain |-> Fold[
        {acc, tau} |-> AssociationThread[Keys[acc], Lookup[tau, Values[acc], Missing[]]],
        AssociationThread[sphere, sphere], chain],
      Tuples[used]]
  ];

(* Mean direction-rotation angle of a holonomy self-map: average of angle(u, hol[u]) over *)
(* moved germs (dropped germs ignored); 0 when the map is the identity. *)

holonomyAngleValue[dd_, ix_, base_, hol_] :=
  With[{moved = Select[Keys[hol], hol[#] =!= # && Not @ MissingQ[hol[#]] &]},
    If[moved === {}, 0,
      N @ Mean[(a |-> ArcCos[Clip[
        infraScalarProductValue[dd, ix, base, a, hol[a]] /
          (dd[[ix[base], ix[a]]] dd[[ix[base], ix[hol[a]]]]), {-1, 1}]]) /@ moved]]
  ];

(* ===================== Shared edge computation ===================== *)

(* InfraScalarProduct[g, o, a, b] (Method -> Alexandrov, Curvature -> 0): the kappa=0 *)
(* polar form of the squared metric. Exact-rational; inlined here so the module is *)
(* standalone (no cross-paclet dependency on WolframInstitute`SyntheticInfrageometry`). *)
infraScalarProductValue[dd_, ix_, o_, a_, b_] :=
  (dd[[ix[o], ix[a]]]^2 + dd[[ix[o], ix[b]]]^2 - dd[[ix[a], ix[b]]]^2)/2;

leviCivitaEdgeData[g_Graph, p_, q_, r_] :=
  Module[{vs, ix, dd, sphereP, sphereQ, gramP, gramQ, bondPos, straightIdx, transposed,
    loGram, hiGram, hiLen, loLen, matchings, score, best, reps},
    vs = VertexList[g];
    ix = AssociationThread[vs, Range[Length[vs]]];
    dd = GraphDistanceMatrix[g];
    sphereP = Select[vs, dd[[ix[p], ix[#]]] == r &];
    sphereQ = Select[vs, dd[[ix[q], ix[#]]] == r &];
    gramP = Outer[infraScalarProductValue[dd, ix, p, #1, #2] &, sphereP, sphereP, 1];
    gramQ = Outer[infraScalarProductValue[dd, ix, q, #1, #2] &, sphereQ, sphereQ, 1];
    bondPos = First @ FirstPosition[sphereP, First @ MinimalBy[sphereP, dd[[ix[q], ix[#]]] &]];
    straightIdx = MinimalBy[Range[Length[sphereQ]], infraScalarProductValue[dd, ix, q, sphereQ[[#]], p] &];
    transposed = Length[sphereP] > Length[sphereQ];
    {loGram, hiGram} = If[transposed, {gramQ, gramP}, {gramP, gramQ}];
    {loLen, hiLen} = {Length[loGram], Length[hiGram]};
    matchings = Select[Permutations[Range[hiLen], {loLen}],
      m |-> If[transposed, AnyTrue[straightIdx, m[[#]] == bondPos &], MemberQ[straightIdx, m[[bondPos]]]]];
    score = m |-> Total[Flatten[(loGram - hiGram[[m, m]])^2]];
    If[matchings === {},
      <|"defect" -> Infinity, "transports" -> {}, "sphereP" -> sphereP, "sphereQ" -> sphereQ|>,
      best = Min[score /@ matchings];
      reps = Select[matchings, score[#] == best &];
      <|
        "defect" -> Sqrt[best],
        "sphereP" -> sphereP,
        "sphereQ" -> sphereQ,
        "transports" -> Map[
          m |-> If[transposed,
            AssociationThread[sphereP[[m]], sphereQ],
            AssociationThread[sphereP, sphereQ[[m]]]],
          reps]
      |>
    ]
  ];
