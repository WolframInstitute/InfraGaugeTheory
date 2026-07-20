Package["WolframInstitute`InfraGaugeTheory`"]

PackageExport["InfraParallelTransport"]
PackageExport["InfraLeviCivitaDefect"]
PackageExport["FindInfraLeviCivitaConnection"]
PackageExport["InfraHolonomy"]
PackageExport["InfraHolonomyAngle"]
PackageExport["InfraConnectionCurvature"]
PackageExport["FindInfraParallelFrame"]
PackageExport["InfraCanonicalOneForm"]

InfraParallelTransport::usage =
  "InfraParallelTransport[g, p, q, r] is the metric Levi-Civita parallel transport of directions from p to q at scale r: the angle-isometry of the radius-r direction spheres sending the radial germ toward q to its straightest continuation. Returns the list of realisations, each an Association germ -> image (source germs with no image omitted). InfraParallelTransport[g, walk, r] composes along a vertex walk. Options \"Angle\" (\"Arclength\" (default): arc distance in the ball-punched graph / r, as InfraAngle; \"Alexandrov\": kappa=0 polar-form Gram), \"MaxRealizations\".";
InfraLeviCivitaDefect::usage =
  "InfraLeviCivitaDefect[g, p, q, r] is the angle-structure mismatch of the best transport p -> q at scale r: 0 iff an exact angle-isometry exists (vertex-transitive case), > 0 when the fibers are incongruent. Option \"Angle\".";
FindInfraLeviCivitaConnection::usage =
  "FindInfraLeviCivitaConnection[g, r] returns the metric Levi-Civita connection on the radius-r direction-sphere fibration as <|\"TotalGraph\" -> _, \"Projection\" -> _, \"Connection\" -> _|>, feedable to ParallelTransport / HolonomyMatrix / VisualizeFiberedGraph. Read the gauge-invariant curvature with InfraHolonomyAngle. Options Method -> \"Metric\", \"Angle\".";
InfraHolonomy::usage =
  "InfraHolonomy[g, loop, r] is the scale-r holonomy self-map of the direction sphere over the base point of a closed walk: the minimal-rotation realisation over the per-edge transport choices. Options \"Angle\", \"MaxRealizations\".";
InfraHolonomyAngle::usage =
  "InfraHolonomyAngle[g, loop, r] is the rotation angle of InfraHolonomy: 0 iff the loop bounds a flat region, > 0 measuring the enclosed curvature (arc radians for \"Angle\" -> \"Arclength\", ArcCos radians for \"Alexandrov\"). Options \"Angle\", \"MaxRealizations\".";
InfraConnectionCurvature::usage =
  "InfraConnectionCurvature[g, face, r] is the scale-r holonomy angle around the boundary cycle of a face (the discrete curvature 2-form). Options \"Angle\", \"MaxRealizations\".";
FindInfraParallelFrame::usage =
  "FindInfraParallelFrame[g, walk, frame0, r] parallel-transports an orthogonal frame frame0 (a list of germ vertices in the radius-r sphere of the first walk vertex) along walk, returning the list of transported frames at the endpoint -- one per realisation, deduped, with frame germs that have no image dropped. Options \"Angle\", \"MaxRealizations\".";
InfraCanonicalOneForm::usage =
  "InfraCanonicalOneForm[g, {x, v}, {y, w}] is the canonical (tautological) 1-form of the double tangent bundle evaluated on the germ {x, v} -> {y, w}: the pairing <v, y>_x = (d(x,v)^2 + d(x,y)^2 - d(v,y)^2)/2 of the fiber direction v with the base displacement x -> y. It vanishes on vertical germs (y == x), equals r d(x, y) on the geodesic spray, and is independent of w and of any connection. InfraCanonicalOneForm[g, total] tabulates it over the directed edges of a tangent-bundle total graph (vertices {base, fiberPoint}) as an Association.";

PackageScope["infraScalarProductValue"]
PackageScope["arcAngleMatrix"]
PackageScope["germAngleFunction"]
PackageScope["leviCivitaEdgeData"]
PackageScope["leviCivitaWalkComposites"]
PackageScope["holonomyAngleValue"]

InfraParallelTransport::cap =
  "The Cartesian of per-edge realisations has `1` elements, exceeding the cap `2`; each per-edge list was truncated. The returned holonomy is a bound over the truncated set, not the exact optimum. Raise \"MaxRealizations\" or use a finer radius to separate angles.";
FindInfraLeviCivitaConnection::badmethod = "Unknown Method `1`; only \"Metric\" is implemented.";

(* ===================== Metric-native Levi-Civita transport ===================== *)

(* Per-edge parallel transport tau_{p->q}: S_r(p) -> S_r(q), the angle-isometry sending *)
(* the radial germs toward q to their straightest continuations. Realised by matching the *)
(* pairwise-angle matrices of the two direction spheres (metric compatibility) with the *)
(* whole radial cone through q pinned germ-by-germ (geodesics auto-parallel); at r = 1 *)
(* the cone is the single bond germ. Two angle structures: "Arclength" (default) -- *)
(* pairwise distance in the graph with the open ball punched out, / r, InfraAngle's *)
(* synthetic radian measure -- and "Alexandrov" -- the kappa=0 polar-form Gram. The *)
(* residual fiber symmetry is returned as multi-realisation. Output: list of *)
(* Associations u -> tau[u] (one per realisation), source vertices with no match omitted. *)

Options[InfraParallelTransport] = {"Angle" -> "Arclength", "MaxRealizations" -> 200000};

InfraParallelTransport[g_Graph, p_, q_, r_Integer, OptionsPattern[]] :=
  leviCivitaEdgeData[g, p, q, r, OptionValue["Angle"]]["transports"];

(* Composite transport along a walk v0 -> v1 -> ... -> vk: the multi-realisation set of *)
(* maps S_r(v0) -> S_r(vk), one per choice of per-edge realisation (Cartesian, deduped). *)

InfraParallelTransport[g_Graph, walk_List, r_Integer, OptionsPattern[]] :=
  DeleteDuplicates @ leviCivitaWalkComposites[g, walk, r, OptionValue["Angle"], OptionValue["MaxRealizations"]];

(* Levi-Civita defect of the edge: the pairwise-angle mismatch of the best transport; *)
(* 0 iff an exact angle-isometry exists (regular/vertex-transitive case), > 0 when the *)
(* fibers are incongruent (deg p != deg q) -- the obstruction, not a crash. *)

Options[InfraLeviCivitaDefect] = {"Angle" -> "Arclength"};

InfraLeviCivitaDefect[g_Graph, p_, q_, r_Integer, OptionsPattern[]] :=
  leviCivitaEdgeData[g, p, q, r, OptionValue["Angle"]]["defect"];

(* ===================== Holonomy ===================== *)

(* Holonomy of a closed walk = self-map of S_r(base) by composing the per-edge transports *)
(* around the loop. The metric leaves a residual fiber symmetry, so the holonomy is the *)
(* minimal-rotation realisation over all per-edge choices -- the irreducible (gauge- *)
(* invariant) part: identity iff the loop bounds a flat region, a nontrivial rotation *)
(* whose angle measures the enclosed curvature. *)

Options[InfraHolonomy] = {"Angle" -> "Arclength", "MaxRealizations" -> 200000};

InfraHolonomy[g_Graph, loop_List, r_Integer, OptionsPattern[]] :=
  With[{angleFn = germAngleFunction[g, First[loop], r, OptionValue["Angle"]]},
    First @ MinimalBy[
      leviCivitaWalkComposites[g, loop, r, OptionValue["Angle"], OptionValue["MaxRealizations"]],
      holonomyAngleValue[angleFn, #] &]
  ];

(* Rotation angle of the holonomy: the minimal mean direction-rotation over all *)
(* realisations. 0 iff trivial holonomy (flat); the discrete curvature 2-form value. *)

Options[InfraHolonomyAngle] = {"Angle" -> "Arclength", "MaxRealizations" -> 200000};

InfraHolonomyAngle[g_Graph, loop_List, r_Integer, OptionsPattern[]] :=
  With[{angleFn = germAngleFunction[g, First[loop], r, OptionValue["Angle"]]},
    Min[holonomyAngleValue[angleFn, #] & /@
      leviCivitaWalkComposites[g, loop, r, OptionValue["Angle"], OptionValue["MaxRealizations"]]]
  ];

(* Curvature of a face = holonomy angle around its boundary cycle. *)

Options[InfraConnectionCurvature] = {"Angle" -> "Arclength", "MaxRealizations" -> 200000};

InfraConnectionCurvature[g_Graph, face_List, r_Integer, opts : OptionsPattern[]] :=
  InfraHolonomyAngle[g, If[First[face] === Last[face], face, Append[face, First[face]]], r, opts];

(* ===================== Global connection (fibration triple) ===================== *)

(* Assemble the per-edge transports into a sphere-fibration connection feedable to *)
(* HolonomyMatrix[total, proj, connection, loop]: total/connection = the horizontal graph *)
(* on vertices {v, u}, u in S_r(v); proj{v,u} = v. One representative (First per edge); *)
(* the gauge-invariant rotation is InfraHolonomyAngle, not this connection. *)

Options[FindInfraLeviCivitaConnection] = {Method -> "Metric", "Angle" -> "Arclength"};

FindInfraLeviCivitaConnection[g_Graph, r_Integer, OptionsPattern[]] :=
  Module[{sphere, totalV, horizontal},
    If[OptionValue[Method] =!= "Metric",
      Message[FindInfraLeviCivitaConnection::badmethod, OptionValue[Method]]; Return[$Failed, Module]];
    sphere = AssociationMap[v |-> Select[VertexList[g], GraphDistance[g, v, #] == r &], VertexList[g]];
    totalV = Catenate[(v |-> ({v, #} & /@ sphere[v])) /@ VertexList[g]];
    horizontal = Graph[totalV, Catenate[
      (e |-> KeyValueMap[{u, w} |-> UndirectedEdge[{e[[1]], u}, {e[[2]], w}],
         First @ InfraParallelTransport[g, e[[1]], e[[2]], r, "Angle" -> OptionValue["Angle"]]]) /@ EdgeList[g]]];
    <|"TotalGraph" -> horizontal, "Projection" -> AssociationThread[totalV, totalV[[All, 1]]],
      "Connection" -> horizontal|>
  ];

(* ===================== Parallel frame transport ===================== *)

(* Transport an orthogonal frame (a tuple of germ directions at the first walk vertex) *)
(* along the walk: apply each composite transport to every frame germ.  Along a geodesic *)
(* the tangent germ goes to its straight continuation (auto-parallel); around a loop the *)
(* transported frame differs from frame0 by the holonomy.  Returns one frame per per-edge *)
(* realisation (deduped); frame germs with no image are dropped. *)

Options[FindInfraParallelFrame] = {"Angle" -> "Arclength", "MaxRealizations" -> 200000};

FindInfraParallelFrame[g_Graph, walk_List, frame0_List, r_Integer, OptionsPattern[]] :=
  DeleteDuplicates[
    (comp |-> DeleteMissing @ Lookup[comp, frame0]) /@
      leviCivitaWalkComposites[g, walk, r, OptionValue["Angle"], OptionValue["MaxRealizations"]]
  ];

(* ===================== Canonical 1-form of the double tangent bundle ===================== *)

(* theta_{(x,v)}(Xi) = <v, dpi(Xi)>_x: the kappa=0 polar pairing of the fiber germ v with *)
(* the base displacement x -> y of the double-tangent germ Xi = ({x,v} -> {y,w}). The *)
(* discrete pullback of the Liouville form under the metric; connection-independent, *)
(* vanishing on vertical germs, equal to r d(x,y) on the geodesic spray. *)

InfraCanonicalOneForm[g_Graph, {x_, v_}, {y_, w_}] :=
  Module[{ix, dd},
    {ix, dd} = graphMetricData[g];
    infraScalarProductValue[dd, ix, x, v, y]
  ];

(* Global form: theta on every directed edge of a tangent-bundle total graph (both *)
(* orientations of each undirected edge), e.g. FindInfraLeviCivitaConnection[g, r] *)
(* ["TotalGraph"] or First @ GraphTangentBundle[g]. The scale rides on the fiber points. *)

InfraCanonicalOneForm[g_Graph, total_Graph] :=
  Module[{ix, dd},
    {ix, dd} = graphMetricData[g];
    Association @ Catenate[
      (e |-> {
        DirectedEdge[e[[1]], e[[2]]] -> infraScalarProductValue[dd, ix, e[[1, 1]], e[[1, 2]], e[[2, 1]]],
        DirectedEdge[e[[2]], e[[1]]] -> infraScalarProductValue[dd, ix, e[[2, 1]], e[[2, 2]], e[[1, 1]]]
      }) /@ EdgeList[total]]
  ];

(* ===================== Walk composition & angle helpers ===================== *)

(* Composite maps S_r(first walk vertex) -> S_r(last) for every per-edge realisation choice. *)
(* Composition threads each base germ through the chain; a germ with no image is dropped *)
(* (Missing). The Cartesian is capped at maxReals (truncating each per-edge list, with a *)
(* message) so high-symmetry fibers (e.g. the r=1 cubic lattice) stay tractable. *)

leviCivitaWalkComposites[g_Graph, walk_List, r_, angle_, maxReals_] :=
  Module[{sphere, taus, prod, used},
    sphere = Select[VertexList[g], GraphDistance[g, First[walk], #] == r &];
    taus = (e |-> InfraParallelTransport[g, e[[1]], e[[2]], r, "Angle" -> angle]) /@ Partition[walk, 2, 1];
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

(* Mean direction-rotation angle of a holonomy self-map: average of angleFn[u, hol[u]] *)
(* over moved germs (dropped germs ignored); 0 when the map is the identity. *)

holonomyAngleValue[angleFn_, hol_] :=
  With[{moved = Select[Keys[hol], hol[#] =!= # && Not @ MissingQ[hol[#]] &]},
    If[moved === {}, 0, N @ Mean[(a |-> angleFn[a, hol[a]]) /@ moved]]
  ];

(* Pairwise germ angle at base, per angle structure: "Arclength" indexes the punched- *)
(* graph arc matrix of S_r(base); "Alexandrov" is the arccos'd normalised polar form. *)

germAngleFunction[g_Graph, base_, r_, angle_] :=
  Module[{ix, dd},
    {ix, dd} = graphMetricData[g];
    Switch[angle,
      "Arclength",
      With[{sphere = Select[VertexList[g], dd[[ix[base], ix[#]]] == r &]},
        {mat = arcAngleMatrix[g, dd, ix, base, sphere, r],
         pos = AssociationThread[sphere, Range @ Length[sphere]]},
        {u, w} |-> mat[[pos[u], pos[w]]]],
      "Alexandrov",
      {u, w} |-> ArcCos[Clip[
        infraScalarProductValue[dd, ix, base, u, w] /
          (dd[[ix[base], ix[u]]] dd[[ix[base], ix[w]]]), {-1, 1}]]]
  ];

(* ===================== Shared edge computation ===================== *)

(* InfraScalarProduct[g, o, a, b] (Method -> Alexandrov, Curvature -> 0): the kappa=0 *)
(* polar form of the squared metric. Exact-rational; inlined here so the module is *)
(* standalone (no cross-paclet dependency on WolframInstitute`SyntheticInfrageometry`). *)
infraScalarProductValue[dd_, ix_, o_, a_, b_] :=
  (dd[[ix[o], ix[a]]]^2 + dd[[ix[o], ix[b]]]^2 - dd[[ix[a], ix[b]]]^2)/2;

(* Vertex index and distance matrix, memoized per graph. *)
graphMetricData[g_Graph] := graphMetricData[g] =
  {AssociationThread[VertexList[g], Range @ VertexCount[g]], GraphDistanceMatrix[g]};

(* Arc angle structure on S_r(c): pairwise shortest-path distance in the graph with the *)
(* open ball B_{<r}(c) deleted, / r -- InfraAngle's synthetic radian measure. Pairs *)
(* disconnected in the punched graph get a common finite sentinel (2 |V|), so that *)
(* Infinity must match Infinity in the isometry matching. *)

arcAngleMatrix[g_, dd_, ix_, c_, pts_, r_] :=
  With[{punched = VertexDelete[g, Select[VertexList[g], dd[[ix[c], ix[#]]] < r &]]},
    (Outer[GraphDistance[punched, #1, #2] &, pts, pts, 1] /. Infinity -> 2 VertexCount[g]) / r
  ];

(* Pins {sourceIndex, targetIndices} for the radial cone through q: germs u in S_r(p) *)
(* with d(p, u) = d(p, q) + d(q, u). The continuations of u are the germs w in S_r(q) *)
(* extending the ray q -> u (d(u, w) = d(p, q)), chordally farthest from p, arc-farthest *)
(* from the backward cone under "Arclength". At r = 1: u = q, every w is a ray germ, and *)
(* the backward cone is {p} -- exactly the previous single bond-germ criterion. *)

radialPins[dd_, ix_, p_, q_, r_, sphereP_List, sphereQ_List, gramQ_, angle_] :=
  Module[{s = dd[[ix[p], ix[q]]], backIdx, continuations},
    backIdx = Select[Range[Length[sphereQ]], dd[[ix[p], ix[sphereQ[[#]]]]] == r - s &];
    continuations = iu |-> Module[{cand},
      cand = Select[Range[Length[sphereQ]], dd[[ix[sphereP[[iu]]], ix[sphereQ[[#]]]]] == s &];
      If[cand =!= {}, cand = MaximalBy[cand, dd[[ix[p], ix[sphereQ[[#]]]]] &]];
      If[cand =!= {} && angle === "Arclength" && backIdx =!= {},
        cand = MaximalBy[cand, iw |-> Total @ gramQ[[iw, backIdx]]]];
      cand];
    DeleteCases[
      ({#, continuations[#]} &) /@
        Select[Range[Length[sphereP]], dd[[ix[q], ix[sphereP[[#]]]]] == r - s &],
      {_, {}}]
  ];

(* Injections Range[loLen] -> Range[hiLen] consistent with the pins, generated directly: *)
(* one partial assignment per choice of pin targets, completed by all injections of the *)
(* free positions into the free values. Non-transposed pins fix domain position pin[[1]] *)
(* to a value in pin[[2]]; transposed pins require value pin[[1]] at a position in pin[[2]]. *)

pinnedMatchings[loLen_, hiLen_, pins_List, transposed_] :=
  Module[{partials},
    partials = Tuples[
      If[transposed,
        (pin |-> ({#, pin[[1]]} & /@ pin[[2]])) /@ pins,
        (pin |-> ({pin[[1]], #} & /@ pin[[2]])) /@ pins]];
    partials = Select[partials, DuplicateFreeQ[#[[All, 1]]] && DuplicateFreeQ[#[[All, 2]]] &];
    Catenate[completeMatchings[loLen, hiLen, #] & /@ partials]
  ];

completeMatchings[loLen_, hiLen_, partial_List] :=
  Module[{posFixed = partial[[All, 1]], valFixed = partial[[All, 2]], freePos, freeVals},
    freePos = Complement[Range[loLen], posFixed];
    freeVals = Complement[Range[hiLen], valFixed];
    Map[
      perm |-> Module[{m = ConstantArray[0, loLen]},
        If[posFixed =!= {}, m[[posFixed]] = valFixed];
        If[freePos =!= {}, m[[freePos]] = perm];
        m],
      Permutations[freeVals, {Length[freePos]}]]
  ];

leviCivitaEdgeData[g_Graph, p_, q_, r_, angle_] := leviCivitaEdgeData[g, p, q, r, angle] =
  Module[{vs = VertexList[g], ix, dd, sphereP, sphereQ, gramP, gramQ, bondPos, straightIdx, transposed,
    loGram, hiGram, hiLen, loLen, pins, matchings, score, best, reps},
    {ix, dd} = graphMetricData[g];
    sphereP = Select[vs, dd[[ix[p], ix[#]]] == r &];
    sphereQ = Select[vs, dd[[ix[q], ix[#]]] == r &];
    {gramP, gramQ} = Switch[angle,
      "Arclength",
      {arcAngleMatrix[g, dd, ix, p, sphereP, r], arcAngleMatrix[g, dd, ix, q, sphereQ, r]},
      "Alexandrov",
      {Outer[infraScalarProductValue[dd, ix, p, #1, #2] &, sphereP, sphereP, 1],
       Outer[infraScalarProductValue[dd, ix, q, #1, #2] &, sphereQ, sphereQ, 1]}];
    transposed = Length[sphereP] > Length[sphereQ];
    {loGram, hiGram} = If[transposed, {gramQ, gramP}, {gramP, gramQ}];
    {loLen, hiLen} = {Length[loGram], Length[hiGram]};
    (* Radial-cone pin: every germ of S_r(p) whose geodesic passes through q is pinned to *)
    (* its own straightest continuations. At r = 1 the cone is the single bond germ and *)
    (* this is the previous behaviour; at r >= 2 it excludes the spurious sphere rotations *)
    (* that a single ambiguously-chosen bond germ admits (e.g. the square lattice). Only *)
    (* pin-consistent injections are generated (never the full permutation list). *)
    pins = radialPins[dd, ix, p, q, r, sphereP, sphereQ, gramQ, angle];
    matchings = If[pins === {}, {}, pinnedMatchings[loLen, hiLen, pins, transposed]];
    (* Fallback to the single bond-germ pin when the cone pin is empty or unsatisfiable *)
    (* (incongruent fibers, boundary-clipped spheres, d(p, q) > r). *)
    If[pins === {} || matchings === {},
      bondPos = First @ FirstPosition[sphereP, First @ MinimalBy[sphereP, dd[[ix[q], ix[#]]] &]];
      straightIdx = MinimalBy[Range[Length[sphereQ]], infraScalarProductValue[dd, ix, q, sphereQ[[#]], p] &];
      (* C2 tiebreak under "Arclength": among tied metric extensions, the straightest *)
      (* continuation is the arc-farthest germ from the backward germ -- disambiguates *)
      (* bent geodesic endpoints the chordal metric cannot (e.g. the triangular lattice). *)
      If[angle === "Arclength",
        With[{backPos = First @ FirstPosition[sphereQ, First @ MinimalBy[sphereQ, dd[[ix[p], ix[#]]] &]]},
          straightIdx = MaximalBy[straightIdx, gramQ[[backPos, #]] &]]];
      matchings = pinnedMatchings[loLen, hiLen, {{bondPos, straightIdx}}, transposed]];
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
