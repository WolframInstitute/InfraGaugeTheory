Package["WolframInstitute`InfraGaugeTheory`"]

PackageExport["GraphCotangentBundle"]
PackageExport["CotangentFlip"]
PackageExport["CotangentFlipQ"]
PackageExport["InfraTautologicalSection"]
PackageExport["InfraTautologicalOneForm"]

GraphCotangentBundle::usage =
  "GraphCotangentBundle[g] is the radius-r cotangent graph of g as a fibered graph {total, projection}. A covector at p is a germ {p, v} with v in the geodesic graph at p, the same datum as a tangent vector; the two bundles therefore carry the same construction, and the identification is CotangentFlip. Options \"Radius\", Method, \"Fiber\" as for GraphTangentBundle; \"Fiber\" -> \"Sphere\" with \"Radius\" -> 1 gives the model in which a covector is a directed edge of g.";

CotangentFlip::usage =
  "CotangentFlip[g] is the germ flip {p, v} -> {v, p} of the radius-r cotangent graph of g, as an Association on its vertices; germs whose flip is not a vertex are omitted. It is an involution, and it is the identification of the tangent with the cotangent bundle exactly when it is a graph automorphism -- see CotangentFlipQ. Same options as GraphCotangentBundle.";

CotangentFlipQ::usage =
  "CotangentFlipQ[g] is True when CotangentFlip[g] is a total involutive graph automorphism of the cotangent graph of g. Same options as GraphCotangentBundle.";

InfraTautologicalSection::usage =
  "InfraTautologicalSection[g] is the tautological (canonical) 1-form of g as a section of the double cotangent graph: the germ {s, t} of the cotangent graph is sent to the germ {{s, t}, {t, s}} of the cotangent graph of the cotangent graph, whose second component is the flip of the first. It is a section of the projection T*T*G -> T*G, and it is the unique choice using no data beyond s and t. Returns $Failed if some image is not a vertex. Same options as GraphCotangentBundle.";

InfraTautologicalOneForm::usage =
  "InfraTautologicalOneForm[g] evaluates InfraCanonicalOneForm on the germs of InfraTautologicalSection[g], as an Association germ -> value. It is the pairing of a covector with its own direction, and equals d(s, t)^2 in the sphere model. Same options as GraphCotangentBundle.";

InfraTautologicalSection::nosection =
  "The flip germ of `1` is not a vertex of the double cotangent graph; no tautological section exists for this Method.";

(* ========================= The cotangent graph ========================= *)

(* At the infra scale a covector at p is not dual data: the only thing a germ can
   be paired against is another germ, and the pairing is the metric polar form
   (InfraCanonicalOneForm). So the cotangent graph is built exactly as the
   tangent graph, and the content of the duality sits in the flip below. *)

Options[ GraphCotangentBundle ] = Options[ GraphTangentBundle ];

GraphCotangentBundle[ g_Graph, opts : OptionsPattern[] ] :=
  GraphTangentBundle[ g, opts ];

(* ========================= The flip ========================= *)

(* {p, v} -> {v, p}. On the ball model it fixes every zero germ {p, p}; on the
   sphere model at radius 1 it is the reversal of a directed edge. It is defined
   on the whole vertex set whenever the fiber relation is symmetric, which the
   ball and sphere models both are. *)

Options[ CotangentFlip ] = Options[ GraphTangentBundle ];

CotangentFlip[ g_Graph, opts : OptionsPattern[] ] :=
  CotangentFlip @ GraphCotangentBundle[ g, opts ];

CotangentFlip[ { total_Graph, _Association } ] :=
  CotangentFlip @ { total };

CotangentFlip[ { total_Graph } ] :=
  With[ { vertices = Select[ VertexList[ total ], MatchQ[ #, { _, _ } ] & ] },
    With[ { present = Association @ Thread[ vertices -> True ] },
      Association @ Map[
        vertex |-> If[ TrueQ @ Lookup[ present, Key @ Reverse @ vertex, False ], vertex -> Reverse @ vertex, Nothing ],
        vertices ] ] ];

Options[ CotangentFlipQ ] = Options[ GraphTangentBundle ];

CotangentFlipQ[ g_Graph, opts : OptionsPattern[] ] :=
  CotangentFlipQ @ GraphCotangentBundle[ g, opts ];

CotangentFlipQ[ { total_Graph, _Association } ] :=
  CotangentFlipQ @ { total };

CotangentFlipQ[ { total_Graph } ] :=
  Module[ { flip, edges },
    flip = CotangentFlip @ { total };
    If[ Length @ flip =!= VertexCount[ total ], Return[ False, Module ] ];
    edges = Sort /@ ( List @@@ EdgeList[ total ] );
    Sort[ edges ] === Sort[ Sort /@ Map[ Lookup[ flip, Key @ # ] &, edges, { 2 } ] ]
  ];

(* ========================= The tautological 1-form ========================= *)

(* theta sends the point {s, t} of T*G to the germ of T*T*G whose base is {s, t}
   -- so theta is a section -- and whose fiber component is {t, s}. The base of
   the fiber component is forced: it must be a neighbour of s realising the
   direction t, hence t. Only the second slot is free, and {t, s} is the one
   choice expressible in s and t alone. *)

Options[ InfraTautologicalSection ] = Options[ GraphTangentBundle ];

InfraTautologicalSection[ g_Graph, opts : OptionsPattern[] ] :=
  Module[ { cotangent, doubleCotangent, present, section },
    cotangent = First @ GraphCotangentBundle[ g, opts ];
    doubleCotangent = First @ GraphCotangentBundle[ cotangent, opts ];
    present = Association @ Thread[ VertexList[ doubleCotangent ] -> True ];
    section = AssociationMap[
      vertex |-> With[ { germ = { vertex, Reverse @ vertex } },
        If[ TrueQ @ Lookup[ present, Key @ germ, False ], germ, Missing[ "NoTautologicalGerm" ] ] ],
      VertexList[ cotangent ] ];
    If[ AnyTrue[ section, MissingQ ],
      Message[ InfraTautologicalSection::nosection, First @ Keys @ Select[ section, MissingQ ] ];
      Return[ $Failed, Module ] ];
    section
  ];

(* ========================= Its value ========================= *)

Options[ InfraTautologicalOneForm ] = Options[ GraphTangentBundle ];

InfraTautologicalOneForm[ g_Graph, opts : OptionsPattern[] ] :=
  With[ { section = InfraTautologicalSection[ g, opts ] },
    If[ FailureQ @ section, $Failed,
      Association @ KeyValueMap[
        Function[ { base, germ }, base -> InfraCanonicalOneForm[ g, germ[[ 1 ]], germ[[ 2 ]] ] ],
        section ] ] ];
