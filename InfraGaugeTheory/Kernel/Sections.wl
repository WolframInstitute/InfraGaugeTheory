Package["WolframInstitute`InfraGaugeTheory`"]

PackageExport["RandomSection"]
PackageExport["SmoothSectionQ"]
PackageExport["SmoothSectionExtension"]
PackageExport["FindZeroSection"]
PackageExport["FindCanonicalSection"]

RandomSection::usage = "RandomSection[total, proj, domain] generates a random section of a fibered graph.";
SmoothSectionQ::usage = "SmoothSectionQ[total, proj, section, domain] tests whether a section respects the fibered graph structure.";
SmoothSectionExtension::usage = "SmoothSectionExtension[total, proj] finds a smooth section from scratch. SmoothSectionExtension[total, proj, section] extends a partial section by maintaining smoothness.";

(* ========================= Sections ========================= *)

RandomSection[total_Graph, proj_Association, givenDomain : {___} : All] :=
  With[{
    domain = If[givenDomain === All, DeleteDuplicates @ Values @ proj, givenDomain],
    fibers = GroupBy[Normal @ proj, Last -> First]},
    AssociationMap[RandomChoice @ fibers[#] &, domain]
  ];

SmoothSectionQ[total_Graph, proj_Association, section_Association, givenDomain : {___} : All] :=
  With[{
    domain = If[givenDomain === All, Keys @ section, givenDomain],
    base = ReconstructBaseGraph[total, proj]},
    {totalEdges = Sort /@ Select[EdgeList @ total, edge |-> SubsetQ[domain, List @@ (proj /@ edge)]]},
    AllTrue[
      Sort /@ (EdgeList @ Subgraph[base, domain]),
      edge |-> MemberQ[totalEdges, section /@ edge]
    ]
  ];

Options[SmoothSectionExtension] = {
  "MaxShells" -> Infinity,
  "PruneSampleSize" -> Infinity,
  "PruneShellExtensions" -> Infinity,
  "MaxRecursiveBranches" -> Infinity
};

SmoothSectionExtension[total_Graph, proj_Association, opts : OptionsPattern[]] :=
  SmoothSectionExtension[total, proj, <||>, opts];

SmoothSectionExtension[total_Graph, proj_Association, section_Association, opts : OptionsPattern[]] :=
  Module[{base, fibers, maxShells, maxExtensions, maxBranches,
    originalDomain, totalEdges, validChoices, pruneChoices,
    extendToShell, recurseShells, extendWithinShell, bestSoFar},
    maxShells = OptionValue["MaxShells"];
    maxExtensions = OptionValue["PruneShellExtensions"];
    maxBranches = OptionValue["MaxRecursiveBranches"];
    bestSoFar = section;
    pruneChoices =
      With[{size = OptionValue["PruneSampleSize"]},
        If[size === Infinity, Identity,
          choices |-> If[Length @ choices <= size, choices, RandomSample[choices, size]]]];
    base = ReconstructBaseGraph[total, proj];
    fibers = GroupBy[Normal @ proj, Last -> First];
    originalDomain = Keys @ section;
    totalEdges = Map[Sort, EdgeList @ total];
    validChoices[v_, partial_] :=
      With[{adjacent = Map[AdjacencyList[total, #] &, KeyTake[partial, AdjacencyList[base, v]]]},
        Select[fibers[v], fv |-> AllTrue[Keys @ adjacent, u |-> MemberQ[adjacent[u], fv]]]];
    recurseShells[partial_, shellNum_] := (
      If[Length @ partial > Length @ bestSoFar, bestSoFar = partial];
      If[shellNum > maxShells, partial,
        With[{extensions = extendToShell[partial, shellNum]},
          If[Length @ extensions == 0, partial,
            With[{validResults =
              Select[
                Map[ext |-> recurseShells[ext, shellNum + 1],
                  If[maxBranches === Infinity, extensions,
                    RandomSample[extensions, Min[maxBranches, Length @ extensions]]]],
                AssociationQ]},
              If[Length @ validResults == 0, partial,
                First @ MaximalBy[validResults, Length]]]]]]);
    extendToShell[partial_, shellNum_] :=
      With[{shell =
        RandomSample @ Complement[
          If[Length @ originalDomain == 0 && shellNum == 1,
            VertexList[base],
            VertexOutComponent[base, originalDomain, shellNum]],
          Keys @ partial]},
        If[Length @ shell == 0, {}, extendWithinShell[partial, shell, 0]]];
    extendWithinShell[partial_, {}, _] := {partial};
    extendWithinShell[partial_, _, count_] /;
      maxExtensions =!= Infinity && count >= maxExtensions := {};
    extendWithinShell[partial_, {v_, rest___}, count_] :=
      Module[{choices, accumulated},
        choices = RandomSample @ pruneChoices @ validChoices[v, partial];
        If[Length @ choices == 0, {},
          accumulated = {};
          Do[
            If[maxExtensions === Infinity || Length @ accumulated < maxExtensions,
              accumulated = Join[accumulated,
                extendWithinShell[Append[partial, v -> choice], {rest}, Length @ accumulated]]],
            {choice, choices}];
          accumulated]];
    recurseShells[section, 1];
    bestSoFar
  ];

(* ========================= FindZeroSection ========================= *)

FindZeroSection[ total_Graph, proj_Association ] :=
  Module[ { baseVertices, fibers, section },
    baseVertices = DeleteDuplicates @ Values @ proj;
    fibers = GroupBy[ Normal @ proj, Last -> First ];
    section = AssociationMap[
      baseVertex |-> FirstCase[ fibers[ baseVertex ], { baseVertex, baseVertex }, Missing[ "NoZero" ] ],
      baseVertices
    ];
    If[ AnyTrue[ section, MissingQ ], $Failed, section ]
  ]

(* ========================= FindCanonicalSection ========================= *)

FindCanonicalSection[ total_Graph, proj_Association ] :=
  Module[ { ttg, ttgVertexSet, section },
    If[ !AllTrue[ VertexList[ total ], ListQ ], Return[ $Failed, Module ] ];
    ttg = First @ GraphTangentBundle[ total ];
    ttgVertexSet = Association @ Thread[ VertexList[ ttg ] -> True ];
    section = AssociationMap[
      vertex |-> With[ { canonical = { vertex, Reverse[ vertex ] } },
        If[ TrueQ @ ttgVertexSet[ canonical ], canonical, Missing[ "NoCanonical" ] ]
      ],
      VertexList[ total ]
    ];
    If[ AnyTrue[ section, MissingQ ], $Failed, section ]
  ]
