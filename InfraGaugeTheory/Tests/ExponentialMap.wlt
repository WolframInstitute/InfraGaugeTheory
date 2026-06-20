(* ========================= ExponentialCoordinates ========================= *)

VerificationTest[
    coords = ExponentialCoordinates[ PathGraph[ { 1, 2, 3 } ], 1 ];
    { AssociationQ[ coords ], Length[ coords ] },
    { True, 3 },
    TestID -> "ExponentialCoordinates-PathGraph-BasicOutput"
]

VerificationTest[
    coords = ExponentialCoordinates[ CycleGraph[ 6 ], 1 ];
    coords[ 1 ],
    0,
    TestID -> "ExponentialCoordinates-BasePointIsZero"
]

VerificationTest[
    coords = ExponentialCoordinates[ PathGraph[ { 1, 2, 3, 4 } ], 1 ];
    Length[ coords ] === 4 && AllTrue[ Values[ coords ], # === 0 || ! FreeQ[ #, DirectedEdge ] & ],
    True,
    TestID -> "ExponentialCoordinates-WellFormedVectors"
]

VerificationTest[
    coords = ExponentialCoordinates[ CycleGraph[ 6 ], 1, "Radius" -> 1 ];
    Length[ coords ],
    3,
    TestID -> "ExponentialCoordinates-RadiusCutoff"
]

VerificationTest[
    c = ExponentialCoordinates[ PathGraph[ { 1, 2, 3 } ], 1, 2 ];
    c === 0 || ! FreeQ[ c, DirectedEdge ],
    True,
    TestID -> "ExponentialCoordinates-SingleTarget"
]

VerificationTest[
    ExponentialCoordinates[ PathGraph[ { 1, 2, 3 } ], 1, 1 ],
    0,
    TestID -> "ExponentialCoordinates-SingleTarget-BasePoint"
]

(* ========================= InjectivityRadius ========================= *)

VerificationTest[
    InjectivityRadius[ PathGraph[ Range[ 5 ] ], 1 ],
    4,
    TestID -> "InjectivityRadius-PathGraph-Endpoint"
]

VerificationTest[
    InjectivityRadius[ CompleteGraph[ 5 ], 1 ],
    1,
    TestID -> "InjectivityRadius-CompleteGraph"
]

VerificationTest[
    r = InjectivityRadius[ CycleGraph[ 6 ], 1 ];
    IntegerQ[ r ] && r >= 1,
    True,
    TestID -> "InjectivityRadius-CycleGraph-Positive"
]

VerificationTest[
    InjectivityRadius[ PathGraph[ Range[ 10 ] ], 5 ] >= 4,
    True,
    TestID -> "InjectivityRadius-PathGraph-Interior"
]
