VerificationTest[
    Needs["WolframInstitute`InfraGaugeTheory`"];
    True,
    True,
    TestID -> "PackageLoads"
]

VerificationTest[
    NameQ["WolframInstitute`InfraGaugeTheory`RandomFiberedGraph"],
    True,
    TestID -> "RandomGraphFibrationDefined"
]
