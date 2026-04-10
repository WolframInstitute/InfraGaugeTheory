#!/usr/bin/env wolframscript

(* build_paclet.wls - Build paclet archive and install locally *)

root = ParentDirectory[ DirectoryName[ ExpandFileName[ $ScriptCommandLine[[ 1 ]] ] ] ];
pacletDir = FileNameJoin[ { root, "InfraGaugeTheory" } ];

(* Copy only paclet-relevant files to a temp directory *)
tmpDir = FileNameJoin[ { $TemporaryDirectory, "InfraGaugeTheory-build" } ];
If[ DirectoryQ[ tmpDir ], DeleteDirectory[ tmpDir, DeleteContents -> True ] ];
CreateDirectory[ tmpDir ];
CopyFile[ FileNameJoin[ { pacletDir, "PacletInfo.wl" } ], FileNameJoin[ { tmpDir, "PacletInfo.wl" } ] ];
CopyDirectory[ FileNameJoin[ { pacletDir, "Kernel" } ], FileNameJoin[ { tmpDir, "Kernel" } ] ];
If[ DirectoryQ[ FileNameJoin[ { pacletDir, "Documentation" } ] ],
  CopyDirectory[ FileNameJoin[ { pacletDir, "Documentation" } ], FileNameJoin[ { tmpDir, "Documentation" } ] ] ];
If[ DirectoryQ[ FileNameJoin[ { pacletDir, "Tests" } ] ],
  CopyDirectory[ FileNameJoin[ { pacletDir, "Tests" } ], FileNameJoin[ { tmpDir, "Tests" } ] ] ];

Print[ "Building paclet archive..." ];
pacletFile = CreatePacletArchive[ tmpDir ];
Print[ "  Created: ", pacletFile ];

DeleteDirectory[ tmpDir, DeleteContents -> True ];

Print[ "Installing locally..." ];
paclet = PacletInstall[ pacletFile, ForceVersionInstall -> True ];
Print[ "  Installed: ", paclet[ "Name" ], " v", paclet[ "Version" ] ];
