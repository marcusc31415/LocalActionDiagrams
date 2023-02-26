#
# LocalActionDiagrams: Super awesome package for local action diagrams.
#
# This file runs package tests. It is also referenced in the package
# metadata in PackageInfo.g.
#
LoadPackage( "LocalActionDiagrams" );

TestDirectory(DirectoriesPackageLibrary( "LocalActionDiagrams", "tst" ),
  rec(exitGAP := true));

FORCE_QUIT_GAP(1); # if we ever get here, there was an error
