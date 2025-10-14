#
# LocalActionDiagrams: Super awesome package for local action diagrams.
#
# This file contains package meta data. For additional information on
# the meaning and correct usage of these fields, please consult the
# manual of the "Example" package as well as the comments in its
# PackageInfo.g file.
#

SetPackageInfo( rec(

#! @Title Local Action Diagrams
PackageName := "LocalActionDiagrams",
Subtitle := "Super awesome package for local action diagrams.",
Version := "0.1",
Date := "29/01/2023", # dd/mm/yyyy format
License := "GPL-2.0-or-later",

Persons := [
  rec(
    FirstNames := "Marcus",
    LastName := "Chijoff",
    WWWHome := "https://newcastle.edu.au",
    Email := "marcus.chijoff@uon.edu.au",
    IsAuthor := true,
    IsMaintainer := true,
    PostalAddress := "No",
    Place := "Newcastle",
    Institution := "The University of Newcastle, Australia",
  ),
],

#SourceRepository := rec( Type := "TODO", URL := "URL" ),
#IssueTrackerURL := "TODO",
PackageWWWHome := "https://newcastle.edu.au/",
PackageInfoURL := Concatenation( ~.PackageWWWHome, "PackageInfo.g" ),
README_URL     := Concatenation( ~.PackageWWWHome, "README.md" ),
ArchiveURL     := Concatenation( ~.PackageWWWHome,
                                 "/", ~.PackageName, "-", ~.Version ),

ArchiveFormats := ".tar.gz",

##  Status information. Currently the following cases are recognized:
##    "accepted"      for successfully refereed packages
##    "submitted"     for packages submitted for the refereeing
##    "deposited"     for packages for which the GAP developers agreed
##                    to distribute them with the core GAP system
##    "dev"           for development versions of packages
##    "other"         for all other packages
##
Status := "dev",

AbstractHTML   :=  "",

PackageDoc := rec(
  BookName  := "LocalActionDiagrams",
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0.html",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := "Super awesome package for local action diagrams.",
),

Dependencies := rec(
  GAP := ">= 4.11",
  NeededOtherPackages := [ ["Digraphs", "1.5.3"], ["datastructures", "0.2.7"] ],
  SuggestedOtherPackages := [ ],
  ExternalConditions := [ ],
),

Extensions := [
	rec (
		needed := [ ["ugaly", "4.1.3"] ],
		filename := "gap/ugaly_extension_read.g",
	),
],

AvailabilityTest := ReturnTrue,

TestFile := "tst/testall.g",

#Keywords := [ "TODO" ],

));


