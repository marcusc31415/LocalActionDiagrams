LAD_AvailableData@ := [];

# The files are stored as "{degree}_{no_verts}_library.txt".
# This stores a list of available ones in memory in the form
# [[degree, no_verts], ...]. 

for LAD_filename@ in DirectoryContents(DirectoriesPackageLibrary("localactiondiagrams", "data")[1]) do
	if LAD_filename@ = "." or LAD_filename@ = ".." then
		continue;
	fi;
	Add(LAD_AvailableData@, [Int(LAD_filename@{[1]}), Int(LAD_filename@{[3]})]);
od;

LAD_AvailableData@ := Set(LAD_AvailableData@);

# Make the LAD_AvailableData@ variable constant and
# immutable so the sublists can't be changed. 
MakeImmutable(LAD_AvailableData@);
MakeConstantGlobal("LAD_AvailableData@");
Unbind(LAD_filename@);

BindGlobal("LAD_RSGraphsRecord@", rec());
BindGlobal("LAD_LocalActionDiagramsRecord@", rec());

DeclareOperation("AllRSGraphs", [IsInt, IsInt]);

DeclareOperation("AllRSGraphs", [IsInt, IsInt, IsInt]);

DeclareOperation("AllLocalActionDiagrams", [IsInt, IsInt]);

DeclareOperation("AllLocalActionDiagrams", [IsInt, IsInt, IsInt]);

