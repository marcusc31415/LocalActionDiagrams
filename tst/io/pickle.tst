# Testing all possible errors leading to an invalid local action diagram construction. 
gap> START_TEST("pickle.tst");

#
# Pickle a local action diagram. 
gap> s_1 := "";;
gap> f := IO_WrapFD(-1, false, s_1);; # Writes the output to the string s. 
gap> lad_1 := LocalActionDiagramFromBurgerMozesUniversalGroup(Group((1,2,3)));;
gap> IO_Pickle(f, lad_1);
IO_OK
gap> IO_Close(f);;
gap> s_2 := "";;
gap> f := IO_WrapFD(-1, false, s_2);; # Writes the output to the string s. 
gap> lad_2 := LocalActionDiagramFromBurgerMozesUniversalGroup(Group((1,2,3)));;
gap> LocalActionDiagramIsDiscrete(lad_2);;
gap> IO_Pickle(f, lad_2);
IO_OK
gap> IO_Close(f);;

# 
# Read from pickled strings. 
gap> f := IO_WrapFD(-1, s_1, false);;
gap> lad_1_r := IO_Unpickle(f);;
gap> IO_Close(f);;
gap> f := IO_WrapFD(-1, s_2, false);;
gap> lad_2_r := IO_Unpickle(f);;
gap> IO_Close(f);;

# 
# Test that they are the same.
gap> String(lad_1) = String(lad_1_r);
true
gap> "LocalActionDiagramGroupName" in KnownAttributesOfObject(lad_1_r);
true
gap> KnownPropertiesOfObject(lad_1) = KnownPropertiesOfObject(lad_1_r);
true
gap> String(lad_2) = String(lad_2_r);
true
gap> "LocalActionDiagramGroupType" in KnownAttributesOfObject(lad_2_r);
true
gap> "LocalActionDiagramScopos" in KnownAttributesOfObject(lad_2_r);
true
gap> "LocalActionDiagramGroupName" in KnownAttributesOfObject(lad_2_r);
true
gap> KnownPropertiesOfObject(lad_2) = KnownPropertiesOfObject(lad_2_r);
true

#
gap> STOP_TEST("pickle.tst");
