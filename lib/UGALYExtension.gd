#! @Chapter Extensions
#! @Section UGALY
#!
#! The package <Package>UGALY</Package> implements Tornier's generalisation of the Burger-Mozes universal group to <M>k</M>-closures. In the case when <M>k = 1</M> this corresponds to the original Burger-Mozes universal group. It represents these groups as objects in the category <C>IsLocalAction</C>. 
#!
#! As objects in the category <C>IsLocalAction</C> are also in the category <C>IsPermGroup</C> they can be directly used in <Ref Func="LocalActionDiagramFromBurgerMozesUniversalGroup"/>. We provide a function to convert a local action diagram representing a Burger-Mozes group to a local action object from <Package>UGALY</Package>. 

#! @Returns A local action. 
#! @Arguments lad
#! @Label 
#! @Description
#! Given a local action diagram <A>lad</A> corresponding to a Burger-Mozes group this returns the corresponding group as an object of the category <C>IsLocalAction</C> from the package <Package>UGALY</Package>. The local action diagram <A>lad</A> must have a single vertex and have only self-reverse edges, and must also have at least one edge (see <Ref Func="LocalActionDiagramIsBurgerMozesUniversalGroup"/>. 
DeclareOperation("LocalActionDiagramToUGALY", [IsLocalActionDiagram]);

#! @BeginLogSession
#! gap> lad := LocalActionDiagramFromBurgerMozesUniversalGroup(Group((1, 2), \ 
#!                                                                   (3,4)));
#! <U(Group( [ (1,2), (3,4) ] )) (as a Local Action Diagram)>
#! gap> group := LocalActionDiagramToUGALY(lad);
#! Group([ (1,2), (3,4) ])
#! gap> LocalActionDegree(group);
#! 4
#! gap> LocalActionRadius(group);
#! 1
#! gap> LocalActionDiagramFromBurgerMozesUniversalGroup(group);
#! <U(Group( [ (1,2), (3,4) ] )) (as a Local Action Diagram)>
#! @EndLogSession
