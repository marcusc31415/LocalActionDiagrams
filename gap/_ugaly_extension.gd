#! @Chapter Introduction
#! @Chapter Creating Local Action Diagrams
#! @Section Creating Local Action Diagrams 

#! @Returns A local action. 
#! @Arguments lad
#! @Description
#! Given a local action diagram <A>lad</A> corresponding to a Burger-Mozes group this returns the corresponding group as an object of the category <C>IsLocalAction</C> from the package <Package>UGALY</Package>. The local action diagram <A>lad</A> must have a single vertex and have only self-reverse edges. It must also have at least one edge. 
DeclareOperation("LocalActionDiagramToUniversalGroup", [IsLocalActionDiagram]);
