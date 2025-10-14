# Adding a comment here to stop autodoc from throwing an error.
InstallMethod(LocalActionDiagramToUniversalGroup, [IsLocalActionDiagram],
function(lad)
	local degree;

	# Add the edge labels together to get the degree of the tree. 
	degree := Sum(List(LocalActionDiagramEdgeLabels(lad), x -> Size(x)));
	return LocalAction(degree, 1, LocalActionDiagramVertexLabels(lad)[1]);
end);
