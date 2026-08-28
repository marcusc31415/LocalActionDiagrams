InstallMethod(LocalActionDiagramToUGALY, [IsLocalActionDiagram],
function(lad)
	local degree, arc_labels, arc_label_list;

	arc_labels := LocalActionDiagramArcLabels(lad);

	arc_label_list := List(RecNames(arc_labels), x -> arc_labels.(x));

	# Add the edge labels together to get the degree of the tree. 
	degree := Sum(List(arc_label_list, Size));
	return LocalAction(degree, 1, LocalActionDiagramVertexLabels(lad).1);
end);
