# Family
BindGlobal("LocalActionDiagramFamily", NewFamily("LocalActionDiagramFamily", IsLocalActionDiagram));

# Type
BindGlobal("LocalActionDiagramType", NewType(LocalActionDiagramFamily, IsLocalActionDiagramRep));

InstallMethod(LocalActionDiagramConsNC, "Construction Local Action Diagram Object", [IsLocalActionDiagram, IsRecord],
function(_, lad_data)
	Assert(1, IsBound(lad_data.graph));
	Assert(1, IsBound(lad_data.vert_labels));
	Assert(1, IsBound(lad_data.arc_labels));

	return ObjectifyWithAttributes(rec(), LocalActionDiagramType,
		LocalActionDiagramRSGraph, lad_data.graph,
		LocalActionDiagramVertexLabels, lad_data.vert_labels,
		LocalActionDiagramArcLabels, lad_data.arc_labels
	);
end);

BindGlobal("LAD_LocalActionDiagramConsCheck@", 
function(graph, vert_labels, arc_labels)
	local orbits, arc, arc_label_combined, idx;
	# Check vertex label orbit condition. 
	
	orbits := rec();

	for idx in Set(RecNames(vert_labels)) do
		orbits.(idx) := Orbits(vert_labels.(idx));
	od;

	arc_label_combined := List(RecNames(vert_labels), x -> []);


	for arc in RSGraphArcIterator(graph) do
		if not arc_labels.(arc[1]) in orbits.(arc[2].origin) then
			ErrorNoReturn("Arc labels must be orbits of the group labels");
		fi;
		Add(arc_label_combined[arc[2].origin], arc_labels.(arc[1]));
	od;

	for idx in [1..Length(RecNames(vert_labels))] do
		if Set(orbits.(idx)) <> Set(arc_label_combined[idx]) then
			ErrorNoReturn("Must have an arc label for each orbit of the vertex labels.");
		fi;
	od;
end);

InstallMethod(LocalActionDiagramFromData, "Constructing Local Action Diagram (With Checks)", [IsRSGraph, IsList, IsList],
function(graph, _vert_labels, _arc_labels)
	local vert_labels, arc_labels, idx;

	if Length(_vert_labels) <> RSGraphNumberVertices(graph) then
		ErrorNoReturn("Must have a vertex label for each vertex in the graph.");
	fi;

	if Length(_arc_labels) <> Length(RSGraphArcIds(graph)) then
		ErrorNoReturn("Must have an arc label for each arc in the graph.");
	fi;

	vert_labels := rec();
	arc_labels := rec();

	for idx in [1..Length(_vert_labels)] do
		vert_labels.(RSGraphVertices(graph)[idx]) := _vert_labels[idx];
	od;

	for idx in [1..Length(_arc_labels)] do
		arc_labels.(RSGraphArcIds(graph)[idx]) := _arc_labels[idx];
	od;

	LAD_LocalActionDiagramConsCheck@(graph, vert_labels, arc_labels);

	return LocalActionDiagramFromDataNC(graph, vert_labels, arc_labels);
end);

InstallMethod(LocalActionDiagramFromData, "Constructing Local Action Diagram (With Checks)", [IsRSGraph, IsRecord, IsRecord],
function(graph, vert_labels, arc_labels)
	if Length(RecNames(vert_labels)) <> RSGraphNumberVertices(graph) then
		ErrorNoReturn("Must have a vertex label for each vertex in the graph.");
	fi;

	if Length(RecNames(arc_labels)) <> Length(RSGraphArcIds(graph)) then
		ErrorNoReturn("Must have an arc label for each arc in the graph.");
	fi;

	LAD_LocalActionDiagramConsCheck@(graph, vert_labels, arc_labels);

	return LocalActionDiagramFromDataNC(graph, vert_labels, arc_labels);
end);

InstallMethod(LocalActionDiagramFromDataNC, "Construction Local Action Diagram (Without Checks)", [IsRSGraph, IsList, IsList],
function(graph, _vert_labels, _arc_labels)
	local vert_labels, arc_labels, idx;

	vert_labels := rec();
	arc_labels := rec();

	for idx in [1..Length(_vert_labels)] do
		vert_labels.(RSGraphVertices(graph)[idx]) := _vert_labels[idx];
	od;

	for idx in [1..Length(_arc_labels)] do
		arc_labels.(RSGraphArcIds(graph)[idx]) := _arc_labels[idx];
	od;

	return LocalActionDiagramFromDataNC(graph, vert_labels, arc_labels);
end);

InstallMethod(LocalActionDiagramFromDataNC, "Construction Local Action Diagram (Without Checks)", [IsRSGraph, IsRecord, IsRecord],
function(graph, vert_labels, arc_labels)
	local lad_data;

	lad_data := rec();

	lad_data.graph := graph;
	lad_data.vert_labels := vert_labels;
	lad_data.arc_labels := arc_labels;

	return LocalActionDiagramConsNC(IsLocalActionDiagram, lad_data);
end);

InstallMethod(LocalActionDiagramFromUniversalGroup, "Construct Local Action Diagram Corresponding To Universal Group U(F)", [IsPermGroup],
function(perm_group)
	local orbits, graph;

	orbits := Orbits(perm_group, PermGroupDomain(perm_group));
	graph := RSGraphByAdjacencyMatrix([[Length(orbits)]], ());

	return LocalActionDiagramFromDataNC(graph, [perm_group], orbits);

end);

# Visualising Local Action Diagrams
InstallMethod(ViewString, "for a local action diagram", [IsLocalActionDiagram],
function(lad)
	local graph;

	graph := LocalActionDiagramRSGraph(lad);
	return StringFormatted("<LocalActionDiagram with {1} vertices and {2} arcs>", RSGraphNumberVertices(graph), RSGraphNumberArcs(graph));
end);

InstallMethod(PrintString, "for a local action diagram", [IsLocalActionDiagram], String);

InstallMethod(String, "for a local action diagram", [IsLocalActionDiagram],
function(lad)
	local print_string, graph, vert_ids, arc_ids, vert_labels, arc_labels, id;

	graph := LocalActionDiagramRSGraph(lad);
	vert_labels := LocalActionDiagramVertexLabels(lad);
	arc_labels := LocalActionDiagramArcLabels(lad);
	vert_ids := SortedList(RecNames(vert_labels));
	arc_ids := SortedList(RecNames(arc_labels));

	# First Print The Graph Details
	print_string := String(graph);

	print_string := Concatenation(print_string, "Vertex Labels = {\n");

	for id in vert_ids do
		print_string := Concatenation(print_string, StringFormatted("\t{1} = {2}\n", id, vert_labels.(id)));
	od;

	print_string := Concatenation(print_string, "}\nArc Labels = {\n");

	for id in arc_ids do
		print_string := Concatenation(print_string, StringFormatted("\t{1} = {2}\n", id, arc_labels.(id)));
	od;

	print_string := Concatenation(print_string, "}\n");
	
	return print_string;

end);






