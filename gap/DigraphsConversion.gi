InstallMethod(RSGraphToDigraph, "Converts an RSGraph to a Digraph from the Digraphs package", [IsRSGraph],
function(graph)
	local standard_graph, digraph, adj_list, ret, arc;

	standard_graph := RSGraphToStandardForm(graph);

	adj_list := [];

	for arc in RSGraphArcIterator(standard_graph.graph) do
		Add(adj_list, [arc[2].origin, arc[2].terminus]);
	od;

	digraph := DigraphByEdges(adj_list);

	ret := rec();

	ret.digraph := digraph;
	ret.reverse_map := RSGraphReverseMap(standard_graph.graph);
	ret.vertex_id_map := standard_graph.vertex_id_map;
	ret.arc_id_map := standard_graph.arc_id_map;

	return ret;
end);

InstallMethod(RSGraphFromDigraph, "Converts a digraph from the Digraphs package to an RSGraph", [IsDigraph, IsPerm],
function(digraph, reverse_map)
	return RSGraphByAdjacencyList(DigraphEdges(digraph), reverse_map);
end);
