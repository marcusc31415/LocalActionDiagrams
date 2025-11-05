# Family
BindGlobal("RSGraphFamily", NewFamily("RSGraphFamily", IsRSGraph));

# Type
BindGlobal("RSGraphType", NewType(RSGraphFamily, IsRSGraph));

InstallMethod(RSGraphConsNC, "Constructing RSGraph Object", [IsRSGraph, IsRecord],
function(_, graph_data)
	Assert(1, IsBound(graph_data.vertices));
	Assert(1, IsBound(graph_data.arcs));

	return Objectify(RSGraphType, graph_data);
end);

# Input Checking For RSGraph Construction.

BindGlobal("LAD_RSGraphConsCheck@", 
function(arc_list, rev_map, vertex_ids)
	local idx;

	if rev_map*rev_map <> () then
		ErrorNoReturn("Reverse map must be an involution.");
	fi;

	for idx in [1..Length(arc_list)] do
		if arc_list[idx^rev_map][1] <> arc_list[idx][2] or arc_list[idx^rev_map][2] <> arc_list[idx][1] then
			ErrorNoReturn("Reversal mapping must send the terminal vertex of an arc to the origin vertex of the arc.");
		fi;

		if not arc_list[idx][1] in vertex_ids or not arc_list[idx][2] in vertex_ids then
			ErrorNoReturn(StringFormatted("Arc {1} origin and terminus vertices do not match vertex ids.", idx));
		fi;
	od;

end);

BindGlobal("LAD_RSGraphAdjMatToAdjList@",
function(adj_mat, vertex_ids)
	local adj_list, idx_x, idx_y, _;

	adj_list := [];

	for idx_x in [1..NumberRows(adj_mat)] do
		for idx_y in [1..NumberColumns(adj_mat)] do
			for _ in [1..adj_mat[idx_x][idx_y]] do
				Add(adj_list, [vertex_ids[idx_x], vertex_ids[idx_y]]);
			od;
		od;
	od;

	return adj_list;
end);

InstallMethod(RSGraphByAdjacencyList, [IsList, IsPerm],
function(arc_list, rev_map)
	local vertex_ids;

	if IsEmpty(arc_list) then
		vertex_ids := [];
	else
		vertex_ids := [1..Maximum(Flat(arc_list))];
	fi;

	LAD_RSGraphConsCheck@(arc_list, rev_map, vertex_ids);
	
	return RSGraphByAdjacencyListNC(arc_list, rev_map, vertex_ids);
end);

InstallMethod(RSGraphByAdjacencyList, [IsList, IsPerm, IsList],
function(arc_list, rev_map, vertex_ids)

	LAD_RSGraphConsCheck@(arc_list, rev_map, vertex_ids);
	
	return RSGraphByAdjacencyListNC(arc_list, rev_map, vertex_ids);
end);

InstallMethod(RSGraphByAdjacencyListNC, [IsList, IsPerm],
function(arc_list, rev_map)
	local vertex_ids;

	if IsEmpty(arc_list) then
		vertex_ids := [];
	else
		vertex_ids := [1..Maximum(Flat(arc_list))];
	fi;
	
	return RSGraphByAdjacencyListNC(arc_list, rev_map, vertex_ids);
end);

InstallMethod(RSGraphByAdjacencyListNC, [IsList, IsPerm, IsList],
function(arc_list, rev_map, vertex_ids)
	local graph_data, arc_records, arc, idx;

	graph_data := rec();

	graph_data.vertices := vertex_ids;

	arc_records := rec();

	for idx in [1..Size(arc_list)] do
		arc := rec();
		arc.origin := arc_list[idx][1];
		arc.terminus := arc_list[idx][2];
		arc.inverse := idx^rev_map;
		
		arc_records.(idx) := arc;
	od;

	graph_data.arcs := arc_records;
	graph_data.reverse_map := rev_map;
	
	return RSGraphConsNC(IsRSGraph, graph_data);
end);

InstallMethod(RSGraphByAdjacencyMatrix, [IsMatrix, IsPerm],
function(adj_mat, rev_map)
	local adj_list, vertex_ids;

	vertex_ids := [1..NumberRows(adj_mat)];

	adj_list := LAD_RSGraphAdjMatToAdjList@(adj_mat, vertex_ids);

	return RSGraphByAdjacencyList(adj_list, rev_map, vertex_ids);
end);

InstallMethod(RSGraphByAdjacencyMatrix, [IsMatrix, IsPerm, IsList],
function(adj_mat, rev_map, vertex_ids)
	local adj_list;

	adj_list := LAD_RSGraphAdjMatToAdjList@(adj_mat, vertex_ids);

	return RSGraphByAdjacencyList(adj_list, rev_map, vertex_ids);
end);

InstallMethod(RSGraphByAdjacencyMatrixNC, [IsMatrix, IsPerm],
function(adj_mat, rev_map)
	local adj_list, vertex_ids;

	vertex_ids := [1..NumberRows(adj_mat)];

	adj_list := LAD_RSGraphAdjMatToAdjList@(adj_mat, vertex_ids);

	return RSGraphByAdjacencyListNC(adj_list, rev_map, vertex_ids);
end);

InstallMethod(RSGraphByAdjacencyMatrixNC, [IsMatrix, IsPerm, IsList],
function(adj_mat, rev_map, vertex_ids)
	local adj_list;

	adj_list := LAD_RSGraphAdjMatToAdjList@(adj_mat, vertex_ids);

	return RSGraphByAdjacencyListNC(adj_list, rev_map, vertex_ids);
end);

# Have adjacency matrix functions convert it to an adjacency list then use the above. 
