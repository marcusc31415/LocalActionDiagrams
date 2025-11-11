# Family
BindGlobal("RSGraphFamily", NewFamily("RSGraphFamily", IsRSGraph));

# Type
BindGlobal("RSGraphType", NewType(RSGraphFamily, IsRSGraphRep));

InstallMethod(RSGraphConsNC, "Constructing RSGraph Object", [IsRSGraph, IsRecord],
function(_, graph_data)
	Assert(1, IsBound(graph_data.vertices));
	Assert(1, IsBound(graph_data.arcs));
	Assert(1, IsBound(graph_data.arc_ids));
	Assert(1, IsBound(graph_data.reverse_map));

	return ObjectifyWithAttributes(rec(), RSGraphType, 
		RSGraphVertices, graph_data.vertices,
		RSGraphArcs, graph_data.arcs,
		RSGraphArcIds, graph_data.arc_ids,
		RSGraphReverseMap, graph_data.reverse_map
	);
end);

# Input Checking For RSGraph Construction.

BindGlobal("LAD_RSGraphConsCheck@", 
function(arc_list, rev_map, vertex_ids)
	local idx;

	if rev_map*rev_map <> () then
		ErrorNoReturn("Reverse map must be an involution.");
	fi;

	if not ForAll(vertex_ids, IsInt) then
		ErrorNoReturn("Vertex Ids must be integers.");
	fi;

	# The reverse map moves more points then there
	# are arcs. 
	if Maximum(MovedPoints(rev_map)) > Size(arc_list) then
		ErrorNoReturn("Reverse map has a mapping for a non-existant arc.");
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
	local graph_data, arc_records, arc, idx, print_string, vert;

	graph_data := rec();

	graph_data.vertices := vertex_ids;
	graph_data.arc_ids := [1..Size(arc_list)];

	arc_records := rec();

	for idx in graph_data.arc_ids do
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
	local adj_list, vertex_ids, graph;

	vertex_ids := [1..NumberRows(adj_mat)];

	adj_list := LAD_RSGraphAdjMatToAdjList@(adj_mat, vertex_ids);

	graph := RSGraphByAdjacencyList(adj_list, rev_map, vertex_ids);
	SetRSGraphAdjacencyMatrix(graph, adj_mat);
	return graph;
end);

InstallMethod(RSGraphByAdjacencyMatrix, [IsMatrix, IsPerm, IsList],
function(adj_mat, rev_map, vertex_ids)
	local adj_list, graph;

	adj_list := LAD_RSGraphAdjMatToAdjList@(adj_mat, vertex_ids);

	graph := RSGraphByAdjacencyList(adj_list, rev_map, vertex_ids);
	SetRSGraphAdjacencyMatrix(graph, adj_mat);
	return graph;
end);

InstallMethod(RSGraphByAdjacencyMatrixNC, [IsMatrix, IsPerm],
function(adj_mat, rev_map)
	local adj_list, vertex_ids, graph;

	vertex_ids := [1..NumberRows(adj_mat)];

	adj_list := LAD_RSGraphAdjMatToAdjList@(adj_mat, vertex_ids);

	graph := RSGraphByAdjacencyListNC(adj_list, rev_map, vertex_ids);
	SetRSGraphAdjacencyMatrix(graph, adj_mat);
	return graph;
end);

InstallMethod(RSGraphByAdjacencyMatrixNC, [IsMatrix, IsPerm, IsList],
function(adj_mat, rev_map, vertex_ids)
	local adj_list, graph;

	adj_list := LAD_RSGraphAdjMatToAdjList@(adj_mat, vertex_ids);

	graph := RSGraphByAdjacencyListNC(adj_list, rev_map, vertex_ids);
	SetRSGraphAdjacencyMatrix(graph, adj_mat);
	return graph;
end);

InstallMethod(PrintString, "for an RSGraph", [IsRSGraph], String);

InstallMethod(String, "for and RSGraph", [IsRSGraph], 
function(graph)
	local print_string, vertex_ids, arc_rec, rev_map, idx, vert;

	vertex_ids := RSGraphVertices(graph);
	arc_rec := RSGraphArcs(graph);
	rev_map := RSGraphReverseMap(graph);

	print_string := "Vertices = { ";
	if Size(vertex_ids) = 1 then
		print_string := Concatenation(print_string, vertex_ids[1]);
	else
		for vert in vertex_ids do
			print_string := Concatenation(print_string, StringFormatted("{1}, ", vert));
		od;
		Remove(print_string);
		Remove(print_string);
	fi;

	print_string := Concatenation(print_string, " }\nArcs = {\n");

	for idx in RSGraphArcIds(graph) do
		print_string := Concatenation(print_string, StringFormatted("\t{1} = ( origin = {2}, terminus = {3}, inverse = {4} )\n", idx, arc_rec.(idx).origin, arc_rec.(idx).terminus, idx^rev_map));
	od;

	print_string := Concatenation(print_string, "}");
	print_string := Concatenation(print_string, StringFormatted("\nReverse Map = {1}\n", rev_map));

	return print_string;
end);

InstallMethod(ViewString, "for an RSGraph", [IsRSGraph],
function(graph)
	return StringFormatted("<RSGraph with {1} vertices and {2} arcs>", Size(RSGraphVertices(graph)), Size(RSGraphArcIds(graph)));
end);

InstallMethod(RSGraphNumberVertices, "for an RSGraph", [IsRSGraph], graph -> Size(RSGraphVertices(graph)));

InstallMethod(RSGraphNumberArcs, "for an RSGraph", [IsRSGraph], graph -> Size(RSGraphArcIds(graph)));

InstallMethod(RSGraphAdjacencyMatrix, "for an RSGraph", [IsRSGraph],
function(graph)
	local vertex_ids, arc_ids, arc_rec, standard_range, vert_id_map, adj_mat, id;

	vertex_ids := SortedList(RSGraphVertices(graph));
	standard_range := [1..RSGraphNumberVertices(graph)];
	arc_ids := RSGraphArcIds(graph);
	arc_rec := RSGraphArcs(graph);

	vert_id_map := MappingPermListList(vertex_ids, standard_range);

	adj_mat := List(standard_range, x -> List(standard_range, x -> 0));

	for id in arc_ids do
		adj_mat[arc_rec.(id).origin^vert_id_map][arc_rec.(id).terminus^vert_id_map] := adj_mat[arc_rec.(id).origin^vert_id_map][arc_rec.(id).terminus^vert_id_map] + 1;
	od;

	return adj_mat;
end);

InstallMethod(RSGraphCannonicalForm, "for an RSGraph", [IsRSGraph],
function(graph)
	local adj_mat, adj_string;

	adj_mat := RSGraphAdjacencyMatrix(graph);

	# Convert the adjacency matrix into a mutable string. 
	adj_string := MutableCopyMat(String(Flat(adj_mat)));
	# Remove whitespace, [, and ]. 
	RemoveCharacters(adj_string, " []");

	return StringFormatted("{1}&{2}", adj_string, RSGraphReverseMap(graph));
end);

InstallMethod(RSGraphFromCannonicalFrom, "for an RSGraph", [IsString],
function(graph_string)
	local split_string, adj_mat, rev_map, no_verts, row, col, number, graph;

	split_string := SplitString(graph_string, "&");

	rev_map := EvalString(split_string[2]); 

	# Number of ',' charters + 1 is the square of the number of vertices. 
	no_verts := Sqrt(Size(Positions(split_string[1], ','))+1);


	adj_mat := List([1..no_verts], x -> List([1..no_verts], x -> 0));

	row := 1;
	col := 1;

	for number in SplitString(split_string[1], ",") do
		adj_mat[row][col] := Int(number);

		row := row+1;

		if row > no_verts then
			row := 1;
			col := col + 1;
		fi;
	od;

	graph := RSGraphByAdjacencyMatrix(adj_mat, rev_map);
	SetRSGraphCannonicalForm(graph, graph_string);
	SetRSGraphNumberVertices(graph, no_verts);
	return graph;
end);

BindGlobal("LAD_dec_to_bin@", 
function(dec_no)
	local output_string, quot, idx, len_number;

	output_string := [];
	quot := QuotientRemainder(Integers, dec_no, 2);
	
	while quot[1] <> 0 do
		# Add the character corresponding to the remainder
		# to the start of the string. (The +48 makes a
		# 0 or 1 correspond to it's ASCII character.)
		Add(output_string, CharInt(quot[2] + 48), 1); 
		quot := QuotientRemainder(Integers, quot[1], 2);
	od;

	# Need to do the final bit. 
	Add(output_string, CharInt(quot[2] + 48), 1); 

	return output_string;
end);

BindGlobal("LAD_bin_to_dec@",
function(bin_string)
	local i, dec_no;

	dec_no := 0;
	# Need to deal with this case separately. 
	for i in [1..Length(bin_string)] do
		# If the ith character is 1 then add 2^(Length - i)
		# to the decimal number. Need to do Length - i
		# because the numbers are in big-endian. The -48
		# is there to convert the ASCII character to an integer. 
		dec_no := dec_no + 2^((Length(bin_string)-i))*(IntChar(bin_string[i])-48);
	od;

	return dec_no;
end);


InstallMethod(RSGraphMG5String, "for an RSGraph", [IsRSGraph],
function(graph)
	local no_vert, flat_adj_mat, idx, output_string, dec_no, bin_no, pos;

	no_vert := RSGraphNumberVertices(graph);
	flat_adj_mat := Flat(RSGraphAdjacencyMatrix(graph));
	output_string := [];

	for dec_no in flat_adj_mat do
		# Need to add one so that dec(0) is not represented
		# by the white space (space bar) character. 
		bin_no := LAD_dec_to_bin@(dec_no+1);
		# Pad the front of the number with zeroes so it's 
		# length is a multiple of five. 
		while Length(bin_no) mod 5 <> 0 do
			Add(bin_no, '0', 1);
		od;

		pos := 1;
		while pos < Length(bin_no) do
			# At the last block of 5 digits. 
			if (pos+5-1) = Length(bin_no) then
				Add(output_string, StringFormatted("001{1}", bin_no{[pos..pos+5-1]}));
			else
				Add(output_string, StringFormatted("010{1}", bin_no{[pos..pos+5-1]}));
			fi;
			pos := pos+5;
		od;
	od;

	for idx in [1..Length(output_string)] do
		# Replace the binary number with the corresponding ASCII character. 
		output_string[idx] := CharInt(LAD_bin_to_dec@(output_string[idx]));
	od;

	return StringFormatted("{1}|{2}", output_string, String(RSGraphReverseMap(graph)));

end);

InstallMethod(RSGraphFromMG5String, "for RSGraphs", [IsString], 
function(mg5_string)
	local adj_mat, flat_adj_mat, bin_string, idx, dec_no, block, bin_no, no_vert, char, rev_map, split_string;

	split_string := SplitString(mg5_string, "|");

	rev_map := EvalString(split_string[2]);

	bin_string := [];
	for char in split_string[1] do
		# Subtract 1 to get back to the right number (added one
		# in the encoding). 
		block := LAD_dec_to_bin@(IntChar(char)-1);

		# Write the full 8 bits of the ASCII character. 
		while Length(block) <> 8 do
			Add(block, '0', 1);
		od;
		Add(bin_string, block);
	od;

	flat_adj_mat := [];
	bin_no := [];
	for block in bin_string do
		Assert(1, block{[1..3]} = "001" or block{[1..3]} = "010");
		
		# If it's the last block for a digit then turn the binary
		# number into a decimal number and add it to the adjacency
		# matrix. Otherwise add the block to the binary number and
		# go to the next block. 
		if block{[1..3]} = "001" then
			Add(bin_no, block{[4..8]});
			bin_no := Concatenation(bin_no);
			dec_no := LAD_bin_to_dec@(bin_no);
			Add(flat_adj_mat, dec_no);
			bin_no := [];
		else
			Add(bin_no, block{[4..8]});
		fi;
	od;

	Assert(1, IsInt(Sqrt(Length(flat_adj_mat))));
	no_vert := Sqrt(Length(flat_adj_mat));

	# Turn the flat list into a matrix. Unpack it from IsMatrixObj
	# to IsMatrix list of lists. 
	adj_mat := Unpack(Matrix(Integers, flat_adj_mat, no_vert));

	return RSGraphByAdjacencyMatrix(adj_mat, rev_map);

end);

InstallMethod(\=, "for RSGraphs", IsIdenticalObj, [IsRSGraph, IsRSGraph], 0,
function(graph1, graph2)
	return RSGraphCannonicalForm(graph1) = RSGraphCannonicalForm(graph2);
end);
