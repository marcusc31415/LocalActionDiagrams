function(graph)
	local mg6_map, adj_mat, adj_string, output_start, output_list, idx, dec_to_bin, vert_string, output_string_bin, pos, output_string, block;

	mg6_map := rec(
	    0 := "111",
	    1 := "100",
		2 := "1100",
	    3 := "11011",
		4 := "11010",
	   	5 := "10110",
		6 := "10101",
	    7 := "10100",
		8 := "101111",
	    9 := "101110"
		);
	mg6_map.(",") := "0";

	dec_to_bin := LAD_dec_to_bin@;

	# Start with '%' character to recognise MG6 format. 
	output_start := [];

	if RSGraphNumberVertices(graph) <= 63 then
		Add(output_start, dec_to_bin(RSGraphNumberVertices(graph) + 63));
	elif RSGraphNumberVertices(graph) <= 258047 then
		vert_string := dec_to_bin(RSGraphNumberVertices(graph)+63);
		# Is either a 16 bit or 24 bit string.
		# Need to either pad it (16 case) or take the last 18 bits.
		# Also need to add one copy of 126 at the start. 
		if Length(vert_string) = 16 then
			Add(output_start, Concatenation("01111110", "00", vert_string));
		else
			Add(output_start, Concatenation("011111110", vert_string{[7..24]}));
		fi;
	elif RSGraphNumberVertices(graph) <= 68719476735 then
		vert_string := dec_to_bin(RSGraphNumberVertices(graph)+63);
		# Same logic as above case but need to add two copies of
		# 126 at the start. 
		if Length(vert_string) = 24 then
			Add(output_start, Concatenation("011111110", "011111110", "000000000000", vert_string));
		elif Length(vert_string) = 32 then
			Add(output_start, Concatenation("011111110", "011111110", "0000", vert_string));
		else
			Add(output_start, Concatenation("011111110", "011111110", vert_string));
		fi;

	else
		ErrorNoReturn("Graphs with more than 68719476735 are not supported in the MG6 format.");
	fi;



	adj_mat := RSGraphAdjacencyMatrix(graph);

	# Convert the adjacency matrix into a mutable string. 
	adj_string := MutableCopyMat(String(Flat(adj_mat)));

	# Remove whitespace, [, and ]. 
	RemoveCharacters(adj_string, " []");

	# Pre-fill output list. 
	output_list := [1..Length(adj_string)];

	for idx in [1..Length(adj_string)] do
		# Put the character in a list to make it a string. 
		output_list[idx] := mg6_map.([adj_string[idx]]);
	od;

	output_string_bin := Concatenation(output_start);
	output_string_bin := Concatenation(output_string_bin, Concatenation(output_list));

	# Pad the end of the string so its length is a multiple of six. 
	while (Length(output_string_bin) mod 6) <> 0 do
		output_string_bin := Concatenation(output_string_bin, "0");
	od;

	# Start with % character to identify the MG6 string. 
	output_string := ['%'];

	pos := 1;
	while pos < Length(output_string_bin) do
		# Add 63 to each block of 6 binary digits then convert
		# to the corresponding readable ASCII character. 
		block := output_string_bin{[pos..(pos+6-1)]};
		Add(output_string, CharInt(LAD_bin_to_dec@(block) + 63));
		pos := pos + 6;
	od;

	# Convert the blocks of six to ASCII then concatenate reverse map. 
	output_string := Concatenation(output_string, String(RSGraphReverseMap(graph)));

	return output_string;
end;
