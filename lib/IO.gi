# Put local functions in a read-only record here.
# Stops bad code duplication. 

BindGlobal("LAD_IOLocalFunctions",
	rec(
		("MapToString") := function(map)
			local ret, i;

			if IsPerm(map) then # Permutation
				ret := ["P"];

				if Size(MovedPoints(map)) <> 0 then
					for i in [1..Maximum(MovedPoints(map))] do
						Append(ret, [String(i^map), ","]);
					od;
				else
					Add(ret, ",");
				fi;

				Remove(ret);

				return ret;
			else # General mapping. 
				ret := ["M"];

				for i in Source(map) do
					Append(ret, [String(i), "-", String(i^map), ","]);
				od;

				Remove(ret);

				return ret;
			fi;

		end,

		
		("StringToMap") := function(string)
			local dp_elms, domain, range, elm, elm2;

			# Permutation
			if string[1] = 'P' then
				Remove(string, 1);
				if string = "" then
					return ();
				else
					return PermList(List(SplitString(string, ","), Int));
				fi;
			else # General mapping. 
				domain := [];
				range := [];
				for elm in SplitString(string, ",") do
					elm2 := SplitString(elm, "-");
					Add(domain, Int(elm2[1]));
					Add(range, Int(elm2[2]));
					Add(dp_elms, DirectProductElement([Int(elm2[1]), Int(elm2[2])]));
				od;
				return GeneralMappingByElements(Domain(domain), Domain(range), dp_elms);
			fi;
		end
	)
);

InstallMethod(RSGraphToWritableString, "Returns a string representing an RSGraph.", [IsRSGraph],
function(graph)
	local ret_string_list, arc, arc_list, idx, canon_labelling, gen_set, MapToString, gen;

	MapToString := LAD_IOLocalFunctions.MapToString;

	# List the vertex ids (comma separated). 
	ret_string_list := List(RSGraphVertices(graph), x -> StringFormatted("{1},", String(x)));

	# Remove the last comma, add the separator. 
	Remove(Last(ret_string_list));
	Add(ret_string_list, "|");

	# Add arc information in the form id,origin,terminus. 
	for arc in RSGraphArcIterator(graph) do
		Append(ret_string_list, [String(arc[1]), ",", String(arc[2].origin), ",", String(arc[2].terminus), ","]);
	od;

	# Remove the last comma. 
	Remove(ret_string_list);
	Add(ret_string_list, "|");


	# Reverse map. 
	Append(ret_string_list, MapToString(RSGraphReverseMap(graph)));
	Add(ret_string_list, "|");


	# Canonical Labelling (if known)
	if NameFunction(RSGraphCanonicalLabelling) in KnownAttributesOfObject(graph) then
		Append(ret_string_list, [NameFunction(RSGraphCanonicalLabelling), "!"]);
		canon_labelling := RSGraphCanonicalLabelling(graph);

		# Store them differently if they are permutations or general mappings. 

		# Arc isomorphism.  
		Append(ret_string_list, MapToString(canon_labelling.arc_isomorphism));
		Add(ret_string_list, ":");

		# Vertex isomorphism. 
		Append(ret_string_list, MapToString(canon_labelling.vertex_isomorphism));
		Add(ret_string_list, ":");

		# Arc standard map. 
		Append(ret_string_list, MapToString(canon_labelling.arc_standard_map));
		Add(ret_string_list, ":");
		
		# Reverse map.
		Append(ret_string_list, MapToString(canon_labelling.reverse_map));
		Add(ret_string_list, ":");

		# Canonical certificate. 
		Append(ret_string_list, [canon_labelling.canon_certificate, "|"]);
	fi;

	# Automorphism Group (if known)
	if NameFunction(AutomorphismGroup) in KnownAttributesOfObject(graph) then
		Append(ret_string_list, [NameFunction(AutomorphismGroup), "!"]);
		gen_set := MinimalGeneratingSet(AutomorphismGroup(graph));

		if Size(gen_set) = 0 then
			gen_set := [()];
		fi;

		for gen in gen_set do
			Append(ret_string_list, MapToString(gen));
			Add(ret_string_list, ":");
		od;
		Remove(ret_string_list);
		Add(ret_string_list, "|");
	fi;

	# Remove the last "|". 
	Remove(ret_string_list);

	return Concatenation(ret_string_list);
end);


InstallMethod(LocalActionDiagramToWritableString, "Returns a string representing a local action diagram.", [IsLocalActionDiagram],
function(lad)
	local graph, ret_string_list, RecIterator, arc_labels, vertex_labels, vert, gens, domain, gen, MapToString, elm, arc, scopo;

	RecIterator := r -> List(RecNames(r), x -> [Int(x), r.(x)]);

	MapToString := LAD_IOLocalFunctions.MapToString;

	# Graph string.
	graph := LocalActionDiagramRSGraph(lad);
	ret_string_list := [RSGraphToWritableString(graph), "/"];

	# Vertex labels and Perm Group Domains.
	# In form id:gen1_gen2_...:elm1,elm2,...;...
	for vert in RecIterator(LocalActionDiagramVertexLabels(lad)) do
		Append(ret_string_list, [String(vert[1]), ":"]);
		gens := MinimalGeneratingSet(vert[2]);
		if Size(gens) = 0 then
			gens := [()];
		fi;
		domain := PermGroupDomain(vert[2]);

		for gen in gens do
			Append(ret_string_list, MapToString(gen));
			Add(ret_string_list, "_");
		od;
		
		Remove(ret_string_list);
		Add(ret_string_list, ":");

		for elm in domain do
			Append(ret_string_list, [String(elm), ","]);
		od;
		Remove(ret_string_list);
		Add(ret_string_list, ";");
	od;

	Remove(ret_string_list);
	Add(ret_string_list, "|");


	# Arc labels. 
	# In form id:elm1,elm2,...;
	for arc in  RecIterator(LocalActionDiagramArcLabels(lad)) do
		Append(ret_string_list, [String(arc[1]), ":"]);
		for elm in arc[2] do
			Append(ret_string_list, [String(elm), ","]);
		od;
		Remove(ret_string_list);
		Add(ret_string_list, ";");
	od;
	Remove(ret_string_list);
	Add(ret_string_list, "|");

	# Potentially known attributes and properties.
	if Size(LocalActionDiagramGroupName(lad)) <> 0 then
		Append(ret_string_list, [NameFunction(LocalActionDiagramGroupName), "!", LocalActionDiagramGroupName(lad), "|"]);
	fi;

	if NameFunction(LocalActionDiagramGroupType) in KnownAttributesOfObject(lad) then
		Append(ret_string_list, [NameFunction(LocalActionDiagramGroupType), "!", LocalActionDiagramGroupType(lad), "|"]);
	fi;

	# Store list of scopos. In form elm1,elm2,...:...
	if NameFunction(LocalActionDiagramScopos) in KnownAttributesOfObject(lad) then
		Append(ret_string_list, [NameFunction(LocalActionDiagramScopos), "!"]);
		for scopo in LocalActionDiagramScopos(lad) do
			if Size(scopo) <> 0 then # Not empty scopo. 
				for elm in scopo do
					Append(ret_string_list, [String(elm), ","]);
				od;
				Remove(ret_string_list);
				Add(ret_string_list, ":");
			fi;
		od;
		# If there's only the empty scopo then leave a white space character. 
		if Last(ret_string_list) <> "!" then
			Remove(ret_string_list);
		else
			Add(ret_string_list, " ");
		fi;
		Add(ret_string_list, "|");
	fi;

	if NameFunction(LocalActionDiagramIsDiscrete) in KnownPropertiesOfObject(lad) then
		Append(ret_string_list, [NameFunction(LocalActionDiagramIsDiscrete), "!", String(LocalActionDiagramIsDiscrete(lad)), "|"]);
	fi;

	if NameFunction(LocalActionDiagramIsUniscalar) in KnownPropertiesOfObject(lad) then
		Append(ret_string_list, [NameFunction(LocalActionDiagramIsUniscalar), "!", String(LocalActionDiagramIsUniscalar(lad)), "|"]);
	fi;

	if NameFunction(LocalActionDiagramIsUnimodular) in KnownPropertiesOfObject(lad) then
		Append(ret_string_list, [NameFunction(LocalActionDiagramIsUnimodular), "!", String(LocalActionDiagramIsUnimodular(lad)), "|"]);
	fi;

	# Remove the last "|". 
	Remove(ret_string_list);

	return Concatenation(ret_string_list);

end);

InstallMethod(RSGraphFromWritableString, "Constructs an RSGraph from a string.", [IsString],
function(string)
	local graph_data, split_string, arc_split, StringToMap, func_args, property, gens, gen, gen_split, data, canon_labelling, canon_split, property_split, idx, arc_id, terminus, origin, rev_id;

	StringToMap := LAD_IOLocalFunctions.StringToMap;

	graph_data := rec();

	split_string := SplitString(string, "|");

	# Vertex ids. 
	graph_data.vertex_list := List(SplitString(split_string[1], ","), Int);

	# Reverse map (needed to construct the arc records).
	graph_data.reverse_map := StringToMap(split_string[3]);

	# Arcs. 
	graph_data.arc_rec := rec();
	graph_data.arc_ids := [];

	arc_split := SplitString(split_string[2], ",");

	# Each arc comes in a triple of information.  
	for idx in [1..Length(arc_split)/3] do
		arc_id := Int(arc_split[3*idx-2]); 
		origin := Int(arc_split[3*idx-1]); 
		terminus := Int(arc_split[3*idx-0]);
		rev_id := arc_id^graph_data.reverse_map;

		graph_data.arc_rec.(arc_id) := rec(
			("origin") := origin,
			("terminus") := terminus,
			("inverse") := rev_id
		);
		Add(graph_data.arc_ids, arc_id);
	od;

	func_args := [rec(), RSGraphType,
	              RSGraphVertices, graph_data.vertex_list,
				  RSGraphArcIDs, graph_data.arc_ids,
				  RSGraphArcs, graph_data.arc_rec,
				  RSGraphReverseMap, graph_data.reverse_map];

	# The extra properties that could be known. 
	for idx in [4..Length(split_string)] do
		property_split := SplitString(split_string[idx], "!");
		property := property_split[1];
		data := property_split[2];

		if property = NameFunction(RSGraphCanonicalLabelling) then
			canon_labelling := rec();
			canon_split := SplitString(data, ":");

			# Arc isomorphism. 
			canon_labelling.arc_isomorphism := StringToMap(canon_split[1]);

			# Vertex isomorphism. 
			canon_labelling.vertex_isomorphism := StringToMap(canon_split[2]);

			# Arc standard map. 
			canon_labelling.arc_standard_map := StringToMap(canon_split[3]);

			# Reverse map. 
			canon_labelling.reverse_map := StringToMap(canon_split[4]);

			# Canonical certificate. 
			canon_labelling.canon_certificate := canon_split[5];

			Append(func_args, [RSGraphCanonicalLabelling, canon_labelling]);
		elif property = NameFunction(AutomorphismGroup) then

			gens := [];
			if Length(data) <> 0 then
				gen_split := SplitString(data, ":");
				for gen in gen_split do
					Add(gens, StringToMap(gen));
				od;
			else
				Add(gens, ());
			fi;

			Append(func_args, [AutomorphismGroup, Group(gens)]);


		else
			Info(InfoWarning, 1, "\"", property, "\" is not an RSGraph attribute. Ignoring");
		fi;
	od;

	return CallFuncList(ObjectifyWithAttributes, func_args);
	
end);



InstallMethod(LocalActionDiagramFromWritableString, "Constructs an local action diagram from a string.", [IsString, IsBool],
function(string, graph_ret)
	local ToRec, first_split, graph, split_string, vertex_labels, vert_string, vert_split, label, gens, gen, group, domain, elm, arc_labels, arc_string, arc_split, func_args, lad, property, data, scopos, scopo, scopo_string, scopo_split, idx, property_split, StringToMap;

	ToRec := function(list)
		local record, elm;

		record := rec();

		for elm in list do
			record.(elm[1]) := elm[2];
		od;

		return record;
	end;

	StringToMap := LAD_IOLocalFunctions.StringToMap;

	first_split := SplitString(string, "/");

	graph := RSGraphFromWritableString(first_split[1]);

	split_string := SplitString(first_split[2], "|");

	# Vertex labels. 
	vertex_labels := [];

	for vert_string in SplitString(split_string[1], ";") do
		vert_split := SplitString(vert_string, ":");

		# Vertex id. 
		label := [Int(vert_split[1])]; 

		# The group. 
		gens := [];

		for gen in SplitString(vert_split[2], "_") do
			Add(gens, StringToMap(gen));
		od;

		if Size(gens) = 0 then
			gens := [()];
		fi;

		group := Group(gens);

		# The groups domain. 
		domain := [];

		for elm in SplitString(vert_split[3], ",") do
			Add(domain, Int(elm));
		od;

		SetPermGroupDomain(group, domain);
		Add(label, group);

		Add(vertex_labels, label);
	od;

	vertex_labels := ToRec(vertex_labels);


	# Arc labels. 
	arc_labels := [];

	for arc_string in SplitString(split_string[2], ";") do
		arc_split := SplitString(arc_string, ":");

		label := [Int(arc_split[1])];

		domain := [];
		for elm in SplitString(arc_split[2], ",") do
			Add(domain, Int(elm));
		od;

		Add(label, domain);
		Add(arc_labels, label);
	od;

	arc_labels := ToRec(arc_labels);

	func_args := [rec(), LocalActionDiagramType, LocalActionDiagramRSGraph, graph,
					LocalActionDiagramVertexLabels, vertex_labels,
					LocalActionDiagramArcLabels, arc_labels];

	# Go through the rest of the properties. 
	for idx in [4..Length(split_string)] do
		property_split := SplitString(split_string[idx], "!");

		property := property_split[1];
		data := property_split[2];
			
		if property = NameFunction(LocalActionDiagramGroupType) then
			Add(func_args, LocalActionDiagramGroupType);
			Add(func_args, data);
		elif property = NameFunction(LocalActionDiagramScopos) then
			Add(func_args, LocalActionDiagramScopos);

			scopos := [[]];

			if data <> " " then
				scopo_split := SplitString(data, ":");

				for scopo_string in scopo_split do
					scopo := [];
					for elm in SplitString(scopo_string, ",") do
						Add(scopo, Int(elm));
					od;
					Add(scopos, scopo);
				od;
			fi;

			Add(func_args, scopos);
		elif property = NameFunction(LocalActionDiagramIsDiscrete) then
			Add(func_args, LocalActionDiagramIsDiscrete);
			if data = "true" then
				Add(func_args, true);
			else
				Add(func_args, false);
			fi;
		elif property = NameFunction(LocalActionDiagramIsUniscalar) then
			Add(func_args, LocalActionDiagramIsUniscalar);
			if data = "true" then
				Add(func_args, true);
			else
				Add(func_args, false);
			fi;
		elif property = NameFunction(LocalActionDiagramIsUnimodular) then
			Add(func_args, LocalActionDiagramIsUnimodular);
			if data = "true" then
				Add(func_args, true);
			else
				Add(func_args, false);
			fi;
		else
			Info(InfoWarning, 1, "\"", property, "\" is not a LocalActionDiagram attribute. Ignoring");
		fi;
	od;

	lad := CallFuncList(ObjectifyWithAttributes, func_args);
	
	if graph_ret = true then
		return [lad, graph];
	else
		return graph;
	fi;
end);


InstallMethod(LocalActionDiagramFromWritableString, "Constructs an local action diagram from a string.", [IsString],
function(lad_string)
	return LocalActionDiagramFromWritableString(lad_string, false);
end);
# Create local action diagram from the data as it's needed. 
# This can save time as not all the data may be needed? 
