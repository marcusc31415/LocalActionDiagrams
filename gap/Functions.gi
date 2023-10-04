# Helper function to construct a bijection as a mapping. The bijection will map
# list1[i] to list2[i]. 
ConstructBijection@ := function(list1, list2)
	local bijection, i, map_list;
	
	if Length(list1) <> Length(list2) then
		Error("Must have the same number of elements in list1 and list2.");
	fi;

	map_list := [];

	for i in [1..Length(list1)] do
		Add(map_list, Tuple([list1[i], list2[i]]));
	od;

	bijection := GeneralMappingByElements(Domain(list1), Domain(list2), map_list);
	return bijection;
end;

# Find the group corresponding to bijection^(-1)*G*bijection by conjugating
# the generators of G. 
GroupConjugateByBijection@ := function(G, bijection)
	local i, inv_bijection, gen, gen_map, conjugate_map, cm_l1, cm_l2, conjugate_gen, conjugate_gens, conjugate_group;

	conjugate_gens := [];
	inv_bijection := InverseGeneralMapping(bijection);

	for gen in GeneratorsOfGroup(G) do
		cm_l1 := [];
		cm_l2 := [];
		gen_map := MappingByFunction(Domain(PermGroupDomain(G)), Domain(PermGroupDomain(G)), x -> x^gen);
		conjugate_map := CompositionMapping(bijection, gen_map, inv_bijection);
		for i in Source(conjugate_map) do
			Add(cm_l1, i);
			Add(cm_l2, i^conjugate_map);
		od;
		conjugate_gen := MappingPermListList(cm_l1, cm_l2);
		Add(conjugate_gens, conjugate_gen);
	od;
	if conjugate_gens = [] then
		conjugate_group := Group(());
	else
		conjugate_group := GroupByGenerators(conjugate_gens);
	fi;
	return conjugate_group;
end;

# Find every digraph isomorphism between D1 and D2. 
# Note that it uses the automorphism group of D2 so you can speed up repeated
# calculations by ensuring D2 is a digraph that you've already run on this
# function. 
FindEveryDigraphIsomorphism@ := function(D1, D2)
	local aut_grp, aut, base_iso, iso_list, proj_1, proj_2, iso, rev_1, rev_2, edge_iso, edges_1, edges_2, edges_after_iso, edge_movement, edge, good_reversal, edge_no, edges;

	base_iso := IsomorphismDigraphs(D1, D2); # Find one isomorphism between them.

	if base_iso = fail then
		return fail;
	fi;

	rev_1 := LocalActionDiagramEdgeReversal(D1);
	rev_2 := LocalActionDiagramEdgeReversal(D2);
	edges_1 := DigraphEdges(D1);
	edges_2 := DigraphEdges(D2);

	# Isomorphism and automorphism will having mappings for vertices and edges. 
	if IsMultiDigraph(D1) then
		aut_grp := AutomorphismGroup(D2); # Use the automorphisms of D2 to find the rest.
		proj_1 := Projection(aut_grp, 1); # Vertices under automorphism.
		proj_2 := Projection(aut_grp, 2); # Edges under automorphism. 

		iso_list := [];

		# Find each isomorphism by composing the 'base isomorphism' with each automorphism.  
		for aut in aut_grp do
			iso := [base_iso[1]*Image(proj_1, aut), base_iso[2]*Image(proj_2, aut)];
			# Check that the reversal mapping is correct under the isomorphism.
			edges := ShallowCopy(edges_1);
			edge_movement := [1..Length(edges)];
			edges := List(edges, x -> List(x, y -> y^iso[1]));
			StableSortParallel(edges, edge_movement);
			edge_iso := MappingPermListList([1..Length(edges)], edge_movement);
			edge_iso := MappingPermListList(edge_movement, [1..Length(edges)]);
			edge_iso := edge_iso*iso[2];
			if rev_1*edge_iso = edge_iso*rev_2 then
				Add(iso_list, [iso[1], edge_iso]);
				#Add(iso_list, ["HI", edge_movement, iso[2]]);
			fi;
		od;
	# Not a multi-digraph so only mappings for vertices. 
	else
		aut_grp := AutomorphismGroup(D2);
		iso_list := [];
		# Find each isomorphism by composing the 'base isomorphism' with each automorphism.  
		for aut in aut_grp do
			iso := base_iso*aut;
			edges_after_iso := List(edges_1, x -> List(x, y -> y^iso));
			edge_movement := List(edges_after_iso, x -> Position(edges_2, x));
			edge_iso := MappingPermListList([1..DigraphNrEdges(D1)], edge_movement);
			edge_iso := MappingPermListList(edge_movement, [1..DigraphNrEdges(D1)]);
			
			#good_reversal := true;
			#for edge_no in [1..DigraphNrEdges(D1)] do
			#	if (edge_no^rev_1)^edge_iso <> (edge_no^edge_iso)^rev_2 then
			#		good_reversal := false;
			#		break;
			#	fi;
			#od;
			
			# If the reversal map is correct under the isomorphism. 
			if rev_1*edge_iso = edge_iso*rev_2 then
				Add(iso_list, [iso, edge_iso]);
			fi;
		od;

	fi;

	return iso_list;

end;

# Find all valid bijection between *orbits1* and *orbits2* --- i.e. all bijections
# that map elements from orbits1[i] to elements in orbits2[i]. 
FindValidGroupBijections@ := function(orbits1, orbits2)
	local orbits2_arrangments, i, bijections;

	orbits2_arrangments := [];

	# Lists every possible arrangement of each orbit. 
	# For example: [[1,2],[3]] will become [[[1,2],[2,1]], [[3]]].
	for i in [1..Length(orbits2)] do
		orbits2_arrangments[i] := Arrangements(orbits2[i], Length(orbits2[i]));
	od;

	# Finds every possible bijection by taking the Cartesian product of each possible
	# arrangement of the orbits. 
	# For example: [[[1,2],[2,1]], [[3]]] will become [[[1,2],[3]],[[2,1],[3]]]. 
	bijections := Cartesian(orbits2_arrangments);

	# Return a list of each bijection. 
	return List(bijections, Concatenation);
end;

# Returns the first conjugate bijection found between two groups that restricts correctly
# to the orbit structure given. Returns *fail* if no valid bijection exists. 
FindConjugateBijectionBetweenGroups@ := function(G1, G2, orbits1, orbits2)
	local G1_elms, potential_bijections, bijection, bijection_map, conjugate_group;
	if IsomorphismGroups(G1, G2) = fail then
		return fail;
	fi;

	# These are all the potential bijections which restrict correctly on the arc labels. 
	potential_bijections := FindValidGroupBijections@(orbits1, orbits2);

	# Use the same arrangement for the elements of G1 for each bijection. 
	G1_elms := Concatenation(orbits1);

	# Go through each bijection and check if it can form the conjugate group. 
	for bijection in potential_bijections do
		bijection_map := ConstructBijection@(G1_elms, bijection);
		conjugate_group := GroupConjugateByBijection@(G1, bijection_map);
		if G2 = conjugate_group then 
			return bijection_map;
		fi;
	od;
	return fail;
end;

# Checks if the edges in the list *scopo* form a scopo of *lad*. 
CheckIfScopo@ := function(lad, scopo)
	local i, edge_labels, edges, rev, terminal_edges;

	edge_labels := LocalActionDiagramEdgeLabels(lad);
	rev := LocalActionDiagramEdgeReversal(lad);
	edges := DigraphEdges(lad);

	for i in scopo do

		# Check if reversal is in scopo. 
		if i^rev in scopo then
			return false;
		fi;

		if Size(edge_labels[i]) <> 1 then
			return false;
		fi;
		
		# Find every edge that that goes to starting vertex of edge *i*. 
		terminal_edges := Positions(List(edges, x -> x[2]), edges[i][1]);

		# Remove the edge reversal from it. 
		terminal_edges := Difference(terminal_edges, [i^rev]); 

		if not IsSubset(scopo, terminal_edges) then
			return false;
		fi;
	od;

	return true;
end;

# Checks the second condition from the scopo definition (if a is in the scopo
# then all arcs that terminate at o(a) are also in it apart from Reverse(a)). 
CheckScopoSecondCondition@ := function(lad, edge_no)
	local edges, edge_labels, rev, terminal_edges;

	edges := DigraphEdges(lad);
	edge_labels := LocalActionDiagramEdgeLabels(lad);
	rev := LocalActionDiagramEdgeReversal(lad);

	terminal_edges := Positions(List(edges, x -> x[2]), edges[edge_no][1]); # Edges that terminate at o(a). 

	terminal_edges := Difference(terminal_edges, [edge_no^rev]);

	if not ForAll(List(edge_labels{terminal_edges}, Size), x -> x = 1) then
		return false;
	else
		return true;
	fi;

end;

# Find all scopos of *lad*. Will always return at least the empty scopo. 
FindAllScopos@ := function(lad)
	local candidate_edges, candidate, candidate_scopos, scopo_list;

	scopo_list := [];

	candidate_edges := Positions(List(LocalActionDiagramEdgeLabels(lad), Size), 1);
	
	candidate_edges := ListBlist(candidate_edges, List(candidate_edges, x -> CheckScopoSecondCondition@(lad, x)));

	for candidate in IteratorOfCombinations(candidate_edges) do
		if CheckIfScopo@(lad, candidate) then
			Add(scopo_list, candidate);
		fi;
	od;

	return scopo_list;
end;

# Find all order two elements of Sym(domain). 
Order2Elements@ :=function(domain)
	local tr, elms, t, points, elm;
	
	if Size(domain) in [0,1] then return [()]; fi;
	
	tr := List(Combinations(domain,2), c->(c[1],c[2]));	
	elms := tr;
	for t in Difference(tr,[()]) do
		points := MovedPoints(t);
		for elm in Order2Elements@(Difference(domain, Union(points,[1..Minimum(points)]))) do
			Add(elms, t*elm);
		od;
	od;
	return Union(elms, [()]);
end;

CotreeFromScopo@ := function(lad, scopo)
	local vertices, v_in_scopo, edge_in_scopo, cotree, cotree_edge_labels, edge_label_positions, rev;

	vertices := DigraphVertices(lad);
	rev := LocalActionDiagramEdgeReversal(lad);
	v_in_scopo := [];
	# Add the origin vertex of each edge in scopo to the list.
	# Cotree is formed by every other vertex in the lad. 
	for edge_in_scopo in scopo do
		Add(v_in_scopo, DigraphEdges(lad)[edge_in_scopo][1]);
	od;
	cotree := InducedSubdigraph(lad, Difference(vertices, v_in_scopo));
	edge_label_positions := Difference([1..DigraphNrEdges(lad)], scopo);
	edge_label_positions := Difference(edge_label_positions, List(scopo, x -> x^rev));
	cotree_edge_labels := LocalActionDiagramEdgeLabels(lad){edge_label_positions};

	return [cotree, cotree_edge_labels];
end;

# Find the group type of *lad*.
GroupType@ := function(lad)
	local scopos, scopo, scopo_size, largest_scopos_position, cotrees, vertices, v_in_scopo, edge_in_scopo, idx, cotree_edge_labels, edge_label_positions, orientation_edges, start_vertex, current_vertex, prev_vertex, out_edges, rev, cotree, orientation_edge_numbers, edge, temp, orientation_edge_labels, other_orientation_edge_labels, max_orientation, max_other_orientation; 

	scopos := LocalActionDiagramScopos(lad);
	scopo_size := List(scopos, Length);
	vertices := DigraphVertices(lad);
	rev := LocalActionDiagramEdgeReversal(lad);
	cotrees := [];
	cotree_edge_labels := [];
	
	# Find the corresponding cotree of each scopo. 
	for scopo in scopos do
		cotree := CotreeFromScopo@(lad, scopo);
		Add(cotrees, cotree[1]);
		Add(cotree_edge_labels, cotree[2]);
	od;

	# Positions of the largest scopos.  
	largest_scopos_position := Positions(scopo_size, Maximum(scopo_size));

	# Check if fixed vertex and edge inversion.
	for idx in largest_scopos_position do
		# Single vertex and no loop.
		if DigraphNrVertices(cotrees[idx]) = 1 and DigraphNrEdges(cotrees[idx]) = 0 then
			return "Fixed Vertex";
		# Single vertex and single loop (if single vertex then one edge = one loop). 
		elif DigraphNrVertices(cotrees[idx]) = 1 and DigraphNrEdges(cotrees[idx]) = 1 then
			# Check the single loop has colour set of size 1. 
			if Length(cotree_edge_labels[idx][1]) = 1 then
				return "Edge Inversion";
			fi;
		fi;
	od;

	# Check if lineal or focal. 
	for idx in [1..Length(cotrees)] do
		cotree := cotrees[idx];
		# Cotree which is a cycle of order 1.
		if DigraphNrVertices(cotree) = 1 and DigraphNrEdges(cotree) = 2 then
			if Maximum(List(cotree_edge_labels[idx], Length)) = 1 then
				return "Lineal";
			elif Length(cotree_edge_labels[idx][1]) = 1 or Length(cotree_edge_labels[idx][2]) = 1 then
				return "Focal";
			fi;
		# Need to manually find out if it's a cycle because Digraphs package is mean with it's definitions. 
		elif DigraphNrEdges(cotree) = 2*DigraphNrVertices(cotree) then
			# Find one orientation of the cycle. 
			orientation_edges := [];
			Add(orientation_edges, DigraphEdges(cotree)[1]);
			start_vertex := orientation_edges[1][1];
			prev_vertex := start_vertex;
			current_vertex := orientation_edges[1][2];
			# If it's a cycle then this process will end with current_vertex = start_vertex.
			for temp in [2..DigraphNrVertices(cotree)] do
				# Find the two edges with origin vertex *current_vertex*. 
				out_edges := List(DigraphEdges(cotree), x -> x[1] = current_vertex);
				out_edges := ListBlist(DigraphEdges(cotree), out_edges);
				if out_edges[1][2] <> prev_vertex then
					Add(orientation_edges, out_edges[1]);
					prev_vertex := current_vertex;
					current_vertex := out_edges[1][2];
				else
					Add(orientation_edges, out_edges[2]);
					prev_vertex := current_vertex;
					current_vertex := out_edges[2][2];
				fi;
			od;

			# It's not a cycle cotree. 
			if current_vertex <> start_vertex then
				continue;
			fi;

			# Get index of each edge in cyclic orientation.
			orientation_edge_numbers := [];
			for edge in orientation_edges do
				Add(orientation_edge_numbers, Position(DigraphEdges(cotree), edge));
			od;
			
			# Now check if the cycles have the right colour sets for lineal or focal. 
			orientation_edge_labels := cotree_edge_labels[idx]{orientation_edge_numbers};
			other_orientation_edge_labels := cotree_edge_labels[idx]{Difference([1..DigraphNrEdges(cotree)], orientation_edge_numbers)};
			max_orientation := Maximum(List(orientation_edge_labels, Length));
			max_other_orientation := Maximum(List(other_orientation_edge_labels, Length));
			if max_orientation = 1 and max_other_orientation = 1 then
				return "Lineal";
			elif max_orientation = 1 and max_other_orientation > 1 then
				return "Focal";
			elif max_orientation > 1 and max_other_orientation = 1 then
				return "Focal";
			fi;
		fi;
	od;

	# A finite LAD can't be horocyclic so if it's not any of the above types it must be of General Type. 
	return "General Type";
end;

# Check if *lad* corresponds to a discrete group. 
IsDiscrete@ := function(lad)
	local group_type, v_label, max_cotree, max_scopo, scopos, edge_no, v_in_scopo, idx, scopo;

	group_type := LocalActionDiagramGroupType(lad);

	# These types are always discrete for finite LADs. 
	if group_type = "Fixed Vertex" or group_type = "Edge Inversion" then
		return true;
	# Focal is never discrete. 
	elif group_type = "Focal" then
		return false;
	elif group_type = "Lineal" then
		# Discrete if and only if each vertex label is trivial. 
		for v_label in LocalActionDiagramVertexLabels(lad) do
			if v_label <> Group(()) then
				return false;
			fi;
		od;
		return true; 
	elif group_type = "General Type" then
		# Get the unique maximum scopo.
		scopos := LocalActionDiagramScopos(lad);
		max_scopo := Position(List(scopos, Length), Maximum(List(scopos, Length)));
		max_scopo := scopos[max_scopo];

		# Get every vertex in the scopo (origin vertices of edges in scopo).
		# These are the vertices not in the cotree.
		v_in_scopo := [];
		for edge_no in max_scopo do
			Add(v_in_scopo, DigraphEdges(edge_no)[1]);
		od;

		for idx in [1..DigraphNrVertices(lad)] do
			v_label := LocalActionDiagramVertexLabels(lad)[idx];
			# Vertex label not in cotree and not trivial.
			if idx in v_in_scopo then
				if v_label <> Group(()) then
					return false;
				fi;
			# Vertex label in cotree and not semi-regular. 
			elif not idx in v_in_scopo then
				if not IsSemiRegular(v_label, PermGroupDomain(v_label)) then
					return false;
				fi;
			else
				Error("Something wrong with genearl type discrete check. Vertex not in scopo and not in cotree...");
			fi;
		od;
		
		# General type is discrete if above loop finished. 
		return true;
	else
		Error("Something really bad happend with the group type check.");
	fi;
end;


