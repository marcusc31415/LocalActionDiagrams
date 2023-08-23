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
