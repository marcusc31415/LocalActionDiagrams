InstallMethod(AutomorphismGroup, "Automorphism Group of RSGraph", [IsRSGraph],
function(graph1, graphs2)
	local StandardGraph;

	# Function that converts vertex and arc ids to a sequential dense list.
	# I.e. *N* vertices will have ids [1..N] and *M* arcs will have ids [1..M]. 
	StandardGraph := function(graph)
		local arc_ids, vertex_ids;
	end;
end);

# Function for automorphism group of RSGraph (aut*rev = rev*aut). 
# Function for an isomorphism between RSGraphs (ism*rev = rev*ism). 
# Function for an isomorphism between local action diagrams. 
#
# Use digraphs for this. The NautyTracesInterface behaves very weirdly 
# with multiple edges. Best to avoid it. 
