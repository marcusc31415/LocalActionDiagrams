
# Example
# G := Group(());; SetPermGroupDomain(G,[1..6]);
# D0 := LocalActionDiagramFromData(DigraphByAdjacencyMatrix(IsImmutableDigraph, [[6]]), [G],[[1], [2], [3], [4], [5], [6]], ());

# D1 := LocalActionDiagramFromData(DigraphByAdjacencyMatrix(IsImmutableDigraph, [[2,1],[1,0]]), [Group((1,2),(3,4),(5,6)), SymmetricGroup(3)],[[1,2], [3,4],[5,6],[1,2,3]], (3,4));
# D2 := LocalActionDiagramFromData(DigraphByAdjacencyMatrix(IsImmutableDigraph, [[2,1],[1,2]]), [Group((1,2),(3,4),(5,6)), Group((1,2),(3,4),(5,6))], [[1,2],[3,4],[5,6],[1,2],[3,4],[5,6]], (3,4));

# G:=Group((4,5));; SetPermGroupDomain(G,[1..5]);
# D3 := LocalActionDiagramFromData(DigraphByAdjacencyMatrix(IsImmutableDigraph, [[1,3],[3,1]]), [G, G], [[4,5],[1],[2],[3],[1],[2],[3],[4,5]], (2,7)(3,6)(4,5));

COLOURS := [["blue"], ["blue", "red"], ["blue", "green", "red"], ["blue", "green", "yellow", "orange"], ["blue", "green", "yellow", "orange", "red"], ["blue", "cyan", "green", "yellow", "orange", "red"]];

GAP_NAMES := ["1", "C2", "C3", "S3", "C2 x C2", "C4", "D8", "A4", "S4", "C5", "C6", "D10", "D12", "C5 : C4", "A5", "S5", "C6", "C2 x C2 x C2", "C4 x C2", "C3 x C3", "C2 x D8", "(C3 x C3) : C2", "C3 x S3", "C2 x A4", "S3 x S3", "(C3 x C3) : C4", "C2 x S4", "(S3 x S3) : C2", "A6", "S6"];
LATEX_NAMES := ["\\langle\\mathrm{id}\\rangle", "C_{2}", "C_{3}", "S_{3}", "C_{2}\\times C_{2}", "C_{4}", "D_{4}", "A_{4}", "S_{4}", "C_{5}", "C_{6}", "D_{5}", "D_{6}", "\\mathrm{AGL}(1,5)", "A_{5}", "S_{5}", "C_{6}", "C_{2}^{3}", "C_{4}\\times C_{2}", "C_{3}\\times C_{3}", "C_{2}\\times D_{4}", "C_{3}^{2}\\rtimes C_{2}", "C_{3}\\times S_{3}", "C_{2}\\times A_{4}", "S_{3}\\times S_{3}", "C_{3}^{2}\\rtimes C_{4}", "C_{2}\\times S_{4}", "S_{3} \\wr C_{2}", "A_{6}", "S_{6}"];

InstallMethod(GenerateTikZCode, [IsLocalActionDiagram],
function(D)
	local grp, deg, grp_name, nr, grps, degs, grp_names, M, arc_labels, arc_reversal, arcs, arc_colours, i, j, domain, orbits, nr_colours, k, l, f;

	if DigraphNrVertices(D) > 2 then
		Error("Drawing is currently only implemented for local action diagrams on up to two vertices. Try Splash(DotDigraph(D)) for more vertices to get an indication.");
	elif DigraphNrVertices(D) = 1 then
		# Setup
		grp := LocalActionDiagramVertexLabels(D)[1];
		deg := Maximum(PermGroupDomain(grp));
		grp_name := LATEX_NAMES[Position(GAP_NAMES, StructureDescription(grp))];
		arc_labels := LocalActionDiagramEdgeLabels(D);
		arc_reversal := LocalActionDiagramEdgeReversal(D);
		# rainbow-y arc colours
		arcs := DigraphEdges(D);
		arc_colours := [];

		domain := Positions(arcs, [1,1]);
		orbits := AsSortedList(Orbits(Group(arc_reversal), domain));
		nr_colours := Size(orbits);
		for k in [1..Length(orbits)] do
			for l in orbits[k] do
				arc_colours[l] := COLOURS[nr_colours][k];
			od;
		od;
#		return arc_colours;
		
		# Drawing
#		f := Filename(DirectoryCurrent(), Concatenation("local_action_diagram", ".tex"));
		f := OutputTextFile( Concatenation("local_action_diagram", ".tex"), false );;
		SetPrintFormattingStatus(f, false);		
		
		PrintTo(f, "\\documentclass{standalone}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\usepackage{tikz}");
		AppendTo(f, "\n");
		AppendTo(f, "\\usepackage{bm}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\usetikzlibrary{decorations.markings}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\begin{document}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		# Picture
		AppendTo(f, "\\begin{tikzpicture}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\begin{scope}[xscale=1, yscale=1, thick]");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\def\\loopangle{10}");
		AppendTo(f, "\n");
		AppendTo(f, "\\def\\loopsize{2}");
		AppendTo(f, "\n");
		AppendTo(f, "\\def\\looplabelsep{0}");
		AppendTo(f, "\n");
		AppendTo(f, "\\def\\vertexlabelsep{1mm}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");	
		
		# Loops
		domain := Positions(arcs, [1,1]);
		AppendTo(f, Concatenation("\\def\\nrloops{", String(Size(arcs)), "}"));
		AppendTo(f, "\n");
		for j in [1..Size(domain)] do
			AppendTo(f, Concatenation("\\draw [", arc_colours[domain[j]], "] (0:0) .. controls ({\\loopangle+", String(j-1), "*(180-2*\\loopangle)/\\nrloops}:\\loopsize) and ({\\loopangle+", String(j), "*(180-2*\\loopangle)/\\nrloops}:\\loopsize) .. (0:0);"));
			AppendTo(f, "\n");
			AppendTo(f, Concatenation("\\node at ({\\loopangle+", String(j-1),"*(180-2*\\loopangle)/\\nrloops+(180-2*\\loopangle)/\\nrloops/2}:\\loopsize-0.3+\\looplabelsep) {\\scriptsize{$\\{", String(arc_labels[domain[j]]){[2..Length(String(arc_labels[domain[j]]))-1]},"\\}$}};"));
			AppendTo(f, "\n");
			AppendTo(f, "\n");
		od;
		
		# Vertices and their labels
		AppendTo(f, "\\node (0) at (0:0) {};");
		AppendTo(f, "\n");
		AppendTo(f, "\\draw [fill] (0) circle [radius=1pt];");
		AppendTo(f, "\n");
		AppendTo(f, Concatenation("\\node [below=\\vertexlabelsep] at (0) {\\scriptsize{$", String(grp_name), "$}};"));
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		
		# End
		AppendTo(f, "\\end{scope}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\end{tikzpicture}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\end{document}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
	else
		# Setup
		nr := DigraphNrVertices(D);
		grps := LocalActionDiagramVertexLabels(D);
		degs := List(grps, grp -> Maximum(PermGroupDomain(grp)));
		grp_names := List(grps, grp -> LATEX_NAMES[Position(GAP_NAMES, StructureDescription(grp))]);
		M := AdjacencyMatrix(D);
		arc_labels := LocalActionDiagramEdgeLabels(D);
		arc_reversal := LocalActionDiagramEdgeReversal(D);
		# rainbow-y arc colours
		arcs := DigraphEdges(D);
		arc_colours := [];
		for i in [1..nr] do
			for j in [i..nr] do
				domain := Union(Positions(arcs, [i,j]), Positions(arcs, [j,i]));
				orbits := AsSortedList(Orbits(Group(arc_reversal), domain));
				nr_colours := Size(orbits);
				for k in [1..Length(orbits)] do
					for l in orbits[k] do
						arc_colours[l] := COLOURS[nr_colours][k];
					od;
				od;
			od;				
		od;	
#		return arc_colours;
		
		# Drawing
#		f := Filename(DirectoryCurrent(), Concatenation("local_action_diagram", ".tex"));
		f := OutputTextFile( Concatenation("local_action_diagram", ".tex"), false );;
		SetPrintFormattingStatus(f, false);
		
		
		PrintTo(f, "\\documentclass{standalone}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\usepackage{tikz}");
		AppendTo(f, "\n");
		AppendTo(f, "\\usepackage{bm}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\usetikzlibrary{decorations.markings}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\begin{document}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		# Picture
		AppendTo(f, "\\begin{tikzpicture}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\begin{scope}[xscale=2, yscale=2, thick]");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, Concatenation("\\def\\nr{", String(nr), "}"));
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\foreach \\n in {0,...,\\nr} {\\node (\\n) at (\\n*360/\\nr:1) {};}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\def\\loopangle{45}");
		AppendTo(f, "\n");
		AppendTo(f, "\\def\\loopsize{1.3}");
		AppendTo(f, "\n");
		AppendTo(f, "\\def\\looplabelsep{-2mm}");
		AppendTo(f, "\n");
		AppendTo(f, "\\def\\edgestart{0.15}");
		AppendTo(f, "\n");
		AppendTo(f, "\\def\\edgesep{0.3}");
		AppendTo(f, "\n");
		AppendTo(f, "\\def\\edgelabelsep{-0.3mm}");
		AppendTo(f, "\n");
		AppendTo(f, "\\def\\vertexlabelsep{3mm}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");	
		
		# Loops
		for i in [1..nr] do
			domain := Positions(arcs, [i,i]);
			if domain = [] then continue; else
				AppendTo(f, Concatenation("\\def\\nrloops", String(i), "{", String(Size(domain)), "}"));
				AppendTo(f, "\n");
				for j in [1..Size(domain)] do
					AppendTo(f, Concatenation("\\begin{scope}[rotate={", String(i-1), "*360/\\nr}, xshift=1cm, decoration={markings, mark=at position 0.5 with {\\node[label={[black, label distance=\\looplabelsep]{", String(i-1), "*360/\\nr-90+\\loopangle+", String(j-1), "*(180-2*\\loopangle)/\\nrloops", String(i), "+(180-2*\\loopangle)/\\nrloops", String(i), "/2}:\\tiny{$\\{", String(arc_labels[domain[j]]){[2..Length(String(arc_labels[domain[j]]))-1]}, "\\}$}}]{};}}] \\draw [", arc_colours[domain[j]], ", postaction=decorate] (0:0) .. controls ({-90+\\loopangle+", String(j-1), "*(180-2*\\loopangle)/\\nrloops", String(i), "}:\\loopsize) and ({-90+\\loopangle+", String(j), "*(180-2*\\loopangle)/\\nrloops", String(i), "}:\\loopsize) .. (0:0); \\end{scope}"));
					AppendTo(f, "\n");
					AppendTo(f, "\n");
				od;
			fi;
		od;
		
		# Arcs (only works for one or two vertices at the moment, TODO: improve this to work for any number of vertices, the rest of the code should be mostly fine)
		# 1 to 2
		domain := Positions(arcs, [1,2]);
		if not domain = [] then
			AppendTo(f, Concatenation("\\def\\nrarcs_", String(i), "_", String(j), "{", String(Size(domain)), "}"));
			AppendTo(f, "\n");
			for k in [1..Size(domain)] do
				# arc
				AppendTo(f, Concatenation("\\begin{scope} [decoration={markings, mark=at position 0.5 with {\\arrow{>}}, mark=at position 0.5 with {\\node[rotate=-90+180/\\nr, above=\\edgelabelsep]{\\tiny{$\\{", String(arc_labels[domain[k]]){[2..Length(String(arc_labels[domain[k]]))-1]}, "\\}$}};} }] \\draw [", arc_colours[domain[k]], ", postaction=decorate] (0:1) .. controls (360/\\nr/3:\\edgestart+", String(k-1), "*\\edgesep) and (2*360/\\nr/3:\\edgestart+", String(k-1), "*\\edgesep) .. (360/\\nr:1); \\end{scope}"));
				AppendTo(f, "\n");
				AppendTo(f, "\n");
			od;
		fi;
		
		
		# 2 to 1
		domain := Positions(arcs, [2,1]);
		if not domain = [] then
			AppendTo(f, Concatenation("\\def\\nrarcs_", String(i), "_", String(j), "{", String(Size(domain)), "}"));
			AppendTo(f, "\n");
			for k in [1..Size(domain)] do
				# arc
				AppendTo(f, Concatenation("\\begin{scope} [decoration={markings, mark=at position 0.5 with {\\arrow{<}}, mark=at position 0.5 with {\\node[rotate=-90+180/\\nr, below=\\edgelabelsep]{\\tiny{$\\{", String(arc_labels[domain[k]]){[2..Length(String(arc_labels[domain[k]]))-1]}, "\\}$}};} }] \\draw [", arc_colours[domain[k]], ", postaction=decorate, xshift=1cm, rotate=360/\\nr/2, xscale=-1, rotate=-360/\\nr/2, xshift=-1cm] (0:1) .. controls (360/\\nr/3:\\edgestart+", String(k-1), "*\\edgesep) and (2*360/\\nr/3:\\edgestart+", String(k-1), "*\\edgesep) .. (360/\\nr:1); \\end{scope}"));
				AppendTo(f, "\n");
				AppendTo(f, "\n");
			od;
		fi;		
		
		# Vertices and their labels
		for i in [1..nr] do
			AppendTo(f, Concatenation("\\node (", String(i), ") at (", String(i), "*360/\\nr:1) {};"));
			AppendTo(f, "\n");
			AppendTo(f, Concatenation("\\draw [fill] (", String(i), ") circle [radius=1pt];"));
			AppendTo(f, "\n");
			AppendTo(f, Concatenation("\\node [below=\\vertexlabelsep] at (", String(i), ") {\\tiny{$", String(grp_names[i]), "$}};"));
			AppendTo(f, "\n");

		od;
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		
		# End
		AppendTo(f, "\\end{scope}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\end{tikzpicture}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\end{document}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");			
	fi;
end);














InstallMethod(GenerateTikZCode, [IsLocalActionDiagram, IsList],
function(D, scopo)
	local grp, deg, grp_name, nr, grps, degs, grp_names, M, arc_labels, arc_reversal, arcs, arc_colours, i, j, domain, orbits, nr_colours, k, l, f;

	if not ForAll(scopo, i -> i in [1..DigraphNrEdges(D)]) then
		Error("The scopo must be set of indices of list of edges of D");
	elif DigraphNrVertices(D) > 2 then
		Error("Drawing is currently only implemented for local action diagrams on up to two vertices. Try Splash(DotDigraph(D)) for more vertices to get an indication.");
	elif DigraphNrVertices(D) = 1 then
		# Setup
		grp := LocalActionDiagramVertexLabels(D)[1];
		deg := Maximum(PermGroupDomain(grp));
		grp_name := LATEX_NAMES[Position(GAP_NAMES, StructureDescription(grp))];
		arc_labels := LocalActionDiagramEdgeLabels(D);
		arc_reversal := LocalActionDiagramEdgeReversal(D);
		# rainbow-y arc colours
		arcs := DigraphEdges(D);
		arc_colours := [];

		domain := Positions(arcs, [1,1]);
		orbits := AsSortedList(Orbits(Group(arc_reversal), domain));
		nr_colours := Size(orbits);
		for k in [1..Length(orbits)] do
			for l in orbits[k] do
				arc_colours[l] := COLOURS[nr_colours][k];
			od;
		od;
#		return arc_colours;
		
		# Drawing
#		f := Filename(DirectoryCurrent(), Concatenation("local_action_diagram", ".tex"));
		f := OutputTextFile( Concatenation("local_action_diagram", ".tex"), false );;
		SetPrintFormattingStatus(f, false);		
		
		PrintTo(f, "\\documentclass{standalone}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\usepackage{tikz}");
		AppendTo(f, "\n");
		AppendTo(f, "\\usepackage{bm}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\usetikzlibrary{decorations.markings}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\begin{document}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		# Picture
		AppendTo(f, "\\begin{tikzpicture}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\begin{scope}[xscale=1, yscale=1, thick]");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\def\\loopangle{10}");
		AppendTo(f, "\n");
		AppendTo(f, "\\def\\loopsize{2}");
		AppendTo(f, "\n");
		AppendTo(f, "\\def\\looplabelsep{0}");
		AppendTo(f, "\n");
		AppendTo(f, "\\def\\vertexlabelsep{1mm}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");	
		
		# Loops
		domain := Positions(arcs, [1,1]);
		AppendTo(f, Concatenation("\\def\\nrloops{", String(Size(arcs)), "}"));
		AppendTo(f, "\n");
		for j in [1..Size(domain)] do
			if domain[j] in scopo then
				AppendTo(f, Concatenation("\\draw [dashed, ", arc_colours[domain[j]], "] (0:0) .. controls ({\\loopangle+", String(j-1), "*(180-2*\\loopangle)/\\nrloops}:\\loopsize) and ({\\loopangle+", String(j), "*(180-2*\\loopangle)/\\nrloops}:\\loopsize) .. (0:0);"));
				AppendTo(f, "\n");
				AppendTo(f, Concatenation("\\node at ({\\loopangle+", String(j-1),"*(180-2*\\loopangle)/\\nrloops+(180-2*\\loopangle)/\\nrloops/2}:\\loopsize-0.3+\\looplabelsep) {\\scriptsize{$\\{", String(arc_labels[domain[j]]){[2..Length(String(arc_labels[domain[j]]))-1]},"\\}$}};"));
				AppendTo(f, "\n");
				AppendTo(f, "\n");
			else
				AppendTo(f, Concatenation("\\draw [", arc_colours[domain[j]], "] (0:0) .. controls ({\\loopangle+", String(j-1), "*(180-2*\\loopangle)/\\nrloops}:\\loopsize) and ({\\loopangle+", String(j), "*(180-2*\\loopangle)/\\nrloops}:\\loopsize) .. (0:0);"));
				AppendTo(f, "\n");
				AppendTo(f, Concatenation("\\node at ({\\loopangle+", String(j-1),"*(180-2*\\loopangle)/\\nrloops+(180-2*\\loopangle)/\\nrloops/2}:\\loopsize-0.3+\\looplabelsep) {\\scriptsize{$\\{", String(arc_labels[domain[j]]){[2..Length(String(arc_labels[domain[j]]))-1]},"\\}$}};"));
				AppendTo(f, "\n");
				AppendTo(f, "\n");
			fi;
		od;
		
		# Vertices and their labels
		AppendTo(f, "\\node (0) at (0:0) {};");
		AppendTo(f, "\n");
		AppendTo(f, "\\draw [fill] (0) circle [radius=1pt];");
		AppendTo(f, "\n");
		AppendTo(f, Concatenation("\\node [below=\\vertexlabelsep] at (0) {\\scriptsize{$", String(grp_name), "$}};"));
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		
		# End
		AppendTo(f, "\\end{scope}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\end{tikzpicture}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\end{document}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
	else
		# Setup
		nr := DigraphNrVertices(D);
		grps := LocalActionDiagramVertexLabels(D);
		degs := List(grps, grp -> Maximum(PermGroupDomain((grp))));
		grp_names := List(grps, grp -> LATEX_NAMES[Position(GAP_NAMES, StructureDescription(grp))]);
		M := AdjacencyMatrix(D);
		arc_labels := LocalActionDiagramEdgeLabels(D);
		arc_reversal := LocalActionDiagramEdgeReversal(D);
		# rainbow-y arc colours
		arcs := DigraphEdges(D);
		arc_colours := [];
		for i in [1..nr] do
			for j in [i..nr] do
				domain := Union(Positions(arcs, [i,j]), Positions(arcs, [j,i]));
				orbits := AsSortedList(Orbits(Group(arc_reversal), domain));
				nr_colours := Size(orbits);
				for k in [1..Length(orbits)] do
					for l in orbits[k] do
						arc_colours[l] := COLOURS[nr_colours][k];
					od;
				od;
			od;				
		od;	
#		return arc_colours;
		
		# Drawing
#		f := Filename(DirectoryCurrent(), Concatenation("local_action_diagram", ".tex"));
		f := OutputTextFile( Concatenation("local_action_diagram", ".tex"), false );;
		SetPrintFormattingStatus(f, false);
		
		
		PrintTo(f, "\\documentclass{standalone}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\usepackage{tikz}");
		AppendTo(f, "\n");
		AppendTo(f, "\\usepackage{bm}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\usetikzlibrary{decorations.markings}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\begin{document}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		# Picture
		AppendTo(f, "\\begin{tikzpicture}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\begin{scope}[xscale=2, yscale=2, thick]");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, Concatenation("\\def\\nr{", String(nr), "}"));
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\foreach \\n in {0,...,\\nr} {\\node (\\n) at (\\n*360/\\nr:1) {};}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\def\\loopangle{45}");
		AppendTo(f, "\n");
		AppendTo(f, "\\def\\loopsize{1.3}");
		AppendTo(f, "\n");
		AppendTo(f, "\\def\\looplabelsep{-2mm}");
		AppendTo(f, "\n");
		AppendTo(f, "\\def\\edgestart{0.15}");
		AppendTo(f, "\n");
		AppendTo(f, "\\def\\edgesep{0.3}");
		AppendTo(f, "\n");
		AppendTo(f, "\\def\\edgelabelsep{-0.3mm}");
		AppendTo(f, "\n");
		AppendTo(f, "\\def\\vertexlabelsep{3mm}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");	
		
		# Loops
		for i in [1..nr] do
			domain := Positions(arcs, [i,i]);
			if domain = [] then continue; else
				AppendTo(f, Concatenation("\\def\\nrloops", String(i), "{", String(Size(domain)), "}"));
				AppendTo(f, "\n");
				for j in [1..Size(domain)] do
					if domain[j] in scopo then
						AppendTo(f, Concatenation("\\begin{scope}[rotate={", String(i-1), "*360/\\nr}, xshift=1cm, decoration={markings, mark=at position 0.5 with {\\node[label={[black, label distance=\\looplabelsep]{", String(i-1), "*360/\\nr-90+\\loopangle+", String(j-1), "*(180-2*\\loopangle)/\\nrloops", String(i), "+(180-2*\\loopangle)/\\nrloops", String(i), "/2}:\\tiny{$\\{", String(arc_labels[domain[j]]){[2..Length(String(arc_labels[domain[j]]))-1]}, "\\}$}}]{};}}] \\draw [dashed,", arc_colours[domain[j]], ", postaction=decorate] (0:0) .. controls ({-90+\\loopangle+", String(j-1), "*(180-2*\\loopangle)/\\nrloops", String(i), "}:\\loopsize) and ({-90+\\loopangle+", String(j), "*(180-2*\\loopangle)/\\nrloops", String(i), "}:\\loopsize) .. (0:0); \\end{scope}"));
					else
						AppendTo(f, Concatenation("\\begin{scope}[rotate={", String(i-1), "*360/\\nr}, xshift=1cm, decoration={markings, mark=at position 0.5 with {\\node[label={[black, label distance=\\looplabelsep]{", String(i-1), "*360/\\nr-90+\\loopangle+", String(j-1), "*(180-2*\\loopangle)/\\nrloops", String(i), "+(180-2*\\loopangle)/\\nrloops", String(i), "/2}:\\tiny{$\\{", String(arc_labels[domain[j]]){[2..Length(String(arc_labels[domain[j]]))-1]}, "\\}$}}]{};}}] \\draw [", arc_colours[domain[j]], ", postaction=decorate] (0:0) .. controls ({-90+\\loopangle+", String(j-1), "*(180-2*\\loopangle)/\\nrloops", String(i), "}:\\loopsize) and ({-90+\\loopangle+", String(j), "*(180-2*\\loopangle)/\\nrloops", String(i), "}:\\loopsize) .. (0:0); \\end{scope}"));
					fi;
					AppendTo(f, "\n");
					AppendTo(f, "\n");
				od;
			fi;
		od;
		
		# Arcs (only works for one or two vertices at the moment, TODO: improve this to work for any number of vertices, the rest of the code should be mostly fine)
		# 1 to 2
		domain := Positions(arcs, [1,2]);
		if not domain = [] then
			AppendTo(f, Concatenation("\\def\\nrarcs_", String(i), "_", String(j), "{", String(Size(domain)), "}"));
			AppendTo(f, "\n");
			for k in [1..Size(domain)] do
				# arc
				if domain[k] in scopo then
					AppendTo(f, Concatenation("\\begin{scope} [decoration={markings, mark=at position 0.5 with {\\arrow{>}}, mark=at position 0.5 with {\\node[rotate=-90+180/\\nr, above=\\edgelabelsep]{\\tiny{$\\{", String(arc_labels[domain[k]]){[2..Length(String(arc_labels[domain[k]]))-1]}, "\\}$}};} }] \\draw [dashed,", arc_colours[domain[k]], ", postaction=decorate] (0:1) .. controls (360/\\nr/3:\\edgestart+", String(k-1), "*\\edgesep) and (2*360/\\nr/3:\\edgestart+", String(k-1), "*\\edgesep) .. (360/\\nr:1); \\end{scope}"));
				else
					AppendTo(f, Concatenation("\\begin{scope} [decoration={markings, mark=at position 0.5 with {\\arrow{>}}, mark=at position 0.5 with {\\node[rotate=-90+180/\\nr, above=\\edgelabelsep]{\\tiny{$\\{", String(arc_labels[domain[k]]){[2..Length(String(arc_labels[domain[k]]))-1]}, "\\}$}};} }] \\draw [", arc_colours[domain[k]], ", postaction=decorate] (0:1) .. controls (360/\\nr/3:\\edgestart+", String(k-1), "*\\edgesep) and (2*360/\\nr/3:\\edgestart+", String(k-1), "*\\edgesep) .. (360/\\nr:1); \\end{scope}"));
				fi;
				AppendTo(f, "\n");
				AppendTo(f, "\n");
			od;
		fi;
		
		
		# 2 to 1
		domain := Positions(arcs, [2,1]);
		if not domain = [] then
			AppendTo(f, Concatenation("\\def\\nrarcs_", String(i), "_", String(j), "{", String(Size(domain)), "}"));
			AppendTo(f, "\n");
			for k in [1..Size(domain)] do
				# arc
				if domain[k] in scopo then
					AppendTo(f, Concatenation("\\begin{scope} [decoration={markings, mark=at position 0.5 with {\\arrow{<}}, mark=at position 0.5 with {\\node[rotate=-90+180/\\nr, below=\\edgelabelsep]{\\tiny{$\\{", String(arc_labels[domain[k]]){[2..Length(String(arc_labels[domain[k]]))-1]}, "\\}$}};} }] \\draw [dashed,", arc_colours[domain[k]], ", postaction=decorate, xshift=1cm, rotate=360/\\nr/2, xscale=-1, rotate=-360/\\nr/2, xshift=-1cm] (0:1) .. controls (360/\\nr/3:\\edgestart+", String(k-1), "*\\edgesep) and (2*360/\\nr/3:\\edgestart+", String(k-1), "*\\edgesep) .. (360/\\nr:1); \\end{scope}"));
				else
					AppendTo(f, Concatenation("\\begin{scope} [decoration={markings, mark=at position 0.5 with {\\arrow{<}}, mark=at position 0.5 with {\\node[rotate=-90+180/\\nr, below=\\edgelabelsep]{\\tiny{$\\{", String(arc_labels[domain[k]]){[2..Length(String(arc_labels[domain[k]]))-1]}, "\\}$}};} }] \\draw [", arc_colours[domain[k]], ", postaction=decorate, xshift=1cm, rotate=360/\\nr/2, xscale=-1, rotate=-360/\\nr/2, xshift=-1cm] (0:1) .. controls (360/\\nr/3:\\edgestart+", String(k-1), "*\\edgesep) and (2*360/\\nr/3:\\edgestart+", String(k-1), "*\\edgesep) .. (360/\\nr:1); \\end{scope}"));
				fi;
				AppendTo(f, "\n");
				AppendTo(f, "\n");
			od;
		fi;

		
		
		
#		for i in [1..nr] do
#			for j in [1..nr] do
#				if not j=i then
#					# clockwise arcs
#					if j-i in [1,-2] then
#						domain := Positions(arcs, [i,j]);
#						if domain = [] then continue; else
#							AppendTo(f, Concatenation("\\def\\nrarcs_", String(i), "_", String(j), "{", String(Size(domain)), "}"));
#							AppendTo(f, "\n");
#							for k in [1..Size(domain)] do
#								# arc
#								AppendTo(f, Concatenation("\\begin{scope} [decoration={markings, mark=at position 0.5 with {\\arrow{>}}, mark=at position 0.5 with {\\node[rotate=-90+180/\\nr, above]{\\tiny{$\\{", String(arc_labels[domain[k]]){[2..Length(String(arc_labels[domain[k]]))-1]}, "\\}$}};} }] \\draw [", arc_colours[domain[k]], ", postaction=decorate, rotate={(", String(i), "-1)*360/\\nr}] (0:1) .. controls (360/\\nr/3:\\edgestart+", String(k-1), "*\\edgesep) and (2*360/\\nr/3:\\edgestart+", String(k-1), "*\\edgesep) .. (360/\\nr:1); \\end{scope}"));
#								AppendTo(f, "\n");
#								AppendTo(f, "\n");
#							od;
#						fi;
#					else
#						# counter-clockwise arcs
#					fi;
#				fi;
#			od;
#		od;
		
		
		
		# Vertices and their labels
		for i in [1..nr] do
			AppendTo(f, Concatenation("\\node (", String(i), ") at (", String(i), "*360/\\nr:1) {};"));
			AppendTo(f, "\n");
			AppendTo(f, Concatenation("\\draw [fill] (", String(i), ") circle [radius=1pt];"));
			AppendTo(f, "\n");
			AppendTo(f, Concatenation("\\node [below=\\vertexlabelsep] at (", String(i), ") {\\tiny{$", String(grp_names[i]), "$}};"));
			AppendTo(f, "\n");

		od;
		AppendTo(f, "\n");
		AppendTo(f, "\n");

		
		# End
		AppendTo(f, "\\end{scope}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\end{tikzpicture}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
		AppendTo(f, "\\end{document}");
		AppendTo(f, "\n");
		AppendTo(f, "\n");
			
	fi;
end);
