_LAD_WriteSingleLADStringFormat@ := function(lad)
	local output_string, vl, el, attr, attr_list, attr_output_list;

	output_string := "";

	# First write the digraph using the digraph package IO. 
	output_string := Concatenation(output_string, PrintString(DigraphImmutableCopy(lad))); 
	output_string := Concatenation(output_string, "\n");
	
	# Store the vertex labels.
	for vl in LocalActionDiagramVertexLabels(lad) do
		output_string := Concatenation(output_string, String(vl));
		output_string := Concatenation(output_string, "|");
	od;
	Remove(output_string); # Remove the trailing separator. 
	output_string := Concatenation(output_string, "\n");

	# Store the edge labels.
	for el in LocalActionDiagramEdgeLabels(lad) do
		output_string := Concatenation(output_string, String(el));
		output_string := Concatenation(output_string, "|");
	od;
	Remove(output_string); # Remove the trailing separator. 
	output_string := Concatenation(output_string, "\n");

	# Store the edge reversal.
	output_string := Concatenation(output_string, String(LocalActionDiagramEdgeReversal(lad)));
	output_string := Concatenation(output_string, "\n");
	
	# Store all known attributes of the object. 
	# Does not compute any unknown attributes. 
	attr_list := ["LocalActionDiagramScopos", "LocalActionDiagramGroupType", "LocalActionDiagramIsDiscrete"];
	attr_output_list := [];

	for attr in attr_list do
		if attr in KnownAttributesOfObject(x) then
			Add(attr_output_list, EvalString(attr)(x));
		fi;
	od;

	output_string := Concatenation(output_string, String(attr_output_list));
	output_string := Concatenation(output_string, "\n");

	return output_string;
end;

_LAD_ReadLADString@ := function(lad_string)
	local input_lines, lad_list, idx_x, line, digraph, v_labels, v_label, e_labels, e_label, rev, lad, attr_list, idx_y, lad_attrs, input_line;

	input_lines := SplitString(lad_string, "\n");

	lad_list := [];
	idx_x := 1; 

	attr_list := ["LocalActionDiagramScopos", "LocalActionDiagramGroupType", "LocalActionDiagramIsDiscrete"];

	input_line := 0;
	for line in input_lines do
		input_line := input_line+1;
		if idx_x = 1 then
			# Read the digraph. 
			digraph := EvalString(line);
			if IsDigraph(digraph) = false then
				return input_line;
			fi;
			idx_x := idx_x+1;
		elif idx_x = 2 then
			# Read the vertex labels. 
			v_labels := [];
			for v_label in SplitString(line, "|") do
				Add(v_labels, EvalString(v_label));
			od;
			if ForAll(v_labels, IsGroup) = false then
				return input_line;
			fi;
			idx_x := idx_x+1;
		elif idx_x = 3 then
			# Read the edge labels. 
			e_labels := [];
			for e_label in SplitString(line, "|") do
				Add(e_labels, EvalString(e_label));
			od;
			if ForAll(e_labels, IsList) = false then
				return input_line;
			fi;
			idx_x := idx_x+1;
		elif idx_x = 4 then
			# Read the edge reversal and create the local action diagram. 
			rev := EvalString(line);
			if IsPerm(rev) = false then
				return input_line;
			fi;
			idx_x := idx_x+1;
			lad := LocalActionDiagramFromData(digraph, v_labels, e_labels, rev);
		else
			# Read the stored attributes of the local action diagram and set them. 
			lad_attrs := EvalString(line);
			if IsList(lad_attrs) = false then
				return input_line;
			fi;
			for idx_y in [1..Length(attr_list)] do
				if IsBound(lad_attrs[idx_y]) then
					EvalString(Concatenation("Set", attr_list[idx_y]))(lad, lad_attrs[idx_y]);
				fi;
			od;
			# Add the local action diagram to the list and reset the index. 
			Add(lad_list, lad);
			idx_x := 1;
		fi;
	od;

	return lad_list;
end;

InstallMethod(WriteLocalActionDiagram, "Label This", [IsLocalActionDiagram, IsString],
function(lad, file_name)
	local output_string, output_stream, vl, el, fl_name;
	
	output_string := _LAD_WriteSingleLADStringFormat@(lad);

	fl_name := Filename(DirectoryCurrent(), StringFormatted("{1}.lad", file_name));
	output_stream := OutputTextFile(fl_name, true);
	AppendTo(output_stream, output_string);
	CloseStream(output_stream);

	return true;
end);

InstallMethod(WriteLocalActionDiagram, "Label This", [IsLocalActionDiagram, IsString, IsDirectory],
function(lad, file_name, dir)
	local output_string, output_stream, vl, el, fl_name;

	# dir![1] is the string of the directory dir. Yes... I looked through the source code
	# to find this out. 
	if DirectoryContents(dir) = fail then
		ErrorNoReturn(StringFormatted("Directory <{1}> does not exist.", dir![1]));
		return fail;
	fi;
	
	output_string := _LAD_WriteSingleLADStringFormat@(lad);

	fl_name := Filename(dir, StringFormatted("{1}.lad", file_name));
	output_stream := OutputTextFile(fl_name, true);
	AppendTo(output_stream, output_string);
	CloseStream(output_stream);

	return true;
end);

InstallMethod(WriteLocalActionDiagram, "Label This", [IsLocalActionDiagram, IsString, IsString],
function(lad, file_name, dir_string)
	local dir, output_string, output_stream, vl, el, fl_name;

	dir := Directory(dir_string);

	if DirectoryContents(dir) = fail then
		ErrorNoReturn(StringFormatted("Directory <{1}> does not exist.", dir_string));
		return fail;
	fi;
	
	output_string := _LAD_WriteSingleLADStringFormat@(lad);

	fl_name := Filename(dir, StringFormatted("{1}.lad", file_name));
	output_stream := OutputTextFile(fl_name, true);
	AppendTo(output_stream, output_string);
	CloseStream(output_stream);

	return true;
end);


InstallMethod(WriteLocalActionDiagram, "Label This", [IsList, IsString],
function(lad_list, file_name)
	local lad, output_string, output_stream, vl, el, fl_name;


	if not ForAll(lad_list, IsLocalActionDiagram) then
		ErrorNoReturn("The argument <lad_list> must be a dense list of local action diagrams.");
		return fail;
	fi;
	
	output_string := "";
	for lad in lad_list do
		output_string := Concatenation(output_string, _LAD_WriteSingleLADStringFormat@(lad));
	od;


	fl_name := Filename(DirectoryCurrent(), StringFormatted("{1}.lad", file_name));
	output_stream := OutputTextFile(fl_name, true);
	AppendTo(output_stream, output_string);
	CloseStream(output_stream);

	return true;
end);

InstallMethod(WriteLocalActionDiagram, "Label This", [IsList, IsString, IsDirectory],
function(lad_list, file_name, dir)
	local lad, output_string, output_stream, vl, el, fl_name;
	
	if DirectoryContents(dir) = fail then
		ErrorNoReturn(StringFormatted("Directory <{1}> does not exist.", dir![1]));
		return fail;
	fi;

	if not ForAll(lad_list, IsLocalActionDiagram) then
		ErrorNoReturn("The argument <lad_list> must be a dense list of local action diagrams.");
		return fail;
	fi;
	
	output_string := "";
	for lad in lad_list do
		output_string := Concatenation(output_string, _LAD_WriteSingleLADStringFormat@(lad));
	od;

	fl_name := Filename(dir, StringFormatted("{1}.lad", file_name));
	output_stream := OutputTextFile(fl_name, true);
	AppendTo(output_stream, output_string);
	CloseStream(output_stream);

	return true;
end);

InstallMethod(WriteLocalActionDiagram, "Label This", [IsList, IsString, IsString],
function(lad_list, file_name, dir_string)
	local lad, output_string, output_stream, vl, el, fl_name, dir;

	dir := Directory(dir_string);
	
	if DirectoryContents(dir) = fail then
		ErrorNoReturn(StringFormatted("Directory <{1}> does not exist.", dir_string));
		return fail;
	fi;

	if not ForAll(lad_list, IsLocalActionDiagram) then
		ErrorNoReturn("The argument <lad_list> must be a dense list of local action diagrams.");
		return fail;
	fi;
	
	output_string := "";
	for lad in lad_list do
		output_string := Concatenation(output_string, _LAD_WriteSingleLADStringFormat@(lad));
	od;

	fl_name := Filename(dir, StringFormatted("{1}.lad", file_name));
	output_stream := OutputTextFile(fl_name, true);
	AppendTo(output_stream, output_string);
	CloseStream(output_stream);

	return true;
end);



InstallMethod(ReadLocalActionDiagram, "Label This", [IsString],
function(file_name)
	local lad_list, input_string, fl_name, input_stream;

	# Read the file *file_name* and write its contents to *input_string*.
	fl_name := Filename(DirectoryCurrent(), file_name);
	input_stream := InputTextFile(fl_name);
	if input_stream = fail then
		ErrorNoReturn(StringFormatted("File <{1}> does not exist.", file_name));
		return fail;
	fi;
	input_string := ReadAll(input_stream);
	CloseStream(input_stream);

	lad_list := _LAD_ReadLADString@(input_string);

	if IsInt(lad_list) then
		ErrorNoReturn(StringFormatted("Error on line {1} of {2}", lad_list, file_name));
		return fail;
	fi;

	return lad_list;
end);

InstallMethod(ReadLocalActionDiagram, "Label This", [IsString, IsDirectory],
function(file_name, dir)
	local lad_list, input_string, fl_name, input_stream;

	if DirectoryContents(dir) = fail then
		ErrorNoReturn(StringFormatted("Directory <{1}> does not exist.", dir![1]));
		return fail;
	fi;

	# Read the file *file_name* and write its contents to *input_string*.
	fl_name := Filename(dir, file_name);
	input_stream := InputTextFile(fl_name);
	if input_stream = fail then
		ErrorNoReturn(StringFormatted("File <{1}> does not exist.", file_name));
		return fail;
	fi;
	input_string := ReadAll(input_stream);
	CloseStream(input_stream);

	lad_list := _LAD_ReadLADString@(input_string);

	if IsInt(lad_list) then
		ErrorNoReturn(StringFormatted("Error on line {1} of {2}", lad_list, file_name));
		return fail;
	fi;

	return lad_list;
end);

InstallMethod(ReadLocalActionDiagram, "Label This", [IsString, IsString],
function(file_name, dir_string)
	local lad_list, input_string, fl_name, input_stream, dir;

	dir := Directory(dir_string);

	if DirectoryContents(dir) = fail then
		ErrorNoReturn(StringFormatted("Directory <{1}> does not exist.", dir_string));
		return fail;
	fi;

	# Read the file *file_name* and write its contents to *input_string*.
	fl_name := Filename(dir, file_name);
	input_stream := InputTextFile(fl_name);
	if input_stream = fail then
		ErrorNoReturn(StringFormatted("File <{1}> does not exist.", file_name));
		return fail;
	fi;
	input_string := ReadAll(input_stream);
	CloseStream(input_stream);

	lad_list := _LAD_ReadLADString@(input_string);

	if IsInt(lad_list) then
		ErrorNoReturn(StringFormatted("Error on line {1} of {2}", lad_list, file_name));
		return fail;
	fi;

	return lad_list;
end);

