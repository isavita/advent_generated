
program LanParty;

{$mode objfpc}{$H+} // Use Free Pascal mode and enable AnsiStrings

uses
  SysUtils; // Provides AnsiString, file operations, string manipulation

const
  InputFileName = 'input.txt';
  // Estimate maximum number of unique computer names. Adjust if needed.
  MaxNodes = 2000;

var
  // Adjacency matrix to represent connections
  Adj: array[1..MaxNodes, 1..MaxNodes] of Boolean;
  // Array to store the unique computer names
  NodeNames: array[1..MaxNodes] of AnsiString;
  // Current count of unique nodes found
  NodeCount: Integer = 0;
  // Counter for triangles meeting the criteria
  TriangleCount: LongInt = 0; // Use LongInt for potentially large counts
  // Loop variables
  i, j, k: Integer;
  // File handling
  inputFile: TextFile;
  line: AnsiString;
  // Temporary variables for parsing
  name1, name2: AnsiString;
  hyphenPos: Integer;
  id1, id2: Integer;
  found: Boolean;

// Function to find the ID of a node name.
// If the name is not found, it adds it to the NodeNames array
// and returns the new ID. Returns 0 if MaxNodes is exceeded.
function FindOrAddNode(const name: AnsiString): Integer;
var
  idx: Integer;
begin
  Result := 0; // Default to 0 (error or not found initially)

  // Search for existing node
  for idx := 1 to NodeCount do
  begin
    if NodeNames[idx] = name then
    begin
      Result := idx; // Found existing node
      Exit;
    end;
  end;

  // Node not found, add it if space allows
  if NodeCount < MaxNodes then
  begin
    Inc(NodeCount);
    NodeNames[NodeCount] := name;
    Result := NodeCount; // Return the new ID
  end
  else
  begin
    // Handle error: Too many nodes
    WriteLn(StdErr, 'Error: Maximum number of nodes (', MaxNodes, ') exceeded.');
    Halt(1); // Terminate program with an error code
  end;
end;

// --- Main Program Logic ---
begin
  // --- Initialization ---
  // Initialize adjacency matrix to all False
  // Note: Global boolean arrays are often initialized to False by default in FPC,
  // but explicit initialization is safer and more portable.
  for i := 1 to MaxNodes do
    for j := 1 to MaxNodes do
      Adj[i, j] := False;

  NodeCount := 0;
  TriangleCount := 0;

  // --- Read Input and Build Graph ---
  AssignFile(inputFile, InputFileName);
  try
    Reset(inputFile); // Open for reading

    while not Eof(inputFile) do
    begin
      ReadLn(inputFile, line);
      line := Trim(line); // Remove leading/trailing whitespace

      if line = '' then continue; // Skip empty lines

      // Find the hyphen separator
      hyphenPos := Pos('-', line);
      if hyphenPos > 0 then
      begin
        // Extract the two names
        name1 := Trim(Copy(line, 1, hyphenPos - 1));
        name2 := Trim(Copy(line, hyphenPos + 1, Length(line) - hyphenPos));

        // Get or create IDs for the nodes
        id1 := FindOrAddNode(name1);
        id2 := FindOrAddNode(name2);

        // Check if FindOrAddNode succeeded (returned non-zero IDs)
        if (id1 > 0) and (id2 > 0) then
        begin
          // Mark connection in adjacency matrix (bidirectional)
          Adj[id1, id2] := True;
          Adj[id2, id1] := True;
        end;
      end
      else
      begin
         WriteLn(StdErr, 'Warning: Malformed line skipped: ', line);
      end;
    end;

  finally
    CloseFile(inputFile); // Ensure file is closed even if errors occur
  end;

  // --- Find Triangles ---
  // Iterate through all unique combinations of three nodes (i, j, k)
  // Ensure i < j < k to avoid duplicates and self-loops
  for i := 1 to NodeCount do
  begin
    for j := i + 1 to NodeCount do
    begin
      // Optimization: Only proceed if i and j are connected
      if Adj[i, j] then
      begin
        for k := j + 1 to NodeCount do
        begin
          // Check if i, j, and k form a triangle
          if Adj[j, k] and Adj[k, i] then // We already know Adj[i,j] is true
          begin
            // Found a triangle (i, j, k). Now check the name condition.
            // Check if at least one name starts with 't'
            // Pascal strings are 1-indexed
            if (Length(NodeNames[i]) > 0) and (NodeNames[i][1] = 't') or
               (Length(NodeNames[j]) > 0) and (NodeNames[j][1] = 't') or
               (Length(NodeNames[k]) > 0) and (NodeNames[k][1] = 't') then
            begin
              Inc(TriangleCount);
            end;
          end;
        end;
      end;
    end;
  end;

  // --- Output Result ---
  WriteLn(TriangleCount);

// Standard Pascal doesn't require an explicit return code,
// but finishing the main block implies success (return code 0).
end.
