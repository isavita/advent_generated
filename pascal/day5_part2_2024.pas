
program Solve;

{$mode delphi}{$H+}

uses
  SysUtils, Math; // Added Math for Max function

type
  TIntArray = array of integer;
  TPair = record
    x, y: integer;
  end;
  TRuleArray = array of TPair;
  TUpdatesArray = array of TIntArray;

  // Helper type for mapping pages to indices and vice-versa in CorrectOrder
  TPageMapItem = record
    page: integer;
    index: integer;
  end;
  TPageMap = array of TPageMapItem;

  // Adjacency list for the graph in CorrectOrder
  TAdjList = array of TIntArray;


// --- Helper Functions ---

// Function to split a string by a delimiter
function SplitString(const s, delimiter: string): TArray<string>;
var
  start, posIdx: Integer;
  item: string;
  count: Integer;
begin
  SetLength(Result, 0);
  start := 1;
  count := 0;
  while start <= Length(s) do
  begin
    posIdx := Pos(delimiter, Copy(s, start, Length(s) - start + 1));
    if posIdx = 0 then
    begin
      item := Trim(Copy(s, start, Length(s) - start + 1));
      if item <> '' then
      begin
        SetLength(Result, count + 1);
        Result[count] := item;
        Inc(count);
      end;
      Break;
    end
    else
    begin
      item := Trim(Copy(s, start, posIdx - 1));
       if item <> '' then // Handle potential empty strings between delimiters
       begin
         SetLength(Result, count + 1);
         Result[count] := item;
         Inc(count);
       end;
      start := start + posIdx + Length(delimiter) - 1;
    end;
  end;
end;

// Function to convert an array of strings to an array of integers
function StringsToInts(const arr: TArray<string>): TIntArray;
var
  i: Integer;
begin
  SetLength(Result, Length(arr));
  for i := 0 to High(arr) do
  begin
    Result[i] := StrToIntDef(Trim(arr[i]), 0); // Use StrToIntDef for safety
  end;
end;

// Function to find the index of a value in an integer array
function IndexOf(value: integer; const arr: TIntArray): integer;
var
  i: Integer;
begin
  Result := -1; // Not found by default
  for i := 0 to High(arr) do
  begin
    if arr[i] = value then
    begin
      Result := i;
      Exit; // Found, exit function
    end;
  end;
end;

// --- Core Logic Functions ---

// Checks if an update sequence respects the rules
function IsCorrect(const update: TIntArray; const rules: TRuleArray): boolean;
var
  i, posX, posY: Integer;
  rule: TPair;
begin
  Result := True; // Assume correct initially
  for i := 0 to High(rules) do
  begin
    rule := rules[i];
    posX := IndexOf(rule.x, update);
    posY := IndexOf(rule.y, update);

    // If both pages exist in the update, check their order
    if (posX >= 0) and (posY >= 0) then
    begin
      if posX > posY then
      begin
        Result := False; // Found a violation
        Exit; // No need to check further
      end;
    end;
  end;
end;

// Reorders an update sequence to satisfy rules using topological sort
function CorrectOrder(const update: TIntArray; const rules: TRuleArray): TIntArray;
var
  i, j, k, u, v, pageCount, qHead, qTail : integer;
  uniquePages: TIntArray;      // Stores unique page numbers from update
  pageToIndex: TPageMap;     // Maps page number -> temp index (0..N-1)
  indexToPage: TIntArray;      // Maps temp index -> page number
  adj: TAdjList;             // Adjacency list using temp indices
  inDegree: TIntArray;         // In-degree count using temp indices
  queue: TIntArray;            // Queue for topological sort (temp indices)
  sortedIndices: TIntArray;    // Result of topo sort (temp indices)
  found: boolean;
  rule: TPair;
  idxX, idxY: integer;
  maxPage: integer; // For sizing temporary map if needed

  // Helper function to get the temporary index for a page number
  function GetIndex(page: integer): integer;
  var l, h, m: integer;
  begin
      // Efficient lookup using the sorted indexToPage array
      l := 0;
      h := High(indexToPage);
      Result := -1;
      while l <= h do
      begin
        m := (l + h) div 2;
        if indexToPage[m] = page then exit(m);
        if indexToPage[m] < page then l := m + 1
        else h := m - 1;
      end;
  end;

  // Simple QuickSort for sorting unique pages for faster lookup
  procedure QuickSort(var A: TIntArray; L, R: Integer);
  var i, j, p, t : integer;
  begin
    if L < R then begin
      i := L; j := R; p := A[(L+R) div 2];
      repeat
        while A[i] < p do Inc(i);
        while A[j] > p do Dec(j);
        if i <= j then begin
          t := A[i]; A[i] := A[j]; A[j] := t;
          Inc(i); Dec(j);
        end;
      until i > j;
      QuickSort(A, L, j);
      QuickSort(A, i, R);
    end;
  end;

begin
  // 1. Extract unique pages and build mappings
  SetLength(uniquePages, 0);
  pageCount := 0;
  if Length(update) = 0 then begin
    SetLength(Result, 0);
    Exit;
  end;

  // Find unique pages (simple O(N^2), could optimize with sort/hash)
  for i := 0 to High(update) do
  begin
    found := false;
    for j := 0 to High(uniquePages) do
    begin
      if uniquePages[j] = update[i] then
      begin
        found := true;
        break;
      end;
    end;
    if not found then
    begin
       SetLength(uniquePages, Length(uniquePages) + 1);
       uniquePages[High(uniquePages)] := update[i];
    end;
  end;
  pageCount := Length(uniquePages);

  // Sort unique pages for efficient GetIndex lookup
  QuickSort(uniquePages, 0, High(uniquePages));
  indexToPage := uniquePages; // Now sorted

  // 2. Build Graph (adjacency list and in-degrees) using indices
  SetLength(adj, pageCount);
  for i := 0 to pageCount - 1 do
    SetLength(adj[i], 0);
  SetLength(inDegree, pageCount);
  for i := 0 to pageCount - 1 do
    inDegree[i] := 0;

  for i := 0 to High(rules) do
  begin
    rule := rules[i];
    idxX := GetIndex(rule.x);
    idxY := GetIndex(rule.y);

    // If both pages are relevant to this update
    if (idxX >= 0) and (idxY >= 0) then
    begin
      // Add edge idxX -> idxY
      // Check if edge already exists to avoid duplicates (important for inDegree)
      found := false;
      for k := 0 to High(adj[idxX]) do begin
        if adj[idxX][k] = idxY then begin
           found := true;
           break;
        end;
      end;
      if not found then begin
         SetLength(adj[idxX], Length(adj[idxX]) + 1);
         adj[idxX][High(adj[idxX])] := idxY;
         Inc(inDegree[idxY]);
      end;
    end;
  end;

  // 3. Topological Sort (Kahn's Algorithm)
  SetLength(queue, pageCount); // Max possible size
  qHead := 0;
  qTail := 0;
  SetLength(sortedIndices, 0);

  // Initialize queue with nodes having in-degree 0
  for i := 0 to pageCount - 1 do
  begin
    if inDegree[i] = 0 then
    begin
      queue[qTail] := i;
      Inc(qTail);
    end;
  end;

  // Process the queue
  while qHead < qTail do
  begin
    u := queue[qHead];
    Inc(qHead);

    SetLength(sortedIndices, Length(sortedIndices) + 1);
    sortedIndices[High(sortedIndices)] := u; // Add index to result

    // Process neighbors
    for j := 0 to High(adj[u]) do
    begin
      v := adj[u][j];
      Dec(inDegree[v]);
      if inDegree[v] = 0 then
      begin
        queue[qTail] := v;
        Inc(qTail);
      end;
    end;
  end;

  // 4. Convert sorted indices back to page numbers
  // Check for cycles (if not all nodes are in sortedIndices) - problem implies acyclic?
  // if Length(sortedIndices) <> pageCount then // Handle cycle if necessary

  SetLength(Result, Length(sortedIndices));
  for i := 0 to High(sortedIndices) do
  begin
    Result[i] := indexToPage[sortedIndices[i]];
  end;

end;

// --- Main Program ---
var
  inputFile: TextFile;
  line: string;
  rulesData: TRuleArray;
  updatesData: TUpdatesArray;
  parts: TArray<string>;
  currentUpdate: TIntArray;
  fixedOrder: TIntArray;
  i, midIndex: integer;
  totalSum: Int64; // Use Int64 for sum to avoid overflow

begin
  AssignFile(inputFile, 'input.txt');
  try
    Reset(inputFile);
  except
    on E: EInOutError do
    begin
      WriteLn('Error opening input.txt: ', E.Message);
      Halt(1);
    end;
  end;

  SetLength(rulesData, 0);
  SetLength(updatesData, 0);

  // Read rules and updates from file
  while not EoF(inputFile) do
  begin
    ReadLn(inputFile, line);
    line := Trim(line);
    if line = '' then continue; // Skip empty lines

    if Pos('|', line) > 0 then // It's a rule
    begin
      parts := SplitString(line, '|');
      if Length(parts) = 2 then
      begin
        SetLength(rulesData, Length(rulesData) + 1);
        try
          rulesData[High(rulesData)].x := StrToInt(Trim(parts[0]));
          rulesData[High(rulesData)].y := StrToInt(Trim(parts[1]));
        except
           on E: EConvertError do begin
             WriteLn('Invalid integer in rule line: ', line);
             // Optionally skip this rule or halt
           end;
        end;
      end;
    end
    else // It's an update
    begin
      parts := SplitString(line, ',');
      if Length(parts) > 0 then
      begin
         SetLength(updatesData, Length(updatesData) + 1);
         try
            updatesData[High(updatesData)] := StringsToInts(parts);
         except
            on E: EConvertError do begin
              WriteLn('Invalid integer in update line: ', line);
              // Optionally clear the last update or halt
              SetLength(updatesData[High(updatesData)], 0);
            end;
         end;
      end;
    end;
  end;

  CloseFile(inputFile);

  // Process updates
  totalSum := 0;
  for i := 0 to High(updatesData) do
  begin
    currentUpdate := updatesData[i];
    if Length(currentUpdate) = 0 then continue; // Skip empty updates potentially caused by errors

    if not IsCorrect(currentUpdate, rulesData) then
    begin
      fixedOrder := CorrectOrder(currentUpdate, rulesData);
      if Length(fixedOrder) > 0 then
      begin
         // Pascal indices are 0-based, Python's // is floor division
         midIndex := Length(fixedOrder) div 2;
         totalSum := totalSum + fixedOrder[midIndex];
      end;
    end;
  end;

  // Print the final sum
  WriteLn(totalSum);

end.
