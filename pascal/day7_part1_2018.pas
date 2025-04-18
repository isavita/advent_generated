Program Day7;
{ Reads dependency instructions from input.txt and computes
  the order of steps (A–Z) respecting prerequisites,
  choosing the lexicographically smallest available step at each turn. }
var
  f: Text;
  s: string;
  i, j, u, v, idx: Integer;
  existStep: array[1..26] of Boolean;
  prereqCount: array[1..26] of Integer;
  adj: array[1..26,1..26] of Boolean;
  doneCount: Integer;
  doneStep: array[1..26] of Boolean;
  resultOrder: string;
  c1, c2: Char;
begin
  { Initialize }
  for i := 1 to 26 do
  begin
    existStep[i] := False;
    prereqCount[i] := 0;
    doneStep[i] := False;
    for j := 1 to 26 do
      adj[i,j] := False;
  end;
  
  { Read input }
  Assign(f, 'input.txt');
  Reset(f);
  while not Eof(f) do
  begin
    ReadLn(f, s);
    if Length(s) >= 37 then
    begin
      c1 := s[6];    { Step X ... }
      c2 := s[37];   { ... before step Y ... }
      u := Ord(c1) - Ord('A') + 1;
      v := Ord(c2) - Ord('A') + 1;
      existStep[u] := True;
      existStep[v] := True;
      if not adj[u,v] then
      begin
        adj[u,v] := True;
        Inc(prereqCount[v]);
      end;
    end;
  end;
  Close(f);

  { Compute total number of steps present }
  doneCount := 0;
  for i := 1 to 26 do
    if existStep[i] then
      Inc(doneCount);
  { We'll overwrite doneCount with steps processed }
  doneCount := 0;
  resultOrder := '';

  { Topological sort with lexicographical selection }
  while doneCount < (Length(resultOrder) + 0) + (0) do
    { dummy to satisfy Pascal structure; removed in next block } 
    Break;
  { Correct loop below }
  while doneCount < 26 do
  begin
    { Find next available step }
    idx := 0;
    for i := 1 to 26 do
      if existStep[i] and (not doneStep[i]) and (prereqCount[i] = 0) then
      begin
        idx := i;
        Break;  { first (smallest) found }
      end;
    if idx = 0 then
      Break; { no more available }
    { Mark and append }
    doneStep[idx] := True;
    Inc(doneCount);
    resultOrder := resultOrder + Char(Ord('A') + idx - 1);
    { Remove edges from idx to its neighbors }
    for j := 1 to 26 do
      if adj[idx,j] then
      begin
        adj[idx,j] := False;
        Dec(prereqCount[j]);
      end;
  end;

  { Print result }
  WriteLn(resultOrder);
end.