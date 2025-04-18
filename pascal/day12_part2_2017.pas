program DigitalPlumber;

{$mode objfpc}{$H+}

uses
  SysUtils;

const
  MAXN = 2000;

type
  TIntArr = array of Integer;

var
  adj: array[0..MAXN-1] of TIntArr;
  presentNode: array[0..MAXN-1] of Boolean;
  visited: array[0..MAXN-1] of Boolean;
  maxID: Integer;

procedure AddEdges(line: string);
var
  arrowPos, i, nID: Integer;
  idPart, rest: string;
  piece: string;
begin
  // Find the "<->" delimiter
  arrowPos := Pos('<->', line);
  idPart := Trim(Copy(line, 1, arrowPos - 1));
  i := StrToInt(idPart);
  presentNode[i] := True;
  if i > maxID then
    maxID := i;
  // The rest are neighbors
  rest := Trim(Copy(line, arrowPos + 3, Length(line)));
  // Split by commas
  while rest <> '' do
  begin
    if Pos(',', rest) > 0 then
    begin
      piece := Trim(Copy(rest, 1, Pos(',', rest) - 1));
      rest := Trim(Copy(rest, Pos(',', rest) + 1, Length(rest)));
    end
    else
    begin
      piece := Trim(rest);
      rest := '';
    end;
    nID := StrToInt(piece);
    presentNode[nID] := True;
    if nID > maxID then
      maxID := nID;
    // add neighbor
    SetLength(adj[i], Length(adj[i]) + 1);
    adj[i][High(adj[i])] := nID;
  end;
end;

procedure DFS(u: Integer);
var
  v: Integer;
begin
  visited[u] := True;
  for v in adj[u] do
    if not visited[v] then
      DFS(v);
end;

var
  f: TextFile;
  line: string;
  part1Count, groupCount, i: Integer;
begin
  // Initialize
  for i := 0 to MAXN - 1 do
  begin
    presentNode[i] := False;
    visited[i] := False;
    SetLength(adj[i], 0);
  end;
  maxID := -1;

  // Read input from file
  AssignFile(f, 'input.txt');
  Reset(f);
  try
    while not EOF(f) do
    begin
      ReadLn(f, line);
      if Trim(line) <> '' then
        AddEdges(line);
    end;
  finally
    CloseFile(f);
  end;

  // Part 1: size of group containing 0
  if presentNode[0] then
  begin
    DFS(0);
    part1Count := 0;
    for i := 0 to maxID do
      if presentNode[i] and visited[i] then
        Inc(part1Count);
  end
  else
    part1Count := 0;

  // Part 2: count total groups
  // reset visited
  for i := 0 to maxID do
    visited[i] := False;
  groupCount := 0;
  for i := 0 to maxID do
    if presentNode[i] and not visited[i] then
    begin
      Inc(groupCount);
      DFS(i);
    end;

  // Output results
  WriteLn(part1Count);
  WriteLn(groupCount);
end.