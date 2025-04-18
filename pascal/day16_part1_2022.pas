
program VolcanoValves;
{$mode objfpc}{$H+}
uses
  SysUtils;

type
  TStringArray    = array of string;
  TNeighborLists  = array of TStringArray;
  TIntArray       = array of Integer;
  TIntMatrix      = array of TIntArray;

var
  rawNames   : TStringArray;
  rawFlows   : TIntArray;
  rawNeiStr  : TNeighborLists;
  neiIndices : TIntMatrix;
  distAll    : TIntMatrix;
  useful     : TIntArray;    // indices of valves with flow>0
  idxAA      : Integer;
  n, m       : Integer;
  dist2      : TIntMatrix;
  flows2     : TIntArray;

// Simple DFS over (pos, timeLeft, mask) without memo; m<=15 so ~250k calls
function DFS(pos, timeLeft, mask: Integer): LongInt;
var
  best, i, d, nt, gain, tot: LongInt;
begin
  best := 0;
  for i := 1 to m do
    if (mask and (1 shl (i-1))) = 0 then
    begin
      d := dist2[pos][i];
      if (d >= 0) and (d+1 <= timeLeft) then
      begin
        nt := timeLeft - d - 1;
        gain := flows2[i] * nt;
        tot  := gain + DFS(i, nt, mask or (1 shl (i-1)));
        if tot > best then best := tot;
      end;
    end;
  DFS := best;
end;

// Split a comma‐separated string into array of substrings (no spaces)
procedure SplitCSV(const s: string; var arr: TStringArray);
var
  tmp, part: string;
  p: Integer;
begin
  tmp := StringReplace(s, ' ', '', [rfReplaceAll]);
  SetLength(arr, 0);
  while tmp <> '' do
  begin
    p := Pos(',', tmp);
    if p = 0 then
    begin
      part := tmp;
      tmp := '';
    end else
    begin
      part := Copy(tmp, 1, p-1);
      Delete(tmp, 1, p);
    end;
    SetLength(arr, Length(arr)+1);
    arr[High(arr)] := part;
  end;
end;

var
  f       : Text;
  line    : string;
  i, j    : Integer;
  vName   : string;
  fr, p1  : Integer;
  rest    : string;
  toPos   : Integer;
  nml     : TStringArray;
  q       : array of Integer;
  head, tail, u, v: Integer;
  seen    : array of Boolean;

begin
  // Read input.txt
  Assign(f, 'input.txt'); Reset(f);
  n := 0;
  SetLength(rawNames, 0);
  SetLength(rawFlows, 0);
  SetLength(rawNeiStr, 0);
  while not Eof(f) do
  begin
    ReadLn(f, line);
    // parse "Valve XX has flow rate=YY; tunnels lead to valves A, B, C"
    vName := Copy(line, 7, 2);
    p1 := Pos('rate=', line);
    fr := StrToInt(Copy(line, p1+5, Pos(';', line)- (p1+5)));
    toPos := LastDelimiter(' ', line);
    // better find "to " and take the substring after it
    toPos := Pos('to ', line);
    rest := Copy(line, toPos+3, MaxInt);
    // strip leading "valves " or "valve "
    if Copy(rest,1,7) = 'valves ' then rest := Copy(rest,8,MaxInt)
    else if Copy(rest,1,6) = 'valve ' then rest := Copy(rest,7,MaxInt);
    // split by commas
    SplitCSV(rest, nml);

    SetLength(rawNames, n+1);
    SetLength(rawFlows, n+1);
    SetLength(rawNeiStr, n+1);
    rawNames[n] := vName;
    rawFlows[n] := fr;
    rawNeiStr[n] := Copy(nml,0,0);  // hack to get correct type
    rawNeiStr[n] := nml;

    Inc(n);
  end;
  Close(f);

  // build neighbor‐index lists
  SetLength(neiIndices, n);
  for i := 0 to n-1 do
  begin
    SetLength(neiIndices[i], Length(rawNeiStr[i]));
    for j := 0 to High(rawNeiStr[i]) do
      // find index of rawNeiStr[i][j]
      for v := 0 to n-1 do
        if rawNames[v] = rawNeiStr[i][j] then
          neiIndices[i][j] := v;
  end;

  // find AA and useful valves
  idxAA := -1;
  SetLength(useful, 0);
  for i := 0 to n-1 do
  begin
    if rawNames[i] = 'AA' then
      idxAA := i;
    if rawFlows[i] > 0 then
    begin
      SetLength(useful, Length(useful)+1);
      useful[High(useful)] := i;
    end;
  end;
  m := Length(useful);  // number of positive‐flow valves

  // all‐pairs distances by BFS
  SetLength(distAll, n);
  for i := 0 to n-1 do
  begin
    SetLength(distAll[i], n);
    for j := 0 to n-1 do
      distAll[i][j] := -1;
    // BFS from i
    SetLength(q, n);
    head := 0; tail := 0;
    q[tail] := i; Inc(tail);
    distAll[i][i] := 0;
    while head < tail do
    begin
      u := q[head]; Inc(head);
      for v in neiIndices[u] do
        if distAll[i][v] < 0 then
        begin
          distAll[i][v] := distAll[i][u] + 1;
          q[tail] := v; Inc(tail);
        end;
    end;
  end;

  // build reduced graph of size m+1: node 0 = AA, nodes 1..m = useful valves
  SetLength(dist2, m+1);
  SetLength(flows2, m+1);
  for i := 0 to m do
    SetLength(dist2[i], m+1);

  flows2[0] := 0;
  for i := 1 to m do
    flows2[i] := rawFlows[ useful[i-1] ];

  // fill distances
  for i := 0 to m do
    for j := 0 to m do
    begin
      if i = 0 then u := idxAA else u := useful[i-1];
      if j = 0 then v := idxAA else v := useful[j-1];
      dist2[i][j] := distAll[u][v];
    end;

  // compute max pressure in 30 minutes from node 0, empty mask
  Writeln( DFS(0, 30, 0) );
end.
