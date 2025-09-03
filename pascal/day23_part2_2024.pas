program MaxCliqueFromInput;

const
  MAX_NODES = 2048;
  MAX_NAME_LEN = 16;

type
  TName = string[MAX_NAME_LEN];
  TNodeArray = array[0..MAX_NODES-1] of Integer;
  TBoolMatrix = array[0..MAX_NODES-1, 0..MAX_NODES-1] of Boolean;

var
  NodeNames: array[0..MAX_NODES-1] of TName;
  Adj: TBoolMatrix;
  NodeCount: Integer = 0;
  MaxCliqueNodes: array[0..MAX_NODES-1] of Integer;
  MaxCliqueSize: Integer = 0;

procedure GetNodeID(Name: string; var ID: Integer);
var
  i: Integer;
begin
  ID := -1;
  for i := 0 to NodeCount - 1 do
    if NodeNames[i] = Name then begin
      ID := i;
      Exit;
    end;
  if NodeCount >= MAX_NODES then Exit;
  NodeNames[NodeCount] := Copy(Name, 1, MAX_NAME_LEN);
  ID := NodeCount;
  Inc(NodeCount);
end;

procedure FindMaxClique(R: TNodeArray; RSize: Integer; P: TNodeArray; PSize: Integer);
var
  NextR: TNodeArray;
  NextP: TNodeArray;
  PCopy: TNodeArray;
  i, j, v, w, NextRSize, NextPSize, k: Integer;
begin
  if PSize = 0 then begin
    if RSize > MaxCliqueSize then begin
      MaxCliqueSize := RSize;
      for k := 0 to RSize - 1 do MaxCliqueNodes[k] := R[k];
    end;
    Exit;
  end;

  if RSize + PSize <= MaxCliqueSize then Exit;

  for i := 0 to PSize - 1 do PCopy[i] := P[i];
  for i := 0 to PSize - 1 do begin
    v := PCopy[i];

    for k := 0 to RSize - 1 do NextR[k] := R[k];
    NextR[RSize] := v;
    NextRSize := RSize + 1;

    NextPSize := 0;
    for j := i + 1 to PSize - 1 do begin
      w := PCopy[j];
      if Adj[v, w] then begin
        NextP[NextPSize] := w;
        Inc(NextPSize);
      end;
    end;

    FindMaxClique(NextR, NextRSize, NextP, NextPSize);
  end;
end;

procedure QuickSortDynamic(var A: array of string; L, R: Integer);
var
  i, j: Integer;
  pivot, tmp: string;
begin
  if L >= R then Exit;
  i := L; j := R;
  pivot := A[(L + R) div 2];
  while i <= j do begin
    while A[i] < pivot do Inc(i);
    while A[j] > pivot do Dec(j);
    if i <= j then begin
      tmp := A[i]; A[i] := A[j]; A[j] := tmp;
      Inc(i); Dec(j);
    end;
  end;
  if L < j then QuickSortDynamic(A, L, j);
  if i < R then QuickSortDynamic(A, i, R);
end;

var
  f: Text;
  line: string;
  dashPos: Integer;
  name1, name2: string;
  id1, id2: Integer;
  initialP: TNodeArray;
  initialR: TNodeArray;
  i: Integer;
  resultStrings: array of string;

begin
  FillChar(Adj, SizeOf(Adj), False);

  Assign(f, 'input.txt');
  Reset(f);
  if IOResult <> 0 then Halt(1);

  while not Eof(f) do begin
    ReadLn(f, line);
    dashPos := Pos('-', line);
    if dashPos = 0 then Continue;

    name1 := Copy(line, 1, dashPos - 1);
    name2 := Copy(line, dashPos + 1, Length(line) - dashPos);

    if (Length(name2) > 0) and (name2[Length(name2)] = #13) then Delete(name2, Length(name2), 1);
    if (Length(name1) = 0) or (Length(name2) = 0) then Continue;

    id1 := -1; id2 := -1;
    GetNodeID(name1, id1);
    if id1 = -1 then begin Close(f); Halt(1); end;
    GetNodeID(name2, id2);
    if id2 = -1 then begin Close(f); Halt(1); end;

    if (id1 <> id2) then begin
      Adj[id1, id2] := True;
      Adj[id2, id1] := True;
    end;
  end;

  Close(f);

  if NodeCount = 0 then begin
    WriteLn('No nodes found in input.');
    Halt(0);
  end;

  for i := 0 to NodeCount - 1 do initialP[i] := i;
  FindMaxClique(initialR, 0, initialP, NodeCount);

  if MaxCliqueSize = 0 then begin
    WriteLn('No clique found (graph might be empty or have no edges).');
    Halt(0);
  end;

  SetLength(resultStrings, MaxCliqueSize);
  for i := 0 to MaxCliqueSize - 1 do resultStrings[i] := NodeNames[MaxCliqueNodes[i]];
  if MaxCliqueSize > 1 then QuickSortDynamic(resultStrings, 0, MaxCliqueSize - 1);

  for i := 0 to MaxCliqueSize - 1 do begin
    Write(resultStrings[i]);
    if i < MaxCliqueSize - 1 then Write(',');
  end;
  WriteLn;
end.