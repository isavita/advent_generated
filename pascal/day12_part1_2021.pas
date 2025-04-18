
program PassagePathing;
{$mode objfpc}{$h+}

const
  MAXN = 1000;        { Maximum distinct cave names }

type
  TStringArray = array[1..MAXN] of string;
  TAdjList     = array[1..MAXN, 1..MAXN] of Integer;
  TIntArray    = array[1..MAXN] of Integer;
  TBoolArray   = array[1..MAXN] of Boolean;

var
  Names    : TStringArray;   { index→cave name }
  N        : Integer = 0;    { number of distinct caves }
  Adj      : TAdjList;       { adjacency lists }
  Deg      : TIntArray;      { Deg[i] = number of neighbors of i }
  IsSmall  : TBoolArray;     { IsSmall[i] = true if cave i is all lowercase }
  Visited  : TBoolArray;     { DFS visited marker for small caves }
  StartIdx,
  EndIdx   : Integer;
  PathCount: LongInt = 0;

{--------------------------------------------------------------------}
function GetIndex(const nm: string): Integer;
{ Return the index of cave nm, creating a new one if needed. }
var
  i: Integer;
begin
  for i := 1 to N do
    if Names[i] = nm then
      Exit(i);
  { new cave }
  Inc(N);
  Names[N] := nm;
  { detect if small (all lowercase assumes first char test) }
  IsSmall[N] := (nm[1] in ['a'..'z']);
  Deg[N] := 0;
  Result := N;
end;

{--------------------------------------------------------------------}
procedure AddEdge(u, v: Integer);
begin
  Inc(Deg[u]);
  Adj[u, Deg[u]] := v;
end;

{--------------------------------------------------------------------}
procedure DFS(u: Integer);
var
  i, v: Integer;
  wasVisited: Boolean;
begin
  if u = EndIdx then
  begin
    Inc(PathCount);
    Exit;
  end;
  for i := 1 to Deg[u] do
  begin
    v := Adj[u, i];
    if IsSmall[v] and Visited[v] then
      Continue;
    wasVisited := False;
    if IsSmall[v] then
    begin
      Visited[v] := True;
      wasVisited := True;
    end;
    DFS(v);
    if wasVisited then
      Visited[v] := False;
  end;
end;

{--------------------------------------------------------------------}
var
  InF    : Text;
  line,
  a, b   : string;
  dash   : Integer;
  u, v   : Integer;
begin
  { Redirect input from file }
  Assign(InF, 'input.txt');
  Reset(InF);
  { Read edges until EOF }
  while not EOF(InF) do
  begin
    ReadLn(InF, line);
    if line = '' then Continue;
    dash := Pos('-', line);
    a := Copy(line, 1, dash - 1);
    b := Copy(line, dash + 1, Length(line) - dash);
    u := GetIndex(a);
    v := GetIndex(b);
    { undirected }
    AddEdge(u, v);
    AddEdge(v, u);
  end;
  Close(InF);

  { Locate start/end }
  StartIdx := GetIndex('start');
  EndIdx   := GetIndex('end');

  { Mark start as visited so we never return to it }
  Visited[StartIdx] := True;

  { DFS from start }
  DFS(StartIdx);

  { Output the total count }
  WriteLn(PathCount);
end.
