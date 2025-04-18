
program UltraCrucible;

{$mode objfpc}{$H+}

uses
  SysUtils;

const
  MAXN = 200;       { maximum grid dimension we support }
  MAXSEG = 10;      { maximum segment length }
  MINSEG = 4;       { minimum segment length for ultra	crucible }
  INF = 1000000000; { a large "infinity" }

type
  TInt = longint;
  TGrid = array[1..MAXN,1..MAXN] of TInt;
  TDist = array[1..MAXN,1..MAXN,0..3,1..MAXSEG] of TInt;

  TState = record
    dist: TInt;
    i,j: smallint;
    dir: 0..3;
    len: 1..MAXSEG;
  end;

var
  inFile: Text;
  raw: array[1..MAXN] of string;
  grid: TGrid;
  dista: TDist;
  N, M: TInt;

  { a simple binary‐min‐heap of TState for Dijkstra }
  heap: array[1..600000] of TState;
  heapSize: TInt;

  dx: array[0..3] of TInt = (-1, 0, 1,  0);
  dy: array[0..3] of TInt = ( 0, 1, 0, -1);

procedure HeapPush(const st: TState);
var
  idx, parent: TInt;
  tmp: TState;
begin
  Inc(heapSize);
  idx := heapSize;
  heap[idx] := st;
  while idx > 1 do
  begin
    parent := idx shr 1;
    if heap[parent].dist <= heap[idx].dist then
      Break;
    tmp := heap[parent];
    heap[parent] := heap[idx];
    heap[idx] := tmp;
    idx := parent;
  end;
end;

function HeapPop: TState;
var
  res, tmp: TState;
  idx, child: TInt;
begin
  res := heap[1];
  heap[1] := heap[heapSize];
  Dec(heapSize);
  idx := 1;
  while True do
  begin
    child := idx shl 1;
    if child > heapSize then Break;
    if (child < heapSize) and (heap[child+1].dist < heap[child].dist) then
      Inc(child);
    if heap[child].dist >= heap[idx].dist then
      Break;
    tmp := heap[idx];
    heap[idx] := heap[child];
    heap[child] := tmp;
    idx := child;
  end;
  HeapPop := res;
end;

var
  i,j,d,l: TInt;
  ans: TInt;
  st, nxt: TState;
  ni,nj, nd, nl, newCost: TInt;
  line: string;
begin
  { 1) Read input.txt into raw[1..N] }
  Assign(inFile,'input.txt');
  Reset(inFile);
  N := 0;
  while not Eof(inFile) do
  begin
    Inc(N);
    ReadLn(inFile, raw[N]);
  end;
  Close(inFile);
  if N = 0 then
    Halt(1);
  M := Length(raw[1]);

  { 2) Build grid of integer costs }
  for i := 1 to N do
    for j := 1 to M do
      grid[i,j] := Ord(raw[i][j]) - Ord('0');

  { 3) Initialize dist array to INF }
  for i := 1 to N do
    for j := 1 to M do
      for d := 0 to 3 do
        for l := 1 to MAXSEG do
          dista[i,j,d,l] := INF;

  heapSize := 0;

  { 4) Seed Dijkstra from the start cell (1,1) by stepping once in each dir }
  for d := 0 to 3 do
  begin
    ni := 1 + dx[d];
    nj := 1 + dy[d];
    if (ni>=1) and (ni<=N) and (nj>=1) and (nj<=M) then
    begin
      newCost := grid[ni,nj];
      if newCost < dista[ni,nj,d,1] then
      begin
        dista[ni,nj,d,1] := newCost;
        st.dist := newCost;
        st.i := ni; st.j := nj;
        st.dir := d; st.len := 1;
        HeapPush(st);
      end;
    end;
  end;

  { 5) Dijkstra over (i,j,dir,len) states }
  while heapSize > 0 do
  begin
    st := HeapPop;
    { lazy‐discard if outdated }
    if st.dist <> dista[st.i,st.j,st.dir,st.len] then
      Continue;

    { expand three possible "turn choices": -1=left,0=straight,+1=right }
    for d := -1 to 1 do
    begin
      nd := (st.dir + d + 4) mod 4;
      { check segment rules }
      if d = 0 then
      begin
        { straight: only if we haven't hit MAXSEG yet }
        if st.len >= MAXSEG then
          Continue;
        nl := st.len + 1;
      end
      else
      begin
        { turn: only allowed if current segment length >= MINSEG }
        if st.len < MINSEG then
          Continue;
        nl := 1;
      end;

      ni := st.i + dx[nd];
      nj := st.j + dy[nd];
      if (ni < 1) or (ni > N) or (nj < 1) or (nj > M) then
        Continue;

      newCost := st.dist + grid[ni,nj];
      if newCost < dista[ni,nj,nd,nl] then
      begin
        dista[ni,nj,nd,nl] := newCost;
        nxt.dist := newCost;
        nxt.i := ni; nxt.j := nj;
        nxt.dir := nd; nxt.len := nl;
        HeapPush(nxt);
      end;
    end;
  end;

  { 6) Extract the answer at the goal cell (N,M), allowing final segment len in [MINSEG..MAXSEG] }
  ans := INF;
  for d := 0 to 3 do
    for l := MINSEG to MAXSEG do
      if dista[N,M,d,l] < ans then
        ans := dista[N,M,d,l];

  if ans >= INF then
    WriteLn('No valid path found.')
  else
    WriteLn(ans);
end.
