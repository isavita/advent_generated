program Chiton;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

type
  TItem = record
    dist: LongInt;
    id: Integer;
  end;

var
  lines: TStringList;
  risk: array of Integer;
  distArr: array of LongInt;
  visited: array of Boolean;
  heap: array of TItem;
  heapSize: Integer;
  rows, cols, totalNodes: Integer;

// Swap two heap items
procedure HeapSwap(i, j: Integer);
var
  tmp: TItem;
begin
  tmp := heap[i];
  heap[i] := heap[j];
  heap[j] := tmp;
end;

// Sift up the node at index i
procedure SiftUp(i: Integer);
var
  parent: Integer;
begin
  while i > 0 do
  begin
    parent := (i - 1) div 2;
    if heap[i].dist < heap[parent].dist then
    begin
      HeapSwap(i, parent);
      i := parent;
    end
    else
      Break;
  end;
end;

// Sift down the node at index i
procedure SiftDown(i: Integer);
var
  left, right, smallest: Integer;
begin
  while True do
  begin
    left := 2 * i + 1;
    right := 2 * i + 2;
    smallest := i;
    if (left < heapSize) and (heap[left].dist < heap[smallest].dist) then
      smallest := left;
    if (right < heapSize) and (heap[right].dist < heap[smallest].dist) then
      smallest := right;
    if smallest = i then
      Break;
    HeapSwap(i, smallest);
    i := smallest;
  end;
end;

// Push a new item onto the min-heap
procedure HeapPush(d: LongInt; id: Integer);
begin
  heap[heapSize].dist := d;
  heap[heapSize].id := id;
  SiftUp(heapSize);
  Inc(heapSize);
end;

// Pop the minimum item from the heap
function HeapPop: TItem;
begin
  Result := heap[0];
  Dec(heapSize);
  heap[0] := heap[heapSize];
  SiftDown(0);
end;

var
  i, r, c, id, nid, nr, nc: Integer;
  cur: TItem;
  newDist: LongInt;
  dr: array[0..3] of Integer = ( -1, 1, 0, 0 );
  dc: array[0..3] of Integer = ( 0, 0, -1, 1 );
  endId: Integer;
begin
  // Read input
  lines := TStringList.Create;
  try
    lines.LoadFromFile('input.txt');
    rows := lines.Count;
    if rows = 0 then
    begin
      WriteLn('Empty input.');
      Exit;
    end;
    cols := Length(lines[0]);
    totalNodes := rows * cols;
    // Allocate arrays
    SetLength(risk, totalNodes);
    SetLength(distArr, totalNodes);
    SetLength(visited, totalNodes);
    // Parse risk levels
    for r := 0 to rows - 1 do
      for c := 0 to cols - 1 do
      begin
        id := r * cols + c;
        risk[id] := Ord(lines[r][c+1]) - Ord('0');
      end;
  finally
    lines.Free;
  end;

  // Prepare Dijkstra
  SetLength(heap, totalNodes * 4); // enough space for pushes
  heapSize := 0;
  for i := 0 to totalNodes - 1 do
  begin
    distArr[i] := High(LongInt);
    visited[i] := False;
  end;
  // Start at top-left (id = 0) with distance 0
  distArr[0] := 0;
  HeapPush(0, 0);

  endId := totalNodes - 1;
  // Main loop
  while heapSize > 0 do
  begin
    cur := HeapPop;
    id := cur.id;
    // If already processed, skip
    if visited[id] then
      Continue;
    visited[id] := True;
    // If reached target, we can stop
    if id = endId then
      Break;
    // Coordinates
    r := id div cols;
    c := id mod cols;
    // Explore neighbors
    for i := 0 to 3 do
    begin
      nr := r + dr[i];
      nc := c + dc[i];
      if (nr >= 0) and (nr < rows) and (nc >= 0) and (nc < cols) then
      begin
        nid := nr * cols + nc;
        if not visited[nid] then
        begin
          newDist := cur.dist + risk[nid];
          if newDist < distArr[nid] then
          begin
            distArr[nid] := newDist;
            HeapPush(newDist, nid);
          end;
        end;
      end;
    end;
  end;

  // Output result: minimal total risk to bottom-right
  WriteLn(distArr[endId]);
end.