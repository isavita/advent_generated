program Solver;

uses
  SysUtils;

const
  MAXR = 200;
  MAXC = 200;
  MAXHEAP = MAXR * MAXC * 4 + 10;

type
  TNode = record
    cost: LongInt;
    r, c, d: Integer;
  end;

var
  grid: array[1..MAXR] of string;
  rows, cols: Integer;
  startR, startC, endR, endC: Integer;

  visited: array[1..MAXR, 1..MAXC, 0..3] of Boolean;

  heap: array[1..MAXHEAP] of TNode;
  heapSize: Integer;

  dx: array[0..3] of Integer = (0, 1, 0, -1);
  dy: array[0..3] of Integer = (1, 0, -1, 0);

procedure Push(n: TNode);
var
  i, parent: Integer;
  tmp: TNode;
begin
  inc(heapSize);
  heap[heapSize] := n;
  i := heapSize;
  while i > 1 do begin
    parent := i div 2;
    if heap[parent].cost <= heap[i].cost then
      break;
    tmp := heap[i];
    heap[i] := heap[parent];
    heap[parent] := tmp;
    i := parent;
  end;
end;

function Pop: TNode;
var
  min: TNode;
  i, left, right, smallest: Integer;
  tmp: TNode;
begin
  min := heap[1];
  heap[1] := heap[heapSize];
  dec(heapSize);
  if heapSize > 0 then begin
    i := 1;
    while True do begin
      left := i * 2;
      right := left + 1;
      smallest := i;
      if (left <= heapSize) and (heap[left].cost < heap[smallest].cost) then
        smallest := left;
      if (right <= heapSize) and (heap[right].cost < heap[smallest].cost) then
        smallest := right;
      if smallest = i then
        break;
      tmp := heap[i];
      heap[i] := heap[smallest];
      heap[smallest] := tmp;
      i := smallest;
    end;
  end;
  Pop := min;
end;

function EmptyHeap: Boolean;
begin
  EmptyHeap := heapSize = 0;
end;

var
  line: string;
  r, c: Integer;
  ch: Char;
  cur, nnode: TNode;
  nr, nc: Integer;
  nd: Integer;
  answer: LongInt;

begin
  rows := 0;
  startR := -1; startC := -1;
  endR := -1; endC := -1;

  assign(input, 'input.txt');
  reset(input);
  while not Eof(input) do begin
    Readln(input, line);
    line := Trim(line);
    if Length(line) > 0 then begin
      inc(rows);
      grid[rows] := line;
    end;
  end;
  Close(input);

  if rows = 0 then begin
    WriteLn(-1);
    Halt;
  end;

  cols := Length(grid[1]);

  for r := 1 to rows do
    for c := 1 to cols do begin
      ch := grid[r][c];
      if ch = 'S' then begin
        startR := r; startC := c;
      end;
      if ch = 'E' then begin
        endR := r; endC := c;
      end;
    end;

  for r := 1 to rows do
    for c := 1 to cols do
      for nd := 0 to 3 do visited[r][c][nd] := False;

  heapSize := 0;
  cur.cost := 0;
  cur.r := startR;
  cur.c := startC;
  cur.d := 0;

  Push(cur);

  answer := -1;

  while not EmptyHeap do begin
    cur := Pop;
    if visited[cur.r][cur.c][cur.d] then
      continue;
    visited[cur.r][cur.c][cur.d] := True;

    if (cur.r = endR) and (cur.c = endC) then begin
      answer := cur.cost;
      break;
    end;

    // Move forward
    nr := cur.r + dx[cur.d];
    nc := cur.c + dy[cur.d];
    if (nr >= 1) and (nr <= rows) and (nc >= 1) and (nc <= cols) then
      if grid[nr][nc] <> '#' then begin
        nnode := cur;
        nnode.r := nr;
        nnode.c := nc;
        nnode.d := cur.d;
        nnode.cost := cur.cost + 1;
        Push(nnode);
      end;

    // Rotate clockwise
    nnode := cur;
    nnode.d := (cur.d + 1) mod 4;
    nnode.cost := cur.cost + 1000;
    Push(nnode);

    // Rotate counter-clockwise
    nnode := cur;
    nnode.d := (cur.d + 3) mod 4;
    nnode.cost := cur.cost + 1000;
    Push(nnode);
  end;

  WriteLn(answer);
end.