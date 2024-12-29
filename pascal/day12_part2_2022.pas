
program day12;

{$O+,Q+}
uses sysutils;

type
  TPoint = record
    x, y: Integer;
  end;

  TGrid = array of array of Char;
  TDist = array of array of Integer;
  TQueueItem = record
    p: TPoint;
    dist: Integer;
  end;
  TQueue = array of TQueueItem;

var
  grid: TGrid;
  dist: TDist;
  start, endp: TPoint;
  as: array of TPoint;
  rows, cols, aCount, qHead, qTail: Integer;
  queue: TQueue;
  dx: array[0..3] of Integer = (0, 0, 1, -1);
  dy: array[0..3] of Integer = (1, -1, 0, 0);

function min(a, b: Integer): Integer;
begin
  if a < b then
    min := a
  else
    min := b;
end;

procedure enqueue(p: TPoint; d: Integer);
begin
  queue[qTail].p := p;
  queue[qTail].dist := d;
  Inc(qTail);
end;

function dequeue: TQueueItem;
begin
  dequeue := queue[qHead];
  Inc(qHead);
end;

function isValid(p: TPoint): Boolean;
begin
  isValid := (p.x >= 0) and (p.x < cols) and (p.y >= 0) and (p.y < rows);
end;

procedure djikstra;
var
  curr: TQueueItem;
  next: TPoint;
  i, nextDist: Integer;
begin
  qHead := 0;
  qTail := 0;
  enqueue(endp, 0);
  dist[endp.y][endp.x] := 0;

  while qHead < qTail do
  begin
    curr := dequeue;
    for i := 0 to 3 do
    begin
      next.x := curr.p.x + dx[i];
      next.y := curr.p.y + dy[i];
      if not isValid(next) then
        continue;
      if ord(grid[curr.p.y][curr.p.x]) - ord(grid[next.y][next.x]) > 1 then
        continue;
      nextDist := curr.dist + 1;
      if (dist[next.y][next.x] = -1) or (nextDist < dist[next.y][next.x]) then
      begin
        dist[next.y][next.x] := nextDist;
        enqueue(next, nextDist);
      end;
    end;
  end;
end;

var
  f: Text;
  line: string;
  x, y, l, i: Integer;
  b: Char;
begin
  Assign(f, 'input.txt');
  Reset(f);
  rows := 0;
  aCount := 0;
  while not Eof(f) do
  begin
    ReadLn(f, line);
    if rows = 0 then
      cols := Length(line);
    SetLength(grid, rows + 1);
    SetLength(grid[rows], cols);
    for x := 0 to cols - 1 do
    begin
      b := line[x + 1];
      grid[rows][x] := b;
      if b = 'S' then
      begin
        start.x := x;
        start.y := rows;
      end
      else if b = 'E' then
      begin
        endp.x := x;
        endp.y := rows;
      end
      else if b = 'a' then
      begin
        SetLength(as, aCount + 1);
        as[aCount].x := x;
        as[aCount].y := rows;
        Inc(aCount);
      end;
    end;
    Inc(rows);
  end;
  Close(f);

  grid[start.y][start.x] := 'a';
  grid[endp.y][endp.x] := 'z';

  SetLength(dist, rows);
  for y := 0 to rows - 1 do
  begin
    SetLength(dist[y], cols);
    for x := 0 to cols - 1 do
      dist[y][x] := -1;
  end;

  SetLength(queue, rows * cols);
  djikstra;

  l := dist[start.y][start.x];
  for i := 0 to aCount - 1 do
    if dist[as[i].y][as[i].x] <> -1 then
      l := min(l, dist[as[i].y][as[i].x]);

  WriteLn(l);
end.
