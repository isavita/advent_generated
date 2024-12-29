
program solve;

{$O+,Q+}

type
  TPoint = record
    x, y: Integer;
  end;
  TQueueItem = record
    pt: TPoint;
    steps: Integer;
  end;

const
  size = 71;
  dirs: array[0..3, 0..1] of Integer = ((1, 0), (-1, 0), (0, 1), (0, -1));

var
  grid: array[0..size - 1, 0..size - 1] of Boolean;
  visited: array[0..size - 1, 0..size - 1] of Boolean;
  q: array[0..size * size] of TQueueItem;
  head, tail: Integer;
  f: Text;
  line: string;
  x, y, i, j, nx, ny, steps: Integer;
  parts: array[0..1] of string;
  found: Boolean;

procedure Split(const s: string; const delimiter: Char; var result: array of string);
var
  i, start, count: Integer;
begin
  count := 0;
  start := 1;
  for i := 1 to Length(s) do
  begin
    if s[i] = delimiter then
    begin
      result[count] := Copy(s, start, i - start);
      Inc(count);
      start := i + 1;
    end;
  end;
  result[count] := Copy(s, start, Length(s) - start + 1);
end;

begin
  Assign(f, 'input.txt');
  Reset(f);
  for i := 0 to 1023 do
  begin
    if Eof(f) then
      break;
    ReadLn(f, line);
    Split(line, ',', parts);
    Val(parts[0], x);
    Val(parts[1], y);
    if (x >= 0) and (x < size) and (y >= 0) and (y < size) then
      grid[y, x] := True;
  end;
  Close(f);

  for i := 0 to size - 1 do
    for j := 0 to size - 1 do
      visited[i, j] := False;

  head := 0;
  tail := 1;
  q[1].pt.x := 0;
  q[1].pt.y := 0;
  q[1].steps := 0;
  visited[0, 0] := True;
  found := False;

  while head < tail do
  begin
    Inc(head);
    x := q[head].pt.x;
    y := q[head].pt.y;
    steps := q[head].steps;

    if (x = size - 1) and (y = size - 1) then
    begin
      Writeln(steps);
      found := True;
      break;
    end;

    for i := 0 to 3 do
    begin
      nx := x + dirs[i, 0];
      ny := y + dirs[i, 1];
      if (nx >= 0) and (ny >= 0) and (nx < size) and (ny < size) and not grid[ny, nx] and not visited[ny, nx] then
      begin
        Inc(tail);
        q[tail].pt.x := nx;
        q[tail].pt.y := ny;
        q[tail].steps := steps + 1;
        visited[ny, nx] := True;
      end;
    end;
  end;

  if not found then
    Writeln('No path');
end.
