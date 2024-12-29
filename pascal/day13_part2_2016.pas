
program day13;

const
  favoriteNumber = 1362;
  maxSteps = 50;

type
  Point = record
    x, y: Integer;
  end;

var
  visited: array of array of Boolean;
  queue: array of Point;
  head, tail: Integer;
  dx: array[1..4] of Integer = (1, -1, 0, 0);
  dy: array[1..4] of Integer = (0, 0, 1, -1);
  reachableLocations: Integer;
  x, y, num, bits, steps, size, i, j, nextX, nextY: Integer;
  start: Point;
  inputFile: Text;

function isWall(x, y: Integer): Boolean;
begin
  num := x * x + 3 * x + 2 * x * y + y + y * y + favoriteNumber;
  bits := 0;
  while num > 0 do
  begin
    if (num mod 2) = 1 then
      bits := bits + 1;
    num := num div 2;
  end;
  isWall := (bits mod 2) <> 0;
end;

procedure bfsMaxSteps(start: Point; maxSteps: Integer);
begin
  SetLength(visited, 1000, 1000);
  SetLength(queue, 100000);
  head := 0;
  tail := 1;
  queue[1] := start;
  visited[start.x][start.y] := True;
  steps := 0;
  reachableLocations := 1;

  while (head < tail) and (steps < maxSteps) do
  begin
    size := tail - head;
    for i := 1 to size do
    begin
      head := head + 1;
      x := queue[head].x;
      y := queue[head].y;

      for j := 1 to 4 do
      begin
        nextX := x + dx[j];
        nextY := y + dy[j];
        if (nextX >= 0) and (nextY >= 0) and not isWall(nextX, nextY) and not visited[nextX][nextY] then
        begin
          visited[nextX][nextY] := True;
          tail := tail + 1;
          queue[tail].x := nextX;
          queue[tail].y := nextY;
          reachableLocations := reachableLocations + 1;
        end;
      end;
    end;
    steps := steps + 1;
  end;
end;

begin
  Assign(inputFile, 'input.txt');
  Reset(inputFile);
  start.x := 1;
  start.y := 1;
  bfsMaxSteps(start, maxSteps);
  writeln(reachableLocations);
  Close(inputFile);
end.
