
program MazeSolver;

const
  MaxQueueSize = 20000; // Increased queue size for safety
  MaxCoord = 100;      // Max coordinate dimension for visited array
  StartX = 1;
  StartY = 1;
  TargetX = 31;
  TargetY = 39;

type
  Point = record
    x, y: Longint;
  end;

  QueueItem = record
    pos: Point;
    steps: Longint;
  end;

var
  favorite_number: Longint;
  queue: array[1..MaxQueueSize] of QueueItem;
  qHead, qTail: Longint;
  visited: array[0..MaxCoord, 0..MaxCoord] of Boolean;
  inputFile: Text;

function CountSetBits(n: Longint): Integer;
var
  count: Integer;
begin
  count := 0;
  while n > 0 do
  begin
    if (n and 1) = 1 then
      count := count + 1;
    n := n shr 1;
  end;
  CountSetBits := count;
end;

function IsWall(x, y: Longint): Boolean;
var
  num: Longint;
  bit_count: Integer;
begin
  if (x < 0) or (y < 0) then
  begin
    IsWall := True; // Treat negative coordinates as walls
    Exit;
  end;
  num := x*x + 3*x + 2*x*y + y + y*y + favorite_number;
  bit_count := CountSetBits(num);
  IsWall := (bit_count mod 2) <> 0;
end;

function Bfs(startPos, targetPos: Point): Longint;
var
  currentItem: QueueItem;
  currentX, currentY, newX, newY, i: Longint;
  dx: array[1..4] of Integer = (1, -1, 0, 0);
  dy: array[1..4] of Integer = (0, 0, 1, -1);
  nextItem: QueueItem;
begin
  // Initialize Visited Array
  for currentX := 0 to MaxCoord do
    for currentY := 0 to MaxCoord do
      visited[currentX, currentY] := False;

  // Initialize Queue
  qHead := 1;
  qTail := 0;

  // Enqueue Start Position
  qTail := qTail + 1;
  queue[qTail].pos := startPos;
  queue[qTail].steps := 0;
  visited[startPos.x, startPos.y] := True; // Mark start as visited immediately

  Bfs := -1; // Default return value if target not found

  while qHead <= qTail do
  begin
    currentItem := queue[qHead];
    qHead := qHead + 1;

    currentX := currentItem.pos.x;
    currentY := currentItem.pos.y;

    if (currentX = targetPos.x) and (currentY = targetPos.y) then
    begin
      Bfs := currentItem.steps;
      Exit; // Found the target
    end;

    // Explore Neighbors
    for i := 1 to 4 do
    begin
      newX := currentX + dx[i];
      newY := currentY + dy[i];

      // Check bounds and if valid (not wall, not visited)
      if (newX >= 0) and (newY >= 0) and
         (newX <= MaxCoord) and (newY <= MaxCoord) then
      begin
          if not visited[newX, newY] and not IsWall(newX, newY) then
          begin
            visited[newX, newY] := True;

            // Check for queue overflow before adding
            if qTail < MaxQueueSize then
            begin
                qTail := qTail + 1;
                nextItem.pos.x := newX;
                nextItem.pos.y := newY;
                nextItem.steps := currentItem.steps + 1;
                queue[qTail] := nextItem;
            end
            else
            begin
                 // Handle queue overflow - might indicate an issue or need larger queue
                 Writeln('Error: Queue overflow');
                 Exit; // Exit BFS function on overflow
            end;
          end;
      end;
    end;
  end;
end;

var
  startPos, targetPos: Point;
  result: Longint;
begin
  Assign(inputFile, 'input.txt');
  Reset(inputFile);
  ReadLn(inputFile, favorite_number);
  Close(inputFile);

  startPos.x := StartX;
  startPos.y := StartY;
  targetPos.x := TargetX;
  targetPos.y := TargetY;

  result := Bfs(startPos, targetPos);

  WriteLn(result);
end.
