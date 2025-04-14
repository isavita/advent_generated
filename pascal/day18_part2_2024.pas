
program SolveMaze;

{$MODE OBJFPC} // Use Object Pascal extensions like Break if available, otherwise adapt.
{$OPTIMIZATION ON}

uses
  SysUtils; // For Val, Pos, Copy, Str

const
  GridSize = 71;
  MaxCoords = 10000; // Maximum expected coordinates
  MaxQueueSize = GridSize * GridSize; // Max BFS queue size = 71*71 = 5041
  TargetR = GridSize - 1;
  TargetC = GridSize - 1;

type
  TPoint = record
    x, y: Integer;
  end;
  TCoordArray = array[1..MaxCoords] of TPoint;
  TGrid = array[0..TargetR, 0..TargetC] of Char;

  TQueueItem = record
    r, c, dist: Integer;
  end;
  TQueueArray = array[0..MaxQueueSize-1] of TQueueItem;
  TVisitedArray = array[0..TargetR, 0..TargetC] of Boolean;

var
  inputFile: Text;
  bytePositions: TCoordArray;
  coordCount: Integer;
  grid: TGrid;
  r, c, i, x, y: Integer;
  line, part: string;
  commaPos: Integer;
  errorCode: Integer;
  part1Result: Integer;
  q: TQueueArray;        // BFS Queue
  visited: TVisitedArray; // BFS Visited Set
  head, tail: Integer;    // Queue pointers

// BFS Implementation (optimized to reuse global queue/visited arrays)
function BFS(const g: TGrid): Integer;
var
  currentItem: TQueueItem;
  cr, cc, dist, nr, nc: Integer;
  dr, dc: Integer;
  // Directions: right, left, down, up
  delta: array[1..4, 1..2] of Integer = ((0, 1), (0, -1), (1, 0), (-1, 0));
  idx: Integer;
begin
  // Reset Visited array
  for cr := 0 to TargetR do
    for cc := 0 to TargetC do
      visited[cr, cc] := False;

  // Reset Queue
  head := 0;
  tail := 0;

  // Add start node (0, 0) if valid
  if g[0, 0] = '.' then
  begin
    q[tail].r := 0;
    q[tail].c := 0;
    q[tail].dist := 0;
    tail := (tail + 1) mod MaxQueueSize;
    visited[0, 0] := True;
  end else
  begin
    Result := -1; // Start point blocked
    Exit;
  end;

  while head <> tail do
  begin
    // Dequeue
    currentItem := q[head];
    head := (head + 1) mod MaxQueueSize;

    cr := currentItem.r;
    cc := currentItem.c;
    dist := currentItem.dist;

    // Check if target reached
    if (cr = TargetR) and (cc = TargetC) then
    begin
      Result := dist; // Found shortest path
      Exit;
    end;

    // Explore neighbors
    for idx := 1 to 4 do
    begin
      nr := cr + delta[idx, 1]; // Delta R
      nc := cc + delta[idx, 2]; // Delta C

      // Check validity (bounds, obstacle, visited)
      if (nr >= 0) and (nr <= TargetR) and
         (nc >= 0) and (nc <= TargetC) and
         (g[nr, nc] = '.') and
         not visited[nr, nc] then
      begin
        visited[nr, nc] := True;
        // Enqueue
        q[tail].r := nr;
        q[tail].c := nc;
        q[tail].dist := dist + 1;
        tail := (tail + 1) mod MaxQueueSize;
        // Basic overflow check (should not happen with MaxQueueSize = R*C)
        // if tail = head then Halt(1);
      end;
    end;
  end;

  // Target not reachable
  Result := -1;
end;

// Procedure to initialize the grid to all '.'
procedure InitializeGrid(var g: TGrid);
var
  r_init, c_init: Integer;
begin
  for r_init := 0 to TargetR do
    for c_init := 0 to TargetC do
      g[r_init, c_init] := '.';
end;

begin
  // Read coordinates from input file
  AssignFile(inputFile, 'input.txt');
  try
    Reset(inputFile);
    coordCount := 0;
    while not Eof(inputFile) and (coordCount < MaxCoords) do
    begin
      ReadLn(inputFile, line);
      commaPos := Pos(',', line);
      if commaPos > 0 then
      begin
        part := Copy(line, 1, commaPos - 1);
        Val(part, x, errorCode);
        if errorCode = 0 then
        begin
          part := Copy(line, commaPos + 1, Length(line) - commaPos);
          Val(part, y, errorCode);
          if errorCode = 0 then
          begin
            Inc(coordCount);
            bytePositions[coordCount].x := x;
            bytePositions[coordCount].y := y;
          end;
        end;
      end;
    end;
  finally
    CloseFile(inputFile);
  end;

  // Part 1
  InitializeGrid(grid);
  i := 1;
  while (i <= coordCount) and (i <= 1024) do
  begin
     y := bytePositions[i].y;
     x := bytePositions[i].x;
     // Place obstacle only if within grid bounds
     if (y >= 0) and (y <= TargetR) and (x >= 0) and (x <= TargetC) then
         grid[y, x] := '#';
     Inc(i);
  end;

  part1Result := BFS(grid);
  WriteLn(part1Result);

  // Part 2
  InitializeGrid(grid);
  for i := 1 to coordCount do
  begin
    y := bytePositions[i].y;
    x := bytePositions[i].x;

    // Add the obstacle only if within grid bounds
    if (y >= 0) and (y <= TargetR) and (x >= 0) and (x <= TargetC) then
    begin
        grid[y, x] := '#';
        // Run BFS to check connectivity after adding the obstacle
        if BFS(grid) = -1 then
        begin
            WriteLn(x, ',', y);
            Halt; // Exit after finding the first blocking obstacle
        end;
    end;
     // If obstacle out of bounds, ignore it and continue
  end;

end.
