program Solution;

const
  gridSize = 100;
  steps = 100;

var
  inputFile: text;
  grid: array[1..gridSize, 1..gridSize] of Boolean;
  x, y, dx, dy, onNeighbors, i, onCount: Integer;
  line: String;

function countOnNeighbors(x, y: Integer): Integer;
var
  on: Integer;
begin
  on := 0;
  for dx := -1 to 1 do
  begin
    for dy := -1 to 1 do
    begin
      if (dx <> 0) or (dy <> 0) then
      begin
        if (x + dx >= 1) and (x + dx <= gridSize) and (y + dy >= 1) and (y + dy <= gridSize) and grid[x + dx, y + dy] then
          Inc(on);
      end;
    end;
  end;
  countOnNeighbors := on;
end;

function step: Boolean;
var
  newGrid: array[1..gridSize, 1..gridSize] of Boolean;
begin
  for x := 1 to gridSize do
  begin
    for y := 1 to gridSize do
    begin
      onNeighbors := countOnNeighbors(x, y);
      if grid[x, y] then
        newGrid[x, y] := (onNeighbors = 2) or (onNeighbors = 3)
      else
        newGrid[x, y] := onNeighbors = 3;
    end;
  end;

  newGrid[1, 1] := True;
  newGrid[1, gridSize] := True;
  newGrid[gridSize, 1] := True;
  newGrid[gridSize, gridSize] := True;

  grid := newGrid;
end;

begin
  Assign(inputFile, 'input.txt');
  Reset(inputFile);

  y := 1;
  while not Eof(inputFile) do
  begin
    Readln(inputFile, line);
    for x := 1 to Length(line) do
    begin
      grid[x, y] := line[x] = '#';
    end;
    Inc(y);
  end;

  grid[1, 1] := True;
  grid[1, gridSize] := True;
  grid[gridSize, 1] := True;
  grid[gridSize, gridSize] := True;

  for i := 1 to steps do
  begin
    step;
  end;

  onCount := 0;
  for x := 1 to gridSize do
  begin
    for y := 1 to gridSize do
    begin
      if grid[x, y] then
        Inc(onCount);
    end;
  end;

  Writeln(onCount);

  Close(inputFile);
end.