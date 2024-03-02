program solution;

uses
  SysUtils;

const
  gridSize = 100;
  steps = 100;

function countOnNeighbors(grid: array of array of boolean; x, y: integer): integer;
var
  on, dx, dy, nx, ny: integer;
begin
  on := 0;
  for dx := -1 to 1 do
  begin
    for dy := -1 to 1 do
    begin
      if (dx = 0) and (dy = 0) then
        continue;
      nx := x + dx;
      ny := y + dy;
      if (nx >= 0) and (nx < gridSize) and (ny >= 0) and (ny < gridSize) and grid[nx][ny] then
        on := on + 1;
    end;
  end;
  countOnNeighbors := on;
end;

function step(grid: array of array of boolean): array of array of boolean;
var
  newGrid: array of array of boolean;
  x, y, onNeighbors: integer;
begin
  SetLength(newGrid, gridSize);
  for x := 0 to gridSize - 1 do
    SetLength(newGrid[x], gridSize);

  for x := 0 to gridSize - 1 do
  begin
    for y := 0 to gridSize - 1 do
    begin
      onNeighbors := countOnNeighbors(grid, x, y);
      if grid[x][y] then
        newGrid[x][y] := (onNeighbors = 2) or (onNeighbors = 3)
      else
        newGrid[x][y] := (onNeighbors = 3);
    end;
  end;

  step := newGrid;
end;

var
  fileInput: TextFile;
  grid: array of array of boolean;
  line: string;
  x, y, i, onCount: integer;
begin
  AssignFile(fileInput, 'input.txt');
  Reset(fileInput);

  SetLength(grid, gridSize);
  for i := 0 to gridSize - 1 do
    SetLength(grid[i], gridSize);

  y := 0;
  while not Eof(fileInput) do
  begin
    ReadLn(fileInput, line);
    for x := 1 to Length(line) do
      grid[x - 1][y] := line[x] = '#';
    y := y + 1;
  end;

  CloseFile(fileInput);

  for i := 1 to steps do
    grid := step(grid);

  onCount := 0;
  for y := 0 to gridSize - 1 do
  begin
    for x := 0 to gridSize - 1 do
    begin
      if grid[x][y] then
        onCount := onCount + 1;
    end;
  end;

  WriteLn(onCount);
end.