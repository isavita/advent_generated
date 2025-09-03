program GridRocks;

{$mode objfpc}{$H+}

type
  TCoord = record
    X: Integer;
    Y: Integer;
  end;

  TGrid = record
    Width: Integer;
    Height: Integer;
    Data: array of array of Char;
  end;

const
  North: TCoord = (X: 0;  Y: -1);
  West:  TCoord = (X: -1; Y: 0);
  South: TCoord = (X: 0;  Y: 1);
  East:  TCoord = (X: 1;  Y: 0);

function InBounds(coord: TCoord; grid: TGrid): Boolean;
begin
  Result := (coord.X >= 0) and (coord.X < grid.Width) and
            (coord.Y >= 0) and (coord.Y < grid.Height);
end;

procedure ShiftSingleRock(var grid: TGrid; coord: TCoord; dir: TCoord);
var
  current, before: TCoord;
begin
  if grid.Data[coord.Y][coord.X] = 'O' then
  begin
    current := coord;
    before.X := current.X + dir.X;
    before.Y := current.Y + dir.Y;

    while InBounds(before, grid) and (grid.Data[before.Y][before.X] = '.') do
    begin
      grid.Data[before.Y][before.X] := 'O';
      grid.Data[current.Y][current.X] := '.';
      current := before;
      before.X := before.X + dir.X;
      before.Y := before.Y + dir.Y;
    end;
  end;
end;

procedure ShiftRocks(var grid: TGrid; dir: TCoord);
var
  x, y: Integer;
  c: TCoord;
begin
  if ((dir.X = 0) and (dir.Y = -1)) or ((dir.X = -1) and (dir.Y = 0)) then
  begin
    for x := 0 to grid.Width - 1 do
      for y := 0 to grid.Height - 1 do
      begin
        c.X := x; c.Y := y;
        ShiftSingleRock(grid, c, dir);
      end;
  end
  else
  begin
    for x := grid.Width - 1 downto 0 do
      for y := grid.Height - 1 downto 0 do
      begin
        c.X := x; c.Y := y;
        ShiftSingleRock(grid, c, dir);
      end;
  end;
end;

function CalculateLoad(grid: TGrid): Integer;
var
  x, y, load: Integer;
begin
  load := 0;
  for y := 0 to grid.Height - 1 do
    for x := 0 to grid.Width - 1 do
      if grid.Data[y][x] = 'O' then
        Inc(load, grid.Height - y);
  Result := load;
end;

procedure BuildGridFromLines(lines: array of string; rows, cols: Integer; var grid: TGrid);
var
  y, x: Integer;
begin
  grid.Width := cols;
  grid.Height := rows;
  SetLength(grid.Data, rows);
  for y := 0 to rows - 1 do
  begin
    SetLength(grid.Data[y], cols);
    for x := 0 to cols - 1 do
      grid.Data[y][x] := lines[y][x+1];
  end;
end;

var
  lines: array of string;
  rows, cols: Integer;
  grid: TGrid;
  f: Text;
  s: string;
  load: Integer;

begin
  AssignFile(f, 'input.txt');
  Reset(f);
  SetLength(lines, 0);
  while not Eof(f) do
  begin
    ReadLn(f, s);
    SetLength(lines, Length(lines) + 1);
    lines[Length(lines) - 1] := s;
  end;
  CloseFile(f);

  rows := Length(lines);
  if rows > 0 then cols := Length(lines[0]) else cols := 0;

  BuildGridFromLines(lines, rows, cols, grid);
  ShiftRocks(grid, North);
  load := CalculateLoad(grid);
  WriteLn(load);
end.