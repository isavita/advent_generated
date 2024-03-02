program Day6FireHazard;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

const
  GRID_SIZE = 1000;

type
  TLightGrid = array[0..GRID_SIZE-1, 0..GRID_SIZE-1] of Boolean;

var
  grid: TLightGrid;
  inputFile: TextFile;
  line: string;
  action, coords: string;
  xStart, yStart, xEnd, yEnd, x, y: Integer;

procedure TurnOnOffToggle(xStart, yStart, xEnd, yEnd: Integer; action: string);
begin
  for x := xStart to xEnd do
    for y := yStart to yEnd do
      case action of
        'turn on': grid[x, y] := True;
        'turn off': grid[x, y] := False;
        'toggle': grid[x, y] := not grid[x, y];
      end;
end;

begin
  // Initialize grid
  FillChar(grid, SizeOf(grid), 0);

  AssignFile(inputFile, 'input.txt');
  Reset(inputFile);
  while not EOF(inputFile) do
  begin
    ReadLn(inputFile, line);
    // Extract action and coordinates
    if Pos('turn on', line) = 1 then
    begin
      action := 'turn on';
      Delete(line, 1, 8);
    end
    else if Pos('turn off', line) = 1 then
    begin
      action := 'turn off';
      Delete(line, 1, 9);
    end
    else if Pos('toggle', line) = 1 then
    begin
      action := 'toggle';
      Delete(line, 1, 7);
    end;
    coords := StringReplace(line, ' through ', ',', [rfReplaceAll]);
    // Parse coordinates
    xStart := StrToInt(Copy(coords, 1, Pos(',', coords)-1));
    Delete(coords, 1, Pos(',', coords));
    yStart := StrToInt(Copy(coords, 1, Pos(',', coords)-1));
    Delete(coords, 1, Pos(',', coords));
    xEnd := StrToInt(Copy(coords, 1, Pos(',', coords)-1));
    Delete(coords, 1, Pos(',', coords));
    yEnd := StrToInt(coords);

    // Apply action
    TurnOnOffToggle(xStart, yStart, xEnd, yEnd, action);
  end;
  CloseFile(inputFile);

  // Count lit lights
  y := 0;
  for xStart := 0 to GRID_SIZE - 1 do
    for yStart := 0 to GRID_SIZE - 1 do
      if grid[xStart, yStart] then Inc(y);

  WriteLn('Lights on: ', y);
end.
