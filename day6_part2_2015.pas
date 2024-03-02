program Day6BrightnessControl;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

const
  GRID_SIZE = 1000;

type
  TBrightnessGrid = array[0..GRID_SIZE-1, 0..GRID_SIZE-1] of Integer;

var
  grid: TBrightnessGrid;
  inputFile: TextFile;
  line: string;
  action, coords: string;
  xStart, yStart, xEnd, yEnd, x, y: Integer;

procedure AdjustBrightness(xStart, yStart, xEnd, yEnd: Integer; action: string);
var
  deltaX, deltaY: Integer;
begin
  for deltaX := xStart to xEnd do
    for deltaY := yStart to yEnd do
      case action of
        'turn on': Inc(grid[deltaX, deltaY]);
        'turn off': if grid[deltaX, deltaY] > 0 then Dec(grid[deltaX, deltaY]);
        'toggle': Inc(grid[deltaX, deltaY], 2);
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
    AdjustBrightness(xStart, yStart, xEnd, yEnd, action);
  end;
  CloseFile(inputFile);

  // Calculate total brightness
  y := 0;
  for xStart := 0 to GRID_SIZE - 1 do
    for yStart := 0 to GRID_SIZE - 1 do
      Inc(y, grid[xStart, yStart]);

  WriteLn('Total brightness: ', y);
end.
