
program Solve;

{$mode delphi}{$H+}

uses
  SysUtils, Math;

type
  TCoord = record
    x, y: Int64; // Use Int64 for potentially large expanded coordinates
  end;
  TCoordDynArray = array of TCoord;
  TIntegerDynArray = array of Integer;
  TBooleanDynArray = array of Boolean;
  TStringDynArray = array of string;

const
  ExpansionFactor = 1000000;
  EmptyChar = '.';
  InputFileName = 'input.txt';

// Function to calculate offsets based on empty rows/columns
function CalculateOffsets(const emptyIndexes: TIntegerDynArray; bound: Integer): TIntegerDynArray;
var
  i, idx: Integer;
begin
  SetLength(Result, bound);
  for i := 0 to bound - 1 do
    Result[i] := 0;

  for idx in emptyIndexes do
  begin
    // Indices are 0-based, affect subsequent elements
    for i := idx + 1 to bound - 1 do
      Result[i] := Result[i] + 1;
  end;
end;

// Function to calculate Manhattan distance
function CalculateManhattanDistance(const c1, c2: TCoord): Int64;
begin
  Result := Abs(c2.x - c1.x) + Abs(c2.y - c1.y);
end;

var
  inputFile: TextFile;
  line: string;
  lines: TStringDynArray;
  galaxies: TCoordDynArray;
  emptyRows, emptyCols: TIntegerDynArray;
  rowOffsets, colOffsets: TIntegerDynArray;
  hasGalaxyInRow, hasGalaxyInCol: TBooleanDynArray;
  expandedGalaxies: TCoordDynArray;
  width, height: Integer;
  x, y, i, j: Integer;
  galaxyCount: Integer;
  totalDistance: Int64;
  numLinesToAdd: Int64;
  newX, newY: Int64;

begin
  // Read input file
  AssignFile(inputFile, InputFileName);
  try
    Reset(inputFile);
    galaxyCount := 0;
    height := 0;
    width := 0;
    SetLength(lines, 0); // Initialize dynamic array
    while not Eof(inputFile) do
    begin
      ReadLn(inputFile, line);
      if length(line) > 0 then
      begin
         SetLength(lines, Length(lines) + 1);
         lines[High(lines)] := line;
         if width = 0 then
             width := Length(line);
         Inc(height);
      end;
    end;
  finally
    CloseFile(inputFile);
  end;

  if (width = 0) or (height = 0) then
  begin
    WriteLn(0);
    Halt;
  end;

  // Identify galaxy locations and mark rows/cols containing galaxies
  SetLength(galaxies, 0);
  SetLength(hasGalaxyInRow, height);
  SetLength(hasGalaxyInCol, width);
  for y := 0 to height - 1 do hasGalaxyInRow[y] := False;
  for x := 0 to width - 1 do hasGalaxyInCol[x] := False;

  for y := 0 to height - 1 do
  begin
    for x := 0 to width - 1 do
    begin
      if lines[y][x + 1] <> EmptyChar then // Pascal strings are 1-based
      begin
        SetLength(galaxies, Length(galaxies) + 1);
        galaxies[High(galaxies)].x := x;
        galaxies[High(galaxies)].y := y;
        hasGalaxyInRow[y] := True;
        hasGalaxyInCol[x] := True;
      end;
    end;
  end;

  // Find empty rows
  SetLength(emptyRows, 0);
  for y := 0 to height - 1 do
  begin
    if not hasGalaxyInRow[y] then
    begin
      SetLength(emptyRows, Length(emptyRows) + 1);
      emptyRows[High(emptyRows)] := y;
    end;
  end;

  // Find empty columns
  SetLength(emptyCols, 0);
  for x := 0 to width - 1 do
  begin
    if not hasGalaxyInCol[x] then
    begin
      SetLength(emptyCols, Length(emptyCols) + 1);
      emptyCols[High(emptyCols)] := x;
    end;
  end;

  // Calculate offsets for expansion
  rowOffsets := CalculateOffsets(emptyRows, height);
  colOffsets := CalculateOffsets(emptyCols, width);

  // Calculate expanded coordinates
  numLinesToAdd := ExpansionFactor - 1;
  galaxyCount := Length(galaxies);
  SetLength(expandedGalaxies, galaxyCount);

  for i := 0 to galaxyCount - 1 do
  begin
    x := galaxies[i].x;
    y := galaxies[i].y;
    newX := galaxies[i].x + colOffsets[x] * numLinesToAdd;
    newY := galaxies[i].y + rowOffsets[y] * numLinesToAdd;
    expandedGalaxies[i].x := newX;
    expandedGalaxies[i].y := newY;
  end;

  // Calculate total distance between all pairs of galaxies
  totalDistance := 0;
  for i := 0 to galaxyCount - 2 do // Iterate up to second-to-last
  begin
    for j := i + 1 to galaxyCount - 1 do // Iterate from next element to last
    begin
      totalDistance := totalDistance + CalculateManhattanDistance(expandedGalaxies[i], expandedGalaxies[j]);
    end;
  end;

  // Print the result
  WriteLn(totalDistance);

end.
