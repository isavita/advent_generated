
program PlanetOfDiscord;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  TGrid = array[0..4, 0..4] of Boolean;

function GridToInteger(const Grid: TGrid): Integer;
var
  X, Y: Integer;
  Power: Integer;
begin
  Result := 0;
  Power := 1;
  for Y := 0 to 4 do
    for X := 0 to 4 do
    begin
      if Grid[X, Y] then
        Result := Result + Power;
      Power := Power * 2;
    end;
end;

function CountAdjacentBugs(const Grid: TGrid; X, Y: Integer): Integer;
var
  Count: Integer;
begin
  Count := 0;
  if X > 0 then if Grid[X - 1, Y] then Inc(Count);
  if X < 4 then if Grid[X + 1, Y] then Inc(Count);
  if Y > 0 then if Grid[X, Y - 1] then Inc(Count);
  if Y < 4 then if Grid[X, Y + 1] then Inc(Count);
  Result := Count;
end;

procedure NextMinute(const CurrentGrid: TGrid; var NextGrid: TGrid);
var
  X, Y: Integer;
  AdjacentBugs: Integer;
begin
  for Y := 0 to 4 do
    for X := 0 to 4 do
    begin
      AdjacentBugs := CountAdjacentBugs(CurrentGrid, X, Y);
      if CurrentGrid[X, Y] then
      begin
        // A bug dies (becoming an empty space) unless there is exactly one bug adjacent to it.
        NextGrid[X, Y] := (AdjacentBugs = 1);
      end
      else
      begin
        // An empty space becomes infested with a bug if exactly one or two bugs are adjacent to it.
        NextGrid[X, Y] := (AdjacentBugs = 1) or (AdjacentBugs = 2);
      end;
    end;
end;

var
  InputFile: TextFile;
  InitialGrid, CurrentGrid, NextGrid: TGrid;
  X, Y: Integer;
  Line: String;
  Biodiversity: Integer;
  SeenLayouts: array of Integer;
  FoundDuplicate: Boolean;
  LayoutInteger: Integer;
  I: Integer;
  NumLayouts: Integer;

begin
  // Read initial grid from input.txt
  Assign(InputFile, 'input.txt');
  Reset(InputFile);
  try
    for Y := 0 to 4 do
    begin
      ReadLn(InputFile, Line);
      for X := 0 to 4 do
        InitialGrid[X, Y] := (Line[X + 1] = '#');
    end;
  finally
    Close(InputFile);
  end;

  // Initialize
  CurrentGrid := InitialGrid;
  SetLength(SeenLayouts, 0);
  FoundDuplicate := False;
  NumLayouts := 0;


  // Simulate until a duplicate layout is found
  while not FoundDuplicate do
  begin
    LayoutInteger := GridToInteger(CurrentGrid);

    // Check if layout has been seen before
    for I := 0 to NumLayouts - 1 do
    begin
      if SeenLayouts[I] = LayoutInteger then
      begin
        FoundDuplicate := True;
        break;
      end;
    end;

    // If not seen before, add to list of seen layouts
    if not FoundDuplicate then
    begin
      SetLength(SeenLayouts, NumLayouts + 1);
      SeenLayouts[NumLayouts] := LayoutInteger;
      Inc(NumLayouts);

      NextMinute(CurrentGrid, NextGrid);
      CurrentGrid := NextGrid; //Deep copy

    end;
  end;

  // Calculate and print biodiversity rating
  Biodiversity := GridToInteger(CurrentGrid);
  WriteLn(Biodiversity);

end.
