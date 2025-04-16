
program LumberCollection;

{$MODE DELPHI} // Use Delphi mode for dynamic arrays

uses
  SysUtils;

type
  TAcre = Char;
  TArea = array of array of TAcre;

const
  WIDTH = 50; // Assuming input is always 50x50
  HEIGHT = 50;

function ReadInitialState(const Filename: string): TArea;
var
  F: TextFile;
  I, J: Integer;
begin
  SetLength(Result, HEIGHT);
  for I := 0 to High(Result) do
    SetLength(Result[I], WIDTH);

  AssignFile(F, Filename);
  try
    Reset(F);
    for I := 0 to HEIGHT - 1 do
    begin
      for J := 0 to WIDTH - 1 do
        Read(F, Result[I, J]);
      Readln(F); // Consume newline character
    end;
  finally
    CloseFile(F);
  end;
end;

function CountAdjacent(const Area: TArea; Row, Col: Integer; AcreType: TAcre): Integer;
var
  I, J, Count: Integer;
begin
  Count := 0;
  for I := Row - 1 to Row + 1 do
    for J := Col - 1 to Col + 1 do
    begin
      if (I = Row) and (J = Col) then
        Continue; // Skip the center acre

      if (I >= 0) and (I < HEIGHT) and (J >= 0) and (J < WIDTH) then
      begin
        if Area[I, J] = AcreType then
          Inc(Count);
      end;
    end;
  Result := Count;
end;


function SimulateMinute(const Area: TArea): TArea;
var
  NewArea: TArea;
  I, J: Integer;
  AdjacentTrees, AdjacentLumberyards: Integer;
begin
  SetLength(NewArea, HEIGHT);
  for I := 0 to High(NewArea) do
    SetLength(NewArea[I], WIDTH);

  for I := 0 to HEIGHT - 1 do
    for J := 0 to WIDTH - 1 do
    begin
      case Area[I, J] of
        '.': // Open
          begin
            AdjacentTrees := CountAdjacent(Area, I, J, '|');
            if AdjacentTrees >= 3 then
              NewArea[I, J] := '|'
            else
              NewArea[I, J] := '.';
          end;
        '|': // Trees
          begin
            AdjacentLumberyards := CountAdjacent(Area, I, J, '#');
            if AdjacentLumberyards >= 3 then
              NewArea[I, J] := '#'
            else
              NewArea[I, J] := '|';
          end;
        '#': // Lumberyard
          begin
            if (CountAdjacent(Area, I, J, '#') >= 1) and (CountAdjacent(Area, I, J, '|') >= 1) then
              NewArea[I, J] := '#'
            else
              NewArea[I, J] := '.';
          end;
      end;
    end;

  Result := NewArea;
end;

function CalculateResourceValue(const Area: TArea): Integer;
var
  I, J, WoodedAcres, Lumberyards: Integer;
begin
  WoodedAcres := 0;
  Lumberyards := 0;

  for I := 0 to HEIGHT - 1 do
    for J := 0 to WIDTH - 1 do
    begin
      if Area[I, J] = '|' then
        Inc(WoodedAcres);
      if Area[I, J] = '#' then
        Inc(Lumberyards);
    end;

  Result := WoodedAcres * Lumberyards;
end;

var
  Area: TArea;
  Minute: Integer;

begin
  Area := ReadInitialState('input.txt'); //read input from file called input.txt

  for Minute := 1 to 10 do
  begin
    Area := SimulateMinute(Area);
  end;

  WriteLn(CalculateResourceValue(Area));

  // Clean up the dynamically allocated arrays. Important for memory management.
  for Minute := 0 to High(Area) do
    SetLength(Area[Minute], 0);
  SetLength(Area, 0);
end.
