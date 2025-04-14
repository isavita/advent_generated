
program SurfaceArea;

{$MODE Delphi}{$H+}

uses
  SysUtils;

type
  TCoordinate = record
    x, y, z: Integer;
  end;

function CalculateExposedSides(const cube: TCoordinate; const cubes: array of TCoordinate): Integer;
var
  directions: array[0..5] of TCoordinate;
  exposedSides, i, j: Integer;
  adjacent: TCoordinate;
  found: Boolean;
begin
  directions[0].x := 1; directions[0].y := 0; directions[0].z := 0;
  directions[1].x := -1; directions[1].y := 0; directions[1].z := 0;
  directions[2].x := 0; directions[2].y := 1; directions[2].z := 0;
  directions[3].x := 0; directions[3].y := -1; directions[3].z := 0;
  directions[4].x := 0; directions[4].y := 0; directions[4].z := 1;
  directions[5].x := 0; directions[5].y := 0; directions[5].z := -1;

  exposedSides := 6;
  for i := 0 to 5 do
  begin
    adjacent.x := cube.x + directions[i].x;
    adjacent.y := cube.y + directions[i].y;
    adjacent.z := cube.z + directions[i].z;

    found := False;
    for j := 0 to Length(cubes) - 1 do
    begin
      if (cubes[j].x = adjacent.x) and (cubes[j].y = adjacent.y) and (cubes[j].z = adjacent.z) then
      begin
        found := True;
        Break;
      end;
    end;
    if found then
      exposedSides := exposedSides - 1;
  end;
  CalculateExposedSides := exposedSides;
end;

var
  cubes: array of TCoordinate;
  line: string;
  inputFile: TextFile;
  coords: array[0..2] of Integer;
  surfaceArea, cubeCount, i: Integer;
  strCoords: array of string;

begin
  AssignFile(inputFile, 'input.txt');
  Reset(inputFile);
  cubeCount := 0;
  SetLength(cubes, 0);

  while not Eof(inputFile) do
  begin
    ReadLn(inputFile, line);
    strCoords := line.Split([','], 0);
    coords[0] := StrToInt(strCoords[0]);
    coords[1] := StrToInt(strCoords[1]);
    coords[2] := StrToInt(strCoords[2]);

    SetLength(cubes, Length(cubes) + 1);
    cubes[Length(cubes) - 1].x := coords[0];
    cubes[Length(cubes) - 1].y := coords[1];
    cubes[Length(cubes) - 1].z := coords[2];
    Inc(cubeCount);
  end;
  CloseFile(inputFile);

  surfaceArea := 0;
  for i := 0 to Length(cubes) - 1 do
  begin
    surfaceArea := surfaceArea + CalculateExposedSides(cubes[i], cubes);
  end;

  WriteLn(surfaceArea);
end.
