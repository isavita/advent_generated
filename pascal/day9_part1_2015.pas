
program ShortestRoute;

{$MODE DELPHI}{$APPTYPE CONSOLE}

uses
  SysUtils, Classes;

type
  TDistance = record
    City1, City2: string;
    Distance: Integer;
  end;

  TDistances = array of TDistance;
  TLocations = TStringList;

var
  Distances: TDistances;
  Locations: TLocations;
  MinDistance: Integer;
  Visited: array of Boolean;
  Path: array of string;
  NumLocations: Integer;

function CalculateDistance(const Path: array of string): Integer;
var
  i, j: Integer;
  Distance: Integer;
begin
  Distance := 0;
  for i := 0 to NumLocations - 2 do
  begin
    for j := Low(Distances) to High(Distances) do
    begin
      if (Distances[j].City1 = Path[i]) and (Distances[j].City2 = Path[i + 1]) then
      begin
        Distance := Distance + Distances[j].Distance;
        Break;
      end;
    end;
  end;
  CalculateDistance := Distance;
end;

procedure Permute(Depth: Integer);
var
  i: Integer;
  Distance: Integer;
begin
  if Depth = NumLocations then
  begin
    Distance := CalculateDistance(Path);
    if Distance < MinDistance then
      MinDistance := Distance;
  end
  else
  begin
    for i := 0 to NumLocations - 1 do
    begin
      if not Visited[i] then
      begin
        Visited[i] := True;
        Path[Depth] := Locations[i];
        Permute(Depth + 1);
        Visited[i] := False;
      end;
    end;
  end;
end;

var
  inputFile: TextFile;
  line: string;
  parts: array[0..4] of string;
  i, j: Integer;
  City1, City2: string;
  Distance: Integer;
  Found: Boolean;
begin
  Locations := TStringList.Create;
  try
    Assign(inputFile, 'input.txt');
    Reset(inputFile);

    SetLength(Distances, 0);

    while not Eof(inputFile) do
    begin
      ReadLn(inputFile, line);
      parts[0] := Trim(Copy(line, 1, Pos(' ', line) - 1));
      Delete(line, 1, Pos(' ', line));
      line := Trim(line);

      parts[1] := Trim(Copy(line, 1, Pos(' ', line) - 1));
      Delete(line, 1, Pos(' ', line));
      line := Trim(line);

      parts[2] := Trim(Copy(line, 1, Pos(' ', line) - 1));
      Delete(line, 1, Pos(' ', line));
      line := Trim(line);

      parts[3] := Trim(Copy(line, 1, Pos(' ', line) - 1));
      Delete(line, 1, Pos(' ', line));
      line := Trim(line);

      parts[4] := Trim(line);

      City1 := parts[0];
      City2 := parts[2];
      Distance := StrToInt(parts[4]);

      SetLength(Distances, Length(Distances) + 1);
      Distances[High(Distances)].City1 := City1;
      Distances[High(Distances)].City2 := City2;
      Distances[High(Distances)].Distance := Distance;

      Found := False;
      for i := 0 to Locations.Count - 1 do
      begin
        if Locations[i] = City1 then
        begin
          Found := True;
          Break;
        end;
      end;
      if not Found then
        Locations.Add(City1);

      Found := False;
      for i := 0 to Locations.Count - 1 do
      begin
        if Locations[i] = City2 then
        begin
          Found := True;
          Break;
        end;
      end;
      if not Found then
        Locations.Add(City2);

      SetLength(Distances, Length(Distances) + 1);
      Distances[High(Distances)].City1 := City2;
      Distances[High(Distances)].City2 := City1;
      Distances[High(Distances)].Distance := Distance;

    end;

    CloseFile(inputFile);

    NumLocations := Locations.Count;
    SetLength(Visited, NumLocations);
    SetLength(Path, NumLocations);
    for i := 0 to NumLocations - 1 do
      Visited[i] := False;

    MinDistance := MaxInt;
    Permute(0);

    WriteLn(MinDistance);

  finally
    Locations.Free;
  end;
end.
