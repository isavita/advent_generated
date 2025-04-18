
program RobotSim;

{$mode objfpc}{$H+}

uses
  SysUtils;

const
  SIZE_X = 101;
  SIZE_Y = 103;
  MAX_ITER_PART2 = 1000000;

type
  TRobot = record
    X, Y, VX, VY: Integer;
  end;

  TRobotArray = array of TRobot;
  TBooleanGrid = array[0..SIZE_Y-1, 0..SIZE_X-1] of Boolean;
  TCharGrid = array[0..SIZE_Y-1, 0..SIZE_X-1] of Char;
  TCounts = array[0..3] of Integer;

function Modulo(a, b: Integer): Integer;
begin
  Result := a mod b;
  if Result < 0 then Result := Result + b;
end;

procedure ParseRobotLine(const Line: string; var Robot: TRobot);
var
  s: string;
  pPos, vPos, commaPos1, commaPos2: Integer;
  errorCode: Integer;
begin
  pPos := Pos('p=', Line);
  commaPos1 := Pos(',', Line, pPos + 2);
  vPos := Pos(' v=', Line, commaPos1 + 1);
  commaPos2 := Pos(',', Line, vPos + 3);

  s := Copy(Line, pPos + 2, commaPos1 - (pPos + 2));
  Val(s, Robot.X, errorCode);

  s := Copy(Line, commaPos1 + 1, vPos - (commaPos1 + 1));
  Val(s, Robot.Y, errorCode);

  s := Copy(Line, vPos + 3, commaPos2 - (vPos + 3));
  Val(s, Robot.VX, errorCode);

  s := Copy(Line, commaPos2 + 1, Length(Line) - commaPos2);
  Val(s, Robot.VY, errorCode);
end;

procedure MoveRobot(var Robot: TRobot; SizeX, SizeY: Integer);
begin
  Robot.X := Modulo(Robot.X + Robot.VX, SizeX);
  Robot.Y := Modulo(Robot.Y + Robot.VY, SizeY);
end;

procedure MoveAllRobots(var Robots: TRobotArray; SizeX, SizeY: Integer);
var
  i: Integer;
begin
  for i := 0 to High(Robots) do
    MoveRobot(Robots[i], SizeX, SizeY);
end;

procedure CountQuadrants(const Robots: TRobotArray; SizeX, SizeY: Integer; var Counts: TCounts);
var
  i: Integer;
  centerX, centerY: Integer;
begin
  centerX := SizeX div 2;
  centerY := SizeY div 2;

  Counts[0] := 0;
  Counts[1] := 0;
  Counts[2] := 0;
  Counts[3] := 0;

  for i := 0 to High(Robots) do
  begin
    if Robots[i].X < centerX then
    begin
      if Robots[i].Y < centerY then
        Inc(Counts[0])
      else if Robots[i].Y > centerY then
        Inc(Counts[1]);
    end
    else if Robots[i].X > centerX then
    begin
      if Robots[i].Y < centerY then
        Inc(Counts[2])
      else if Robots[i].Y > centerY then
        Inc(Counts[3]);
    end;
  end;
end;

function HasNoOverlaps(const Robots: TRobotArray; SizeX, SizeY: Integer): Boolean;
var
  Occupied: TBooleanGrid;
  i: Integer;
begin
  FillChar(Occupied, SizeOf(Occupied), 0);

  Result := True;
  for i := 0 to High(Robots) do
  begin
    if Occupied[Robots[i].Y, Robots[i].X] then
    begin
      Result := False;
      Exit;
    end;
    Occupied[Robots[i].Y, Robots[i].X] := True;
  end;
end;

procedure DrawGrid(const Robots: TRobotArray; SizeX, SizeY: Integer);
var
  Grid: TCharGrid;
  x, y, i: Integer;
begin
  for y := 0 to SizeY - 1 do
    for x := 0 to SizeX - 1 do
      Grid[y, x] := '.';

  for i := 0 to High(Robots) do
    Grid[Robots[i].Y, Robots[i].X] := '#';

  for y := 0 to SizeY - 1 do
  begin
    for x := 0 to SizeX - 1 do
      Write(Grid[y, x]);
    Writeln;
  end;
end;

var
  InputFile: TextFile;
  Line: string;
  InitialRobots: TRobotArray;
  RobotsPart1: TRobotArray;
  RobotsPart2: TRobotArray;
  RobotCount: Integer;
  Counts: TCounts;
  SafetyFactor: Int64;
  i: Integer;
  Seconds: Integer;

begin
  SetLength(InitialRobots, 0);
  AssignFile(InputFile, 'input.txt');
  Reset(InputFile);

  while not Eof(InputFile) do
  begin
    ReadLn(InputFile, Line);
    Line := Trim(Line);
    if Line <> '' then
    begin
      RobotCount := Length(InitialRobots);
      SetLength(InitialRobots, RobotCount + 1);
      ParseRobotLine(Line, InitialRobots[RobotCount]);
    end;
  end;
  CloseFile(InputFile);

  SetLength(RobotsPart1, Length(InitialRobots));
  if Length(InitialRobots) > 0 then
    Move(InitialRobots[0], RobotsPart1[0], SizeOf(TRobot) * Length(InitialRobots));

  for i := 1 to 100 do
    MoveAllRobots(RobotsPart1, SIZE_X, SIZE_Y);

  CountQuadrants(RobotsPart1, SIZE_X, SIZE_Y, Counts);

  SafetyFactor := Int64(Counts[0]) * Counts[1] * Counts[2] * Counts[3];
  Writeln('Part 1 - Safety Factor after 100 seconds: ', SafetyFactor);

  SetLength(RobotsPart2, Length(InitialRobots));
    if Length(InitialRobots) > 0 then
      Move(InitialRobots[0], RobotsPart2[0], SizeOf(TRobot) * Length(InitialRobots));

  Seconds := 0;
  while (Seconds < MAX_ITER_PART2) and (not HasNoOverlaps(RobotsPart2, SIZE_X, SIZE_Y)) do
  begin
    MoveAllRobots(RobotsPart2, SIZE_X, SIZE_Y);
    Inc(Seconds);
  end;

  if not HasNoOverlaps(RobotsPart2, SIZE_X, SIZE_Y) then
    Writeln('Exceeded maximum iterations without finding a unique position configuration.')
  else
    Writeln('Part 2 - Fewest seconds to display Easter egg: ', Seconds);

  Writeln('Final positions of robots:');
  DrawGrid(RobotsPart2, SIZE_X, SIZE_Y);

  SetLength(InitialRobots, 0);
  SetLength(RobotsPart1, 0);
  SetLength(RobotsPart2, 0);

end.
