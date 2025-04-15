
program RestroomRedoubt;

{$MODE DELPHI} // Use Delphi mode for string operations

uses
  SysUtils;

type
  TRobot = record
    x, y, vx, vy: Integer;
  end;

const
  WIDTH = 101;
  HEIGHT = 103;
  TIME = 100;

var
  Robots: array of TRobot;
  NumRobots: Integer;

function ParseRobot(Line: string; var Robot: TRobot): Boolean;
var
  PosStr, VelStr: string;
  XStr, YStr, VXStr, VYStr: string;
  CommaPos: Integer;
begin
  try
    PosStr := Copy(Line, Pos('p=', Line) + 2, Pos(' v=', Line) - Pos('p=', Line) - 2);
    VelStr := Copy(Line, Pos('v=', Line) + 2, Length(Line) - Pos('v=', Line) - 1);

    CommaPos := Pos(',', PosStr);
    XStr := Copy(PosStr, 1, CommaPos - 1);
    YStr := Copy(PosStr, CommaPos + 1, Length(PosStr) - CommaPos);

    CommaPos := Pos(',', VelStr);
    VXStr := Copy(VelStr, 1, CommaPos - 1);
    VYStr := Copy(VelStr, CommaPos + 1, Length(VelStr) - CommaPos);

    Robot.x := StrToInt(XStr);
    Robot.y := StrToInt(YStr);
    Robot.vx := StrToInt(VXStr);
    Robot.vy := StrToInt(VYStr);

    Result := True;
  except
    Result := False;
  end;
end;

function CalculateSafetyFactor(Robots: array of TRobot; NumRobots: Integer): Integer;
var
  QuadrantCounts: array[1..4] of Integer;
  i: Integer;
  FinalX, FinalY: Integer;
begin
  QuadrantCounts[1] := 0; // Top-Left
  QuadrantCounts[2] := 0; // Top-Right
  QuadrantCounts[3] := 0; // Bottom-Left
  QuadrantCounts[4] := 0; // Bottom-Right

  for i := 0 to NumRobots - 1 do
  begin
    FinalX := (Robots[i].x + Robots[i].vx * TIME) mod WIDTH;
    FinalY := (Robots[i].y + Robots[i].vy * TIME) mod HEIGHT;

    if FinalX < 0 then FinalX := FinalX + WIDTH;
    if FinalY < 0 then FinalY := FinalY + HEIGHT;
    
    if (FinalX < WIDTH div 2) and (FinalY < HEIGHT div 2) then
      QuadrantCounts[1] := QuadrantCounts[1] + 1
    else if (FinalX > WIDTH div 2) and (FinalY < HEIGHT div 2) then
      QuadrantCounts[2] := QuadrantCounts[2] + 1
    else if (FinalX < WIDTH div 2) and (FinalY > HEIGHT div 2) then
      QuadrantCounts[3] := QuadrantCounts[3] + 1
    else if (FinalX > WIDTH div 2) and (FinalY > HEIGHT div 2) then
      QuadrantCounts[4] := QuadrantCounts[4] + 1;
  end;

  Result := QuadrantCounts[1] * QuadrantCounts[2] * QuadrantCounts[3] * QuadrantCounts[4];
end;

var
  InputFile: TextFile;
  Line: string;
  Robot: TRobot;
  i: Integer;

begin
  // Initialize robot array size and counter
  SetLength(Robots, 0);
  NumRobots := 0;

  // Read robot data from file
  AssignFile(InputFile, 'input.txt');
  try
    Reset(InputFile);
    while not Eof(InputFile) do
    begin
      ReadLn(InputFile, Line);
      if ParseRobot(Line, Robot) then
      begin
        SetLength(Robots, Length(Robots) + 1);
        Robots[NumRobots] := Robot;
        Inc(NumRobots);
      end;
    end;
    CloseFile(InputFile);
  except
    on E: Exception do
    begin
      Writeln('Error reading input file: ', E.Message);
      Halt(1);
    end;
  end;

  // Calculate and print the safety factor
  Writeln(CalculateSafetyFactor(Robots, NumRobots));

end.
