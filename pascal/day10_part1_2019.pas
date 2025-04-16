
program AsteroidDetection;

{$MODE DELPHI}

uses
  SysUtils, Math;

type
  TAsteroid = record
    X, Y: Integer;
  end;

function GetAngle(const A, B: TAsteroid): Double;
begin
  Result := ArcTan2(B.Y - A.Y, B.X - A.X);
end;

function CountVisibleAsteroids(const Station: TAsteroid; const Asteroids: array of TAsteroid): Integer;
var
  Angles: array of Double;
  AngleCount, I, J: Integer;
  Angle: Double;
  Found: Boolean;
begin
  SetLength(Angles, 0);
  AngleCount := 0;

  for I := Low(Asteroids) to High(Asteroids) do
  begin
    if (Asteroids[I].X <> Station.X) or (Asteroids[I].Y <> Station.Y) then
    begin
      Angle := GetAngle(Station, Asteroids[I]);
      Found := False;
      for J := 0 to AngleCount - 1 do
      begin
        if Abs(Angles[J] - Angle) < 1e-9 then
        begin
          Found := True;
          Break;
        end;
      end;
      if not Found then
      begin
        SetLength(Angles, AngleCount + 1);
        Angles[AngleCount] := Angle;
        Inc(AngleCount);
      end;
    end;
  end;
  Result := AngleCount;
end;

function FindBestLocation(const Asteroids: array of TAsteroid): Integer;
var
  MaxVisible, Visible, I: Integer;
  Station: TAsteroid;
begin
  MaxVisible := 0;
  for I := Low(Asteroids) to High(Asteroids) do
  begin
    Station := Asteroids[I];
    Visible := CountVisibleAsteroids(Station, Asteroids);
    if Visible > MaxVisible then
    begin
      MaxVisible := Visible;
    end;
  end;
  Result := MaxVisible;
end;

var
  InputFile: TextFile;
  Line: String;
  Asteroids: array of TAsteroid;
  AsteroidCount, X, Y, I: Integer;
begin
  Assign(InputFile, 'input.txt');
  Reset(InputFile);

  AsteroidCount := 0;
  SetLength(Asteroids, 0);
  Y := 0;

  while not Eof(InputFile) do
  begin
    ReadLn(InputFile, Line);
    for X := 1 to Length(Line) do
    begin
      if Line[X] = '#' then
      begin
        SetLength(Asteroids, AsteroidCount + 1);
        Asteroids[AsteroidCount].X := X - 1;
        Asteroids[AsteroidCount].Y := Y;
        Inc(AsteroidCount);
      end;
    end;
    Inc(Y);
  end;

  CloseFile(InputFile);

  WriteLn(FindBestLocation(Asteroids));
end.
