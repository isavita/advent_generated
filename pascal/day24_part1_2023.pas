
program IntersectSolution;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  TCoordInt = record
    x, y, z: Int64;
  end;
  TPoint = record
    pos, vel: TCoordInt;
  end;

var
  Points: array of TPoint;

function ReadIntFromLine(s: string; var i: Integer): Int64;
var
  sign: Integer;
  val: Int64;
begin
  while (i <= Length(s)) and not (((s[i] >= '0') and (s[i] <= '9')) or (s[i] = '-')) do Inc(i);
  sign := 1;
  if (i <= Length(s)) and (s[i] = '-') then begin
    sign := -1;
    Inc(i);
  end;
  val := 0;
  while (i <= Length(s)) and (s[i] >= '0') and (s[i] <= '9') do
  begin
    val := val * 10 + (Ord(s[i]) - Ord('0'));
    Inc(i);
  end;
  ReadIntFromLine := val * sign;
end;

function IsIntersect(p1, p2: TPoint; var coordX, coordY, t1, t2: Double): Boolean;
var
  det: Int64;
  detD: Double;
begin
  det := p1.vel.x * p2.vel.y - p2.vel.x * p1.vel.y;
  if det = 0 then begin
    coordX := 0; coordY := 0; t1 := 0; t2 := 0;
    Exit(False);
  end;
  detD := det;
  t1 := ((p2.vel.y * (p2.pos.x - p1.pos.x)) - (p2.vel.x * (p2.pos.y - p1.pos.y))) / detD;
  t2 := ((p1.vel.y * (p2.pos.x - p1.pos.x)) - (p1.vel.x * (p2.pos.y - p1.pos.y))) / detD;
  coordX := p1.pos.x + p1.vel.x * t1;
  coordY := p1.pos.y + p1.vel.y * t1;
  Exit(True);
end;

var
  line: string;
  idx: Integer;
  f: Text;
  minVal, maxVal: Double;
  cX, cY, t1, t2: Double;
  count: Int64;
  nPoints, i, j: Integer;
  tmpPt: TPoint;

begin
  SetLength(Points, 0);
  AssignFile(f, 'input.txt');
  Reset(f);
  while not Eof(f) do
  begin
    ReadLn(f, line);
    if Trim(line) = '' then Continue;
    idx := 1;
    tmpPt.pos.x := ReadIntFromLine(line, idx);
    tmpPt.pos.y := ReadIntFromLine(line, idx);
    tmpPt.pos.z := ReadIntFromLine(line, idx);
    tmpPt.vel.x := ReadIntFromLine(line, idx);
    tmpPt.vel.y := ReadIntFromLine(line, idx);
    tmpPt.vel.z := ReadIntFromLine(line, idx);

    SetLength(Points, Length(Points) + 1);
    Points[High(Points)] := tmpPt;
  end;
  CloseFile(f);

  minVal := 200000000000000.0;
  maxVal := 400000000000000.0;

  count := 0;
  nPoints := Length(Points);
  for i := 0 to nPoints - 1 do
  begin
    for j := 0 to i - 1 do
    begin
      if IsIntersect(Points[i], Points[j], cX, cY, t1, t2) then
      begin
        if (cX >= minVal) and (cX <= maxVal) and (cY >= minVal) and (cY <= maxVal) and (t1 >= 0) and (t2 >= 0) then
          Inc(count);
      end;
    end;
  end;

  WriteLn(count);
end.
