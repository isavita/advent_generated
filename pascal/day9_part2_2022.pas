
program RopeBridge;

{$MODE DELPHI}

uses
  SysUtils, Classes;

type
  TPoint = record
    X: Integer;
    Y: Integer;
  end;

  TRope = array[0..9] of TPoint;

var
  Head: TPoint;
  Tail: TPoint;
  Visited: TStringList;

function AreAdjacent(P1, P2: TPoint): Boolean;
begin
  Result := (Abs(P1.X - P2.X) <= 1) and (Abs(P1.Y - P2.Y) <= 1);
end;

procedure MoveTail(var Head, Tail: TPoint);
begin
  if not AreAdjacent(Head, Tail) then
  begin
    if Head.X > Tail.X then
      Tail.X := Tail.X + 1
    else if Head.X < Tail.X then
      Tail.X := Tail.X - 1;

    if Head.Y > Tail.Y then
      Tail.Y := Tail.Y + 1
    else if Head.Y < Tail.Y then
      Tail.Y := Tail.Y - 1;
  end;
end;

procedure MoveRope(var Rope: TRope);
var
  i: Integer;
begin
  for i := 1 to 9 do
  begin
    MoveTail(Rope[i-1], Rope[i]);
  end;
end;

procedure AddVisited(P: TPoint);
var
  S: string;
begin
  S := IntToStr(P.X) + ',' + IntToStr(P.Y);
  if Visited.IndexOf(S) = -1 then
    Visited.Add(S);
end;

procedure SimulateMotions(const FileName: string; RopeLength: Integer);
var
  FS: TStringList;
  Line: string;
  Direction: Char;
  Distance: Integer;
  I: Integer;
  Rope: TRope;
begin
  FS := TStringList.Create;
  Visited := TStringList.Create;
  try
    FS.LoadFromFile(FileName);

    // Initialize the rope
    for I := 0 to RopeLength -1 do
        begin
            Rope[I].X := 0;
            Rope[I].Y := 0;
        end;
    AddVisited(Rope[RopeLength-1]);

    for Line in FS do
    begin
      Direction := Line[1];
      Distance := StrToInt(Copy(Line, 3, Length(Line)));

      for I := 1 to Distance do
      begin
        case Direction of
          'R': Rope[0].X := Rope[0].X + 1;
          'L': Rope[0].X := Rope[0].X - 1;
          'U': Rope[0].Y := Rope[0].Y + 1;
          'D': Rope[0].Y := Rope[0].Y - 1;
        end;

        MoveRope(Rope);
        AddVisited(Rope[RopeLength-1]);
      end;
    end;

    Writeln('Number of visited positions: ', Visited.Count);
  finally
    FS.Free;
    Visited.Free;
  end;
end;

begin
  SimulateMotions('input.txt', 2);
  SimulateMotions('input.txt', 10);
end.
