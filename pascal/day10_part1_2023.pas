
program PipeMaze;

{$MODE DELPHI} // or {$MODE OBJFPC} if using FPC

uses
  SysUtils;

type
  TPipe = record
    X, Y: Integer;
    Char: Char;
  end;

  TGrid = array of array of Char;

  TDirection = (North, South, East, West);

function ReadInput(const FileName: string; out Grid: TGrid; out StartX, StartY: Integer): Boolean;
var
  F: TextFile;
  Line: string;
  Row, Col: Integer;
begin
  AssignFile(F, FileName);
  try
    Reset(F);
    try
      Row := 0;
      while not Eof(F) do
      begin
        ReadLn(F, Line);
        if Length(Line) > 0 then
        begin
          SetLength(Grid, Row + 1, Length(Line));
          for Col := 0 to Length(Line) - 1 do
          begin
            Grid[Row, Col] := Line[Col + 1];
            if Grid[Row, Col] = 'S' then
            begin
              StartX := Col;
              StartY := Row;
            end;
          end;
          Inc(Row);
        end;
      end;
      ReadInput := True;
    finally
      CloseFile(F);
    end;
  except
    on EInOutError do
    begin
      WriteLn('Error opening or reading file: ', FileName);
      ReadInput := False;
    end;
  end;
end;

function GetNextPipe(const Grid: TGrid; X, Y: Integer; PreviousDirection: TDirection; out NextX, NextY: Integer; out NextDirection: TDirection): Boolean;
var
  PipeChar: Char;
begin
  NextX := -1;
  NextY := -1;
  NextDirection := North; // Dummy value
  Result := False;

  if (X < 0) or (Y < 0) or (Y >= Length(Grid)) or (X >= Length(Grid[0])) then
    Exit;

  PipeChar := Grid[Y, X];

  case PipeChar of
    '|':
      begin
        if PreviousDirection = North then
        begin
          NextX := X;
          NextY := Y + 1;
          NextDirection := North;
        end
        else if PreviousDirection = South then
        begin
          NextX := X;
          NextY := Y - 1;
          NextDirection := South;
        end;
      end;
    '-':
      begin
        if PreviousDirection = East then
        begin
          NextX := X - 1;
          NextY := Y;
          NextDirection := East;
        end
        else if PreviousDirection = West then
        begin
          NextX := X + 1;
          NextY := Y;
          NextDirection := West;
        end;
      end;
    'L':
      begin
        if PreviousDirection = North then
        begin
          NextX := X + 1;
          NextY := Y;
          NextDirection := West;
        end
        else if PreviousDirection = East then
        begin
          NextX := X;
          NextY := Y - 1;
          NextDirection := South;
        end;
      end;
    'J':
      begin
        if PreviousDirection = North then
        begin
          NextX := X - 1;
          NextY := Y;
          NextDirection := East;
        end
        else if PreviousDirection = West then
        begin
          NextX := X;
          NextY := Y - 1;
          NextDirection := South;
        end;
      end;
    '7':
      begin
        if PreviousDirection = South then
        begin
          NextX := X - 1;
          NextY := Y;
          NextDirection := East;
        end
        else if PreviousDirection = West then
        begin
          NextX := X;
          NextY := Y + 1;
          NextDirection := North;
        end;
      end;
    'F':
      begin
        if PreviousDirection = South then
        begin
          NextX := X + 1;
          NextY := Y;
          NextDirection := West;
        end
        else if PreviousDirection = East then
        begin
          NextX := X;
          NextY := Y + 1;
          NextDirection := North;
        end;
      end;
  end;

  if (NextX >= 0) and (NextY >= 0) and (NextY < Length(Grid)) and (NextX < Length(Grid[0])) then
    Result := True;
end;

function SolveMaze(const Grid: TGrid; StartX, StartY: Integer): Integer;
var
  X, Y, NextX, NextY, StartDirection: Integer;
  Distance, MaxDistance: Integer;
  Direction, NextDirection, InitialDirection : TDirection;
  FoundStart: Boolean;
begin
  MaxDistance := 0;
  FoundStart := false;
  //Try each direction to get the initial direction
  for StartDirection := 0 to 3 do
  begin
    X := StartX;
    Y := StartY;
    Direction := TDirection(StartDirection);
    case Direction of
      North:
        begin
          Dec(Y);
        end;
      South:
        begin
          Inc(Y);
        end;
      East:
        begin
          Inc(X);
        end;
      West:
        begin
          Dec(X);
        end;
    end;

    // Check if valid starting point
    if (X < 0) or (Y < 0) or (Y >= Length(Grid)) or (X >= Length(Grid[0])) then
      Continue;

    //Determine correct InitialDirection base on adjacent tiles of start position
    case Direction of
      North: InitialDirection := South;
      South: InitialDirection := North;
      East: InitialDirection := West;
      West: InitialDirection := East;
    end;

    //Initialization of variables and flag
    Distance := 1;
    NextDirection := InitialDirection;

    while true do
    begin
      if not GetNextPipe(Grid, X, Y, NextDirection, NextX, NextY, NextDirection) then
        Break;
      X := NextX;
      Y := NextY;
      Inc(Distance);
      if (X = StartX) and (Y = StartY) then
      begin
        FoundStart := true;
        break;
      end;

    end;

    if FoundStart then
      Break;

  end;
    //Return result
  SolveMaze := Distance div 2;
end;

var
  Grid: TGrid;
  StartX, StartY: Integer;
  MaxDistance: Integer;
begin
  if ReadInput('input.txt', Grid, StartX, StartY) then
  begin
    MaxDistance := SolveMaze(Grid, StartX, StartY);
    WriteLn(MaxDistance);
  end;
end.
