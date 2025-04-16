
program HillClimbing;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  THeightMap = array of array of Integer;
  TPoint = record
    X, Y: Integer;
  end;
  TQueueItem = record
    Point: TPoint;
    Distance: Integer;
  end;

var
  HeightMap: THeightMap;
  Width, Height: Integer;
  StartPoint, EndPoint: TPoint;

function CharToElevation(c: Char): Integer;
begin
  case c of
    'S': Result := Ord('a') - Ord('a');
    'E': Result := Ord('z') - Ord('a');
    else Result := Ord(c) - Ord('a');
  end;
end;

function IsValid(x, y: Integer): Boolean;
begin
  Result := (x >= 0) and (x < Width) and (y >= 0) and (y < Height);
end;

function Solve(): Integer;
var
  Visited: array of array of Boolean;
  Queue: array of TQueueItem;
  QueueStart, QueueEnd: Integer;
  Current: TQueueItem;
  NewX, NewY: Integer;
  dx: array[0..3] of Integer = (0, 0, 1, -1);
  dy: array[0..3] of Integer = (1, -1, 0, 0);
  i,j: Integer;
begin
  SetLength(Visited, Height);
  for i := 0 to Height - 1 do
  begin
      SetLength(Visited[i], Width);
      for j := 0 to Width -1 do
          Visited[i,j] := false;
  end;
  
  SetLength(Queue, Width * Height + 1); 

  QueueStart := 0;
  QueueEnd := 0;

  Queue[QueueEnd].Point := StartPoint;
  Queue[QueueEnd].Distance := 0;
  Inc(QueueEnd);

  Visited[StartPoint.Y, StartPoint.X] := True;

  while QueueStart < QueueEnd do
  begin
    Current := Queue[QueueStart];
    Inc(QueueStart);

    if (Current.Point.X = EndPoint.X) and (Current.Point.Y = EndPoint.Y) then
    begin
      Result := Current.Distance;
      Exit;
    end;

    for i := 0 to 3 do
    begin
      NewX := Current.Point.X + dx[i];
      NewY := Current.Point.Y + dy[i];

      if IsValid(NewX, NewY) and not Visited[NewY, NewX] and
         (HeightMap[NewY, NewX] <= HeightMap[Current.Point.Y, Current.Point.X] + 1) then
      begin
        Queue[QueueEnd].Point.X := NewX;
        Queue[QueueEnd].Point.Y := NewY;
        Queue[QueueEnd].Distance := Current.Distance + 1;
        Inc(QueueEnd);
        Visited[NewY, NewX] := True;
      end;
    end;
  end;

  Result := -1; // No path found
end;

var
  InputFile: TextFile;
  Line: String;
  Row: Integer;
  Col: Integer;

begin
  Assign(InputFile, 'input.txt');
  Reset(InputFile);

  Row := 0;
  Width := -1;
  while not Eof(InputFile) do
  begin
    ReadLn(InputFile, Line);
    if Width = -1 then
      Width := Length(Line);
    Inc(Row);
  end;
  Height := Row;
  CloseFile(InputFile);

  SetLength(HeightMap, Height);
  for Row := 0 to Height - 1 do
    SetLength(HeightMap[Row], Width);
    
  Assign(InputFile, 'input.txt');
  Reset(InputFile);

  Row := 0;
  while not Eof(InputFile) do
  begin
    ReadLn(InputFile, Line);
    for Col := 0 to Width - 1 do
    begin
      HeightMap[Row, Col] := CharToElevation(Line[Col + 1]);
      if Line[Col + 1] = 'S' then
      begin
        StartPoint.X := Col;
        StartPoint.Y := Row;
      end;
      if Line[Col + 1] = 'E' then
      begin
        EndPoint.X := Col;
        EndPoint.Y := Row;
      end;
    end;
    Inc(Row);
  end;

  CloseFile(InputFile);

  WriteLn(Solve());
end.
