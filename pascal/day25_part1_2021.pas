
program SeaCucumber;

{$MODE DELPHI}

uses
  SysUtils;

type
  TMap = array of array of Char;

function ReadInput(const FileName: string; var Map: TMap): Integer;
var
  F: TextFile;
  Line: string;
  Row, Col: Integer;
begin
  AssignFile(F, FileName);
  Reset(F);

  Row := 0;
  while not Eof(F) do
  begin
    ReadLn(F, Line);
    if Row = 0 then
    begin
      SetLength(Map, 1, Length(Line));
    end
    else
    begin
      SetLength(Map, Length(Map) + 1, Length(Line));
    end;

    for Col := 1 to Length(Line) do
    begin
      Map[Row, Col - 1] := Line[Col];
    end;

    Inc(Row);
  end;

  CloseFile(F);
  Result := Row;
end;

function Step(var Map: TMap): Boolean;
var
  Rows, Cols, Row, Col: Integer;
  Moved: Boolean;
  NewMap: TMap;
begin
  Rows := Length(Map);
  if Rows = 0 then
  begin
    Result := False;
    Exit;
  end;
  Cols := Length(Map[0]);

  SetLength(NewMap, Rows, Cols);
  for Row := 0 to Rows - 1 do
    for Col := 0 to Cols - 1 do
      NewMap[Row, Col] := Map[Row, Col];

  Moved := False;

  // East-facing
  for Row := 0 to Rows - 1 do
  begin
    for Col := 0 to Cols - 1 do
    begin
      if Map[Row, Col] = '>' then
      begin
        if Map[Row, (Col + 1) mod Cols] = '.' then
        begin
          NewMap[Row, (Col + 1) mod Cols] := '>';
          NewMap[Row, Col] := '.';
          Moved := True;
        end;
      end;
    end;
  end;

  for Row := 0 to Rows - 1 do
    for Col := 0 to Cols - 1 do
      Map[Row, Col] := NewMap[Row, Col];

  // South-facing
  for Row := 0 to Rows - 1 do
  begin
    for Col := 0 to Cols - 1 do
    begin
      if Map[Row, Col] = 'v' then
      begin
        if Map[(Row + 1) mod Rows, Col] = '.' then
        begin
          NewMap[(Row + 1) mod Rows, Col] := 'v';
          NewMap[Row, Col] := '.';
          Moved := True;
        end;
      end;
    end;
  end;

  for Row := 0 to Rows - 1 do
    for Col := 0 to Cols - 1 do
      Map[Row, Col] := NewMap[Row, Col];

  Result := Moved;
end;

var
  Map: TMap;
  Rows, Steps: Integer;
begin
  Rows := ReadInput('input.txt', Map);
  Steps := 0;

  while Step(Map) do
  begin
    Inc(Steps);
  end;

  WriteLn(Steps + 1);
end.
