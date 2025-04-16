
program SmokeBasin;

{$MODE OBJFPC}{$H+}

uses
  SysUtils;

type
  THeightMap = array of array of Integer;

function ReadHeightMap(const FileName: string): THeightMap;
var
  F: TextFile;
  Line: string;
  Row: array of Integer;
  i, j: Integer;
begin
  AssignFile(F, FileName);
  try
    Reset(F);
    SetLength(Result, 0);
    i := 0;
    while not Eof(F) do
    begin
      ReadLn(F, Line);
      SetLength(Result, i + 1);
      SetLength(Result[i], Length(Line));
      for j := 0 to Length(Line) - 1 do
      begin
        Result[i][j] := StrToInt(Line[j + 1]);
      end;
      Inc(i);
    end;
  finally
    CloseFile(F);
  end;
end;

function IsLowPoint(const HeightMap: THeightMap; Row, Col: Integer): Boolean;
var
  Height: Integer;
  Rows, Cols: Integer;
begin
  Height := HeightMap[Row][Col];
  Rows := Length(HeightMap);
  Cols := Length(HeightMap[0]);

  // Check up
  if (Row > 0) and (Height >= HeightMap[Row - 1][Col]) then
    Exit(False);

  // Check down
  if (Row < Rows - 1) and (Height >= HeightMap[Row + 1][Col]) then
    Exit(False);

  // Check left
  if (Col > 0) and (Height >= HeightMap[Row][Col - 1]) then
    Exit(False);

  // Check right
  if (Col < Cols - 1) and (Height >= HeightMap[Row][Col + 1]) then
    Exit(False);

  Result := True;
end;

function CalculateRiskLevelSum(const HeightMap: THeightMap): Integer;
var
  Rows, Cols: Integer;
  Row, Col: Integer;
  Sum: Integer;
begin
  Rows := Length(HeightMap);
  if Rows = 0 then
    Exit(0);
  Cols := Length(HeightMap[0]);
  Sum := 0;

  for Row := 0 to Rows - 1 do
  begin
    for Col := 0 to Cols - 1 do
    begin
      if IsLowPoint(HeightMap, Row, Col) then
      begin
        Sum := Sum + HeightMap[Row][Col] + 1;
      end;
    end;
  end;
  Result := Sum;
end;

var
  HeightMap: THeightMap;
  RiskLevelSum: Integer;

begin
  HeightMap := ReadHeightMap('input.txt');
  RiskLevelSum := CalculateRiskLevelSum(HeightMap);
  WriteLn(RiskLevelSum);
end.
