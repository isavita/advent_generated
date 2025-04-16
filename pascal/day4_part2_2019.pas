
program AdventOfCode;

{$mode objfpc}{$H+}

uses
  SysUtils;

function HasAdjacent(num: integer): boolean;
var
  numStr: string;
  i: integer;
begin
  numStr := IntToStr(num);
  for i := 1 to Length(numStr) - 1 do
  begin
    if numStr[i] = numStr[i + 1] then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function HasOnlyTwoAdjacent(num: integer): boolean;
var
  numStr: string;
  i: integer;
  count: integer;
begin
  numStr := IntToStr(num);
  count := 1;
  for i := 1 to Length(numStr) - 1 do
  begin
    if numStr[i] = numStr[i + 1] then
      count := count + 1
    else
    begin
      if count = 2 then
      begin
        Result := True;
        Exit;
      end;
      count := 1;
    end;
  end;
  Result := (count = 2);
end;

function NeverDecreases(num: integer): boolean;
var
  numStr: string;
  i: integer;
begin
  numStr := IntToStr(num);
  for i := 1 to Length(numStr) - 1 do
  begin
    if StrToInt(numStr[i]) > StrToInt(numStr[i + 1]) then
    begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

var
  start, endVal, i, countPart1, countPart2: integer;
  line: string;
  dataFile: TextFile;

begin
  AssignFile(dataFile, 'input.txt');
  Reset(dataFile);
  Readln(dataFile, line);
  CloseFile(dataFile);

  start := StrToInt(Copy(line, 1, Pos('-', line) - 1));
  endVal := StrToInt(Copy(line, Pos('-', line) + 1, Length(line)));

  countPart1 := 0;
  countPart2 := 0;

  for i := start to endVal do
  begin
    if HasAdjacent(i) and NeverDecreases(i) then
      Inc(countPart1);
    if HasOnlyTwoAdjacent(i) and NeverDecreases(i) then
      Inc(countPart2);
  end;

  writeln(countPart1);
  writeln(countPart2);
end.
