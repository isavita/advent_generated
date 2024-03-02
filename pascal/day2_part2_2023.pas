program CubeConundrumPart2;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

var
  inputFile: TextFile;
  line, part: string;
  gamePowerSum, maxRed, maxGreen, maxBlue, tempRed, tempGreen, tempBlue: Integer;
  parts: TStringArray;
  i: Integer;

function ExtractCubes(combo: string; color: string): Integer;
var
  posColor: Integer;
  numStr: string;
  j: Integer;
begin
  Result := 0;
  posColor := Pos(color, combo);
  if posColor > 0 then
  begin
    numStr := '';
    for j := posColor - 2 downto 1 do
    begin
      if not (combo[j] in ['0'..'9']) then Break;
      numStr := combo[j] + numStr;
    end;
    if numStr <> '' then
      Result := StrToInt(numStr);
  end;
end;

begin
  gamePowerSum := 0;

  AssignFile(inputFile, 'input.txt');
  Reset(inputFile);
  while not EOF(inputFile) do
  begin
    ReadLn(inputFile, line);
    Delete(line, 1, Pos(':', line) + 1);
    maxRed := 0;
    maxGreen := 0;
    maxBlue := 0;

    parts := line.Split([';']);
    for part in parts do
    begin
      tempRed := ExtractCubes(part, 'red');
      tempGreen := ExtractCubes(part, 'green');
      tempBlue := ExtractCubes(part, 'blue');
      
      if tempRed > maxRed then maxRed := tempRed;
      if tempGreen > maxGreen then maxGreen := tempGreen;
      if tempBlue > maxBlue then maxBlue := tempBlue;
    end;

    gamePowerSum := gamePowerSum + (maxRed * maxGreen * maxBlue);
  end;
  CloseFile(inputFile);

  WriteLn('Sum of the power of the minimum sets: ', gamePowerSum);
end.
