program CubeConundrum;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

var
  inputFile: TextFile;
  line, part: string;
  sumIDs, gameID: Integer;
  redCubes, greenCubes, blueCubes, tempRed, tempGreen, tempBlue: Integer;
  parts: TStringArray;
  i: Integer;
  possible: Boolean;

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
  redCubes := 12;
  greenCubes := 13;
  blueCubes := 14;
  sumIDs := 0;

  AssignFile(inputFile, 'input.txt');
  Reset(inputFile);
  while not EOF(inputFile) do
  begin
    ReadLn(inputFile, line);
    gameID := StrToInt(Copy(line, 6, Pos(':', line) - 6));
    Delete(line, 1, Pos(':', line) + 1);

    possible := True;
    parts := line.Split([';']);
    for part in parts do
    begin
      tempRed := ExtractCubes(part, 'red');
      tempGreen := ExtractCubes(part, 'green');
      tempBlue := ExtractCubes(part, 'blue');
      if (tempRed > redCubes) or (tempGreen > greenCubes) or (tempBlue > blueCubes) then
      begin
        possible := False;
        Break;
      end;
    end;
    if possible then
      sumIDs := sumIDs + gameID;
  end;
  CloseFile(inputFile);

  WriteLn('Sum of IDs of possible games: ', sumIDs);
end.
