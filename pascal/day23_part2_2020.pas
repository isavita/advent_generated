
program cup_game;

{$mode objfpc}{$H+}

uses
  SysUtils;

const
  totalCups = 1000000;
  totalMoves = 10000000;

var
  cups: array[1..totalCups] of integer;
  currentCup, lastCup, pickup1, pickup2, pickup3, destinationCup, i, cup: integer;
  inputStr: string;
  inputFile: TextFile;
  j: integer;

begin
  AssignFile(inputFile, 'input.txt');
  Reset(inputFile);
  ReadLn(inputFile, inputStr);
  CloseFile(inputFile);

  lastCup := 0;
  for i := 1 to Length(inputStr) do
  begin
    cup := StrToInt(inputStr[i]);
    if i > 1 then
      cups[lastCup] := cup;
    lastCup := cup;
  end;

  for i := Length(inputStr) + 1 to totalCups do
  begin
    cups[lastCup] := i;
    lastCup := i;
  end;
  cups[lastCup] := StrToInt(inputStr[1]);

  currentCup := StrToInt(inputStr[1]);
  for i := 1 to totalMoves do
  begin
    pickup1 := cups[currentCup];
    pickup2 := cups[pickup1];
    pickup3 := cups[pickup2];

    cups[currentCup] := cups[pickup3];

    destinationCup := currentCup - 1;
    if destinationCup < 1 then
      destinationCup := totalCups;
    while (destinationCup = pickup1) or (destinationCup = pickup2) or (destinationCup = pickup3) do
    begin
      destinationCup := destinationCup - 1;
      if destinationCup < 1 then
        destinationCup := totalCups;
    end;

    cups[pickup3] := cups[destinationCup];
    cups[destinationCup] := pickup1;

    currentCup := cups[currentCup];
  end;

  cup := cups[1];
  WriteLn(Int64(cups[1]) * Int64(cups[cup]));
end.
