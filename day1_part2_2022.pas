program CalorieCountingTopThree;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

var
  inputFile: TextFile;
  line: string;
  caloriesPerElf: Integer;
  elfCalories: array of Integer;
  totalCalories, topCount: Integer;
  i, j, temp: Integer;

begin
  SetLength(elfCalories, 0);
  totalCalories := 0;

  AssignFile(inputFile, 'input.txt');
  Reset(inputFile);
  caloriesPerElf := 0;

  while not EOF(inputFile) do
  begin
    ReadLn(inputFile, line);
    if line = '' then
    begin
      if caloriesPerElf > 0 then
      begin
        SetLength(elfCalories, Length(elfCalories) + 1);
        elfCalories[High(elfCalories)] := caloriesPerElf;
        caloriesPerElf := 0;
      end;
      Continue;
    end;
    Inc(caloriesPerElf, StrToInt(line));
  end;

  // Include the last Elf's calories if not added already
  if caloriesPerElf > 0 then
  begin
    SetLength(elfCalories, Length(elfCalories) + 1);
    elfCalories[High(elfCalories)] := caloriesPerElf;
  end;
  CloseFile(inputFile);

  // Simple selection sort to find the top three values
  for i := 0 to High(elfCalories) - 1 do
  begin
    for j := i + 1 to High(elfCalories) do
    begin
      if elfCalories[i] < elfCalories[j] then
      begin
        temp := elfCalories[i];
        elfCalories[i] := elfCalories[j];
        elfCalories[j] := temp;
      end;
    end;
  end;

  // Determine how many of the top values to sum
  if Length(elfCalories) < 3 then
    topCount := Length(elfCalories)
  else
    topCount := 3;

  // Sum the calories of the top Elves up to topCount
  for i := 0 to topCount - 1 do
  begin
    totalCalories := totalCalories + elfCalories[i];
  end;

  WriteLn('Total Calories carried by the top three Elves: ', totalCalories);
end.
