program CalorieCounting;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

var
  inputFile: TextFile;
  line: string;
  currentElfCalories, maxCalories: Integer;

begin
  maxCalories := 0;
  currentElfCalories := 0;

  // Load the Elves' Calorie counts from the input file
  AssignFile(inputFile, 'input.txt');
  Reset(inputFile);
  while not EOF(inputFile) do
  begin
    ReadLn(inputFile, line);
    // Check for blank lines indicating the start of a new Elf's inventory
    if line = '' then
    begin
      // Update max calories if the current Elf has more
      if currentElfCalories > maxCalories then
        maxCalories := currentElfCalories;
      // Reset current Elf's calories for the next Elf
      currentElfCalories := 0;
    end
    else
      // Add the line's calorie count to the current Elf's total
      Inc(currentElfCalories, StrToInt(line));
  end;

  // Check the last Elf's calories as there might not be a trailing blank line
  if currentElfCalories > maxCalories then
    maxCalories := currentElfCalories;

  CloseFile(inputFile);

  // Output the maximum calories carried by any single Elf
  WriteLn('The Elf carrying the most Calories is carrying a total of ', maxCalories, ' Calories.');
end.
