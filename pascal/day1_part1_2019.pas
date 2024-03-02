program RocketEquation;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

var
  inputFile: TextFile;
  line: string;
  mass, totalFuel, fuelRequired: Integer;

begin
  totalFuel := 0;

  // Load the module masses from the input file
  AssignFile(inputFile, 'input.txt');
  Reset(inputFile);
  while not EOF(inputFile) do
  begin
    ReadLn(inputFile, line);
    mass := StrToInt(line);
    // Calculate fuel for the current module
    fuelRequired := (mass div 3) - 2;
    totalFuel := totalFuel + fuelRequired;
  end;
  CloseFile(inputFile);

  // Output the total fuel requirement
  WriteLn(totalFuel);
end.

