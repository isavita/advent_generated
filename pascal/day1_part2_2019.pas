program RocketEquationPartTwo;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

var
  inputFile: TextFile;
  line: string;
  mass, totalFuel, moduleFuel, extraFuel: Integer;

function CalculateFuel(mass: Integer): Integer;
begin
  Result := (mass div 3) - 2;
  if Result < 0 then
    Result := 0;
end;

begin
  totalFuel := 0;

  // Load the module masses from the input file
  AssignFile(inputFile, 'input.txt');
  Reset(inputFile);
  while not EOF(inputFile) do
  begin
    ReadLn(inputFile, line);
    mass := StrToInt(line);
    
    // Reset moduleFuel for the current module
    moduleFuel := 0;
    
    // Initial fuel calculation for the module's mass
    extraFuel := CalculateFuel(mass);
    
    // Keep calculating fuel for the added fuel, until no more is required
    while extraFuel > 0 do
    begin
      moduleFuel := moduleFuel + extraFuel;
      extraFuel := CalculateFuel(extraFuel);
    end;
    
    // Add the total fuel required for this module to the overall total
    totalFuel := totalFuel + moduleFuel;
  end;
  CloseFile(inputFile);

  // Output the total fuel requirement including fuel for the fuel
  WriteLn(totalFuel);
end.
