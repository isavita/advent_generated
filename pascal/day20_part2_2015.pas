
program Solve;
{$mode objfpc}{$H+} // Enable dynamic arrays and Int64 support

uses
  SysUtils;

var
  inputFile: TextFile;
  inputValue: Int64;
  targetValue: Int64;
  houses: array of Int64;
  elf, house, houseNumber, maxHouseVisit, housesLen: Integer; // Use Integer for loop counters/indices assuming target fits

begin
  Assign(inputFile, 'input.txt');
  try
    Reset(inputFile);
    ReadLn(inputFile, inputValue);
  finally
    Close(inputFile);
  end;

  targetValue := inputValue div 11;

  // Check if targetValue exceeds Integer range for array sizing/indexing
  // Adjust logic or use Int64 for loop vars if necessary for very large inputs.
  // Assuming targetValue fits within Integer limits for typical problem constraints.
  if targetValue > High(Integer) then
  begin
    WriteLn('Error: Target value too large for Integer indexing.');
    Halt(1);
  end;

  housesLen := Trunc(targetValue) + 1;
  SetLength(houses, housesLen); // Automatically initialized to 0 in objfpc mode

  for elf := 1 to housesLen - 1 do // elf up to targetValue
  begin
    // Calculate max house this elf visits, preventing overflow
    if elf > (High(Integer) div 50) then
       maxHouseVisit := housesLen - 1 // If elf*50 overflows, it's > targetValue
    else
       maxHouseVisit := elf * 50;

    if maxHouseVisit > housesLen - 1 then
        maxHouseVisit := housesLen - 1; // Clamp to actual targetValue (housesLen - 1)

    house := elf;
    while house <= maxHouseVisit do
    begin
        houses[house] := houses[house] + elf;
        house := house + elf;
    end;
  end;

  for houseNumber := 1 to housesLen - 1 do // Check houses starting from 1
  begin
    if houses[houseNumber] >= targetValue then
    begin
      WriteLn(houseNumber);
      Halt; // Exit program immediately after finding the first answer
    end;
  end;

end.
