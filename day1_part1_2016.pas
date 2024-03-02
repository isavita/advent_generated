program TaxicabDistance;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

var
  inputFile: TextFile;
  instructions, instrPart: string;
  x, y, direction, blocksAway, i: Integer;
  parts: TStringArray;

begin
  x := 0; // Horizontal position
  y := 0; // Vertical position
  direction := 0; // 0: North, 1: East, 2: South, 3: West

  AssignFile(inputFile, 'input.txt');
  Reset(inputFile);
  ReadLn(inputFile, instructions);
  CloseFile(inputFile);

  // Split instructions by comma and space
  instructions := StringReplace(instructions, ', ', ',', [rfReplaceAll]);
  parts := instructions.Split(',');

  for i := 0 to High(parts) do
  begin
    instrPart := parts[i];
    // Determine turn direction and steps
    if instrPart[1] = 'L' then
      direction := (direction + 3) mod 4 // Turning left
    else
      direction := (direction + 1) mod 4; // Turning right

    // Calculate steps to move
    Delete(instrPart, 1, 1); // Remove the direction part
    if not TryStrToInt(instrPart, blocksAway) then
      Continue; // Skip if conversion fails

    // Move the specified blocks in the current direction
    case direction of
      0: Inc(y, blocksAway); // North
      1: Inc(x, blocksAway); // East
      2: Dec(y, blocksAway); // South
      3: Dec(x, blocksAway); // West
    end;
  end;

  // Calculate Manhattan distance from the starting point
  WriteLn(Abs(x) + Abs(y));
end.
