program TaxicabDistancePart2;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

var
  inputFile: TextFile;
  instructions, instrPart, currentPosition: string;
  x, y, direction, blocksAway, i, j: Integer;
  parts: TStringArray;
  visited: TStringList;
  found: Boolean;

function PositionVisited(const pos: string; list: TStringList): Boolean;
begin
  Result := list.IndexOf(pos) <> -1;
end;

begin
  x := 0; // Horizontal position
  y := 0; // Vertical position
  direction := 0; // 0: North, 1: East, 2: South, 3: West
  found := False;

  visited := TStringList.Create;
  visited.Add('0,0'); // Add starting position

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
    // Determine turn direction
    if instrPart[1] = 'L' then
      direction := (direction + 3) mod 4 // Turning left
    else
      direction := (direction + 1) mod 4; // Turning right

    // Calculate steps to move
    Delete(instrPart, 1, 1); // Remove the direction part
    if not TryStrToInt(instrPart, blocksAway) then
      Continue; // Skip if conversion fails

    // Move the specified blocks in the current direction, checking each position
    for j := 1 to blocksAway do
    begin
      case direction of
        0: Inc(y); // North
        1: Inc(x); // East
        2: Dec(y); // South
        3: Dec(x); // West
      end;
      currentPosition := IntToStr(x) + ',' + IntToStr(y);
      if PositionVisited(currentPosition, visited) then
      begin
        found := True;
        Break;
      end
      else
        visited.Add(currentPosition);
    end;
    if found then Break;
  end;

  visited.Free;

  if found then
    WriteLn(Abs(x) + Abs(y))
  else
    WriteLn('No location was visited twice.');
end.
