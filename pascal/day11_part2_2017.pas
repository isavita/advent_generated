
program HexGridDistance;

uses SysUtils; // Not strictly necessary for basic I/O and math but good practice

var
  inputFile: Text;
  ch: Char;
  currentDirection: string;
  dx, dy, maxDist, currentDist, finalDist: LongInt; // Use LongInt for potentially large distances

// Helper function to calculate distance based on the Python logic
function CalculateDistance(x, y: LongInt): LongInt;
var
  ax, ay: LongInt;
begin
  ax := Abs(x);
  ay := Abs(y);
  if ax > ay then
    CalculateDistance := ax
  else
    CalculateDistance := ay;
end;

// Procedure to update coordinates based on direction
procedure ProcessDirection(const dir: string; var x, y: LongInt);
begin
  if dir = 'n' then
    Inc(y)
  else if dir = 's' then
    Dec(y)
  else if dir = 'ne' then
    Inc(x)
  else if dir = 'sw' then
    Dec(x)
  else if dir = 'nw' then
  begin
    Dec(x);
    Inc(y);
  end
  else if dir = 'se' then
  begin
    Inc(x);
    Dec(y);
  end;
end;

begin
  Assign(inputFile, 'input.txt');
  Reset(inputFile);

  dx := 0;
  dy := 0;
  maxDist := 0;
  currentDirection := '';

  while not Eof(inputFile) do
  begin
    Read(inputFile, ch);
    if ch = ',' then
    begin
      if currentDirection <> '' then
      begin
        ProcessDirection(currentDirection, dx, dy);
        currentDist := CalculateDistance(dx, dy);
        if currentDist > maxDist then
          maxDist := currentDist;
        currentDirection := ''; // Reset for next direction
      end;
    end
    else if ch in ['a'..'z'] then // Build the direction string
    begin
      currentDirection := currentDirection + ch;
    end;
    // Ignore other characters like spaces, newlines, etc.
  end;

  // Process the last direction if the file didn't end with a comma
  if currentDirection <> '' then
  begin
    ProcessDirection(currentDirection, dx, dy);
    currentDist := CalculateDistance(dx, dy);
    if currentDist > maxDist then
      maxDist := currentDist;
  end;

  Close(inputFile);

  // Calculate final distance
  finalDist := CalculateDistance(dx, dy);

  // Print results
  WriteLn(finalDist);
  WriteLn(maxDist);

end.
