
program SolveAntennas;

{$MODE OBJFPC}{$H+} // Use Object Pascal mode for features like dynamic arrays

uses
  SysUtils, Classes, Math; // Math for Max/Min if needed, Classes potentially for lists (not used here)

const
  InputFileName = 'input.txt';

type
  TPoint = record
    Y, X: Integer;
  end;

  // Record to store antenna info including its character type
  TAntennaPoint = record
    CharType: Char;
    Y, X: Integer;
  end;

  // Dynamic array types
  TAntennaArray = array of TAntennaPoint;
  T2DBooleanArray = array of array of Boolean;

// --- Sorting Procedure (Simple Bubble Sort for demonstration) ---
// In a real scenario, use a more efficient sort like QuickSort.
procedure SwapPoints(var A, B: TAntennaPoint);
var Temp: TAntennaPoint;
begin
  Temp := A;
  A := B;
  B := Temp;
end;

procedure SortAntennaPoints(var Points: TAntennaArray);
var
  i, j: Integer;
  N: Integer;
  Swapped: Boolean;
begin
  N := Length(Points);
  if N < 2 then Exit;
  repeat
    Swapped := False;
    for i := 0 to N - 2 do
    begin
      // Sort primarily by character type
      if Points[i].CharType > Points[i+1].CharType then
      begin
         SwapPoints(Points[i], Points[i+1]);
         Swapped := True;
      end
      // Optional secondary sort by coordinates (not strictly needed for grouping)
      // else if (Points[i].CharType = Points[i+1].CharType) and
      //         ((Points[i].Y > Points[i+1].Y) or
      //          ((Points[i].Y = Points[i+1].Y) and (Points[i].X > Points[i+1].X))) then
      // begin
      //    SwapPoints(Points[i], Points[i+1]);
      //    Swapped := True;
      // end;
    end;
    N := N - 1;
  until not Swapped;
end;
// --- End Sorting Procedure ---

var
  InputFile: TextFile;
  line: string;
  AllAntennas: TAntennaArray;
  IsAntinode: T2DBooleanArray;
  h, w: Integer;
  y, x, i, j, k, l: Integer;
  P1, P2: TPoint;
  AntinodeCount: Integer;
  CurrentChar: Char;
  StartIndex, EndIndex: Integer;
  NewPoint: TAntennaPoint;

begin
  Assign(InputFile, InputFileName);
  try
    Reset(InputFile);

    h := 0;
    w := 0;
    SetLength(AllAntennas, 0); // Initialize dynamic array

    // Read grid and store antenna locations
    while not Eof(InputFile) do
    begin
      ReadLn(InputFile, line);
      if Length(line) = 0 then continue; // Skip empty lines if any

      if h = 0 then w := Length(line); // Determine width from the first non-empty line

      for x := 1 to Length(line) do // Pascal strings are 1-based
      begin
        if line[x] <> '.' then
        begin
          SetLength(AllAntennas, Length(AllAntennas) + 1);
          NewPoint.CharType := line[x];
          NewPoint.Y := h;       // Use 0-based Y coordinate internally
          NewPoint.X := x - 1;   // Use 0-based X coordinate internally
          AllAntennas[High(AllAntennas)] := NewPoint;
        end;
      end;
      Inc(h); // Increment height
    end;
  finally
    Close(InputFile);
  end;

  if (h = 0) or (w = 0) then
  begin
      WriteLn(0);
      Halt;
  end;

  // Sort antennas by character type to process groups easily
  SortAntennaPoints(AllAntennas);

  // Initialize the boolean grid to track unique antinodes
  SetLength(IsAntinode, h, w);
  for y := 0 to h - 1 do
    for x := 0 to w - 1 do
      IsAntinode[y, x] := False;

  // Process antennas group by group
  i := 0;
  while i <= High(AllAntennas) do
  begin
    CurrentChar := AllAntennas[i].CharType;
    StartIndex := i;

    // Find the end of the current group of antennas with the same character
    j := i;
    while (j <= High(AllAntennas)) and (AllAntennas[j].CharType = CurrentChar) do
      Inc(j);
    EndIndex := j - 1;

    // Iterate through all pairs within this group
    for k := StartIndex to EndIndex do
    begin
      for l := k + 1 to EndIndex do
      begin
        // Calculate potential antinode positions
        P1.Y := 2 * AllAntennas[k].Y - AllAntennas[l].Y;
        P1.X := 2 * AllAntennas[k].X - AllAntennas[l].X;
        P2.Y := 2 * AllAntennas[l].Y - AllAntennas[k].Y;
        P2.X := 2 * AllAntennas[l].X - AllAntennas[k].X;

        // Check bounds and mark if it's a valid antinode
        if (P1.Y >= 0) and (P1.Y < h) and (P1.X >= 0) and (P1.X < w) then
          IsAntinode[P1.Y, P1.X] := True;

        if (P2.Y >= 0) and (P2.Y < h) and (P2.X >= 0) and (P2.X < w) then
          IsAntinode[P2.Y, P2.X] := True;
      end;
    end;

    // Move to the start of the next group
    i := EndIndex + 1;
  end;

  // Count the number of unique antinodes found
  AntinodeCount := 0;
  for y := 0 to h - 1 do
    for x := 0 to w - 1 do
      if IsAntinode[y, x] then
        Inc(AntinodeCount);

  WriteLn(AntinodeCount);

end.
