
program Solve;

{$MODE Delphi} // Use Delphi compatible mode for AnsiString and Int64
{$H+}         // Enable long strings (AnsiString)

uses
  SysUtils;   // Required for Int64, File I/O helpers if needed, though basic I/O is standard

const
  TotalRows = 400000;

// Function to determine if a tile at the next row's position 'centerIdx' (1-based)
// will be a trap, based on the 'currentRow'.
// Indices outside the row bounds are treated as safe ('.').
function IsTrap(leftIdx, centerIdx, rightIdx: Integer; const currentRow: AnsiString): Boolean;
var
  l, c, r: Char;
  rowLen: Integer;
begin
  rowLen := Length(currentRow);

  // Get left character, default to '.' if out of bounds
  if (leftIdx >= 1) and (leftIdx <= rowLen) then
    l := currentRow[leftIdx]
  else
    l := '.';

  // Get center character (assumed valid by caller loop)
  c := currentRow[centerIdx];

  // Get right character, default to '.' if out of bounds
  if (rightIdx >= 1) and (rightIdx <= rowLen) then
    r := currentRow[rightIdx]
  else
    r := '.';

  // Trap conditions
  Result := ((l = '^') and (c = '^') and (r = '.')) or
            ((c = '^') and (r = '^') and (l = '.')) or
            ((l = '^') and (c = '.') and (r = '.')) or
            ((r = '^') and (c = '.') and (l = '.'));
end;

// Function to calculate the total number of safe tiles over 'numRows'.
function CountSafeTiles(const firstRow: AnsiString; numRows: Integer): Int64;
var
  currentRow, nextRow: AnsiString;
  safeCount: Int64;
  rowLen: Integer;
  i, j: Integer;
begin
  currentRow := firstRow;
  rowLen := Length(currentRow);
  safeCount := 0;

  // Count safe tiles in the first row
  for j := 1 to rowLen do
  begin
    if currentRow[j] = '.' then
      Inc(safeCount);
  end;

  // Pre-allocate space for the next row for efficiency
  SetLength(nextRow, rowLen);

  // Generate subsequent rows
  for i := 1 to numRows - 1 do // Iterate numRows - 1 times
  begin
    for j := 1 to rowLen do
    begin
      if IsTrap(j - 1, j, j + 1, currentRow) then
      begin
        nextRow[j] := '^';
      end
      else
      begin
        nextRow[j] := '.';
        Inc(safeCount); // Increment count for safe tiles in the new row
      end;
    end;
    currentRow := nextRow; // Update current row for the next iteration
  end;

  Result := safeCount;
end;

// Main program entry point
var
  inputFile: TextFile;
  firstRowStr: AnsiString;
  finalSafeCount: Int64;
begin
  AssignFile(inputFile, 'input.txt');
  try
    Reset(inputFile);
    ReadLn(inputFile, firstRowStr);
  finally
    CloseFile(inputFile);
  end;

  finalSafeCount := CountSafeTiles(firstRowStr, TotalRows);
  WriteLn(finalSafeCount);
end.
