
program SolveRegions;

{$MODE OBJFPC}{$H+} // Use Free Pascal mode with AnsiStrings

uses
  SysUtils; // For file operations and potentially StrToIntDef if needed

const
  MaxRows = 1000; // Adjust if needed, maximum expected rows
  MaxCols = 1000; // Adjust if needed, maximum expected columns

type
  TPoint = record
    r, c: Integer;
  end;
  TGrid = array[1..MaxRows] of AnsiString;
  TVisited = array[1..MaxRows, 1..MaxCols] of Boolean;
  TQueue = array[1..MaxRows * MaxCols] of TPoint; // Max possible queue size

var
  grid: TGrid;
  visited: TVisited;
  rows, cols: Integer;
  queue: TQueue;
  qHead, qTail: Integer;
  dr: array[0..3] of Integer = (0, 0, 1, -1);
  dc: array[0..3] of Integer = (1, -1, 0, 0);

// Procedure to perform BFS and calculate area and perimeter
procedure CalculateRegion(startR, startC: Integer; var area, perimeter: Int64);
var
  currentChar: Char;
  curr: TPoint;
  nr, nc, i: Integer;
begin
  // Initialize for this region calculation
  area := 0;
  perimeter := 0;
  qHead := 1;
  qTail := 1; // Points to the next available slot

  currentChar := grid[startR][startC]; // Pascal strings are 1-indexed

  // Add starting point to queue and mark as visited
  queue[qTail].r := startR;
  queue[qTail].c := startC;
  Inc(qTail);
  visited[startR, startC] := True;

  // Breadth-First Search
  while qHead < qTail do
  begin
    // Dequeue
    curr := queue[qHead];
    Inc(qHead);

    Inc(area);

    // Check 4 neighbors
    for i := 0 to 3 do
    begin
      nr := curr.r + dr[i];
      nc := curr.c + dc[i];

      // Check if neighbor is within grid bounds
      if (nr >= 1) and (nr <= rows) and (nc >= 1) and (nc <= cols) then
      begin
        // Neighbor is inside the grid
        if grid[nr][nc] <> currentChar then
        begin
          // Different character - part of the perimeter
          Inc(perimeter);
        end
        else // Same character
        begin
          // If not visited, mark and enqueue
          if not visited[nr, nc] then
          begin
            visited[nr, nc] := True;
            queue[qTail].r := nr;
            queue[qTail].c := nc;
            Inc(qTail);
          end;
        end;
      end
      else
      begin
        // Neighbor is outside the grid - part of the perimeter
        Inc(perimeter);
      end;
    end;
  end;
end;

// Main program logic
var
  inputFile: TextFile;
  r, c: Integer;
  lineStr: AnsiString;
  totalPrice: Int64;
  currentArea, currentPerimeter: Int64;

begin
  AssignFile(inputFile, 'input.txt');
  try
    Reset(inputFile);

    // Read grid from file
    rows := 0;
    while not Eof(inputFile) and (rows < MaxRows) do
    begin
      ReadLn(inputFile, lineStr);
      // Remove trailing CR/LF if present (defensive)
      lineStr := TrimRight(lineStr);
      if Length(lineStr) = 0 then continue; // Skip empty lines

      Inc(rows);
      grid[rows] := lineStr;

      // Determine column count from the first line
      if rows = 1 then
      begin
         cols := Length(lineStr);
         if cols > MaxCols then cols := MaxCols; // Truncate if too wide
      end;
      // Ensure consistent width, truncate if necessary
      if Length(grid[rows]) > cols then
         SetLength(grid[rows], cols);

    end;
  finally
    CloseFile(inputFile);
  end;

  // Check if grid was actually read
  if rows = 0 then
  begin
    WriteLn(0); // No grid, result is 0
    Halt; // Exit program
  end;


  // Initialize visited array (optional, default is false, but good practice)
  // FillChar(visited, SizeOf(visited), False); // Faster way if needed

   for r := 1 to rows do
     for c := 1 to cols do
       visited[r, c] := False;


  totalPrice := 0;

  // Iterate through grid to find regions
  for r := 1 to rows do
  begin
    for c := 1 to cols do
    begin
      // If cell not visited, start a new region calculation
      if not visited[r, c] then
      begin
        CalculateRegion(r, c, currentArea, currentPerimeter);
        totalPrice := totalPrice + (currentArea * currentPerimeter);
      end;
    end;
  end;

  // Print the final result
  WriteLn(totalPrice);

end.
