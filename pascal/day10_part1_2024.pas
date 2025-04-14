
program TrailSolver;

{$MODE Delphi} // Using Delphi mode for features like dynamic arrays
{$OPTIMIZATION ON} // Request optimizations

uses
  SysUtils; // For File I/O, StrToIntDef, FillChar etc.

const
  InputFileName = 'input.txt';

type
  TIntegerGrid = array of array of Integer;
  TCoord = record
    r, c: Integer;
  end;
  TState = record // Represents (coordinate, height)
    pos: TCoord;
    h: Integer;
  end;
  TCoordArray = array of TCoord;
  TStateArray = array of TState;

  // Using dynamic boolean arrays for visited states and reached 9s
  // Dimensions: [row, col, height]
  TVisitedArray = array of array of array of Boolean;
  // Dimensions: [row, col]
  TReachedArray = array of array of Boolean;

var
  Grid: TIntegerGrid;
  NR, NC: Integer;           // Number of rows and columns
  Trailheads: TCoordArray; // Stores coordinates of '0' cells
  SumScores: Int64;        // Use Int64 for potentially large sums
  Visited: TVisitedArray;  // Visited[r, c, h] = True if cell (r,c) reached with height h
  Reached: TReachedArray;  // Reached[r, c] = True if cell (r,c) is a '9' reached from current trailhead
  F: TextFile;
  line: string;
  r, c, i, dr_idx: Integer;
  th: TCoord;              // Current trailhead being processed
  Front: TStateArray;      // Stack for DFS
  CurrentState: TState;
  CurPos: TCoord;
  H, NR2, NC2, CurrentReachedCount: Integer;
  // Directions: Down, Up, Right, Left (matches Python's (1,0), (-1,0), (0,1), (0,-1))
  DR: array[0..3] of Integer = (1, -1, 0, 0);
  DC: array[0..3] of Integer = (0, 0, 1, -1);

// Helper procedure to quickly initialize a 3D boolean array
procedure FastInitializeBoolean3D(var arr: TVisitedArray; Value: Boolean);
var r, c: Integer; RowSize: NativeInt; FillValue: Byte;
begin
  if Value then FillValue := 1 else FillValue := 0;
  for r := 0 to High(arr) do
    for c := 0 to High(arr[r]) do
    begin
      RowSize := Length(arr[r, c]) * SizeOf(Boolean);
      if RowSize > 0 then
        FillChar(arr[r, c, 0], RowSize, FillValue);
    end;
end;

// Helper procedure to quickly initialize a 2D boolean array
procedure FastInitializeBoolean2D(var arr: TReachedArray; Value: Boolean);
var r: Integer; RowSize: NativeInt; FillValue: Byte;
begin
    if Value then FillValue := 1 else FillValue := 0;
    for r := 0 to High(arr) do
    begin
        RowSize := Length(arr[r]) * SizeOf(Boolean);
        if RowSize > 0 then
          FillChar(arr[r, 0], RowSize, FillValue);
    end;
end;

begin
  // --- Read Input Grid ---
  AssignFile(F, InputFileName);
  if not FileExists(InputFileName) then
  begin
    WriteLn('Error: input.txt not found.');
    Halt(1);
  end;

  try
    Reset(F);
    NR := 0;
    NC := 0;
    SetLength(Grid, 0); // Initialize dynamic array

    while not Eof(F) do
    begin
      ReadLn(F, line);
      if line = '' then continue; // Skip empty lines

      if NR = 0 then // First non-empty line determines NC
        NC := Length(line);

      // Basic validation for consistent grid width
      if Length(line) <> NC then
      begin
         WriteLn('Error: Inconsistent grid width.');
         CloseFile(F);
         Halt(1);
      end;

      SetLength(Grid, NR + 1);       // Add a row
      SetLength(Grid[NR], NC);       // Set columns for the new row

      for c := 0 to NC - 1 do
      begin
        // Ord(char) - Ord('0') converts digit character to integer
        Grid[NR, c] := Ord(line[c + 1]) - Ord('0'); // Pascal strings are 1-based
         // Basic validation for digit values
         if (Grid[NR, c] < 0) or (Grid[NR, c] > 9) then
         begin
            WriteLn('Error: Invalid character in grid.');
            CloseFile(F);
            Halt(1);
         end;
      end;
      Inc(NR);
    end;
  finally
    CloseFile(F);
  end;

  // Handle empty grid case
  if (NR = 0) or (NC = 0) then
  begin
    WriteLn(0);
    Halt;
  end;

  // --- Find Trailheads (cells with value 0) ---
  SetLength(Trailheads, 0);
  for r := 0 to NR - 1 do
    for c := 0 to NC - 1 do
      if Grid[r, c] = 0 then
      begin
        SetLength(Trailheads, Length(Trailheads) + 1);
        Trailheads[High(Trailheads)].r := r;
        Trailheads[High(Trailheads)].c := c;
      end;

  // --- Resize Visited and Reached arrays based on actual grid size ---
  SetLength(Visited, NR);
  SetLength(Reached, NR);
  for r := 0 to NR - 1 do
  begin
    SetLength(Visited[r], NC);
    SetLength(Reached[r], NC);
    for c := 0 to NC - 1 do
      SetLength(Visited[r, c], 10); // Heights 0 through 9
  end;

  // --- Process Each Trailhead using Depth First Search (DFS) ---
  SumScores := 0;
  SetLength(Front, 0); // Initialize DFS stack (dynamic array used as stack)

  for i := 0 to High(Trailheads) do
  begin
    th := Trailheads[i];

    // Reset Visited and Reached status for the new search from this trailhead
    FastInitializeBoolean3D(Visited, False);
    FastInitializeBoolean2D(Reached, False);

    // Initialize DFS stack with the starting state (trailhead, height 0)
    SetLength(Front, 1);
    Front[0].pos := th;
    Front[0].h := 0;
    // Note: We mark state (r2, c2, h+1) as visited *before* pushing, matching Python.

    while Length(Front) > 0 do
    begin
      // Pop the last element from the stack (DFS)
      CurrentState := Front[High(Front)];
      SetLength(Front, Length(Front) - 1);

      CurPos := CurrentState.pos;
      H := CurrentState.h;

      // If we reached height 9, mark it and stop exploring this path
      if H = 9 then
      begin
        Reached[CurPos.r, CurPos.c] := True;
        Continue; // Go to next iteration of the while loop
      end;

      // Explore neighbors
      for dr_idx := 0 to 3 do
      begin
        NR2 := CurPos.r + DR[dr_idx];
        NC2 := CurPos.c + DC[dr_idx];

        // Check if neighbor is within grid bounds
        if (NR2 >= 0) and (NR2 < NR) and (NC2 >= 0) and (NC2 < NC) then
        begin
          // Check if neighbor has the expected next height
          if Grid[NR2, NC2] = H + 1 then
          begin
            // Check if this state (neighbor coord, next height) hasn't been visited yet
            if not Visited[NR2, NC2, H + 1] then
            begin
              Visited[NR2, NC2, H + 1] := True; // Mark state as visited

              // Push the new state onto the stack
              SetLength(Front, Length(Front) + 1);
              Front[High(Front)].pos.r := NR2;
              Front[High(Front)].pos.c := NC2;
              Front[High(Front)].h := H + 1;
            end;
          end;
        end;
      end; // End exploring neighbors
    end; // End while (DFS stack not empty)

    // Count how many unique '9' cells were reached *from this specific trailhead*
    CurrentReachedCount := 0;
    for r := 0 to NR - 1 do
      for c := 0 to NC - 1 do
        if Reached[r, c] then
          Inc(CurrentReachedCount);

    // Add the count for this trailhead to the total sum
    SumScores := SumScores + CurrentReachedCount;

  end; // End loop through trailheads

  // --- Print the final result ---
  WriteLn(SumScores);

end.
