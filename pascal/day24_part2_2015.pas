
program QuantumEntanglementFour;

{$mode objfpc}{$H+}
{$Q+} // Overflow checking enabled.
{$R+} // Range checking enabled.
{$O3} // Optimization level 3

uses
  SysUtils, // Provides StrToInt, IntToStr, High, Int64, etc.
  Math;     // Provides Min function

type
  TIntegerDynArray = array of Integer;
  TBooleanDynArray = array of Boolean;

var
  Packages: TIntegerDynArray; // Stores the weights of the packages
  N: Integer;                // Total number of packages
  TargetWeight: Int64;       // The target weight for each of the four groups
  MinQE: Int64;              // Minimum Quantum Entanglement found so far for the smallest group size
  FoundSolutionAtSize: Boolean; // Flag indicating if a solution was found for the current group size being checked
  Used: TBooleanDynArray;    // Tracks which packages are currently assigned to a group during recursive search

// --- Forward Declarations ---
// These are necessary because functions call each other recursively or in a nested way.
function SearchLevel3(Index, Count: Integer; CurrentSum: Int64; Target: Int64): Boolean; forward;
function SearchLevel2(Index, Count: Integer; CurrentSum: Int64; Target: Int64): Boolean; forward;
procedure SearchLevel1(Index, Count: Integer; CurrentSum, CurrentQE: Int64; TargetSize: Integer; Target: Int64); forward;

// --- Recursive Search Functions ---

// SearchLevel3: Tries to find the third group. If successful, checks if the remaining items form the fourth group.
function SearchLevel3(Index, Count: Integer; CurrentSum: Int64; Target: Int64): Boolean;
var
  i: Integer;
  RemainingSum: Int64;
  Possible: Boolean;
begin
  // Base Case: A combination sums exactly to the target weight.
  if CurrentSum = Target then
  begin
    // Check if the sum of the *remaining* unused items equals the target weight (forming the 4th group).
    RemainingSum := 0;
    Possible := True;
    for i := 0 to N - 1 do
    begin
      if not Used[i] then // If the package is not used in groups 1, 2, or 3
      begin
        // Check for potential overflow before adding.
        if High(Int64) - Packages[i] < RemainingSum then
        begin
          Possible := False; // Sum would overflow, cannot be TargetWeight
          Break;
        end;
        RemainingSum := RemainingSum + Packages[i];
      end;
    end;
    // Return true only if the remaining sum is exactly the target weight.
    Result := Possible and (RemainingSum = Target);
    Exit;
  end;

  // Pruning Conditions: Stop searching if the current path is invalid or cannot lead to a solution.
  if (Index >= N) or (CurrentSum > Target) then
  begin
    Result := False;
    Exit;
  end;

  Result := False; // Default result unless a valid partition is found.

  // --- Recursive Step ---

  // Attempt 1: Include Packages[Index] in the current group (Group 3), if it hasn't been used yet.
  if not Used[Index] then
  begin
    Used[Index] := True; // Mark as used for this recursive path.
    // Recurse, trying to complete Group 3.
    if SearchLevel3(Index + 1, Count + 1, CurrentSum + Packages[Index], Target) then
    begin
      Result := True;     // A valid partition was found down this path.
      Used[Index] := False; // Backtrack *after* returning true up the call stack.
      Exit;               // No need to explore further from this state.
    end;
    Used[Index] := False; // Backtrack: Unmark if including this item didn't lead to a solution.
  end;

  // Attempt 2: Exclude Packages[Index] from the current group (Group 3).
  // Continue the search with the next item.
  if SearchLevel3(Index + 1, Count, CurrentSum, Target) then
  begin
    Result := True; // A valid partition was found by excluding this item.
    Exit;           // No need to explore further.
  end;

  // If neither including nor excluding Packages[Index] led to a solution from this point.
  Result := False;
end;

// SearchLevel2: Tries to find the second group. If successful, calls SearchLevel3 to check groups 3 and 4.
function SearchLevel2(Index, Count: Integer; CurrentSum: Int64; Target: Int64): Boolean;
begin
  // Base Case: A combination sums exactly to the target weight (potential Group 2 found).
  if CurrentSum = Target then
  begin
    // Now, check if the remaining unused items can be partitioned into Group 3 and Group 4.
    // Start the search for Group 3 from the beginning (index 0).
    Result := SearchLevel3(0, 0, 0, Target);
    Exit;
  end;

  // Pruning Conditions.
  if (Index >= N) or (CurrentSum > Target) then
  begin
    Result := False;
    Exit;
  end;

  Result := False;

  // --- Recursive Step ---

  // Attempt 1: Include Packages[Index] in Group 2, if not used by Group 1.
  if not Used[Index] then
  begin
    Used[Index] := True; // Mark as used for Group 2 attempt.
    // Recurse to complete Group 2.
    if SearchLevel2(Index + 1, Count + 1, CurrentSum + Packages[Index], Target) then
    begin
      Result := True;
      Used[Index] := False; // Backtrack after success.
      Exit;
    end;
    Used[Index] := False; // Backtrack if include path failed.
  end;

  // Attempt 2: Exclude Packages[Index] from Group 2.
  if SearchLevel2(Index + 1, Count, CurrentSum, Target) then
  begin
    Result := True;
    Exit;
  end;

  Result := False;
end;

// SearchLevel1: Tries to find the first group of a specific size (TargetSize).
// If successful, calls SearchLevel2 to check groups 2, 3, and 4, and updates MinQE.
procedure SearchLevel1(Index, Count: Integer; CurrentSum, CurrentQE: Int64; TargetSize: Integer; Target: Int64);
var
  PartitionPossible: Boolean;
  NextQE: Int64;
begin
  // Optimization: If we already found a solution at this TargetSize,
  // and the current path's QE is already worse than the minimum found, prune.
  if FoundSolutionAtSize and (CurrentQE >= MinQE) then
     Exit;

  // Pruning Conditions.
  if (CurrentSum > Target) or (Count > TargetSize) then
     Exit;

  // Base Case: A combination of the target size is formed.
  if Count = TargetSize then
  begin
    // Check if this combination also has the target weight.
    if CurrentSum = Target then
    begin
      // Found a potential Group 1. The 'Used' array marks its items.
      // Check if the remaining items can form Groups 2, 3, and 4.
      PartitionPossible := SearchLevel2(0, 0, 0, Target); // Start search for Group 2.

      if PartitionPossible then
      begin
        // Valid partition found. Update MinQE if this group's QE is smaller.
        MinQE := Min(MinQE, CurrentQE);
        FoundSolutionAtSize := True; // Mark that we found a solution for this size.
      end;
    end;
    Exit; // Backtrack from the base case.
  end;

  // Reached end of items without forming a group of TargetSize.
  if Index >= N then
     Exit;

  // --- Recursive Step ---

  // Attempt 1: Include Packages[Index] in Group 1.
  Used[Index] := True;

  // Calculate QE, checking for overflow. Assume package weights are non-negative.
  if Packages[Index] = 0 then NextQE := 0 // QE becomes 0 if any package weight is 0.
  else if (CurrentQE > 0) and (Packages[Index] > 0) and (CurrentQE > High(Int64) div Packages[Index]) then NextQE := High(Int64) // Indicate overflow.
  else NextQE := CurrentQE * Packages[Index];

  // Recurse only if the QE hasn't overflowed or could potentially be the new minimum.
  if (not FoundSolutionAtSize) or (NextQE < MinQE) then
     SearchLevel1(Index + 1, Count + 1, CurrentSum + Packages[Index], NextQE, TargetSize, Target);

  Used[Index] := False; // Backtrack: Unmark after the 'include' path returns.

  // Attempt 2: Exclude Packages[Index] from Group 1.
  // Continue search without including the current item.
  SearchLevel1(Index + 1, Count, CurrentSum, CurrentQE, TargetSize, Target);
end;


// --- Main Program ---
var
  InputFile: TextFile;
  Line: string;
  PackageWeight: Integer;
  TotalWeight: Int64;
  FirstGroupSize: Integer;
  i: Integer;
  ReadError: Boolean;

begin
  ReadError := False;
  // --- Read Input from 'input.txt' ---
  Assign(InputFile, 'input.txt');
  try
    Reset(InputFile);
    SetLength(Packages, 0); // Initialize dynamic array.
    while not Eof(InputFile) do
    begin
      ReadLn(InputFile, Line);
      Line := Trim(Line); // Remove leading/trailing whitespace.
      if Line <> '' then
      begin
        try
          PackageWeight := StrToInt(Line);
          if PackageWeight < 0 then
          begin
              WriteLn('Error: Package weights cannot be negative.');
              ReadError := True; Break;
          end;
          SetLength(Packages, Length(Packages) + 1);
          Packages[High(Packages)] := PackageWeight;
        except
          on EConvertError do
          begin
            WriteLn('Error: Invalid integer format in input file: ', Line);
            ReadError := True; Break;
          end;
        end;
      end;
    end;
  finally
    Close(InputFile);
  end;

  if ReadError then Halt(1);

  N := Length(Packages);
  // Basic check: Need enough packages for 4 groups.
  if N < 4 then
  begin
     WriteLn(0); // Output 0 if impossible.
     Halt;
  end;

  // --- Calculate Total and Target Weight ---
  TotalWeight := 0;
  for i := 0 to N - 1 do
  begin
     // Check for potential overflow before adding.
     if High(Int64) - Packages[i] < TotalWeight then
     begin
        WriteLn('Error: Total weight calculation resulted in overflow.');
        Halt(1);
     end;
     TotalWeight := TotalWeight + Packages[i];
  end;

  // Handle edge case where all package weights are 0.
  if TotalWeight = 0 then
  begin
      WriteLn(0); // The minimum QE is 0.
      Halt;
  end;

  // Check if the total weight is divisible by 4.
  if TotalWeight mod 4 <> 0 then
  begin
    // Cannot partition into 4 equal weight groups.
    WriteLn(0); // Output 0 if impossible.
    Halt;
  end;
  TargetWeight := TotalWeight div 4; // Calculate the required weight per group.

  // --- Initialize Search Variables ---
  MinQE := High(Int64);           // Initialize minimum QE to the largest possible value.
  SetLength(Used, N);             // Allocate the global 'Used' array. FoundSolutionAtSize handles loop break.

  // --- Iterate through possible sizes for the first group (smallest sizes first) ---
  for FirstGroupSize := 1 to N - 3 do // Group 1 size can be from 1 up to N-3.
  begin
    FoundSolutionAtSize := False; // Reset flag for this size.
    // Reset the 'Used' array before searching for combinations of this size.
    for i := 0 to N - 1 do Used[i] := False;

    // Start the recursive search for Group 1 combinations of 'FirstGroupSize'.
    // Start QE calculation with 1 (multiplicative identity).
    SearchLevel1(0, 0, 0, 1, FirstGroupSize, TargetWeight);

    // If SearchLevel1 found any valid partition for this FirstGroupSize,
    // MinQE has been updated with the minimum QE for this size.
    // Since we iterate sizes from smallest up, we can stop searching.
    if FoundSolutionAtSize then
      Break; // Exit the loop over FirstGroupSize.
  end;

  // --- Print Result ---
  if MinQE = High(Int64) then
    // This case should ideally not be reached if TotalWeight % 4 == 0 and N >= 4,
    // unless there's some edge case with weights preventing partition.
    WriteLn('0') // Output 0 if no solution found (as per typical contest behavior).
  else
    WriteLn(MinQE); // Output the minimum QE found for the smallest valid group size.

end.
