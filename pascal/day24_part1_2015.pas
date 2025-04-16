
program Day24_ItHangsInTheBalance;

{$MODE OBJFPC}{$H+} // Use Object Pascal mode and enable extended syntax
{$OPTIMIZATION ON} // Enable optimizations
//{$ASSERTIONS ON}   // Enable assertions for debugging (optional)

uses
  SysUtils; // Provides Int64, QWord, High, File I/O routines, StrToInt, Trim

const
  INPUT_FILE = 'input.txt';

type
  // Using dynamic array for flexibility with input size
  TPackageWeights = array of Integer;

var
  weights: TPackageWeights; // Dynamic array to hold package weights
  numPackages: Integer;      // Total number of packages read
  targetWeight: Int64;     // The required weight for each of the three groups
  minCountFound: Integer;  // Minimum number of packages found in a valid first group
  minQEFound: QWord;       // Minimum Quantum Entanglement for groups with minCountFound packages

// Comparison function for sorting Integers in descending order
// Used by the Sort procedure (or manual sort implementation)
function CompareIntDesc(p1, p2: Pointer): integer;
begin
  // Dereference pointers to get integer values and compare
  // Result > 0 if PInteger(p1)^ < PInteger(p2)^ (for descending)
  Result := PInteger(p2)^ - PInteger(p1)^;
end;

// Recursive function to find the best group configuration for the first group
// Parameters:
//   index: Current package index being considered (0-based for dynamic array)
//   currentSum: Sum of weights in the group being built so far
//   currentCount: Number of packages in the group being built so far
//   currentQE: Quantum Entanglement (product) of weights in the group so far
procedure FindBestGroup(index: Integer; currentSum: Int64; currentCount: Integer; currentQE: QWord);
begin
  // --- Pruning ---

  // 1. If the current sum already exceeds the target weight, this path is invalid.
  if currentSum > targetWeight then
    Exit; // Backtrack

  // 2. If the number of packages in the current group under construction
  //    is already equal to or greater than the best count found so far,
  //    this path cannot lead to a solution with fewer packages.
  //    (It might lead to a solution with the same count but different QE,
  //     which is handled when a solution is found).
  if currentCount >= minCountFound then
    Exit; // Backtrack

  // --- Base Case: Found a valid group (sum matches target) ---
  if currentSum = targetWeight then
  begin
    // We found a combination of packages that sums exactly to the target weight.
    // Now, check if this solution is better than the best one found so far.

    // Because of pruning step #2, we know currentCount <= minCountFound.
    if currentCount < minCountFound then
    begin
      // This group uses fewer packages than any previous best solution.
      // Update both the minimum count and the minimum QE.
      minCountFound := currentCount;
      minQEFound := currentQE;
    end
    else if currentCount = minCountFound then
    begin
      // This group uses the same number of packages as the current best.
      // Apply the tie-breaker rule: choose the one with the smaller QE.
      if currentQE < minQEFound then
        minQEFound := currentQE;
    end;

    // We've processed this valid group, no need to explore adding more packages to it.
    Exit; // Backtrack
  end;

  // --- Base Case: Exhausted all available packages ---
  // If the index goes beyond the array bounds, we can't add more packages.
  if index >= Length(weights) then
    Exit; // Backtrack

  // --- Recursive Steps ---

  // Step 1: Explore the possibility of INCLUDING the package at the current 'index'.
  // We check if the weight is positive to avoid issues with QE potentially becoming 0
  // (though the problem implies positive weights).
  if weights[index] > 0 then
  begin
     // Check for potential QWord overflow before multiplication.
     // This is unlikely given QWord's range unless weights are astronomically large.
     // Example check (can be omitted if overflow is deemed impossible for the input scale):
     // if (currentQE > 0) and (QWord(weights[index]) > (High(QWord) div currentQE)) then
     // begin
     //   WriteLn('Warning: Potential QWord overflow detected.');
     //   // Decide how to handle: maybe skip this path or halt with error.
     // end
     // else
     // begin
         FindBestGroup(index + 1,                // Move to next package index
                       currentSum + weights[index], // Add current weight to sum
                       currentCount + 1,            // Increment package count
                       currentQE * QWord(weights[index])); // Multiply QE by current weight (cast to QWord)
     // end;
  end;
  // Note: If weights[index] could be 0, the logic might need adjustment based on how QE=0 is handled.
  // Assuming positive weights as per standard AoC practice.


  // Step 2: Explore the possibility of EXCLUDING the package at the current 'index'.
  // We simply move to the next index without changing the sum, count, or QE.
  FindBestGroup(index + 1, currentSum, currentCount, currentQE);

end;

// --- Main Program Entry Point ---
var
  inputFile: TextFile; // File handle for input.txt
  i, j: Integer;       // Loop counters
  totalWeight: Int64;  // Sum of all package weights
  line: string;        // To read lines from the file
  temp: Integer;       // Temporary variable for sorting
  swapped: Boolean;    // Flag for Bubble Sort optimization

begin
  // --- Read Input from input.txt ---
  if not FileExists(INPUT_FILE) then
  begin
    WriteLn('Error: Input file "', INPUT_FILE, '" not found.');
    Halt(1); // Exit with error code
  end;

  // Initialize dynamic array and counters
  SetLength(weights, 0); // Start with an empty array
  numPackages := 0;
  totalWeight := 0;

  AssignFile(inputFile, INPUT_FILE);
  try
    Reset(inputFile); // Open file for reading
    while not Eof(inputFile) do
    begin
      ReadLn(inputFile, line); // Read one line (one weight)
      line := Trim(line);     // Remove leading/trailing whitespace
      if line = '' then continue; // Skip empty lines

      Inc(numPackages);
      // Resize the dynamic array to accommodate the new package
      SetLength(weights, numPackages);
      try
         // Convert the string to an integer and store it (0-based index)
         weights[numPackages - 1] := StrToInt(line);

         // Validate weight (assuming weights must be positive)
         if weights[numPackages - 1] <= 0 then
         begin
            WriteLn('Error: Package weight must be positive. Found: ', weights[numPackages - 1]);
            Halt(1);
         end;

         // Add to total weight
         totalWeight := totalWeight + weights[numPackages - 1];
      except
         // Handle potential error during string-to-integer conversion
         on EConvertError do
         begin
            WriteLn('Error reading integer weight from line: "', line, '"');
            Halt(1);
         end;
      end;
    end;
  finally
    CloseFile(inputFile); // Ensure file is closed
  end;

  // Check if any packages were read
  if numPackages = 0 then
  begin
    WriteLn('Error: Input file is empty or contains no valid weights.');
    Halt(1);
  end;

  // --- Pre-computation and Validation ---

  // The total weight must be divisible by 3 to allow splitting into three equal groups.
  if (totalWeight mod 3) <> 0 then
  begin
    WriteLn('Error: Total weight (', totalWeight, ') is not divisible by 3. Cannot form three equal groups.');
    // According to the problem, a solution requires 3 equal groups. If not possible, it's an error state.
    Halt(1); // Exit indicating an impossible scenario based on input.
  end;

  // Calculate the target weight for each group.
  targetWeight := totalWeight div 3;

  // --- Sort Weights ---
  // Sorting weights in descending order is a crucial optimization.
  // It helps the recursive search find solutions with fewer items faster (as larger items are tried first)
  // and makes the pruning based on `currentCount >= minCountFound` more effective early on.

  // Using a simple Bubble Sort for demonstration. For larger inputs, a more efficient
  // sort like QuickSort or MergeSort (potentially using FPC's built-in libraries if available/preferred)
  // would be better.
  for i := 0 to numPackages - 2 do // Outer loop for passes (0-based index)
  begin
    swapped := False; // Flag to detect if any swaps occurred in this pass
    // Inner loop for comparisons and swaps
    for j := 0 to numPackages - i - 2 do // Compare weights[j] and weights[j+1]
    begin
      // If elements are in the wrong order (for descending sort)
      if weights[j] < weights[j+1] then
      begin
        // Swap elements
        temp := weights[j];
        weights[j] := weights[j+1];
        weights[j+1] := temp;
        swapped := True; // Mark that a swap happened
      end;
    end;
    // If no swaps occurred in a full pass, the array is sorted.
    if not swapped then Break;
  end;

  // --- Initialize Search Variables ---
  // Initialize minCountFound to a value larger than any possible number of packages.
  minCountFound := numPackages + 1;
  // Initialize minQEFound to the maximum possible value for QWord.
  minQEFound := High(QWord);

  // --- Start the Recursive Search ---
  // Begin the search from the first package (index 0), with initial sum 0, count 0,
  // and initial Quantum Entanglement of 1 (identity element for multiplication).
  FindBestGroup(0, 0, 0, 1);

  // --- Output Result ---
  // After the search completes, minQEFound will hold the result if a solution was found.
  if minCountFound > numPackages then
  begin
    // This should theoretically not happen if the input guarantees a solution exists
    // and the total weight is divisible by 3.
    WriteLn('Error: No solution found (minCountFound was not updated). Check input or logic.');
    Halt(1);
  end
  else
  begin
    // Print the minimum Quantum Entanglement found for the first group
    // that satisfied the conditions (smallest size, then smallest QE).
    WriteLn(minQEFound);
  end;

// End of the main program block
end.
