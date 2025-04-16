
{$mode objfpc}{$H+} // Use Object Pascal mode for AnsiString and Int64
{$OPTIMIZATION ON,LEVEL3} // Request optimizations
{$ASSERTIONS OFF} // Turn off assertions for speed in final build

program ExplosivesInCyberspace;

uses
  SysUtils; // Provides IntToStr, Val, etc.

var
  inputFile: Text;
  compressedData: AnsiString; // Use AnsiString for potentially large inputs and easier manipulation
  line: AnsiString;
  i: Integer;
  totalLengthPart1: Int64;
  totalLengthPart2: Int64;
  currentIndex: Integer;
  currentChar: Char;
  numStrN, numStrM: AnsiString;
  N, M, code, skipCount: Integer;
  dummyChar: Char; // For skipping characters in Part 1

// Function to calculate decompressed length for Part 2 (recursive)
// Takes the compressed string S, the starting index `idx` (passed by reference
// so it advances), and the end index `endIdx`.
// Returns the calculated length of the segment S[idx..endIdx].
function CalculateDecompressedLengthV2(const S: AnsiString; var idx: Integer; endIdx: Integer): Int64;
var
  len: Int64;
  markerN, markerM: Int64; // Use Int64 for safety, though Integer might suffice based on constraints
  valCode: Integer;
  tempStr: AnsiString;
  subLength: Int64;
  dataStartIndex, dataEndIndex, recursionIndex: Integer;
begin
  len := 0;
  while idx <= endIdx do
  begin
    if S[idx] = '(' then
    begin
      Inc(idx); // Move past '('

      // Parse N
      tempStr := '';
      while (idx <= endIdx) and (S[idx] <> 'x') do
      begin
        tempStr := tempStr + S[idx];
        Inc(idx);
      end;
      Val(tempStr, markerN, valCode); // Error check omitted for brevity
      Inc(idx); // Move past 'x'

      // Parse M
      tempStr := '';
      while (idx <= endIdx) and (S[idx] <> ')') do
      begin
        tempStr := tempStr + S[idx];
        Inc(idx);
      end;
      Val(tempStr, markerM, valCode); // Error check omitted for brevity
      Inc(idx); // Move past ')'

      // Define the segment to be recursively processed
      dataStartIndex := idx;
      dataEndIndex := idx + Integer(markerN) - 1; // Calculate end index carefully

      // Recursively calculate the length of the segment
      // Pass a *copy* of the start index for the recursive call
      recursionIndex := dataStartIndex;
      subLength := CalculateDecompressedLengthV2(S, recursionIndex, dataEndIndex);

      // Add the multiplied length
      len := len + markerM * subLength;

      // Advance the main index past the processed segment
      // The recursive call might not have processed the whole segment if it hit endIdx early,
      // but the marker *defined* N characters, so we skip N characters from the *start*
      // of the data section defined by the marker.
      idx := dataEndIndex + 1;

    end
    else
    begin
      // Normal character
      len := len + 1;
      Inc(idx);
    end;
  end;
  Result := len;
end;


// --- Main Program ---
begin
  // --- Read Input and Prepare Data ---
  Assign(inputFile, 'input.txt');
  try
    Reset(inputFile);
  except
    on E: EInOutError do
    begin
      WriteLn('Error: Cannot open input file "input.txt".');
      Halt(1); // Exit program with error code
    end;
  end;

  compressedData := '';
  while not Eof(inputFile) do
  begin
    // Read character by character to handle files without standard line endings
    // and strip whitespace on the fly.
    while not Eof(inputFile) do
    begin
        Read(inputFile, currentChar);
        // Check if character is not whitespace (space, tab, CR, LF)
        if not (CharInSet(currentChar, [#9, #10, #13, ' '])) then
        begin
            // Break if EOF was reached *after* reading the last non-whitespace char
            if Eof(inputFile) and not (CharInSet(currentChar, [#9, #10, #13, ' '])) then
            begin
                 compressedData := compressedData + currentChar;
                 Break; // exit inner read loop
            end
            // Break if EOF was reached and the last char *was* whitespace (already handled)
            else if Eof(inputFile) then
                 Break
            else // Normal non-whitespace character
                 compressedData := compressedData + currentChar;

        end
        // Break if EOF was reached *while* reading whitespace
        else if Eof(inputFile) then
            Break;

    end;
     // Handle case where file ends exactly after last non-whitespace char read by inner loop
     if Eof(inputFile) then Break;
  end;
  Close(inputFile);

  // --- Part 1 Calculation (Iterative) ---
  totalLengthPart1 := 0;
  currentIndex := 1; // Pascal strings are 1-based
  while currentIndex <= Length(compressedData) do
  begin
    currentChar := compressedData[currentIndex];
    if currentChar = '(' then
    begin
      Inc(currentIndex); // Move past '('
      numStrN := '';
      while (currentIndex <= Length(compressedData)) and (compressedData[currentIndex] <> 'x') do
      begin
        numStrN := numStrN + compressedData[currentIndex];
        Inc(currentIndex);
      end;
      Val(numStrN, N, code);
      Inc(currentIndex); // Move past 'x'

      numStrM := '';
      while (currentIndex <= Length(compressedData)) and (compressedData[currentIndex] <> ')') do
      begin
        numStrM := numStrM + compressedData[currentIndex];
        Inc(currentIndex);
      end;
      Val(numStrM, M, code);
      Inc(currentIndex); // Move past ')'

      // Add length contribution for Part 1
      totalLengthPart1 := totalLengthPart1 + Int64(N) * Int64(M);

      // Skip the next N characters *in the input* for Part 1 logic
      currentIndex := currentIndex + N;
    end
    else
    begin
      // Normal character
      totalLengthPart1 := totalLengthPart1 + 1;
      Inc(currentIndex);
    end;
  end;

  // --- Part 2 Calculation (Recursive) ---
  currentIndex := 1; // Reset index for Part 2 calculation
  totalLengthPart2 := CalculateDecompressedLengthV2(compressedData, currentIndex, Length(compressedData));

  // --- Output Results ---
  // The problem asks for the final Part 2 length.
  // We calculated Part 1 as well, but only output Part 2 as requested.
  // Writeln('Part 1 Decompressed Length: ', totalLengthPart1); // Optional: Output Part 1 result
  Writeln(totalLengthPart2); // Output Part 2 result to standard output

end.
