
program Day25LetItSnow;

{$MODE OBJFPC}{$H+} // Enable Object Pascal mode and long strings support in FPC

uses
  SysUtils; // Used for StrToIntDef, FileExists, etc. although not strictly necessary for basic IO

const
  INPUT_FILENAME = 'input.txt';
  INITIAL_CODE: Int64 = 20151125;
  MULTIPLIER: Int64 = 252533;
  DIVISOR: Int64 = 33554393;

var
  inputFile: Text;
  lineBuffer: String; // To read the whole line
  targetRow, targetCol: LongInt; // Row and Column numbers from input
  k: Int64;           // Diagonal number (Row + Col - 1)
  targetIndex: Int64; // The sequence number (1st, 2nd, 3rd...) of the code at (Row, Col)
  currentCode: Int64; // The current code value during calculation
  i: Int64;           // Loop counter
  word: String;       // Temporary storage for words read from file
  parsePos: Integer;  // Position for parsing the line buffer
  tempNumStr: String; // Temporary string to hold number part for parsing
  code: Integer;      // Error code for StrToIntDef

  // Helper function to extract numbers after keywords - more robust parsing
  function ExtractNumberAfterKeyword(const line: String; const keyword: String; var number: LongInt): Boolean;
  var
    keyPos, numStartPos, numEndPos: Integer;
  begin
    Result := False;
    keyPos := Pos(keyword, line);
    if keyPos > 0 then
    begin
      numStartPos := keyPos + Length(keyword);
      // Skip non-digit characters immediately after the keyword
      while (numStartPos <= Length(line)) and (not (line[numStartPos] in ['0'..'9'])) do
      begin
        Inc(numStartPos);
      end;

      if numStartPos <= Length(line) then
      begin
        numEndPos := numStartPos;
        // Find the end of the number
        while (numEndPos <= Length(line)) and (line[numEndPos] in ['0'..'9']) do
        begin
          Inc(numEndPos);
        end;
        // Correct end position (it's now one past the last digit)
        Dec(numEndPos);

        if numEndPos >= numStartPos then
        begin
          tempNumStr := Copy(line, numStartPos, numEndPos - numStartPos + 1);
          number := StrToIntDef(tempNumStr, -1); // Use StrToIntDef for safety
          Result := (number <> -1); // Check if conversion was successful
        end;
      end;
    end;
  end;

begin
  // --- Check if input file exists ---
  if not FileExists(INPUT_FILENAME) then
  begin
    Writeln('Error: Input file "', INPUT_FILENAME, '" not found.');
    Halt(1); // Terminate with error code 1
  end;

  // --- Read Input ---
  Assign(inputFile, INPUT_FILENAME);
  Reset(inputFile);

  targetRow := -1;
  targetCol := -1;

  // Read the input file line by line (assuming the relevant info is on one line)
  if not Eof(inputFile) then
  begin
    ReadLn(inputFile, lineBuffer);
    // Try extracting row and column numbers using the helper function
    if not ExtractNumberAfterKeyword(lineBuffer, 'row', targetRow) then
    begin
       Writeln('Error: Could not find or parse target row number in input file.');
       Close(inputFile);
       Halt(2);
    end;

    if not ExtractNumberAfterKeyword(lineBuffer, 'column', targetCol) then
    begin
       Writeln('Error: Could not find or parse target column number in input file.');
       Close(inputFile);
       Halt(3);
    end;
  end
  else
  begin
    Writeln('Error: Input file "', INPUT_FILENAME, '" is empty.');
    Close(inputFile);
    Halt(4);
  end;

  Close(inputFile);

  // Check if numbers were successfully parsed
  if (targetRow <= 0) or (targetCol <= 0) then
  begin
     Writeln('Error: Invalid row or column number found (must be positive). Row=', targetRow, ', Col=', targetCol);
     Halt(5);
  end;

  // --- Calculate Target Index ---
  // The position (R, C) is on the k-th diagonal where k = R + C - 1.
  // The number of elements before this diagonal starts is Sum(1 to k-1) = (k-1)*k / 2.
  // The position C within the diagonal determines the final offset.
  // Use Int64 for calculations to avoid overflow, especially for (k-1)*k.
  k := Int64(targetRow) + Int64(targetCol) - 1;
  targetIndex := (k * (k - 1)) div 2 + Int64(targetCol);

  // --- Calculate Code Value ---
  currentCode := INITIAL_CODE;

  // We need to perform (targetIndex - 1) update steps.
  // The loop starts from the 2nd code index up to the targetIndex.
  for i := 2 to targetIndex do
  begin
    // Calculate the next code: (previous_code * multiplier) mod divisor
    // Ensure intermediate multiplication uses Int64 to prevent overflow
    currentCode := (currentCode * MULTIPLIER) mod DIVISOR;
  end;

  // --- Print Output ---
  // The final value of currentCode is the answer
  Writeln(currentCode);

  // Program finishes successfully implicitly here
end.
