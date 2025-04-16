
program DecompressLength;

{$MODE OBJFPC}{$H+} // Use Object Pascal mode and enable extended syntax
{$APPTYPE CONSOLE} // Specify console application type

uses
  SysUtils; // Provides StrToInt64, File I/O routines, IntToStr, etc.

var
  inputFile: TextFile;
  inputFileName: String = 'input.txt';
  compressedData: AnsiString; // Using AnsiString for potentially large files
  ch: Char;
  i: LongInt;              // Index for iterating through compressedData
  decompressedLen: Int64;  // Use Int64 for potentially very large lengths
  markerLen, markerRepeat: LongInt; // Values L and R from (LxR)
  parseStart, parseEnd: LongInt;   // Indices for parsing numbers within markers
  numStr: AnsiString;         // Temporary string to hold number parts of marker

begin
  // --- Read Input File and Filter Whitespace ---
  compressedData := '';
  try
    AssignFile(inputFile, inputFileName);
    Reset(inputFile);
    while not Eof(inputFile) do
    begin
      Read(inputFile, ch);
      // Append only non-whitespace characters
      // Common whitespace: space, tab, newline, carriage return
      if not CharInSet(ch, [#0..#32]) then
      begin
        compressedData := compressedData + ch;
      end;
    end;
    CloseFile(inputFile);
  except
    on E: EInOutError do
    begin
      WriteLn(ErrOutput, 'Error reading file "', inputFileName, '": ', E.Message);
      Halt(1); // Exit with error code
    end;
  end;

  if Length(compressedData) = 0 then
  begin
      WriteLn(0); // Handle empty or whitespace-only input
      Halt(0);
  end;

  // --- Calculate Decompressed Length ---
  decompressedLen := 0;
  i := 1; // Pascal strings are 1-indexed
  while i <= Length(compressedData) do
  begin
    if compressedData[i] <> '(' then
    begin
      // Regular character, contributes 1 to the length
      Inc(decompressedLen);
      Inc(i);
    end
    else
    begin
      // Start of a marker (LxR)
      parseStart := i + 1; // Position after '('

      // Find 'x' to parse L (markerLen)
      parseEnd := parseStart;
      while (parseEnd <= Length(compressedData)) and (compressedData[parseEnd] <> 'x') do
      begin
        Inc(parseEnd);
      end;

      // Check for malformed marker (missing 'x')
      if (parseEnd > Length(compressedData)) or (parseEnd = parseStart) then
      begin
          WriteLn(ErrOutput, 'Error: Malformed marker - missing ''x'' or length at index ', i);
          Halt(1);
      end;

      numStr := Copy(compressedData, parseStart, parseEnd - parseStart);
      try
        markerLen := StrToInt64(numStr); // Use StrToInt64 for safety
      except
        on E: EConvertError do
        begin
            WriteLn(ErrOutput, 'Error: Invalid length number in marker at index ', i, ': "', numStr, '"');
            Halt(1);
        end;
      end;


      // Find ')' to parse R (markerRepeat)
      parseStart := parseEnd + 1; // Position after 'x'
      parseEnd := parseStart;
      while (parseEnd <= Length(compressedData)) and (compressedData[parseEnd] <> ')') do
      begin
        Inc(parseEnd);
      end;

       // Check for malformed marker (missing ')')
      if (parseEnd > Length(compressedData)) or (parseEnd = parseStart) then
      begin
          WriteLn(ErrOutput, 'Error: Malformed marker - missing '')'' or repetition count at index ', i);
          Halt(1);
      end;

      numStr := Copy(compressedData, parseStart, parseEnd - parseStart);
       try
        markerRepeat := StrToInt64(numStr); // Use StrToInt64 for safety
      except
        on E: EConvertError do
        begin
            WriteLn(ErrOutput, 'Error: Invalid repetition number in marker at index ', i, ': "', numStr, '"');
            Halt(1);
        end;
      end;

      // Move index past the marker itself (including ')')
      i := parseEnd + 1;

      // Add the contribution of the decompressed section
      decompressedLen := decompressedLen + (markerLen * markerRepeat);

      // Skip the characters *in the input* that correspond to the data
      // section defined by the marker. This is crucial because the data
      // within this section is NOT scanned for further markers.
      i := i + markerLen;

      // Check if skipping pushed index beyond the input length
      // This isn't strictly an error in logic, but might indicate malformed input
      // if markerLen was unexpectedly large. However, the main loop condition
      // (i <= Length(compressedData)) will handle exiting correctly.

    end;
  end;

  // --- Print Output ---
  WriteLn(decompressedLen);

end.
