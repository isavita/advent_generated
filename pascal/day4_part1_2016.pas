
program SecurityThroughObscurity;

{$MODE OBJFPC}{$H+} // Use Object Pascal mode and enable {$H+} for longer strings

uses
  SysUtils; // Provides StrToInt, LastDelimiter, etc.

type
  // Record to store character and its frequency
  TLetterCount = record
    ch: char;
    count: integer;
  end;

  // Array to hold frequency data for sorting
  TLetterCountArray = array[1..26] of TLetterCount;

// --- Function to Compare Letter Counts for Sorting ---
// Sorts descending by count, then ascending by character for ties
function CompareLetterCounts(const a, b: TLetterCount): integer;
begin
  if a.count > b.count then
    Result := -1 // a comes before b
  else if a.count < b.count then
    Result := 1 // b comes before a
  else
  begin // Counts are equal, sort alphabetically
    if a.ch < b.ch then
      Result := -1 // a comes before b
    else if a.ch > b.ch then
      Result := 1 // b comes before a
    else
      Result := 0; // Should not happen for distinct letters
  end;
end;

// --- Simple Swap Procedure for Sorting ---
procedure Swap(var a, b: TLetterCount);
var
  temp: TLetterCount;
begin
  temp := a;
  a := b;
  b := temp;
end;

// --- Bubble Sort for Letter Counts (Simple for small N=26) ---
procedure SortLetterCounts(var arr: TLetterCountArray);
var
  i, j: integer;
  swapped: boolean;
begin
  // Using a slightly optimized bubble sort
  for i := 1 to High(arr) - 1 do
  begin
    swapped := False;
    for j := 1 to High(arr) - i do
    begin
      if CompareLetterCounts(arr[j], arr[j+1]) > 0 then
      begin
        Swap(arr[j], arr[j+1]);
        swapped := True;
      end;
    end;
    // If no two elements were swapped by inner loop, then break
    if not swapped then Break;
  end;
end;


// --- Main Program ---
var
  inputFile: TextFile;
  line: string;
  encryptedNamePart: string;
  sectorIDStr: string;
  checksum: string;
  calculatedChecksum: string;
  lastDashPos: integer;
  openBracketPos: integer;
  closeBracketPos: integer;
  sectorID: integer;
  totalSectorIDSum: Int64; // Use Int64 for potentially large sums
  counts: array['a'..'z'] of integer;
  letterCounts: TLetterCountArray;
  i: integer;
  ch: char;
  idx: integer;

begin
  // --- Initialization ---
  totalSectorIDSum := 0;

  // --- File Input ---
  AssignFile(inputFile, 'input.txt');
  try
    Reset(inputFile);
  except
    on E: EInOutError do
    begin
      Writeln('Error: Cannot open input file "input.txt".');
      Halt(1); // Exit with error code
    end;
  end;

  // --- Process Each Line ---
  while not Eof(inputFile) do
  begin
    Readln(inputFile, line);
    if Length(line) = 0 then continue; // Skip empty lines

    // --- Parsing the Line ---
    // Find the positions of the last dash, '[', and ']'
    // Use Pos as LastDelimiter might not be universally available or behave exactly as needed here.
    // Find '[' first, as it reliably separates ID/checksum from the name.
    openBracketPos := Pos('[', line);
    if openBracketPos = 0 then continue; // Invalid format

    closeBracketPos := Pos(']', line);
    if closeBracketPos = 0 then continue; // Invalid format

    // Find the last dash *before* the open bracket
    lastDashPos := 0;
    for i := openBracketPos - 1 downto 1 do
    begin
        if line[i] = '-' then
        begin
            lastDashPos := i;
            Break;
        end;
    end;
    if lastDashPos = 0 then continue; // Invalid format (no dash before ID)


    encryptedNamePart := Copy(line, 1, lastDashPos - 1);
    sectorIDStr := Copy(line, lastDashPos + 1, openBracketPos - lastDashPos - 1);
    checksum := Copy(line, openBracketPos + 1, closeBracketPos - openBracketPos - 1);

    // Convert Sector ID string to integer
    try
      sectorID := StrToInt(sectorIDStr);
    except
      on E: EConvertError do
        continue; // Skip lines with invalid sector IDs
    end;

    // --- Calculate Letter Frequencies ---
    FillChar(counts, SizeOf(counts), 0); // Reset counts for each line
    for i := 1 to Length(encryptedNamePart) do
    begin
      ch := encryptedNamePart[i];
      if ch in ['a'..'z'] then
        Inc(counts[ch]);
    end;

    // --- Prepare Letter Counts for Sorting ---
    idx := 0;
    for ch := 'a' to 'z' do
    begin
      Inc(idx);
      letterCounts[idx].ch := ch;
      letterCounts[idx].count := counts[ch];
    end;

    // --- Sort Letters by Frequency and Alphabetically ---
    SortLetterCounts(letterCounts);

    // --- Construct Calculated Checksum from Top 5 ---
    calculatedChecksum := '';
    for i := 1 to 5 do // Get the top 5 letters
    begin
       // Add check in case there are fewer than 5 distinct letters (though problem implies 5)
       if i <= High(letterCounts) then
         calculatedChecksum := calculatedChecksum + letterCounts[i].ch;
    end;

    // --- Compare Checksums and Sum Sector IDs ---
    if calculatedChecksum = checksum then
    begin
       //writeln('Real Room Found: ', line, ' (Calculated: ', calculatedChecksum, ')'); // Debug output
       totalSectorIDSum := totalSectorIDSum + sectorID;
    end //else writeln('Decoy Room: ', line, ' (Calculated: ', calculatedChecksum, ')');


  end;

  // --- Close File ---
  CloseFile(inputFile);

  // --- Print Output ---
  Writeln(totalSectorIDSum);

end.
