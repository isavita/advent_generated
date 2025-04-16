
program AdventOfCodeDay7;

{$mode objfpc}{$H+} // Use Free Pascal extensions like dynamic arrays

uses
  SysUtils; // For file operations and string manipulation

// --- Helper Functions ---

function IsAbba(c1, c2, c3, c4: Char): Boolean;
// Checks if four characters form an ABBA sequence (xyyx, x <> y)
begin
  Result := (c1 = c4) and (c2 = c3) and (c1 <> c2);
end;

function IsAba(c1, c2, c3: Char): Boolean;
// Checks if three characters form an ABA sequence (xyx, x <> y)
begin
  Result := (c1 = c3) and (c1 <> c2);
end;

// --- Part 1: TLS Check ---

function SupportsTLS(const ip: string): Boolean;
var
  i, len: Integer;
  inHypernet: Boolean;
  foundAbbaOutside: Boolean;
  foundAbbaInside: Boolean;
begin
  len := Length(ip);
  inHypernet := False;
  foundAbbaOutside := False;
  foundAbbaInside := False;
  Result := False; // Default to not supporting TLS

  // Iterate through the string to check for ABBA sequences
  // Loop up to len - 3 because we need 4 characters
  for i := 1 to len - 3 do
  begin
    // Update hypernet status based on current character ip[i]
    // Note: ABBA check happens *before* processing potential brackets at i+1, i+2, i+3
    // This correctly handles cases like `[abba]` or `ab[ba]`
    if ip[i] = '[' then
    begin
      inHypernet := True;
      Continue; // Skip ABBA check if current char is '['
    end
    else if ip[i] = ']' then
    begin
      inHypernet := False;
      Continue; // Skip ABBA check if current char is ']'
    end;

    // Check for brackets within the potential ABBA sequence itself
    // If any character is a bracket, it cannot form a valid ABBA sequence part
    if (ip[i+1] = '[') or (ip[i+1] = ']') or
       (ip[i+2] = '[') or (ip[i+2] = ']') or
       (ip[i+3] = '[') or (ip[i+3] = ']') then
    begin
        // If we encounter a bracket within the next 3 chars,
        // carefully advance the main loop index or just continue.
        // The bracket check at the start of the loop handles the transitions.
        // We just need to ensure we don't interpret e.g., `a[ba` as an ABBA.
        // The IsAbba check inherently handles this if chars are different,
        // but explicitly skipping if brackets are involved is safer.
        Continue;
    end;


    // Check if the 4 characters starting at i form an ABBA
    if IsAbba(ip[i], ip[i+1], ip[i+2], ip[i+3]) then
    begin
      if inHypernet then
      begin
        // Found ABBA inside hypernet - this IP immediately fails TLS check
        foundAbbaInside := True;
        Exit; // Exit the function early, Result remains False
      end
      else // Found ABBA outside hypernet
      begin
        foundAbbaOutside := True;
        // Continue checking, as we still need to ensure no ABBA inside
      end;
    end;
  end;

  // After checking the entire string:
  // TLS is supported if we found an ABBA outside AND found no ABBA inside
  Result := foundAbbaOutside and not foundAbbaInside;
end;

// --- Part 2: SSL Check ---

function SupportsSSL(const ip: string): Boolean;
type
  // Dynamic array to store ABA/BAB strings (requires {$mode objfpc})
  TStringArray = array of string;
var
  i, len, abaIdx, babIdx: Integer;
  inHypernet: Boolean;
  abas: TStringArray; // List of ABAs found outside brackets
  babs: TStringArray; // List of BABs found inside brackets
  seq, expectedBab: string;
  foundMatch: Boolean;
begin
  len := Length(ip);
  inHypernet := False;
  SetLength(abas, 0); // Initialize dynamic arrays
  SetLength(babs, 0);
  Result := False; // Default to not supporting SSL

  // Phase 1: Collect all ABA sequences (outside) and BAB candidates (inside)
  // Loop up to len - 2 because we need 3 characters
  for i := 1 to len - 2 do
  begin
    if ip[i] = '[' then
    begin
      inHypernet := True;
      Continue;
    end
    else if ip[i] = ']' then
    begin
      inHypernet := False;
      Continue;
    end;

    // Ensure the sequence doesn't contain brackets
    if (ip[i+1] = '[') or (ip[i+1] = ']') or
       (ip[i+2] = '[') or (ip[i+2] = ']') then
    begin
       Continue;
    end;


    // Check if the 3 characters starting at i form an ABA pattern
    if IsAba(ip[i], ip[i+1], ip[i+2]) then
    begin
      seq := ip[i] + ip[i+1] + ip[i+2]; // Build the 3-char string

      if inHypernet then // Inside brackets - potential BAB
      begin
        SetLength(babs, Length(babs) + 1);
        babs[High(babs)] := seq;
      end
      else // Outside brackets - ABA
      begin
        SetLength(abas, Length(abas) + 1);
        abas[High(abas)] := seq;
      end;
    end;
  end;

  // Phase 2: Check if any ABA corresponds to a BAB
  foundMatch := False;
  // Iterate through all found ABAs (outside sequences)
  for abaIdx := 0 to High(abas) do // Dynamic arrays are 0-based
  begin
    // For the current ABA (e.g., 'xyx'), construct the expected BAB (e.g., 'yxy')
    // Pascal strings are 1-based
    expectedBab := abas[abaIdx][2] + abas[abaIdx][1] + abas[abaIdx][2];

    // Check if this expected BAB exists in our list of sequences found inside brackets
    for babIdx := 0 to High(babs) do
    begin
      if babs[babIdx] = expectedBab then
      begin
        foundMatch := True;
        Break; // Exit the inner babIdx loop once a match is found
      end;
    end;

    if foundMatch then
      Break; // Exit the outer abaIdx loop if a match was found
  end;

  Result := foundMatch;

  // Optional: Free dynamic array memory (good practice, though often handled automatically on exit)
  SetLength(abas, 0);
  SetLength(babs, 0);
end;


// --- Main Program ---
var
  inputFile: TextFile;
  line: string;
  tlsCount: Integer;
  sslCount: Integer;

begin
  // Initialize counts
  tlsCount := 0;
  sslCount := 0;

  // Assign and open the input file
  AssignFile(inputFile, 'input.txt');
  try
    Reset(inputFile);

    // Read file line by line
    while not Eof(inputFile) do
    begin
      ReadLn(inputFile, line);
      if Length(Trim(line)) > 0 then // Process non-empty lines
      begin
        // Part 1 check
        if SupportsTLS(line) then
          Inc(tlsCount);

        // Part 2 check
        if SupportsSSL(line) then
          Inc(sslCount);
      end;
    end;

  finally
    // Ensure the file is closed even if errors occur
    CloseFile(inputFile);
  end;

  // Print the results to standard output
  WriteLn(tlsCount);
  WriteLn(sslCount);

end.
