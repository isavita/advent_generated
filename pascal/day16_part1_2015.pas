
program AuntSueFinder;

uses
  SysUtils; // Used for Trim, possibly other string functions depending on Pascal version

const
  InputFileName = 'input.txt';

type
  // Record to hold the properties of an Aunt or the target properties
  // Using -1 to indicate an unknown/unspecified value for a specific Aunt
  AuntProperties = record
    children, cats, samoyeds, pomeranians, akitas,
    vizslas, goldfish, trees, cars, perfumes: Integer;
  end;

var
  inputFile: Text;
  line: string;
  target: AuntProperties;
  currentAunt: AuntProperties;
  auntNumber: Integer;
  i, p, valCode: Integer;
  propertyName, propertyValueStr, propertyPair, remainingProperties: string;
  isMatch: Boolean;

// Helper procedure to initialize an AuntProperties record to all -1 (unknown)
procedure InitializeAunt(var aunt: AuntProperties);
begin
  aunt.children := -1;
  aunt.cats := -1;
  aunt.samoyeds := -1;
  aunt.pomeranians := -1;
  aunt.akitas := -1;
  aunt.vizslas := -1;
  aunt.goldfish := -1;
  aunt.trees := -1;
  aunt.cars := -1;
  aunt.perfumes := -1;
end;

// Helper procedure to parse a property pair like "cats: 7" and update the record
procedure ParseAndUpdateProperty(const pair: string; var aunt: AuntProperties);
var
  colonPos: Integer;
  name: string;
  valueStr: string;
  value, code: Integer;
begin
  colonPos := Pos(':', pair);
  if colonPos > 0 then
  begin
    name := Trim(Copy(pair, 1, colonPos - 1));
    valueStr := Trim(Copy(pair, colonPos + 1, Length(pair) - colonPos));

    Val(valueStr, value, code); // Convert value string to integer

    if code = 0 then // Check if conversion was successful
    begin
      // Update the corresponding field based on the property name
      if name = 'children' then aunt.children := value
      else if name = 'cats' then aunt.cats := value
      else if name = 'samoyeds' then aunt.samoyeds := value
      else if name = 'pomeranians' then aunt.pomeranians := value
      else if name = 'akitas' then aunt.akitas := value
      else if name = 'vizslas' then aunt.vizslas := value
      else if name = 'goldfish' then aunt.goldfish := value
      else if name = 'trees' then aunt.trees := value
      else if name = 'cars' then aunt.cars := value
      else if name = 'perfumes' then aunt.perfumes := value;
      // Ignore unknown properties if any appear in the input
    end;
  end;
end;

// --- Main Program ---
begin
  // Define the target properties from the MFCSAM reading
  target.children := 3;
  target.cats := 7;
  target.samoyeds := 2;
  target.pomeranians := 3;
  target.akitas := 0;
  target.vizslas := 0;
  target.goldfish := 5;
  target.trees := 3;
  target.cars := 2;
  target.perfumes := 1;

  // Assign and open the input file
  Assign(inputFile, InputFileName);
  {$I-} // Disable IO checking
  Reset(inputFile);
  {$I+} // Re-enable IO checking
  if IOResult <> 0 then
  begin
    WriteLn('Error: Cannot open input file ', InputFileName);
    Halt(1); // Exit with an error code
  end;

  // Read the file line by line
  while not Eof(inputFile) do
  begin
    ReadLn(inputFile, line);
    line := Trim(line); // Remove leading/trailing whitespace

    // --- Parse the line ---
    // Example: "Sue 1: cars: 9, akitas: 3, goldfish: 0"

    // Find the first colon to separate "Sue X" from properties
    p := Pos(':', line);
    if p = 0 then Continue; // Skip malformed lines

    // Extract Aunt Number (assuming format "Sue X:")
    // Find the space before the number
    i := Pos(' ', Copy(line, 1, p -1));
    Val(Trim(Copy(line, i + 1, p - i - 1)), auntNumber, valCode);
    if valCode <> 0 then Continue; // Skip if number parsing failed

    // Extract the properties string part
    remainingProperties := Trim(Copy(line, p + 1, Length(line) - p));

    // Initialize the current Aunt's properties to unknown (-1)
    InitializeAunt(currentAunt);

    // Parse each property pair (e.g., "cars: 9", "akitas: 3")
    while Length(remainingProperties) > 0 do
    begin
      p := Pos(',', remainingProperties);
      if p > 0 then
      begin
        // Extract the pair before the comma
        propertyPair := Trim(Copy(remainingProperties, 1, p - 1));
        // Remove the processed pair and the comma+space from the string
        Delete(remainingProperties, 1, p + 1); // Assumes ", " separator
        remainingProperties := Trim(remainingProperties);
      end
      else
      begin
        // Last property pair in the line
        propertyPair := remainingProperties;
        remainingProperties := ''; // Clear the string
      end;

      // Parse the extracted pair and update the currentAunt record
      ParseAndUpdateProperty(propertyPair, currentAunt);
    end;

    // --- Compare current Aunt with target ---
    isMatch := True; // Assume match initially

    // Check each property. If the current Aunt has a value specified for a property,
    // it MUST match the target value. If it's -1 (unknown), it doesn't disqualify.
    if (currentAunt.children <> -1) and (currentAunt.children <> target.children) then isMatch := False;
    if isMatch and (currentAunt.cats <> -1) and (currentAunt.cats <> target.cats) then isMatch := False;
    if isMatch and (currentAunt.samoyeds <> -1) and (currentAunt.samoyeds <> target.samoyeds) then isMatch := False;
    if isMatch and (currentAunt.pomeranians <> -1) and (currentAunt.pomeranians <> target.pomeranians) then isMatch := False;
    if isMatch and (currentAunt.akitas <> -1) and (currentAunt.akitas <> target.akitas) then isMatch := False;
    if isMatch and (currentAunt.vizslas <> -1) and (currentAunt.vizslas <> target.vizslas) then isMatch := False;
    if isMatch and (currentAunt.goldfish <> -1) and (currentAunt.goldfish <> target.goldfish) then isMatch := False;
    if isMatch and (currentAunt.trees <> -1) and (currentAunt.trees <> target.trees) then isMatch := False;
    if isMatch and (currentAunt.cars <> -1) and (currentAunt.cars <> target.cars) then isMatch := False;
    if isMatch and (currentAunt.perfumes <> -1) and (currentAunt.perfumes <> target.perfumes) then isMatch := False;

    // If all specified properties matched the target
    if isMatch then
    begin
      WriteLn(auntNumber); // Print the matching Aunt's number
      Break; // Found the match, exit the loop (Optimization)
    end;
  end;

  // Close the input file
  Close(inputFile);

end. // End of program
