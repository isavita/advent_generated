
program MedicineForRudolph;

{$mode objfpc}{$H+} // Use Object Pascal mode and long strings

uses
  SysUtils, // Provides Trim, Copy, Pos, Length, Val, Str, etc.
  Classes;  // Provides TStringList

type
  // Record to store a single replacement rule
  TReplacement = record
    Source: string;
    Target: string;
  end;

  // Dynamic array to store all replacement rules
  TReplacementArray = array of TReplacement;

// --- Part 1 Helper Functions ---

// Function to check if a string exists in a dynamic array of strings
function IsInArray(const s: string; const arr: array of string): boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Low(arr) to High(arr) do
  begin
    if arr[i] = s then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

// --- Part 1 Logic ---
procedure SolvePart1(const Replacements: TReplacementArray; const Molecule: string);
var
  GeneratedMolecules: array of string; // Dynamic array for distinct molecules
  Rule: TReplacement;
  i, CurrentPos, FoundPos: Integer;
  NewMolecule, Prefix, Suffix: string;
begin
  SetLength(GeneratedMolecules, 0); // Initialize empty array

  // Iterate through each replacement rule
  for Rule in Replacements do
  begin
    CurrentPos := 1; // Start searching from the beginning of the molecule
    while CurrentPos <= Length(Molecule) do
    begin
      // Find the next occurrence of the source string
      FoundPos := Pos(Rule.Source, Copy(Molecule, CurrentPos, Length(Molecule) - CurrentPos + 1));

      if FoundPos > 0 then
      begin
        // Calculate the actual position in the original string
        FoundPos := CurrentPos + FoundPos - 1;

        // Construct the new molecule
        Prefix := Copy(Molecule, 1, FoundPos - 1);
        Suffix := Copy(Molecule, FoundPos + Length(Rule.Source), Length(Molecule));
        NewMolecule := Prefix + Rule.Target + Suffix;

        // Add to the list if it's a new distinct molecule
        if not IsInArray(NewMolecule, GeneratedMolecules) then
        begin
          SetLength(GeneratedMolecules, Length(GeneratedMolecules) + 1);
          GeneratedMolecules[High(GeneratedMolecules)] := NewMolecule;
        end;

        // Move search position past the current match
        CurrentPos := FoundPos + 1;
      end
      else
      begin
        // No more occurrences found for this rule, break inner loop
        Break;
      end;
    end;
  end;

  // Print the result for Part 1
  Writeln('Part 1: ', Length(GeneratedMolecules));
end;

// --- Part 2 Logic ---
procedure SolvePart2(const Molecule: string);
var
  TotalSymbols, NumRnAr, NumY: Integer;
  i: Integer;
  Char1, Char2: Char;
  ElementLength : integer;
begin
  TotalSymbols := 0;
  NumRnAr := 0;
  NumY := 0;
  i := 1;

  // Parse the molecule into symbols/elements
  // Symbols are either single uppercase letters or an uppercase followed by a lowercase
  while i <= Length(Molecule) do
  begin
    Inc(TotalSymbols); // Count each symbol
    Char1 := Molecule[i];
    ElementLength := 1; // Assume 1 char symbol initially

    // Check for a two-character symbol (Uppercase followed by Lowercase)
    if (i < Length(Molecule)) then
    begin
        Char2 := Molecule[i+1];
        if (Char1 >= 'A') and (Char1 <= 'Z') and (Char2 >= 'a') and (Char2 <= 'z') then
        begin
            ElementLength := 2;
            // Check for specific symbols relevant to the formula
            if (Char1 = 'R') and (Char2 = 'n') then Inc(NumRnAr);
            if (Char1 = 'A') and (Char2 = 'r') then Inc(NumRnAr);
            // Note: 'Y' is a single character element in the problem's context
        end;
    end;

    // Check for single character 'Y'
    if (ElementLength = 1) and (Char1 = 'Y') then
    begin
      Inc(NumY);
    end;

    // Advance the index by the length of the symbol found
    Inc(i, ElementLength);
  end;

  // Apply the derived formula: TotalSymbols - NumRnAr - 2 * NumY - 1
  // The formula counts steps from 'e' to the molecule.
  // TotalSymbols: Each symbol requires roughly one step.
  // - NumRnAr: Rn and Ar are generated together in one step, saving NumRnAr/2 * 2 = NumRnAr steps compared to individual creation.
  // - 2 * NumY: Each Y is introduced within an Rn/Ar structure, adding complexity but effectively replacing one basic step with a more complex one, saving 2 steps relative to creating the components separately.
  // - 1: Accounts for the initial 'e' state not being counted as a symbol.
  Writeln('Part 2: ', TotalSymbols - NumRnAr - 2 * NumY - 1);
end;

// --- Main Program ---
var
  InputFile: TextFile;
  Line: string;
  SeparatorPos: Integer;
  Replacements: TReplacementArray;
  Molecule: string;
  CurrentReplacement: TReplacement;

begin
  // --- Input Reading ---
  AssignFile(InputFile, 'input.txt');
  try
    Reset(InputFile);

    SetLength(Replacements, 0); // Initialize dynamic array
    Molecule := '';

    // Read replacement rules
    while not Eof(InputFile) do
    begin
      ReadLn(InputFile, Line);
      Line := Trim(Line); // Remove leading/trailing whitespace

      if Line = '' then Break; // Stop reading rules at the blank line

      SeparatorPos := Pos(' => ', Line);
      if SeparatorPos > 0 then
      begin
        SetLength(Replacements, Length(Replacements) + 1);
        CurrentReplacement.Source := Trim(Copy(Line, 1, SeparatorPos - 1));
        CurrentReplacement.Target := Trim(Copy(Line, SeparatorPos + 4, Length(Line)));
        Replacements[High(Replacements)] := CurrentReplacement;
      end
      else
      begin
          // Handle potential error or unexpected format if needed
          Writeln('Warning: Invalid rule format skipped: ', Line);
      end;
    end;

    // Read the starting molecule (should be the next line after the blank)
    if not Eof(InputFile) then
    begin
        ReadLn(InputFile, Molecule);
        Molecule := Trim(Molecule);
    end
    else
    begin
        Writeln('Error: Could not read molecule from input file.');
        Halt(1); // Exit with error code
    end;

  finally
    CloseFile(InputFile);
  end;

  // --- Solve and Print ---
  if (Length(Replacements) > 0) and (Molecule <> '') then
  begin
    SolvePart1(Replacements, Molecule);
    SolvePart2(Molecule);
  end
  else
  begin
    Writeln('Error: No replacements rules or molecule read from input.');
    Halt(1);
  end;

// Wait for user input before closing console window (optional)
//  ReadLn;
end.
