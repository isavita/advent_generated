
program MoleculeReplacements;

{$mode objfpc}{$H+} // Use Free Pascal mode with AnsiStrings

uses
  SysUtils, // Provides string functions like Trim, Copy, Pos, Length
  Classes;  // Provides TStringList

const
  InputFileName = 'input.txt';

type
  TReplacementRule = record
    Key: string;
    Value: string;
  end;

var
  f: TextFile;
  line: string;
  medicine: string;
  rules: array of TReplacementRule; // Dynamic array for rules
  ruleCount: Integer;
  posSeparator: Integer;
  keyStr, valueStr: string;
  distinctMolecules: TStringList;
  i, j, k: Integer;
  newMolecule: string;

begin
  // --- Read Input ---
  AssignFile(f, InputFileName);
  try
    Reset(f);

    ruleCount := 0;
    SetLength(rules, 0); // Initialize dynamic array
    medicine := '';

    while not Eof(f) do
    begin
      ReadLn(f, line);
      line := Trim(line); // Remove leading/trailing whitespace

      posSeparator := Pos(' => ', line);
      if posSeparator > 0 then
      begin
        // This line is a replacement rule
        keyStr := Copy(line, 1, posSeparator - 1);
        valueStr := Copy(line, posSeparator + 4, Length(line) - (posSeparator + 3));

        // Grow the dynamic array and add the rule
        ruleCount := Length(rules);
        SetLength(rules, ruleCount + 1);
        rules[ruleCount].Key := keyStr;
        rules[ruleCount].Value := valueStr;
      end
      else if Length(line) > 0 then
      begin
        // This line is the medicine molecule (assumes only one after rules)
        medicine := line;
      end;
    end;
  finally
    CloseFile(f);
  end;

  // --- Generate and Count Distinct Molecules ---
  distinctMolecules := TStringList.Create;
  try
    distinctMolecules.Sorted := True;      // Keep the list sorted
    distinctMolecules.Duplicates := dupIgnore; // Automatically ignore duplicates on Add

    // Iterate through each rule
    for i := 0 to High(rules) do
    begin
      keyStr := rules[i].Key;
      valueStr := rules[i].Value;
      k := Length(keyStr); // Length of the key to replace

      // Iterate through possible starting positions in the medicine string
      // Pascal strings are 1-indexed
      for j := 1 to Length(medicine) - k + 1 do
      begin
        // Check if the substring matches the rule's key
        if Copy(medicine, j, k) = keyStr then
        begin
          // Construct the new molecule
          // Part before match + replacement value + part after match
          newMolecule := Copy(medicine, 1, j - 1) + valueStr + Copy(medicine, j + k, Length(medicine) - (j + k) + 1);

          // Add the new molecule to the list. Duplicates are ignored due to settings.
          distinctMolecules.Add(newMolecule);
        end;
      end;
    end;

    // --- Print Result ---
    WriteLn(distinctMolecules.Count);

  finally
    distinctMolecules.Free; // Clean up the TStringList
  end;

end.
