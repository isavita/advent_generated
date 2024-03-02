program ChronalCalibrationPartTwo;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

var
  inputFile: TextFile;
  frequencyChanges: TStringList;
  frequenciesSeen: TStringList;
  currentFrequency, i: Integer;
  change: string;
  foundDuplicate: Boolean;

begin
  currentFrequency := 0;
  foundDuplicate := False;
  frequencyChanges := TStringList.Create;
  frequenciesSeen := TStringList.Create;
  frequenciesSeen.Sorted := True; // Improves the search performance
  frequenciesSeen.Duplicates := dupIgnore; // Ignore duplicates

  // Load the frequency changes from the input file
  AssignFile(inputFile, 'input.txt');
  Reset(inputFile);
  while not EOF(inputFile) do
  begin
    ReadLn(inputFile, change);
    frequencyChanges.Add(change);
  end;
  CloseFile(inputFile);

  // Keep applying changes until a duplicate frequency is found
  while not foundDuplicate do
  begin
    for i := 0 to frequencyChanges.Count - 1 do
    begin
      currentFrequency := currentFrequency + StrToInt(frequencyChanges[i]);
      if frequenciesSeen.IndexOf(IntToStr(currentFrequency)) >= 0 then
      begin
        foundDuplicate := True;
        Break;
      end
      else
        frequenciesSeen.Add(IntToStr(currentFrequency));
    end;
  end;

  // Output the first frequency reached twice
  WriteLn('The first frequency reached twice is: ', currentFrequency);

  frequencyChanges.Free;
  frequenciesSeen.Free;
end.
