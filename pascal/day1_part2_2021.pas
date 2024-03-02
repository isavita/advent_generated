program SonarSweepPartTwo;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

var
  inputFile: TextFile;
  depthReadings, line: string;
  readings: TStringList;
  i, currentSum, previousSum, increaseCount: Integer;

begin
  readings := TStringList.Create;
  increaseCount := 0;
  previousSum := -1; // Initialize to -1 to indicate the first sum has no predecessor

  // Load the depth measurements from the input file
  AssignFile(inputFile, 'input.txt');
  Reset(inputFile);
  while not EOF(inputFile) do
  begin
    ReadLn(inputFile, line);
    readings.Add(line);
  end;
  CloseFile(inputFile);

  // Calculate the sum of measurements within the sliding window and count increases
  for i := 0 to readings.Count - 3 do // Subtract 3 to prevent out-of-bounds access
  begin
    currentSum := StrToInt(readings[i]) +
                  StrToInt(readings[i+1]) +
                  StrToInt(readings[i+2]);

    if (previousSum <> -1) and (currentSum > previousSum) then
      Inc(increaseCount);

    previousSum := currentSum;
  end;

  readings.Free;

  // Output the number of times the sum of measurements in the sliding window increases
  WriteLn('Number of sums larger than the previous sum: ', increaseCount);
end.
