
program Lanternfish;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  TFishCounts = array[0..8] of Int64;

function Solve(const fishAges: array of Integer; days: Integer): Int64;
var
  fishCounts: TFishCounts;
  day: Integer;
  newFish: Int64;
  age: Integer;
begin
  // Initialize the fish counts array
  FillChar(fishCounts, SizeOf(fishCounts), 0);
  for age in fishAges do
  begin
    Inc(fishCounts[age]);
  end;

  // Simulate the fish growth over the given number of days
  for day := 1 to days do
  begin
    newFish := fishCounts[0];
    for age := 0 to 7 do
    begin
      fishCounts[age] := fishCounts[age + 1];
    end;
    fishCounts[6] := fishCounts[6] + newFish;
    fishCounts[8] := newFish;
  end;

  // Calculate the total number of fish
  Result := 0;
  for age := 0 to 8 do
  begin
    Result := Result + fishCounts[age];
  end;
end;

var
  inputFile: TextFile;
  line: string;
  ages: array of Integer;
  ageStr: string;
  i: Integer;
  totalFish: Int64;
begin
  // Read the input from the file
  AssignFile(inputFile, 'input.txt');
  try
    Reset(inputFile);
    try
      if not Eof(inputFile) then
      begin
        ReadLn(inputFile, line);

        // Parse the ages from the input line
        SetLength(ages, 0);
        i := 1;
        while i <= Length(line) do
        begin
          if line[i] = ',' then
            Inc(i)
          else
          begin
            ageStr := '';
            while (i <= Length(line)) and (line[i] <> ',') do
            begin
              ageStr := ageStr + line[i];
              Inc(i);
            end;
            SetLength(ages, Length(ages) + 1);
            ages[Length(ages) - 1] := StrToInt(ageStr);
          end;
        end;


        // Solve the problem for 80 days
        totalFish := Solve(ages, 80);
        WriteLn('Number of lanternfish after 80 days: ', totalFish);

        // Solve the problem for 256 days
        totalFish := Solve(ages, 256);
        WriteLn('Number of lanternfish after 256 days: ', totalFish);
      end
      else
      begin
        WriteLn('Input file is empty.');
      end;
    finally
      CloseFile(inputFile);
    end;
  except
    on E: Exception do
      WriteLn('An error occurred: ', E.Message);
  end;

end.
