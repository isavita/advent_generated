
program HistorianHysteria;

{$MODE DELPHI} // Necessary for using dynamic arrays

uses
  SysUtils;

type
  TIntegerArray = array of Integer;

function ReadNumbersFromFile(const FileName: string; var Numbers: TIntegerArray): Integer;
var
  F: TextFile;
  Line: string;
  Num1, Num2: Integer;
  Count: Integer;
begin
  Count := 0;
  SetLength(Numbers, 0); // Initialize the array

  try
    AssignFile(F, FileName);
    Reset(F);

    while not Eof(F) do
    begin
      ReadLn(F, Line);
      if (Length(Line) > 0) then
      begin
        if (TryStrToInt(Copy(Line, 1, Pos(' ', Line) - 1), Num1) and TryStrToInt(Copy(Line, Pos(' ', Line) + 1, MaxInt), Num2)) then
        begin
          SetLength(Numbers, Length(Numbers) + 2); // Allocate space for two numbers
          Numbers[Length(Numbers) - 2] := Num1;
          Numbers[Length(Numbers) - 1] := Num2;
          Inc(Count, 2);
        end;
      end;
    end;
    CloseFile(F);
    Result := Count;
  except
    on E: Exception do
    begin
      Writeln('Error reading file: ', E.Message);
      Result := -1;
    end;
  end;
end;

function CalculateTotalDistance(const LeftList, RightList: TIntegerArray; ListLength: Integer): Integer;
var
  TotalDistance, i, j, temp: Integer;
begin
  TotalDistance := 0;

  // Sort the lists
  for i := 0 to ListLength - 1 do
    for j := i + 1 to ListLength - 1 do
    begin
      if LeftList[i] > LeftList[j] then
      begin
        temp := LeftList[i];
        LeftList[i] := LeftList[j];
        LeftList[j] := temp;
      end;
      if RightList[i] > RightList[j] then
      begin
        temp := RightList[i];
        RightList[i] := RightList[j];
        RightList[j] := temp;
      end;
    end;

  for i := 0 to ListLength - 1 do
  begin
    TotalDistance := TotalDistance + Abs(LeftList[i] - RightList[i]);
  end;

  Result := TotalDistance;
end;

var
  AllNumbers: TIntegerArray;
  LeftList, RightList: TIntegerArray;
  Count, i, ListLength, TotalDistance: Integer;

begin
  // Read numbers from the file
  Count := ReadNumbersFromFile('input.txt', AllNumbers);

  if Count > 0 then
  begin
    ListLength := Count div 2;  // Each line has two numbers

    // Populate left and right lists
    SetLength(LeftList, ListLength);
    SetLength(RightList, ListLength);

    for i := 0 to ListLength - 1 do
    begin
      LeftList[i] := AllNumbers[i * 2];
      RightList[i] := AllNumbers[i * 2 + 1];
    end;

    // Calculate and print the total distance
    TotalDistance := CalculateTotalDistance(LeftList, RightList, ListLength);
    Writeln(TotalDistance);
  end
  else
  begin
    Writeln('No numbers read from file.');
  end;
end.
