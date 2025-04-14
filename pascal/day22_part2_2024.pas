
program SolveFinancePattern;

{$mode objfpc}{$H+} // Use modern Free Pascal features like Int64 if needed

uses
  SysUtils; // For StrToInt, potentially File Handling enhancements

const
  NumSteps = 2000;
  ModValue = 1 shl 24; // 16777216
  PatternCount = 130321; // 19 * 19 * 19 * 19
  P19_1 = 19;
  P19_2 = 361;   // 19*19
  P19_3 = 6859;  // 19*19*19

type
  TGlobalSumArray = array[0..PatternCount-1] of Int64;
  TLocalPriceArray = array[0..PatternCount-1] of Integer; // Store price (0-9) or -1
  TPricesArray = array[0..NumSteps] of Integer;          // Prices 0-9
  TChangesArray = array[0..NumSteps-1] of Integer;       // Changes -9 to 9

var
  globalSum: TGlobalSumArray;
  inputFile: Text;
  initialValueStr: string;
  initialValue: Integer; // Assuming initial value fits in standard Integer
  i, idx: Integer;
  maxSum: Int64;
  s: Cardinal; // Use unsigned 32-bit, stays below 2^24
  j: Integer;
  c1, c2, c3, c4: Integer;
  prices: TPricesArray;
  changes: TChangesArray;
  localPrice: TLocalPriceArray;


function NextSecret(start_s: Cardinal): Cardinal;
var
  x: Int64; // Use 64-bit for intermediate calculations to avoid overflow
  s_int64: Int64;
begin
  s_int64 := start_s;

  x := s_int64 * 64;
  s_int64 := s_int64 xor x;
  s_int64 := s_int64 and (ModValue - 1);

  x := s_int64 div 32;
  s_int64 := s_int64 xor x;
  s_int64 := s_int64 and (ModValue - 1);

  x := s_int64 * 2048;
  s_int64 := s_int64 xor x;
  s_int64 := s_int64 and (ModValue - 1);

  Result := Cardinal(s_int64);
end;

function EncodeChange4(c1, c2, c3, c4: Integer): Integer;
begin
  Result := (c1 + 9) + (c2 + 9) * P19_1 + (c3 + 9) * P19_2 + (c4 + 9) * P19_3;
end;

begin
  // Initialize globalSum
  for i := 0 to PatternCount - 1 do
    globalSum[i] := 0;

  AssignFile(inputFile, 'input.txt');
  try
    Reset(inputFile);

    // Process each buyer one by one directly
    while not Eof(inputFile) do
    begin
      ReadLn(inputFile, initialValueStr);
      initialValueStr := Trim(initialValueStr); // Handle potential whitespace
      if length(initialValueStr) = 0 then continue; // Skip empty lines

      initialValue := StrToInt(initialValueStr);

      // Generate data for this buyer
      s := Cardinal(initialValue);
      for j := 0 to NumSteps do
      begin
        prices[j] := s mod 10;
        s := NextSecret(s);
      end;

      for j := 0 to NumSteps - 1 do
        changes[j] := prices[j+1] - prices[j];

      // Initialize localPrice for this buyer
      for j := 0 to PatternCount - 1 do
        localPrice[j] := -1;

      // Calculate contributions for this buyer
      for j := 0 to NumSteps - 4 do // Iterate through all possible 4-change windows
      begin
        c1 := changes[j];
        c2 := changes[j+1];
        c3 := changes[j+2];
        c4 := changes[j+3];

        // Check range: -9 <= c <= 9
        if (c1 >= -9) and (c1 <= 9) and
           (c2 >= -9) and (c2 <= 9) and
           (c3 >= -9) and (c3 <= 9) and
           (c4 >= -9) and (c4 <= 9) then
        begin
          idx := EncodeChange4(c1, c2, c3, c4);
          if localPrice[idx] < 0 then // First time seeing this pattern for this buyer
            localPrice[idx] := prices[j + 4]; // Store the price following the pattern
        end;
      end;

      // Add this buyer's contributions to globalSum
      for j := 0 to PatternCount - 1 do
      begin
        if localPrice[j] >= 0 then // If this pattern occurred for the buyer
          globalSum[j] := globalSum[j] + localPrice[j];
      end;
    end;
  finally
    CloseFile(inputFile);
  end;

  // Find maximum in globalSum
  maxSum := 0; // Sums cannot be negative as prices are 0-9
  if PatternCount > 0 then
     maxSum := globalSum[0]; // Initialize with the first element

  for i := 1 to PatternCount - 1 do
  begin
    if globalSum[i] > maxSum then
      maxSum := globalSum[i];
  end;

  // Print result
  WriteLn(maxSum);

end.
