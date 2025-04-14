
program StoneSimulation;

{$MODE DELPHI} // Use Delphi compatibility mode for AnsiString and Generics
{$OPTIMIZATION ON}

uses
  SysUtils, Classes, Generics.Collections;

// Function to trim leading zeros from a string representation of a number.
// Returns "0" if the input is empty or all zeros.
function TrimLeadingZeros(const s: AnsiString): AnsiString;
var
  i, L: Integer;
begin
  L := Length(s);
  if L = 0 then
  begin
    Result := '0';
    Exit;
  end;

  i := 1;
  while (i < L) and (s[i] = '0') do
  begin
    Inc(i);
  end;

  // If the only remaining character is '0', or if we went past the end (all zeros)
  if (i = L) and (s[i] = '0') then
    Result := '0'
  else
    Result := Copy(s, i, L - i + 1);
end;

// Splits a stone string s (must have even length) into two halves,
// trimming leading zeros from each.
procedure SplitStone(const s: AnsiString; out left, right: AnsiString);
var
  mid: Integer;
  leftTmp, rightTmp: AnsiString;
begin
  mid := Length(s) div 2;
  leftTmp := Copy(s, 1, mid);
  rightTmp := Copy(s, mid + 1, Length(s) - mid);

  left := TrimLeadingZeros(leftTmp);
  right := TrimLeadingZeros(rightTmp);
end;

// Multiplies the number represented by string s by 2024.
function MultiplyBy2024(const s: AnsiString): AnsiString;
var
  num, multiplier, resultArr: TArray<Byte>;
  i, j, nLen, mLen, resLen, product, carry, start: Integer;
  sb: TStringBuilder;
begin
  nLen := Length(s);
  if (nLen = 0) or (s = '0') then
  begin
    Result := '0'; // Multiplying 0 by anything is 0
    Exit;
  end;

  // Convert input string to array of digits (bytes 0-9)
  SetLength(num, nLen);
  for i := 1 to nLen do
  begin
    num[i - 1] := Byte(Ord(s[i]) - Ord('0'));
  end;

  // Multiplier 2024
  multiplier := [2, 0, 2, 4];
  mLen := Length(multiplier);

  // Result array size: length of num + length of multiplier
  resLen := nLen + mLen;
  SetLength(resultArr, resLen);
  // Initialize result array to zeros (already default for dynamic arrays)

  // Perform multiplication (similar to Python logic)
  for i := nLen - 1 downto 0 do
  begin
    carry := 0;
    for j := mLen - 1 downto 0 do
    begin
      // Calculate product: num[i] * multiplier[j] + previous result digit + carry
      product := num[i] * multiplier[j] + resultArr[i + j + 1] + carry;
      // Store the last digit of the product
      resultArr[i + j + 1] := product mod 10;
      // Calculate the carry for the next step
      carry := product div 10;
    end;
    // Add remaining carry to the left
    resultArr[i] := resultArr[i] + carry; // Add carry to the position left of the current inner loop scope
  end;

  // Find the start index (skip leading zeros in the result)
  start := 0;
  while (start < resLen - 1) and (resultArr[start] = 0) do
  begin
    Inc(start);
  end;

  // Convert result array back to string
  sb := TStringBuilder.Create;
  try
    for i := start to resLen - 1 do
    begin
      sb.Append(Chr(Ord('0') + resultArr[i]));
    end;
    Result := sb.ToString;
  finally
    sb.Free;
  end;

  if Result = '' then Result := '0'; // Should not happen if input s was not '0', but safety check.

end;

// Main simulation procedure
procedure Solve;
var
  F: TextFile;
  line, word: AnsiString;
  stonesMap, newStonesMap: TDictionary<AnsiString, Int64>;
  words: TStringList;
  i, steps: Integer;
  stone: AnsiString;
  count, currentCount, totalStones: Int64;
  kvp: TPair<AnsiString, Int64>; // For iterating dictionary
  left, right: AnsiString;
  newStone: AnsiString;
begin
  // Input reading
  Assign(F, 'input.txt');
  {$I-} // Disable IO checking
  Reset(F);
  {$I+} // Re-enable IO checking
  if IOResult <> 0 then
  begin
    WriteLn('Error: input.txt not found.');
    Halt(1);
  end;

  ReadLn(F, line);
  Close(F);

  // Initialize stones map
  stonesMap := TDictionary<AnsiString, Int64>.Create;
  words := TStringList.Create;
  try
    words.Delimiter := ' ';
    words.StrictDelimiter := True; // Handle multiple spaces correctly
    words.DelimitedText := line;

    for word in words do
    begin
      if word <> '' then // Ignore empty strings potentially created by split
      begin
         stone := TrimLeadingZeros(word); // Ensure canonical form
         if stonesMap.TryGetValue(stone, currentCount) then
            stonesMap.AddOrSetValue(stone, currentCount + 1)
         else
            stonesMap.Add(stone, 1);
      end;
    end;
  finally
    words.Free;
  end;

  // Simulation steps
  steps := 75;
  for i := 1 to steps do
  begin
    newStonesMap := TDictionary<AnsiString, Int64>.Create;
    try
        // Iterate using KeyValuePair to avoid issues with modifying during iteration (although we modify the *new* map)
      for kvp in stonesMap do
      begin
        stone := kvp.Key;
        count := kvp.Value;

        if stone = '0' then
        begin
          if newStonesMap.TryGetValue('1', currentCount) then
            newStonesMap.AddOrSetValue('1', currentCount + count)
          else
            newStonesMap.Add('1', count);
        end
        else if Length(stone) mod 2 = 0 then
        begin
          SplitStone(stone, left, right);

          // Add left stone
          if newStonesMap.TryGetValue(left, currentCount) then
            newStonesMap.AddOrSetValue(left, currentCount + count)
          else
            newStonesMap.Add(left, count);

          // Add right stone
          if newStonesMap.TryGetValue(right, currentCount) then
            newStonesMap.AddOrSetValue(right, currentCount + count)
          else
            newStonesMap.Add(right, count);
        end
        else // Odd length
        begin
          newStone := MultiplyBy2024(stone);
          if newStonesMap.TryGetValue(newStone, currentCount) then
            newStonesMap.AddOrSetValue(newStone, currentCount + count)
          else
            newStonesMap.Add(newStone, count);
        end;
      end; // end for kvp in stonesMap

      // Swap maps for the next iteration
      stonesMap.Free; // Free the old map
      stonesMap := newStonesMap; // Assign the new map
      newStonesMap := nil; // Avoid freeing it in the finally block if assigned

    finally
        // If an error occurred mid-loop, newStonesMap might still need freeing
      if Assigned(newStonesMap) then
           newStonesMap.Free;
    end;
  end; // end for i := 1 to steps

  // Calculate total stones
  totalStones := 0;
  for kvp in stonesMap do
  begin
    totalStones := totalStones + kvp.Value;
  end;

  WriteLn(totalStones);

  // Cleanup final map
  stonesMap.Free;
end;

// Main program entry point
begin
  Solve;
end.
