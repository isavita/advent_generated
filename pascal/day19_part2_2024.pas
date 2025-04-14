
program SolveDP;

{$mode objfpc}{$H+} // Use Object Pascal mode and enable AnsiStrings

uses
  SysUtils, // For file I/O, Int64, Trim, SetLength, Copy
  StrUtils; // For SplitString (if available and preferred, otherwise implement manually)

type
  TStringDynArray = array of string;
  TIntegerDynArray = array of Int64;

// Function to split a string by a delimiter (implementation if StrUtils.SplitString is not used)
(*
function SplitStringManual(const s, delimiter: string): TStringDynArray;
var
  pos, start: Integer;
  resultList: TStringDynArray;
  count: Integer;
begin
  SetLength(resultList, 0);
  count := 0;
  start := 1;
  pos := Pos(delimiter, s);
  while pos > 0 do
  begin
    Inc(count);
    SetLength(resultList, count);
    resultList[count - 1] := Copy(s, start, pos - start);
    start := pos + Length(delimiter);
    pos := Pos(delimiter, Copy(s, start, Length(s) - start + 1));
    if pos > 0 then pos := pos + start - 1; // Adjust position relative to original string
  end;
  // Add the last part
  Inc(count);
  SetLength(resultList, count);
  resultList[count - 1] := Copy(s, start, Length(s) - start + 1);
  Result := resultList;
end;
*)

function CountWays(const design: string; const patterns: TStringDynArray): Int64;
var
  n, i, lp, patIdx: Integer;
  dp: TIntegerDynArray;
  p: string;
begin
  n := Length(design);
  SetLength(dp, n + 1); // dp array from 0 to n

  // Initialize dp array
  for i := 0 to n do
    dp[i] := 0;
  dp[0] := 1; // Base case: one way to form an empty string

  for i := 1 to n do
  begin
    for patIdx := 0 to High(patterns) do
    begin
      p := patterns[patIdx];
      lp := Length(p);
      // Check if pattern p matches the end of the design substring ending at index i
      // Pascal's Copy is 1-based: Copy(string, startIndex, length)
      if (i >= lp) and (Copy(design, i - lp + 1, lp) = p) then
      begin
        dp[i] := dp[i] + dp[i - lp];
      end;
    end;
  end;

  Result := dp[n];
end;

var
  f: TextFile;
  line, design: string;
  availablePatternsStr: string;
  availablePatterns: TStringDynArray;
  totalWays: Int64;
  i: Integer;

begin
  AssignFile(f, 'input.txt');
  try
    Reset(f);

    // Read and process patterns line
    if not Eof(f) then
    begin
      ReadLn(f, availablePatternsStr);
      // Use StrUtils.SplitString for convenience if available
      availablePatterns := StrUtils.SplitString(availablePatternsStr, ',');
      // Trim whitespace from each pattern
      for i := 0 to High(availablePatterns) do
      begin
        availablePatterns[i] := Trim(availablePatterns[i]);
      end;
    end
    else
    begin
      WriteLn('Error: Input file empty or missing pattern line.');
      Halt(1);
    end;


    // Read and discard the empty line
    if not Eof(f) then
    begin
       ReadLn(f, line); // Discard this line
    end;


    totalWays := 0;
    // Process each design
    while not Eof(f) do
    begin
      ReadLn(f, design);
      design := Trim(design); // Trim whitespace from design string
      if Length(design) > 0 then // Process only non-empty designs
      begin
           totalWays := totalWays + CountWays(design, availablePatterns);
      end;

    end;

    WriteLn(totalWays);

  finally
    CloseFile(f);
  end;
end.
