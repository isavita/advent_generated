
program GrovePositioningSystem;
{$mode objfpc}{$H+}
uses
  SysUtils;

type
  TInt64Array = array of Int64;
  TIntArray   = array of Integer;

// Mixes the list “idx” in‐place.  Each element 0..N–1 is moved in
// the order of its original index.  “val[]” holds the values,
// “N” is the list length, “rounds” the number of full passes.
procedure Mix(const val: TInt64Array; var idx: TIntArray; N, rounds: Integer);
var
  r, i, j, k: Integer;
  offset, target: Int64;
  tmp: Integer;
  modBase: Int64;
begin
  if N < 2 then Exit;
  modBase := N - 1;  // we move each element mod (N–1)
  for r := 1 to rounds do
    for i := 0 to N - 1 do
    begin
      // find current position of original item i
      j := 0;
      while (j < N) and (idx[j] <> i) do
        Inc(j);

      // compute forward shift in range 0..N–2
      offset := val[i] mod modBase;
      if offset < 0 then
        offset := offset + modBase;

      target := (j + offset) mod modBase;
      // if target=j do nothing
      if target > j then
      begin
        // shift left the block [j+1..target]
        tmp := idx[j];
        for k := j to target - 1 do
          idx[k] := idx[k + 1];
        idx[target] := tmp;
      end
      else if target < j then
      begin
        // shift right the block [target..j-1]
        tmp := idx[j];
        for k := j downto target + 1 do
          idx[k] := idx[k - 1];
        idx[target] := tmp;
      end;
    end;
end;

// After mixing, find the index of value zero and sum the values
// 1000th, 2000th, 3000th forward (wrapping).
function GroveSum(const val: TInt64Array; const idx: TIntArray): Int64;
var
  N, zeropos, i: Integer;
  sum: Int64;
  pos: Integer;
begin
  N := Length(idx);
  // locate zero
  zeropos := 0;
  while (zeropos < N) and (val[idx[zeropos]] <> 0) do
    Inc(zeropos);

  sum := 0;
  for i := 1 to 3 do
  begin
    pos := (zeropos + i * 1000) mod N;
    sum := sum + val[idx[pos]];
  end;
  Result := sum;
end;

var
  origVals, workVals: TInt64Array;
  idx: TIntArray;
  inFile: TextFile;
  line: string;
  i, N: Integer;
  part1, part2: Int64;
  DECRYPT_KEY: Int64 = 811589153;
begin
  // 1) Read input.txt
  AssignFile(inFile, 'input.txt');
  Reset(inFile);
  SetLength(origVals, 0);
  while not Eof(inFile) do
  begin
    ReadLn(inFile, line);
    if line <> '' then
    begin
      N := Length(origVals);
      SetLength(origVals, N + 1);
      origVals[N] := StrToInt64(line);
    end;
  end;
  CloseFile(inFile);

  N := Length(origVals);
  if N = 0 then
  begin
    WriteLn('No data found in input.txt');
    Halt(1);
  end;

  // Prepare index array 0..N-1
  SetLength(idx, N);
  for i := 0 to N - 1 do
    idx[i] := i;

  // Part 1: mix once, exact values
  workVals := Copy(origVals);
  Mix(workVals, idx, N, 1);
  part1 := GroveSum(workVals, idx);

  // Part 2: multiply by key, mix 10 times
  for i := 0 to N - 1 do
    workVals[i] := origVals[i] * DECRYPT_KEY;

  // reset index order
  for i := 0 to N - 1 do
    idx[i] := i;

  Mix(workVals, idx, N, 10);
  part2 := GroveSum(workVals, idx);

  // Output
  WriteLn('Part 1: ', part1);
  WriteLn('Part 2: ', part2);
end.
