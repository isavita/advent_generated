program AdapterArray;
{$mode objfpc}{$H+}
uses
  SysUtils;

type
  TIntArr = array of Integer;
  TUInt64Arr = array of QWord;

procedure QuickSort(var A: TIntArr; L, R: Integer);
var
  i, j, pivot, tmp: Integer;
begin
  i := L; j := R;
  pivot := A[(L + R) shr 1];
  repeat
    while A[i] < pivot do Inc(i);
    while A[j] > pivot do Dec(j);
    if i <= j then
    begin
      tmp := A[i]; A[i] := A[j]; A[j] := tmp;
      Inc(i); Dec(j);
    end;
  until i > j;
  if L < j then QuickSort(A, L, j);
  if i < R then QuickSort(A, i, R);
end;

var
  f: TextFile;
  line: string;
  raw: TIntArr;
  adapters: TIntArr;
  ways: TUInt64Arr;
  i, n, d: Integer;
  ones, threes: Integer;
  part1: QWord;
  part2: QWord;
begin
  { Read input from input.txt }
  AssignFile(f, 'input.txt');
  Reset(f);
  raw := nil;
  while not eof(f) do
  begin
    ReadLn(f, line);
    if line <> '' then
    begin
      n := Length(raw);
      SetLength(raw, n + 1);
      raw[n] := StrToInt(line);
    end;
  end;
  CloseFile(f);

  if Length(raw) = 0 then
  begin
    WriteLn('No adapters found in input.');
    Exit;
  end;

  { Sort the adapter ratings }
  QuickSort(raw, 0, High(raw));

  { Build full list: outlet (0), adapters..., device (max+3) }
  n := Length(raw);
  SetLength(adapters, n + 2);
  adapters[0] := 0;
  for i := 1 to n do
    adapters[i] := raw[i - 1];
  adapters[n + 1] := raw[n - 1] + 3;

  { Part 1: count 1-jolt and 3-jolt differences }
  ones := 0;
  threes := 0;
  for i := 1 to High(adapters) do
  begin
    d := adapters[i] - adapters[i - 1];
    if d = 1 then
      Inc(ones)
    else if d = 3 then
      Inc(threes);
  end;
  part1 := QWord(ones) * QWord(threes);
  WriteLn('Part 1: ', part1);

  { Part 2: count arrangements via dynamic programming }
  SetLength(ways, Length(adapters));
  for i := 0 to High(ways) do
    ways[i] := 0;
  ways[0] := 1;
  for i := 1 to High(adapters) do
  begin
    ways[i] := 0;
    { look back up to 3 adapters }
    if (i - 1 >= 0) and (adapters[i] - adapters[i - 1] <= 3) then
      ways[i] := ways[i] + ways[i - 1];
    if (i - 2 >= 0) and (adapters[i] - adapters[i - 2] <= 3) then
      ways[i] := ways[i] + ways[i - 2];
    if (i - 3 >= 0) and (adapters[i] - adapters[i - 3] <= 3) then
      ways[i] := ways[i] + ways[i - 3];
  end;
  part2 := ways[High(adapters)];
  WriteLn('Part 2: ', part2);
end.