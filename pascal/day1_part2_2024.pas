
program HistorianHysteria;
{$mode objfpc}{$H+}
uses
  SysUtils;

type
  TIntArray = array of Integer;

// In-place quicksort on A[Low..High]
procedure QuickSort(var A: TIntArray; Low, High: Integer);
var
  i, j, pivot, tmp: Integer;
begin
  i := Low; j := High;
  pivot := A[(Low + High) shr 1];
  repeat
    while A[i] < pivot do Inc(i);
    while A[j] > pivot do Dec(j);
    if i <= j then
    begin
      tmp := A[i]; A[i] := A[j]; A[j] := tmp;
      Inc(i); Dec(j);
    end;
  until i > j;
  if Low < j then QuickSort(A, Low, j);
  if i < High then QuickSort(A, i, High);
end;

// First index in A[0..N-1] >= X
function LowerBound(const A: TIntArray; N, X: Integer): Integer;
var
  lo, hi, mid: Integer;
begin
  lo := 0; hi := N;
  while lo < hi do
  begin
    mid := (lo + hi) shr 1;
    if A[mid] < X then
      lo := mid + 1
    else
      hi := mid;
  end;
  Result := lo;
end;

// First index in A[0..N-1] > X
function UpperBound(const A: TIntArray; N, X: Integer): Integer;
var
  lo, hi, mid: Integer;
begin
  lo := 0; hi := N;
  while lo < hi do
  begin
    mid := (lo + hi) shr 1;
    if A[mid] <= X then
      lo := mid + 1
    else
      hi := mid;
  end;
  Result := lo;
end;

var
  F: Text;
  leftList, rightList: TIntArray;
  x, y: Integer;
  n, i: Integer;
  sortedL, sortedR: TIntArray;
  part1: Int64 = 0;
  part2: Int64 = 0;
  cnt: Integer;
begin
  // Read input.txt
  Assign(F, 'input.txt');
  Reset(F);
  SetLength(leftList, 0);
  SetLength(rightList, 0);
  while not EOF(F) do
  begin
    Read(F, x, y);
    // append
    n := Length(leftList);
    SetLength(leftList, n+1);
    SetLength(rightList, n+1);
    leftList[n] := x;
    rightList[n] := y;
  end;
  Close(F);

  n := Length(leftList);
  if n = 0 then
  begin
    Writeln('No data.');
    Exit;
  end;

  // Copy and sort for Part 1
  sortedL := Copy(leftList, 0, n);
  sortedR := Copy(rightList, 0, n);
  QuickSort(sortedL, 0, n-1);
  QuickSort(sortedR, 0, n-1);

  // Part 1: sum of absolute differences
  for i := 0 to n-1 do
    part1 := part1 + Int64(Abs(sortedL[i] - sortedR[i]));

  // Part 2: similarity score
  // We'll reuse sortedR and binary-search its counts
  for i := 0 to n-1 do
  begin
    // how many times leftList[i] appears in rightList?
    cnt := UpperBound(sortedR, n, leftList[i])
         - LowerBound(sortedR, n, leftList[i]);
    if cnt > 0 then
      part2 := part2 + Int64(leftList[i]) * cnt;
  end;

  // Output results
  Writeln(part1);
  Writeln(part2);
end.
