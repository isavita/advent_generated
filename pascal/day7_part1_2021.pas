
program TreacheryOfWhales;

{$mode objfpc}{$H+}

uses
  SysUtils;

{ In-place QuickSort for a dynamic array of Integer }
procedure QuickSort(var A: array of Integer; L, R: Integer);
var
  i, j, pivot, tmp: Integer;
begin
  i := L; 
  j := R; 
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
  inputFile: TextFile;
  line: string;
  nums: array of Integer;
  n, i, p: Integer;
  totalFuel: Int64;
  median: Integer;
  numStr: string;
begin
  { 1) Read the entire line from input.txt }
  AssignFile(inputFile, 'input.txt');
  Reset(inputFile);
  if not Eof(inputFile) then
    ReadLn(inputFile, line)
  else
    line := '';
  CloseFile(inputFile);

  { 2) Parse comma-separated integers into dynamic array nums }
  n := 0;
  p := 1;
  while p <= Length(line) do
  begin
    numStr := '';
    while (p <= Length(line)) and (line[p] <> ',') do
    begin
      numStr := numStr + line[p];
      Inc(p);
    end;
    Inc(n);
    SetLength(nums, n);
    nums[n-1] := StrToInt(numStr);
    if (p <= Length(line)) and (line[p] = ',') then
      Inc(p);
  end;

  if n = 0 then
  begin
    WriteLn('No data found in input.txt');
    Exit;
  end;

  { 3) Sort the positions }
  QuickSort(nums, 0, n-1);

  { 4) Choose the median as the alignment point }
  median := nums[n div 2];  

  { 5) Compute total fuel = sum of |xi - median| }
  totalFuel := 0;
  for i := 0 to n-1 do
    totalFuel := totalFuel + Abs(nums[i] - median);

  { 6) Output the result }
  WriteLn(totalFuel);
end.
