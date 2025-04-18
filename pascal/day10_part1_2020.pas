program AdapterArray;

{$mode objfpc}{$H+}
uses
  SysUtils;

type
  TIntArray = array of Integer;

var
  adapters: TIntArray;
  f: TextFile;
  x: Integer;
  n, i: Integer;
  count1, count3, prev: Integer;

procedure QuickSort(var A: TIntArray; L, H: Integer);
var
  i, j, pivot, tmp: Integer;
begin
  i := L; j := H; pivot := A[(L + H) shr 1];
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
  if i < H then QuickSort(A, i, H);
end;

begin
  { Read all adapter ratings from input.txt into a dynamic array }
  AssignFile(f, 'input.txt');
  Reset(f);
  n := 0;
  while not Eof(f) do
  begin
    ReadLn(f, x);
    Inc(n);
    SetLength(adapters, n);
    adapters[n - 1] := x;
  end;
  CloseFile(f);

  { Sort the adapter ratings }
  if n > 1 then
    QuickSort(adapters, 0, n - 1);

  { Count 1-jolt and 3-jolt differences }
  count1 := 0;
  count3 := 0;
  prev := 0;  { charging outlet is 0 jolts }
  for i := 0 to n - 1 do
  begin
    case adapters[i] - prev of
      1: Inc(count1);
      3: Inc(count3);
    end;
    prev := adapters[i];
  end;

  { Account for device's built-in adapter (+3 jolts) }
  Inc(count3);

  { Output the product of 1-jolt and 3-jolt differences }
  WriteLn(count1 * count3);
end.