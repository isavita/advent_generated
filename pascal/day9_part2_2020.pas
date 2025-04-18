
program XMAS;
uses
  SysUtils;
const
  PREAMBLE = 25;
  MAXN = 10000;
label
  AfterPart1, NextI, AfterPart2;
var
  arr: array[0..MAXN-1] of Int64;
  N, i, j, k, startPos, endPos: Integer;
  x, invalidNum, target, sumWindow, mn, mx, encryption: Int64;
  found: Boolean;
begin
  { Read input from input.txt, one number per line }
  Assign(Input, 'input.txt');
  Reset(Input);
  N := 0;
  while not Eof(Input) do
  begin
    ReadLn(Input, x);
    arr[N] := x;
    Inc(N);
  end;
  Close(Input);

  { Part 1: find first invalid number }
  invalidNum := 0;
  for i := PREAMBLE to N-1 do
  begin
    found := False;
    for j := i-PREAMBLE to i-1 do
    begin
      for k := j+1 to i-1 do
        if arr[j] + arr[k] = arr[i] then
        begin
          found := True;
          Break;
        end;
      if found then Break;
    end;
    if not found then
    begin
      invalidNum := arr[i];
      goto AfterPart1;
    end;
    NextI: ;
  end;
AfterPart1:
  WriteLn(invalidNum);

  { Part 2: find a contiguous range summing to invalidNum }
  target := invalidNum;
  for i := 0 to N-2 do
  begin
    sumWindow := 0;
    for j := i to N-1 do
    begin
      sumWindow := sumWindow + arr[j];
      if sumWindow >= target then Break;
    end;
    if (sumWindow = target) and (j - i + 1 >= 2) then
    begin
      startPos := i;
      endPos := j;
      goto AfterPart2;
    end;
  end;
AfterPart2:
  { Compute min and max in the found window }
  mn := arr[startPos];
  mx := arr[startPos];
  for k := startPos to endPos do
  begin
    if arr[k] < mn then mn := arr[k];
    if arr[k] > mx then mx := arr[k];
  end;
  encryption := mn + mx;
  WriteLn(encryption);
end.
