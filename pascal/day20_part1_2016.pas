
program FirewallRules;

{$mode objfpc}{$H+}
uses
  SysUtils;

type
  TRange = record
    StartIP, EndIP: QWord;
  end;

var
  Ranges: array of TRange;
  N, i, dashPos: Integer;
  line, s1, s2: string;
  cur, ans: QWord;
  F: TextFile;

{ In‐place quicksort on Ranges[lo..hi], sorting by StartIP, then EndIP. }
procedure QuickSort(lo, hi: Integer);
var
  i, j: Integer;
  pivot, tmp: TRange;
begin
  i := lo;  j := hi;
  pivot := Ranges[(lo + hi) shr 1];
  repeat
    while (Ranges[i].StartIP < pivot.StartIP) or
          ((Ranges[i].StartIP = pivot.StartIP) and (Ranges[i].EndIP < pivot.EndIP)) do
      Inc(i);
    while (Ranges[j].StartIP > pivot.StartIP) or
          ((Ranges[j].StartIP = pivot.StartIP) and (Ranges[j].EndIP > pivot.EndIP)) do
      Dec(j);
    if i <= j then
    begin
      tmp := Ranges[i];  Ranges[i] := Ranges[j];  Ranges[j] := tmp;
      Inc(i);  Dec(j);
    end;
  until i > j;
  if lo < j then QuickSort(lo, j);
  if i < hi then QuickSort(i, hi);
end;

begin
  { 1) Read and parse all ranges from input.txt }
  Assign(F, 'input.txt');
  Reset(F);
  N := 0;
  while not Eof(F) do
  begin
    ReadLn(F, line);
    if line = '' then
      Continue;
    Inc(N);
    SetLength(Ranges, N);
    dashPos := Pos('-', line);
    s1 := Copy(line, 1, dashPos - 1);
    s2 := Copy(line, dashPos + 1, Length(line) - dashPos);
    Ranges[N-1].StartIP := StrToUInt64(s1);
    Ranges[N-1].EndIP   := StrToUInt64(s2);
  end;
  Close(F);

  if N = 0 then
  begin
    { No blocked ranges at all ⇒ 0 is allowed. }
    Writeln(0);
    Halt;
  end;

  { 2) Sort the ranges by start, then end }
  QuickSort(0, N-1);

  { 3) Sweep through to find the first gap }
  cur := 0;
  ans := High(QWord);  // Placeholder

  for i := 0 to N-1 do
  begin
    if Ranges[i].StartIP > cur then
    begin
      { Found a gap: cur is not covered }
      ans := cur;
      Break;
    end
    else if Ranges[i].EndIP >= cur then
    begin
      { Extend the covered region }
      cur := Ranges[i].EndIP + 1;
    end;
  end;

  { 4) If no gap was found in the loop, cur is the first allowed IP (if ≤ 2^32−1) }
  if ans = High(QWord) then
  begin
    if cur <= $FFFFFFFF then
      ans := cur
    else
      { Everything up to 2^32−1 is blocked; no valid IP }
      ans := 0; 
  end;

  Writeln(ans);
end.
