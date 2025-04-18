
program ShuttleSearch;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

var
  f: TextFile;
  line: string;
  earliest: Int64;
  part1Answer: Int64;
  part2Answer: QWord;

// Simple 64‐bit GCD
function GCD(a, b: QWord): QWord;
begin
  while b <> 0 do
  begin
    Result := b;
    b := a mod b;
    a := Result;
  end;
  Result := a;
end;

// 64‐bit LCM
function LCM(a, b: QWord): QWord;
begin
  Result := (a div GCD(a, b)) * b;
end;

procedure Solve;
var
  i: Integer;
  wait, minWait: Int64;
  busCount: Integer;
  busIDsPart1: array of Int64;
  busIDsPart2: array of QWord;
  offsetsPart2: array of QWord;
  sl: TStringList;
  t, step: QWord;
begin
  // Open and read the two lines
  AssignFile(f, 'input.txt');
  Reset(f);
  ReadLn(f, line);
  earliest := StrToInt64(Trim(line));
  ReadLn(f, line);
  CloseFile(f);

  // --------------------------------------------------------------------------------
  // Part 1
  // parse only the numeric IDs, ignore 'x'
  sl := TStringList.Create;
  try
    sl.Delimiter := ',';
    sl.StrictDelimiter := True;
    sl.DelimitedText := line;

    busCount := 0;
    for i := 0 to sl.Count - 1 do
      if sl[i] <> 'x' then
      begin
        SetLength(busIDsPart1, busCount + 1);
        busIDsPart1[busCount] := StrToInt64(sl[i]);
        Inc(busCount);
      end;

    // find the bus with minimal wait time
    minWait := High(Int64);
    part1Answer := 0;
    for i := 0 to busCount - 1 do
    begin
      wait := (busIDsPart1[i] - (earliest mod busIDsPart1[i]))
              mod busIDsPart1[i];
      if wait < minWait then
      begin
        minWait := wait;
        part1Answer := wait * busIDsPart1[i];
      end;
    end;
  finally
    sl.Free;
  end;

  // --------------------------------------------------------------------------------
  // Part 2
  // we need t ≡ -offset (mod busID) for each busID at that offset
  sl := TStringList.Create;
  try
    sl.Delimiter := ',';
    sl.StrictDelimiter := True;
    sl.DelimitedText := line;

    busCount := 0;
    for i := 0 to sl.Count - 1 do
      if sl[i] <> 'x' then
      begin
        SetLength(busIDsPart2, busCount + 1);
        SetLength(offsetsPart2, busCount + 1);
        busIDsPart2[busCount] := QWord(StrToInt64(sl[i]));
        // offset = the index in the list
        offsetsPart2[busCount] := QWord(i);
        Inc(busCount);
      end;

    // incremental CRT
    t := 0;
    step := busIDsPart2[0];
    for i := 1 to busCount - 1 do
    begin
      // we want (t + offsetsPart2[i]) mod busIDsPart2[i] = 0
      while ( (t + offsetsPart2[i]) mod busIDsPart2[i] ) <> 0 do
        Inc(t, step);
      // merge modulus
      step := LCM(step, busIDsPart2[i]);
    end;
    part2Answer := t;

  finally
    sl.Free;
  end;
end;

begin
  Solve;
  WriteLn(part1Answer);
  WriteLn(part2Answer);
end.
