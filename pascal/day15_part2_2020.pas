
program RambunctiousRecitation;

{$mode objfpc}{$H+}

uses
  sysutils;

const
  TARGET1 = 2020;
  TARGET2 = 30000000;

var
  lastOcc: array of LongInt;
  starts: array of LongInt;
  inp: Text;
  line: string;
  i, n, turn: LongInt;
  num, prevSpoken, nextSpoken: LongInt;
  res1, res2: LongInt;

procedure ParseStartingNumbers(const s: string);
var
  i, len: Integer;
  buf: string;
begin
  buf := '';
  n := 0;
  len := Length(s);
  for i := 1 to len do
  begin
    if s[i] in ['0'..'9'] then
      buf += s[i]
    else if s[i] = ',' then
    begin
      Inc(n);
      SetLength(starts, n);
      starts[n-1] := StrToInt(buf);
      buf := '';
    end;
  end;
  // last number after final comma
  if buf <> '' then
  begin
    Inc(n);
    SetLength(starts, n);
    starts[n-1] := StrToInt(buf);
  end;
end;

begin
  // 1) Read the single line of comma‐separated starting numbers
  AssignFile(inp, 'input.txt');
  Reset(inp);
  try
    ReadLn(inp, line);
  finally
    CloseFile(inp);
  end;
  ParseStartingNumbers(line);

  // 2) Prepare the array to hold, for each number 0..TARGET2, the last turn it was spoken
  //    We index 0..TARGET2 so SetLength = TARGET2+1
  SetLength(lastOcc, TARGET2 + 1);
  for i := 0 to TARGET2 do
    lastOcc[i] := 0;

  // 3) Initialize with all but the last starting number:
  //    record their turn of occurrence
  for i := 0 to n - 2 do
    lastOcc[starts[i]] := i + 1;

  // 4) The "last spoken" number is the final element of the starting list
  prevSpoken := starts[n - 1];

  // 5) Play from turn = n+1 … TARGET2
  for turn := n + 1 to TARGET2 do
  begin
    // when 'prevSpoken' was last spoken?
    num := lastOcc[prevSpoken];
    if num = 0 then
      // never seen before
      nextSpoken := 0
    else
      // age = (previous turn) – (last time it was spoken)
      nextSpoken := (turn - 1) - num;

    // update the last occurrence of prevSpoken to turn-1
    lastOcc[prevSpoken] := turn - 1;

    // move on
    prevSpoken := nextSpoken;

    // capture the 2020th result
    if turn = TARGET1 then
      res1 := prevSpoken;
  end;

  // after the loop, prevSpoken is the TARGET2th number
  res2 := prevSpoken;

  // 6) Print answers
  WriteLn(res1);
  WriteLn(res2);
end.
