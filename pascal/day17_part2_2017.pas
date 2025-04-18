program Spinlock;
{$mode objfpc}{$H+}
uses
  SysUtils;

var
  steps, i, pos, len, result1, valueAfter0: Integer;
  buf: array of Integer;

begin
  { Read the step count from input.txt }
  AssignFile(Input, 'input.txt');
  Reset(Input);
  try
    ReadLn(steps);
  finally
    CloseFile(Input);
  end;

  { Part 1: simulate up to 2017 insertions in a circular buffer }
  SetLength(buf, 2018);
  len := 1;
  pos := 0;
  buf[0] := 0;
  for i := 1 to 2017 do
  begin
    { advance and wrap around }
    pos := (pos + steps) mod len + 1;
    { make room for the new value at index pos }
    Move(buf[pos], buf[pos+1], (len - pos) * SizeOf(buf[0]));
    buf[pos] := i;
    Inc(len);
  end;
  { the value immediately after the last-inserted (2017) }
  result1 := buf[(pos + 1) mod len];

  { Part 2: track only the value that ends up immediately after 0
    over 50_000_000 insertions }
  pos := 0;
  valueAfter0 := 0;
  for i := 1 to 50000000 do
  begin
    pos := (pos + steps) mod i + 1;
    if pos = 1 then
      valueAfter0 := i;
  end;

  { Output the two results }
  Writeln(result1);
  Writeln(valueAfter0);
end.