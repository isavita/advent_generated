program MazeOfTwistyTrampolines;

{$APPTYPE CONSOLE}
uses
  SysUtils;

var
  inFile: Text;
  baseOffsets: array of Integer;
  offsets1, offsets2: array of Integer;
  n, i: Integer;
  pos: Integer;
  steps1, steps2: Int64;
  oldOffset: Integer;
  line: String;
begin
  { Read all offsets from input.txt into baseOffsets }
  Assign(inFile, 'input.txt');
  Reset(inFile);
  while not Eof(inFile) do
  begin
    ReadLn(inFile, line);
    if line = '' then Continue;
    SetLength(baseOffsets, Length(baseOffsets) + 1);
    baseOffsets[High(baseOffsets)] := StrToInt(Trim(line));
  end;
  Close(inFile);

  { Prepare two copies for part 1 and part 2 }
  n := Length(baseOffsets);
  SetLength(offsets1, n);
  SetLength(offsets2, n);
  for i := 0 to n - 1 do
  begin
    offsets1[i] := baseOffsets[i];
    offsets2[i] := baseOffsets[i];
  end;

  { Part 1: increment every jumped offset by 1 }
  pos := 0;
  steps1 := 0;
  while (pos >= 0) and (pos < n) do
  begin
    oldOffset := offsets1[pos];
    Inc(offsets1[pos]);
    Inc(pos, oldOffset);
    Inc(steps1);
  end;

  { Part 2: offsets >= 3 are decremented, otherwise incremented }
  pos := 0;
  steps2 := 0;
  while (pos >= 0) and (pos < n) do
  begin
    oldOffset := offsets2[pos];
    if oldOffset >= 3 then
      Dec(offsets2[pos])
    else
      Inc(offsets2[pos]);
    Inc(pos, oldOffset);
    Inc(steps2);
  end;

  { Output results }
  WriteLn('Part 1 steps: ', steps1);
  WriteLn('Part 2 steps: ', steps2);
end.