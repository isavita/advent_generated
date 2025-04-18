
program DragonChecksum;
{$mode objfpc}{$H+}
uses
  SysUtils;

function ComputeChecksum(const init: string; diskLen: LongInt): string;
var
  a, b: array of Byte;
  curLen: LongInt;
  i: LongInt;
begin
  // load initial state into array 'a'
  curLen := Length(init);
  SetLength(a, curLen);
  for i := 1 to curLen do
    if init[i] = '1' then
      a[i-1] := 1
    else
      a[i-1] := 0;

  // expand with the dragon curve until we have >= diskLen bits
  while curLen < diskLen do
  begin
    // build b = reverse(invert(a))
    SetLength(b, curLen);
    for i := 0 to curLen - 1 do
      b[i] := 1 - a[curLen - 1 - i];
    // concatenate a + [0] + b
    SetLength(a, curLen * 2 + 1);
    a[curLen] := 0;
    for i := 0 to curLen - 1 do
      a[curLen + 1 + i] := b[i];
    curLen := Length(a);
  end;

  // truncate to exactly diskLen
  SetLength(a, diskLen);
  curLen := diskLen;

  // compute checksum until length is odd
  while (curLen mod 2) = 0 do
  begin
    for i := 0 to curLen div 2 - 1 do
      // 1 if bits match, 0 otherwise
      a[i] := Byte(a[2*i] = a[2*i + 1]);
    curLen := curLen div 2;
    SetLength(a, curLen);
  end;

  // convert final bit‑array to string
  SetLength(Result, curLen);
  for i := 1 to curLen do
    Result[i] := Char(Ord('0') + a[i-1]);
end;

var
  inFile: TextFile;
  initState: string;
  sizes: array[1..2] of LongInt = (272, 35651584);
  i: Integer;
begin
  AssignFile(inFile, 'input.txt');
  Reset(inFile);
  ReadLn(inFile, initState);
  CloseFile(inFile);

  for i := 1 to Length(sizes) do
    WriteLn(ComputeChecksum(initState, sizes[i]));
end.
