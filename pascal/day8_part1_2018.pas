program MemoryManeuver;

{ Reads the license data from input.txt, builds the implicit tree,
  and prints the sum of all metadata entries. }

uses
  SysUtils;

type
  IntArray = array of LongInt;

var
  dataArr: IntArray;
  v: LongInt;
  idx: Integer;

function ParseNode(var i: Integer): Int64;
var
  childCount, metaCount, c: Integer;
  sum: Int64;
begin
  // Read header
  childCount := dataArr[i];
  inc(i);
  metaCount := dataArr[i];
  inc(i);

  // Sum metadata from all children
  sum := 0;
  for c := 1 to childCount do
    sum := sum + ParseNode(i);

  // Add this node's metadata
  for c := 1 to metaCount do
  begin
    sum := sum + dataArr[i];
    inc(i);
  end;

  ParseNode := sum;
end;

procedure LoadInput(const FileName: string);
var
  f: Text;
  tmp: LongInt;
begin
  SetLength(dataArr, 0);
  assign(f, FileName);
  {$I-}
  Reset(f);
  {$I+}
  if IOResult <> 0 then
  begin
    Writeln('Error opening input file: ', FileName);
    Halt(1);
  end;

  while not Eof(f) do
  begin
    Read(f, tmp);
    if IOResult <> 0 then Break;
    SetLength(dataArr, Length(dataArr) + 1);
    dataArr[Length(dataArr) - 1] := tmp;
  end;

  Close(f);
end;

var
  startIdx: Integer;
  resultSum: Int64;

begin
  LoadInput('input.txt');
  startIdx := 0;
  resultSum := ParseNode(startIdx);
  WriteLn(resultSum);
end.