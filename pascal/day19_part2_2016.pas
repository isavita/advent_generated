
program Solve;

uses
  Math; // For Max function

var
  inputFile: text;
  num_elves: LongInt;
  result: LongInt;

function josephus(n: LongInt): LongInt;
var
  i: LongInt;
begin
  i := 1;
  while i * 3 <= n do
  begin
    i := i * 3;
  end;
  josephus := n - i + Max(n - 2 * i, 0);
end;

begin
  Assign(inputFile, 'input.txt');
  Reset(inputFile);
  ReadLn(inputFile, num_elves);
  Close(inputFile);

  result := josephus(num_elves);
  WriteLn(result);
end.
