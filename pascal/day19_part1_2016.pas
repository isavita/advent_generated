
program Josephus;

{$mode objfpc}{$H+}

uses
  SysUtils;

function Josephus(n: Integer): Integer;
var
  msb: Integer;
begin
  msb := 1;
  while msb <= n do
    msb := msb shl 1;
  msb := msb shr 1;
  Josephus := (n - msb) shl 1 + 1;
end;

var
  numElves: Integer;
  inputFile: TextFile;

begin
  AssignFile(inputFile, 'input.txt');
  Reset(inputFile);
  ReadLn(inputFile, numElves);
  CloseFile(inputFile);

  WriteLn(Josephus(numElves));
end.
