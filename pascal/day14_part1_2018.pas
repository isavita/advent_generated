
program ChocolateCharts;
{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

const
  EXTRA = 20;  // small safety margin for digits spilling over

var
  infile: TextFile;
  N, cap, len, i1, i2, sum, d: Integer;
  recipes: array of Byte;
  i: Integer;

begin
  // 1) Read the puzzle input (single integer) from input.txt
  AssignFile(infile, 'input.txt');
  Reset(infile);
  try
    Read(infile, N);
  finally
    CloseFile(infile);
  end;

  // 2) Preallocate enough space:
  //    we need at least N + 10 digits, plus a small EXTRA margin.
  cap := N + 10 + EXTRA;
  SetLength(recipes, cap);

  // 3) Initialize the scoreboard and elf positions
  recipes[0] := 3;
  recipes[1] := 7;
  len := 2;
  i1 := 0;
  i2 := 1;

  // 4) Generate until we have at least N+10 recipes
  while len < N + 10 do
  begin
    sum := Integer(recipes[i1]) + Integer(recipes[i2]);
    // if sum >= 10, append tens digit first
    if sum >= 10 then
    begin
      d := sum div 10;
      recipes[len] := d;
      Inc(len);
      sum := sum mod 10;
    end;
    // append the units digit
    recipes[len] := sum;
    Inc(len);

    // move each elf
    i1 := (i1 + 1 + recipes[i1]) mod len;
    i2 := (i2 + 1 + recipes[i2]) mod len;
  end;

  // 5) Output the ten scores immediately after the first N recipes
  //    to standard output
  for i := N to N + 9 do
    Write(recipes[i]);
  Writeln;
end.
