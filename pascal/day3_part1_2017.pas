
program SpiralMemory;

{$MODE OBJFPC}{$H+}

uses
  Math, SysUtils;

var
  inputFile: TextFile;
  numStr: string;
  num, result: Int64;

function FindSteps(n: Int64): Int64;
var
  root, side_length, steps_to_center, max_num, steps_to_max_num, steps_along_side: Int64;
  floatRoot: Double;
begin
  if n = 1 then
  begin
    Result := 0;
    Exit;
  end;

  floatRoot := sqrt(n);
  root := Ceil(floatRoot);

  if root mod 2 = 0 then
    inc(root);

  side_length := root - 1;
  steps_to_center := side_length div 2;
  max_num := sqr(root);
  steps_to_max_num := max_num - n;

  if side_length > 0 then
     steps_along_side := steps_to_max_num mod side_length
  else
     steps_along_side := 0; // Should not happen for n > 1

  Result := steps_to_center + Abs(steps_along_side - steps_to_center);
end;

begin
  Assign(inputFile, 'input.txt');
  try
    Reset(inputFile);
    ReadLn(inputFile, numStr);
    num := StrToInt64(Trim(numStr));
    Close(inputFile);

    result := FindSteps(num);
    WriteLn(result);

  except
    on E: Exception do
    begin
      Halt(1);
    end;
  end;
end.
