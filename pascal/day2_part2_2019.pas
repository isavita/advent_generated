program GravityAssist;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

var
  inputFile: TextFile;
  originalIntcode, intcode: array of Integer;
  noun, verb, output: Integer;
  line: string;
  numbers: TStringArray;
  i: Integer;

function ExecuteIntcode(memory: array of Integer): Integer;
var
  position: Integer;
begin
  position := 0;
  while (position < Length(memory)) and (memory[position] <> 99) do
  begin
    case memory[position] of
      1: memory[memory[position + 3]] := memory[memory[position + 1]] + memory[memory[position + 2]];
      2: memory[memory[position + 3]] := memory[memory[position + 1]] * memory[memory[position + 2]];
    end;
    Inc(position, 4);
  end;
  Result := memory[0];
end;

begin
  // Load the Intcode program from the input file
  AssignFile(inputFile, 'input.txt');
  Reset(inputFile);
  ReadLn(inputFile, line);
  CloseFile(inputFile);

  numbers := line.Split([',']);
  SetLength(originalIntcode, Length(numbers));
  for i := 0 to High(numbers) do
  begin
    originalIntcode[i] := StrToInt(numbers[i]);
  end;

  output := 0;

  // Try every combination of noun and verb
  for noun := 0 to 99 do
    for verb := 0 to 99 do
    begin
      // Reset memory to the original program, with the current noun and verb
      intcode := Copy(originalIntcode, 0, Length(originalIntcode));
      intcode[1] := noun;
      intcode[2] := verb;

      // Run the program
      if ExecuteIntcode(intcode) = 19690720 then
      begin
        output := 100 * noun + verb;
        Break;
      end;
    end;

  if output > 0 then
    WriteLn('The value that produces the output 19690720 is: ', output)
  else
    WriteLn('No combination of noun and verb found that produces the output 19690720.');
end.
