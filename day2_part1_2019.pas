program IntcodeProgram;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

var
  inputFile: TextFile;
  intcode: array of Integer;
  opCode, i, inputLength: Integer;
  line, s: string;
  numbers: TStringArray;

function ExecuteOpCode(position: Integer): Boolean;
var
  param1, param2, outputPos: Integer;
begin
  case intcode[position] of
    1: begin
      param1 := intcode[intcode[position + 1]];
      param2 := intcode[intcode[position + 2]];
      outputPos := intcode[position + 3];
      intcode[outputPos] := param1 + param2;
    end;
    2: begin
      param1 := intcode[intcode[position + 1]];
      param2 := intcode[intcode[position + 2]];
      outputPos := intcode[position + 3];
      intcode[outputPos] := param1 * param2;
    end;
    99: Exit(False);
    else begin
      WriteLn('Unknown opcode encountered: ', intcode[position]);
      Exit(False);
    end;
  end;
  Result := True;
end;

begin
  AssignFile(inputFile, 'input.txt');
  Reset(inputFile);
  ReadLn(inputFile, line);
  CloseFile(inputFile);

  numbers := line.Split([',']);
  SetLength(intcode, Length(numbers));
  for i := 0 to High(numbers) do
  begin
    intcode[i] := StrToInt(numbers[i]);
  end;

  // Restore the "1202 program alarm" state
  intcode[1] := 12;
  intcode[2] := 2;

  i := 0;
  while ExecuteOpCode(i) do
  begin
    i := i + 4; // Move to the next opcode
  end;

  // Output the value at position 0 after the program halts
  WriteLn('Value at position 0: ', intcode[0]);
end.
