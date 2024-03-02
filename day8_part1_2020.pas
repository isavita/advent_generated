program solution;

uses
  SysUtils;

var
  inputFile: Text;
  instructions: array of string;
  accumulator, i: integer;
  visited: array of boolean;
  currentInstruction, arg: integer;
  op: string;

function executeBootCode(instructions: array of string): integer;
begin
  accumulator := 0;
  SetLength(visited, Length(instructions));
  currentInstruction := 0;

  while currentInstruction < Length(instructions) do
  begin
    if visited[currentInstruction] then
    begin
      executeBootCode := accumulator;
      Exit;
    end;

    visited[currentInstruction] := true;
    op := Copy(instructions[currentInstruction], 1, Pos(' ', instructions[currentInstruction]) - 1);
    arg := StrToInt(Copy(instructions[currentInstruction], Pos(' ', instructions[currentInstruction]) + 1));

    case op of
      'acc': 
      begin
        accumulator := accumulator + arg;
        currentInstruction := currentInstruction + 1;
      end;
      'jmp': currentInstruction := currentInstruction + arg;
      'nop': currentInstruction := currentInstruction + 1;
    end;
  end;

  executeBootCode := accumulator;
end;

begin
  Assign(inputFile, 'input.txt');
  Reset(inputFile);

  SetLength(instructions, 0);
  while not Eof(inputFile) do
  begin
    SetLength(instructions, Length(instructions) + 1);
    ReadLn(inputFile, instructions[High(instructions)]);
  end;

  Close(inputFile);

  WriteLn(executeBootCode(instructions));
end.