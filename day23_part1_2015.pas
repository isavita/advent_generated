program Solution;

{$mode objfpc}

uses
  SysUtils;

var
  inputFile: Text;
  instructions: array of String;
  registers: array of Integer;
  i, offset: Integer;

begin
  Assign(inputFile, 'input.txt');
  Reset(inputFile);

  SetLength(instructions, 0);
  while not Eof(inputFile) do
  begin
    SetLength(instructions, Length(instructions) + 1);
    Readln(inputFile, instructions[High(instructions)]);
  end;

  Close(inputFile);

  SetLength(registers, 2);
  registers[0] := 0;
  registers[1] := 0;

  i := 0;

  while i < Length(instructions) do
  begin
    case Copy(instructions[i], 1, 3) of
      'hlf': registers[Pos(instructions[i][5], 'ab') - 1] := registers[Pos(instructions[i][5], 'ab') - 1] div 2;
      'tpl': registers[Pos(instructions[i][5], 'ab') - 1] := registers[Pos(instructions[i][5], 'ab') - 1] * 3;
      'inc': registers[Pos(instructions[i][5], 'ab') - 1] := registers[Pos(instructions[i][5], 'ab') - 1] + 1;
      'jmp': begin
               Val(Copy(instructions[i], 5, Length(instructions[i]) - 4), offset);
               i := i + offset - 1;
             end;
      'jie': begin
               if registers[Pos(instructions[i][5], 'ab') - 1] mod 2 = 0 then
               begin
                 Val(Copy(instructions[i], 8, Length(instructions[i]) - 7), offset);
                 i := i + offset - 1;
               end;
             end;
      'jio': begin
               if registers[Pos(instructions[i][5], 'ab') - 1] = 1 then
               begin
                 Val(Copy(instructions[i], 8, Length(instructions[i]) - 7), offset);
                 i := i + offset - 1;
               end;
             end;
    else
      WriteLn('Unknown instruction: ', Copy(instructions[i], 1, 3));
      Halt;
    end;

    Inc(i);
  end;

  WriteLn(registers[1]);
end.