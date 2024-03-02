program Day1;

uses SysUtils;

var
  Input: string;
  Floor, Position: integer;
  FileInput: TextFile;

begin
  AssignFile(FileInput, 'input.txt');
  reset(FileInput);
  readln(FileInput, Input);
  close(FileInput);

  Floor := 0;
  Position := 0;

  for char in Input do
  begin
    Inc(Position);
    if char = '(' then
      Inc(Floor)
    else if char = ')' then
      Dec(Floor);

    if Floor = -1 then
    begin
      writeln('Position of the character that causes Santa to first enter the basement: ', Position);
      break;
    end;
  end;

  writeln('Floor: ', Floor);
end.