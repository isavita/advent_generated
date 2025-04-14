
program PasswordValidator;

{$mode objfpc}{$H+}

uses
  SysUtils;

var
  inputFile: TextFile;
  line: string;
  policy, password: string;
  positions, letter: string;
  pos1, pos2: Integer;
  validCount: Integer;
  dashIndex, spaceIndex: Integer;

begin
  Assign(inputFile, 'input.txt');
  Reset(inputFile);
  validCount := 0;

  while not Eof(inputFile) do
  begin
    ReadLn(inputFile, line);

    spaceIndex := Pos(': ', line);
    policy := Copy(line, 1, spaceIndex - 1);
    password := Copy(line, spaceIndex + 2, Length(line));

    spaceIndex := Pos(' ', policy);
    positions := Copy(policy, 1, spaceIndex - 1);
    letter := Copy(policy, spaceIndex + 1, Length(policy));

    dashIndex := Pos('-', positions);
    pos1 := StrToInt(Copy(positions, 1, dashIndex - 1));
    pos2 := StrToInt(Copy(positions, dashIndex + 1, Length(positions)));

    if ((password[pos1] = letter[1]) <> (password[pos2] = letter[1])) then
      Inc(validCount);
  end;

  CloseFile(inputFile);
  WriteLn(validCount);
end.
