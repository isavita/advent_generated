
{$mode objfpc}{$H+}
program PasswordPhilosophy;

uses
  SysUtils;

var
  f: TextFile;
  line, pwd: string;
  posDash, posSpace, posColon: Integer;
  minOcc, maxOcc, cnt, validCount, i: Integer;
  policyChar: Char;

begin
  AssignFile(f, 'input.txt');
  Reset(f);
  validCount := 0;

  while not Eof(f) do
  begin
    ReadLn(f, line);
    if line = '' then Continue;

    posDash  := Pos('-', line);
    posSpace := Pos(' ', line);
    posColon := Pos(':', line);

    minOcc := StrToInt(Copy(line, 1, posDash - 1));
    maxOcc := StrToInt(
      Copy(line,
           posDash + 1,
           posSpace - posDash - 1
      )
    );

    policyChar := line[posSpace + 1];

    { Password starts two chars after ':' (skip ": ") }
    pwd := Copy(line, posColon + 2, Length(line));

    cnt := 0;
    for i := 1 to Length(pwd) do
      if pwd[i] = policyChar then
        Inc(cnt);

    if (cnt >= minOcc) and (cnt <= maxOcc) then
      Inc(validCount);
  end;

  CloseFile(f);

  { Output the number of valid passwords }
  WriteLn(validCount);
end.
