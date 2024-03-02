program Day1Trebuchet;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

var
  inputFile: TextFile;
  line: string;
  sum, firstDigit, lastDigit: Integer;
  i: Integer;

function ExtractFirstLastDigit(const s: string; var firstDigit, lastDigit: Integer): Boolean;
var
  i: Integer;
begin
  firstDigit := -1;
  lastDigit := -1;
  for i := 1 to Length(s) do
  begin
    if (s[i] >= '0') and (s[i] <= '9') then
    begin
      if firstDigit = -1 then
        firstDigit := Ord(s[i]) - Ord('0');
      lastDigit := Ord(s[i]) - Ord('0');
    end;
  end;
  Result := (firstDigit <> -1) and (lastDigit <> -1);
end;

begin
  sum := 0;
  AssignFile(inputFile, 'input.txt');
  Reset(inputFile);
  while not EOF(inputFile) do
  begin
    ReadLn(inputFile, line);
    if ExtractFirstLastDigit(line, firstDigit, lastDigit) then
    begin
      sum := sum + (firstDigit * 10 + lastDigit);
    end;
  end;
  CloseFile(inputFile);

  WriteLn('Total sum of calibration values: ', sum);
end.
