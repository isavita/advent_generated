
program AlchemicalReduction;

{$mode objfpc}{$H+}

uses
  SysUtils;

function ReactPolymer(polymer: string): string;
var
  i: Integer;
  reacted: Boolean;
begin
  reacted := True;
  while reacted do
  begin
    reacted := False;
    i := 1;
    while i < Length(polymer) do
    begin
      if (Abs(Ord(polymer[i]) - Ord(polymer[i+1])) = 32) then
      begin
        Delete(polymer, i, 2);
        reacted := True;
        if i > 1 then
          i := i - 1
        else
          i := 1;
      end
      else
        i := i + 1;
    end;
  end;
  ReactPolymer := polymer;
end;

var
  inputFile: TextFile;
  polymer: string;
  reactedPolymer: string;

begin
  AssignFile(inputFile, 'input.txt');
  try
    Reset(inputFile);
    ReadLn(inputFile, polymer);
    CloseFile(inputFile);
  except
    on E: Exception do
    begin
      Writeln('Error reading input file: ', E.Message);
      Halt(1);
    end;
  end;

  reactedPolymer := ReactPolymer(polymer);
  Writeln(Length(reactedPolymer));
end.
