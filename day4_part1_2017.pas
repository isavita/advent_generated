program Day4;

uses SysUtils, Classes;

var
  InputFile: TextFile;
  Line: string;
  Passphrases: TStringList;
  ValidCount, i, j: Integer;
  Words: TStringList;

function IsValidPassphrase(phrase: string): Boolean;
var
  Words: TStringList;
  i: Integer;
begin
  Words := TStringList.Create;
  Words.Delimiter := ' ';
  Words.DelimitedText := phrase;
  
  for i := 0 to Words.Count - 1 do
  begin
    if Words.IndexOf(Words[i]) <> i then
    begin
      Result := False;
      Exit;
    end;
  end;
  
  Result := True;
end;

begin
  AssignFile(InputFile, 'input.txt');
  Reset(InputFile);
  
  Passphrases := TStringList.Create;
  ValidCount := 0;
  
  while not Eof(InputFile) do
  begin
    ReadLn(InputFile, Line);
    Passphrases.Add(Line);
  end;
  
  CloseFile(InputFile);
  
  for i := 0 to Passphrases.Count - 1 do
  begin
    if IsValidPassphrase(Passphrases[i]) then
      Inc(ValidCount);
  end;
  
  Writeln('Number of valid passphrases: ', ValidCount);
  
  Passphrases.Free;
end.