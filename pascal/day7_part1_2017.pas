
program FindUnheld;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

var
  Holders, Held: TStringList;
  F: TextFile;
  Line, Token: string;
  Parts: TStringList;
  I: Integer;
begin
  Holders := TStringList.Create;
  Held    := TStringList.Create;
  Held.Sorted    := True;        { enable binary search }
  Held.Duplicates:= dupIgnore;   { ignore duplicates }

  if not FileExists('input.txt') then
    Exit;

  AssignFile(F, 'input.txt');
  Reset(F);
  while not Eof(F) do
  begin
    Readln(F, Line);
    Line := StringReplace(Line, ',', ' ', [rfReplaceAll]);  { commas are separators }

    Parts := TStringList.Create;
    Parts.Delimiter       := ' ';
    Parts.StrictDelimiter := True;
    Parts.DelimitedText   := Line;

    if Parts.Count > 0 then
    begin
      Holders.Add(Parts[0]);                     { first token is a holder }
      for I := 1 to Parts.Count - 1 do
        if Parts[I] <> '' then
          Held.Add(Parts[I]);                    { remaining tokens are held }
    end;
    Parts.Free;
  end;
  CloseFile(F);

  for I := 0 to Holders.Count - 1 do
    if Held.IndexOf(Holders[I]) = -1 then
      Writeln(Holders[I]);

  Holders.Free;
  Held.Free;
end.
