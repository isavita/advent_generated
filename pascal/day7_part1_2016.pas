
program IPv7Checker;

{$MODE DELPHI}

uses
  SysUtils;

type
  TStringArray = array of string;

function HasABBA(const s: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  if Length(s) < 4 then
    Exit;

  for i := 1 to Length(s) - 3 do
  begin
    if (s[i] <> s[i + 1]) and (s[i] = s[i + 3]) and (s[i + 1] = s[i + 2]) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function SupportsTLS(const ip: string): Boolean;
var
  parts: TStringArray;
  i: Integer;
  inBrackets: Boolean;
  hasABBAOutside: Boolean;
  hasABBAInside: Boolean;
  currentPart: string;

begin
  SetLength(parts, 0);
  inBrackets := False;
  hasABBAOutside := False;
  hasABBAInside := False;
  currentPart := '';

  for i := 1 to Length(ip) do
  begin
    if ip[i] = '[' then
    begin
      SetLength(parts, Length(parts) + 1);
      parts[Length(parts) - 1] := currentPart;
      currentPart := '';
      inBrackets := True;
    end
    else if ip[i] = ']' then
    begin
      SetLength(parts, Length(parts) + 1);
      parts[Length(parts) - 1] := currentPart;
      currentPart := '';
      inBrackets := False;
    end
    else
    begin
      currentPart := currentPart + ip[i];
    end;
  end;

  if Length(currentPart) > 0 then
  begin
    SetLength(parts, Length(parts) + 1);
    parts[Length(parts) - 1] := currentPart;
  end;


  for i := 0 to Length(parts) - 1 do
  begin
    if (i mod 2 = 0) then
    begin
        if HasABBA(parts[i]) then
          hasABBAOutside := True;
    end
    else
    begin
      if HasABBA(parts[i]) then
        hasABBAInside := True;
    end;
  end;

  Result := hasABBAOutside and not hasABBAInside;
end;


var
  inputFile: TextFile;
  ip: string;
  count: Integer;

begin
  AssignFile(inputFile, 'input.txt');
  Reset(inputFile);
  count := 0;

  while not Eof(inputFile) do
  begin
    ReadLn(inputFile, ip);
    if SupportsTLS(ip) then
      Inc(count);
  end;

  CloseFile(inputFile);
  WriteLn(count);
end.
