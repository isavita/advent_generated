program PassportProcessing;
{$mode objfpc}{$h+}
uses
  SysUtils, Classes;

var
  line, cur: string;
  count1, count2: Integer;

function validBYR(const s: string): Boolean;
var
  y: Integer;
begin
  Result := (Length(s) = 4) and TryStrToInt(s, y) and (y >= 1920) and (y <= 2002);
end;

function validIYR(const s: string): Boolean;
var
  y: Integer;
begin
  Result := (Length(s) = 4) and TryStrToInt(s, y) and (y >= 2010) and (y <= 2020);
end;

function validEYR(const s: string): Boolean;
var
  y: Integer;
begin
  Result := (Length(s) = 4) and TryStrToInt(s, y) and (y >= 2020) and (y <= 2030);
end;

function validHGT(const s: string): Boolean;
var
  num: Integer;
  unitPart: string;
begin
  Result := False;
  if Length(s) < 3 then Exit;
  unitPart := Copy(s, Length(s)-1, 2);
  if not TryStrToInt(Copy(s, 1, Length(s)-2), num) then Exit;
  if (unitPart = 'cm') then
    Result := (num >= 150) and (num <= 193)
  else if (unitPart = 'in') then
    Result := (num >= 59) and (num <= 76)
  else
    Result := False;
end;

function validHCL(const s: string): Boolean;
var
  i: Integer;
begin
  Result := (Length(s) = 7) and (s[1] = '#');
  if not Result then Exit;
  for i := 2 to 7 do
    if not (s[i] in ['0'..'9','a'..'f']) then
    begin
      Result := False;
      Exit;
    end;
end;

function validECL(const s: string): Boolean;
const
  ECLs: array[0..6] of string = ('amb','blu','brn','gry','grn','hzl','oth');
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(ECLs) do
    if s = ECLs[i] then
    begin
      Result := True;
      Exit;
    end;
end;

function validPID(const s: string): Boolean;
var
  i: Integer;
begin
  Result := Length(s) = 9;
  if not Result then Exit;
  for i := 1 to 9 do
    if not (s[i] in ['0'..'9']) then
    begin
      Result := False;
      Exit;
    end;
end;

procedure ProcessPassport(const pass: string);
var
  tokens: TStringList;
  i, p: Integer;
  kv, key, val: string;
  hasBYR, hasIYR, hasEYR, hasHGT, hasHCL, hasECL, hasPID: Boolean;
begin
  hasBYR := False; hasIYR := False; hasEYR := False;
  hasHGT := False; hasHCL := False; hasECL := False; hasPID := False;

  tokens := TStringList.Create;
  try
    tokens.Delimiter := ' ';
    tokens.StrictDelimiter := True;
    tokens.DelimitedText := Trim(pass);
    for i := 0 to tokens.Count - 1 do
    begin
      kv := tokens[i];
      p := Pos(':', kv);
      if p = 0 then Continue;
      key := Copy(kv, 1, p - 1);
      val := Copy(kv, p + 1, MaxInt);
      if key = 'byr' then begin hasBYR := True; end
      else if key = 'iyr' then begin hasIYR := True; end
      else if key = 'eyr' then begin hasEYR := True; end
      else if key = 'hgt' then begin hasHGT := True; end
      else if key = 'hcl' then begin hasHCL := True; end
      else if key = 'ecl' then begin hasECL := True; end
      else if key = 'pid' then begin hasPID := True; end;
    end;
  finally
    tokens.Free;
  end;

  if hasBYR and hasIYR and hasEYR and hasHGT and hasHCL and hasECL and hasPID then
  begin
    Inc(count1);
    // Now re-parse to retrieve actual values for validation
    tokens := TStringList.Create;
    try
      tokens.Delimiter := ' ';
      tokens.StrictDelimiter := True;
      tokens.DelimitedText := Trim(pass);
      for i := 0 to tokens.Count - 1 do
      begin
        kv := tokens[i];
        p := Pos(':', kv);
        key := Copy(kv, 1, p - 1);
        val := Copy(kv, p + 1, MaxInt);
        if key = 'byr' then hasBYR := validBYR(val)
        else if key = 'iyr' then hasIYR := validIYR(val)
        else if key = 'eyr' then hasEYR := validEYR(val)
        else if key = 'hgt' then hasHGT := validHGT(val)
        else if key = 'hcl' then hasHCL := validHCL(val)
        else if key = 'ecl' then hasECL := validECL(val)
        else if key = 'pid' then hasPID := validPID(val);
      end;
    finally
      tokens.Free;
    end;
    if hasBYR and hasIYR and hasEYR and hasHGT and hasHCL and hasECL and hasPID then
      Inc(count2);
  end;
end;

begin
  // Read from input.txt
  Assign(Input, 'input.txt');
  Reset(Input);

  count1 := 0;
  count2 := 0;
  cur := '';

  while not Eof(Input) do
  begin
    ReadLn(Input, line);
    if Trim(line) = '' then
    begin
      ProcessPassport(cur);
      cur := '';
    end
    else
    begin
      if cur = '' then cur := line
      else cur := cur + ' ' + line;
    end;
  end;
  // Process last passport if any
  if cur <> '' then
    ProcessPassport(cur);

  // Output results
  WriteLn(count1);
  WriteLn(count2);
end.
