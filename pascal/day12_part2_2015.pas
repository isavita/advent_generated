
program JsonSum;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

function IsDigit(c: Char): Boolean;
begin
  Result := c in ['0'..'9'];
end;

function CalculateSumPart1(const s: string): Int64;
var
  i, j, n: Integer;
  num: Int64;
begin
  Result := 0;
  i := 1;
  n := Length(s);
  while i <= n do
  begin
    if (s[i] = '-') or IsDigit(s[i]) then
    begin
      j := i;
      if s[j] = '-' then Inc(j);
      while (j <= n) and IsDigit(s[j]) do Inc(j);
      if TryStrToInt64(Copy(s, i, j - i), num) then
        Result := Result + num;
      i := j;
    end
    else Inc(i);
  end;
end;

procedure SkipWhite(var p: Integer; const s: string);
begin
  while (p <= Length(s)) and (s[p] <= ' ') do Inc(p);
end;

function ParseStringAndCheckRed(var p: Integer; const s: string): Boolean;
var
  startPos: Integer;
begin
  Result := False;
  if s[p] <> '"' then Exit;
  Inc(p);
  startPos := p;
  while (p <= Length(s)) and (s[p] <> '"') do
  begin
    if s[p] = '\' then
    begin
      Inc(p);
      if p > Length(s) then Exit;
    end;
    Inc(p);
  end;
  if (p - startPos = 3) and (Copy(s, startPos, 3) = 'red') then
    Result := True;
  if (p <= Length(s)) and (s[p] = '"') then Inc(p);
end;

function ParseNumber(var p: Integer; const s: string): Int64;
var
  startPos: Integer;
begin
  startPos := p;
  if s[p] = '-' then Inc(p);
  while (p <= Length(s)) and IsDigit(s[p]) do Inc(p);
  TryStrToInt64(Copy(s, startPos, p - startPos), Result);
end;

function ParseValue(var p: Integer; const s: string; ignoreRed: Boolean): Int64; forward;
function ParseArray(var p: Integer; const s: string; ignoreRed: Boolean): Int64;
var
  sum: Int64;
begin
  sum := 0;
  Inc(p);
  SkipWhite(p, s);
  if (p <= Length(s)) and (s[p] = ']') then
  begin
    Inc(p);
    Exit(0);
  end;
  while p <= Length(s) do
  begin
    sum := sum + ParseValue(p, s, ignoreRed);
    SkipWhite(p, s);
    if (p <= Length(s)) and (s[p] = ']') then
    begin
      Inc(p);
      Break;
    end;
    if (p <= Length(s)) and (s[p] = ',') then Inc(p) else Break;
    SkipWhite(p, s);
  end;
  Result := sum;
end;

function ParseObject(var p: Integer; const s: string; ignoreRed: Boolean): Int64;
var
  sum, val: Int64;
  containsRed: Boolean;
begin
  sum := 0;
  containsRed := False;
  Inc(p);
  SkipWhite(p, s);
  if (p <= Length(s)) and (s[p] = '}') then
  begin
    Inc(p);
    Exit(0);
  end;
  while p <= Length(s) do
  begin
    SkipWhite(p, s);
    ParseStringAndCheckRed(p, s);
    SkipWhite(p, s);
    if (p <= Length(s)) and (s[p] = ':') then Inc(p) else Break;
    SkipWhite(p, s);
    if ignoreRed and (p <= Length(s)) and (s[p] = '"') then
    begin
      if (p + 4 <= Length(s)) and (Copy(s, p + 1, 3) = 'red') and (s[p + 4] = '"') then
        containsRed := True;
    end;
    val := ParseValue(p, s, ignoreRed);
    if not containsRed then sum := sum + val;
    SkipWhite(p, s);
    if (p <= Length(s)) and (s[p] = '}') then
    begin
      Inc(p);
      Break;
    end;
    if (p <= Length(s)) and (s[p] = ',') then Inc(p) else Break;
    SkipWhite(p, s);
  end;
  if ignoreRed and containsRed then Result := 0 else Result := sum;
end;

function ParseValue(var p: Integer; const s: string; ignoreRed: Boolean): Int64;
begin
  SkipWhite(p, s);
  if p > Length(s) then Exit(0);
  case s[p] of
    '{': Result := ParseObject(p, s, ignoreRed);
    '[': Result := ParseArray(p, s, ignoreRed);
    '"': begin ParseStringAndCheckRed(p, s); Result := 0; end;
    '-','0'..'9': Result := ParseNumber(p, s);
    else
      if Copy(s, p, 4) = 'true' then Inc(p, 4)
      else if Copy(s, p, 5) = 'false' then Inc(p, 5)
      else if Copy(s, p, 4) = 'null' then Inc(p, 4);
      Result := 0;
  end;
end;

function CalculateSumPart2(const s: string): Int64;
var
  pos: Integer;
begin
  pos := 1;
  Result := ParseValue(pos, s, True);
end;

var
  input: TStringList;
  data: string;
  sum1, sum2: Int64;
begin
  input := TStringList.Create;
  try
    input.LoadFromFile('input.txt');
    data := input.Text;
  finally
    input.Free;
  end;
  sum1 := CalculateSumPart1(data);
  sum2 := CalculateSumPart2(data);
  WriteLn('Part 1 Sum: ', sum1);
  WriteLn('Part 2 Sum: ', sum2);
end.
