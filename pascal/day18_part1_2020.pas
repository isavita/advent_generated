
program OperationOrder;

{$mode objfpc}{$h+}

uses
  Classes, SysUtils;

{ Evaluate an expression in s starting at position pos.
  Operators + and * have equal precedence and are evaluated strictly left to right.
  Parentheses are handled via recursion. }
function EvalExpr(const s: string; var pos: Integer): Int64;
var
  value, nextVal: Int64;
  op: Char;
begin
  { Skip any leading spaces }
  while (pos <= Length(s)) and (s[pos] = ' ') do
    Inc(pos);

  { Read first term: either a parenthesized sub‑expression or a number }
  if (pos <= Length(s)) and (s[pos] = '(') then
  begin
    Inc(pos);  { skip '(' }
    value := EvalExpr(s, pos);
    Inc(pos);  { skip ')' }
  end
  else
  begin
    value := 0;
    while (pos <= Length(s)) and (s[pos] in ['0'..'9']) do
    begin
      value := value * 10 + Ord(s[pos]) - Ord('0');
      Inc(pos);
    end;
  end;

  { Now consume zero or more “op term” sequences until end or a ')' }
  while True do
  begin
    { Skip spaces }
    while (pos <= Length(s)) and (s[pos] = ' ') do
      Inc(pos);
    if (pos > Length(s)) or (s[pos] = ')') then
      Break;

    op := s[pos];  { '+' or '*' }
    Inc(pos);

    { Skip spaces after operator }
    while (pos <= Length(s)) and (s[pos] = ' ') do
      Inc(pos);

    { Read next term }
    if (pos <= Length(s)) and (s[pos] = '(') then
    begin
      Inc(pos);
      nextVal := EvalExpr(s, pos);
      Inc(pos);
    end
    else
    begin
      nextVal := 0;
      while (pos <= Length(s)) and (s[pos] in ['0'..'9']) do
      begin
        nextVal := nextVal * 10 + Ord(s[pos]) - Ord('0');
        Inc(pos);
      end;
    end;

    { Combine left and right by the operator, strictly left‑to‑right }
    if op = '+' then
      value := value + nextVal
    else
      value := value * nextVal;
  end;

  Result := value;
end;

var
  f: TextFile;
  line: string;
  total: Int64;
  p: Integer;
begin
  AssignFile(f, 'input.txt');
  Reset(f);
  total := 0;

  while not Eof(f) do
  begin
    ReadLn(f, line);
    if line <> '' then
    begin
      p := 1;
      total := total + EvalExpr(line, p);
    end;
  end;

  CloseFile(f);
  WriteLn(total);
end.
