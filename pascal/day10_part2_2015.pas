program LookAndSay;

{$mode objfpc}{$H+}

uses
  SysUtils;

function NextSequence(const s: string): string;
var
  n, i, j, count, k: Integer;
  current: Char;
  cs: string;
begin
  n := Length(s);
  if n = 0 then
    Exit('');
  // Reserve up to twice the length to avoid frequent reallocations
  SetLength(Result, n * 2);
  j := 1;
  count := 1;
  current := s[1];
  // Process runs in the string
  for i := 2 to n do
  begin
    if s[i] = current then
      Inc(count)
    else
    begin
      cs := IntToStr(count);
      for k := 1 to Length(cs) do
      begin
        Result[j] := cs[k];
        Inc(j);
      end;
      Result[j] := current;
      Inc(j);
      current := s[i];
      count := 1;
    end;
  end;
  // Append the final run
  cs := IntToStr(count);
  for k := 1 to Length(cs) do
  begin
    Result[j] := cs[k];
    Inc(j);
  end;
  Result[j] := current;
  Inc(j);
  SetLength(Result, j - 1);
end;

var
  fin: TextFile;
  inputStr, seq: string;
  i: Integer;
begin
  // Read the initial sequence from input.txt
  AssignFile(fin, 'input.txt');
  Reset(fin);
  try
    ReadLn(fin, inputStr);
  finally
    CloseFile(fin);
  end;
  inputStr := Trim(inputStr);
  seq := inputStr;

  // Apply look-and-say 40 times for Part 1
  for i := 1 to 40 do
    seq := NextSequence(seq);
  WriteLn(Length(seq));

  // Continue 10 more times (total 50) for Part 2
  for i := 1 to 10 do
    seq := NextSequence(seq);
  WriteLn(Length(seq));
end.
