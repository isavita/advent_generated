
program BridgeRepair;
{$mode objfpc}
uses
  SysUtils;

const
  MAXN = 30;

var
  nums: array[1..MAXN] of Int64;
  pow10: array[1..MAXN] of Int64;
  n: Integer;
  target: Int64;
  allowConcat: Boolean;

{ Simple left‐to‐right DFS trying +, *, (|| if allowed) }
function dfs(pos: Integer; acc: Int64): Boolean;
var
  valNext, catVal: Int64;
begin
  if pos > n then
    Exit(acc = target);
  valNext := nums[pos];
  { try addition }
  if dfs(pos+1, acc + valNext) then
    Exit(True);
  { try multiplication }
  if dfs(pos+1, acc * valNext) then
    Exit(True);
  { try concatenation }
  if allowConcat then
    begin
      catVal := acc * pow10[pos] + valNext;
      if dfs(pos+1, catVal) then
        Exit(True);
    end;
  Result := False;
end;

{ wrapper to initialize DFS }
function canReach(useConcat: Boolean): Boolean;
begin
  allowConcat := useConcat;
  { start with the first number as the accumulator }
  Result := dfs(2, nums[1]);
end;

var
  f: Text;
  line, rest, tok: string;
  p, code: Integer;
  sum1, sum2: Int64;
  i: Integer;
begin
  Assign(f, 'input.txt');
  Reset(f);
  sum1 := 0;
  sum2 := 0;
  while not Eof(f) do
  begin
    ReadLn(f, line);
    if line = '' then Continue;
    { split target from the sequence }
    p := Pos(':', line);
    target := StrToInt64( Trim(Copy(line, 1, p-1)) );
    rest := Trim( Copy(line, p+1, MaxInt) );
    { tokenize the numbers }
    n := 0;
    while rest <> '' do
    begin
      p := Pos(' ', rest);
      if p = 0 then
      begin
        tok := rest;
        rest := '';
      end
      else
      begin
        tok := Copy(rest, 1, p-1);
        rest := Trim(Copy(rest, p+1, MaxInt));
      end;
      Inc(n);
      nums[n] := StrToInt64(tok);
    end;
    { precompute powers of ten for each nums[i] }
    for i := 1 to n do
    begin
      { count digits of nums[i] }
      pow10[i] := 1;
      tok := IntToStr(nums[i]);
      while Length(tok) > 0 do
      begin
        pow10[i] := pow10[i] * 10;
        Delete(tok, 1, 1);
      end;
    end;
    { Part 1: only + and * }
    if canReach(False) then
      sum1 := sum1 + target;
    { Part 2: +, *, and concatenation }
    if canReach(True) then
      sum2 := sum2 + target;
  end;
  Close(f);

  { output both results }
  WriteLn('Part 1 total = ', sum1);
  WriteLn('Part 2 total = ', sum2);
end.
