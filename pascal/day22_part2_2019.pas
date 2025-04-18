
program SlamShuffle;
{$mode objfpc}{$H+}
uses
  Classes, SysUtils;

const
  M1 = 10007;
  M2 = 119315717514047;
  REPEATS = 101741582076661;

type
  TStringArray = array of string;

procedure LoadInstructions(var instr: TStringArray);
var
  f: TextFile;
  line: string;
begin
  AssignFile(f, 'input.txt');
  Reset(f);
  instr := nil;
  while not Eof(f) do
  begin
    ReadLn(f, line);
    if line <> '' then
    begin
      SetLength(instr, Length(instr) + 1);
      instr[High(instr)] := line;
    end;
  end;
  CloseFile(f);
end;

// Safe (a*b) mod m avoiding 128-bit overflow
function MulMod(a, b, m: Int64): Int64;
var
  res: Int64;
begin
  a := a mod m;
  b := b mod m;
  res := 0;
  while b > 0 do
  begin
    if Odd(b) then
    begin
      res := res + a;
      if res >= m then res := res - m;
    end;
    a := a shl 1;
    if a >= m then a := a - m;
    b := b shr 1;
  end;
  Result := res;
end;

// fast exponentiation mod m
function PowMod(a, e, m: Int64): Int64;
begin
  Result := 1 mod m;
  a := a mod m;
  while e > 0 do
  begin
    if Odd(e) then
      Result := MulMod(Result, a, m);
    a := MulMod(a, a, m);
    e := e shr 1;
  end;
end;

// extended GCD
function ExtGCD(a, b: Int64; out x, y: Int64): Int64;
var
  x1, y1, g: Int64;
begin
  if b = 0 then
  begin
    x := 1; y := 0;
    Exit(a);
  end;
  g := ExtGCD(b, a mod b, x1, y1);
  x := y1;
  y := x1 - (a div b) * y1;
  Result := g;
end;

// modular inverse of a mod m (gcd=1)
function InvMod(a, m: Int64): Int64;
var
  x, y, g: Int64;
begin
  g := ExtGCD(a mod m + m, m, x, y);
  // assume g=1
  Result := (x mod m + m) mod m;
end;

// Compose the full shuffle as f(x)=a*x + b  (mod m)
procedure BuildAffine(const instr: TStringArray; m: Int64; out a, b: Int64);
var
  line: string;
  N, i: Integer;
  v: Int64;
begin
  a := 1;  // f(x)=x
  b := 0;
  for i := 0 to High(instr) do
  begin
    line := instr[i];
    if Copy(line, 1, 19) = 'deal with increment' then
    begin
      N := StrToInt(Copy(line, 20, MaxInt));
      // g(x)=N*x
      a := MulMod(a, N, m);
      b := MulMod(b, N, m);
    end
    else if Copy(line, 1, 3) = 'cut' then
    begin
      N := StrToInt(Copy(line, 5, MaxInt));
      // g(x)=x-N
      b := (b - N) mod m;
      if b < 0 then b := b + m;
    end
    else // "deal into new stack"
    begin
      // g(x) = -x-1
      a := (-a) mod m; if a < 0 then a := a + m;
      b := (-b - 1) mod m; if b < 0 then b := b + m;
    end;
  end;
end;

var
  instr: TStringArray;
  a1, b1, a2, b2: Int64;
  ans1, ans2: Int64;
  A, B, tmp, invA: Int64;
begin
  // load all shuffle lines
  LoadInstructions(instr);

  // Part 1: deck size=10007, find final position of card 2019
  BuildAffine(instr, M1, a1, b1);
  ans1 := (MulMod(a1, 2019, M1) + b1) mod M1;
  WriteLn('Part 1: position of card 2019 is ', ans1);

  // Part 2: deck size=M2, repeated REPEATS times, find card at position 2020
  BuildAffine(instr, M2, a2, b2);
  // f(x)=a2*x + b2 mod M2
  // f^N(x) = A*x + B
  A := PowMod(a2, REPEATS, M2);
  if a2 = 1 then
    B := MulMod(b2, REPEATS mod M2, M2)
  else
  begin
    tmp := (A - 1) mod M2;
    if tmp < 0 then tmp := tmp + M2;
    B := MulMod(b2, MulMod(tmp, InvMod(a2 - 1, M2), M2), M2);
  end;
  // we want the card at final position Y=2020 => solve x in A*x + B ≡ Y
  invA := InvMod(A, M2);
  ans2 := MulMod((2020 - B + M2) mod M2, invA, M2);
  WriteLn('Part 2: card at position 2020 is ', ans2);
end.
