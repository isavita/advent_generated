program ChronospatialComputer;
{$mode objfpc}{$H+}
uses
  SysUtils;

var
  f: Text;
  line, progStr: string;
  A, B, C: Int64;
  prog: array of Integer;
  ip, i, op, v, lst, lenP: Integer;
  outStr: string;

function Pow2(e: Integer): Int64;
var
  j: Integer;
begin
  Result := 1;
  for j := 1 to e do
    Result := Result * 2;
end;

function ComboValue(operand: Integer): Int64;
begin
  case operand of
    0..3: Result := operand;
    4:    Result := A;
    5:    Result := B;
    6:    Result := C;
  else
    Result := 0; // operand 7 won't appear
  end;
end;

begin
  // Read input from file
  AssignFile(f, 'input.txt');
  Reset(f);
  A := 0; B := 0; C := 0;
  progStr := '';
  while not EOF(f) do
  begin
    ReadLn(f, line);
    line := Trim(line);
    if line = '' then Continue;
    if Pos('Register A:', line) = 1 then
      A := StrToInt(Trim(Copy(line, Pos(':', line) + 1)))
    else if Pos('Register B:', line) = 1 then
      B := StrToInt(Trim(Copy(line, Pos(':', line) + 1)))
    else if Pos('Register C:', line) = 1 then
      C := StrToInt(Trim(Copy(line, Pos(':', line) + 1)))
    else if Pos('Program:', line) = 1 then
      progStr := Trim(Copy(line, Pos(':', line) + 1));
  end;
  CloseFile(f);

  // Parse program numbers into array
  lenP := 0;
  i := 1;
  while i <= Length(progStr) do
  begin
    // skip spaces
    while (i <= Length(progStr)) and (progStr[i] = ' ') do Inc(i);
    if i > Length(progStr) then Break;
    // read a number
    lst := i;
    while (i <= Length(progStr)) and (progStr[i] in ['0'..'9']) do Inc(i);
    Inc(lenP);
    SetLength(prog, lenP);
    prog[lenP - 1] := StrToInt(Copy(progStr, lst, i - lst));
    // skip comma
    if (i <= Length(progStr)) and (progStr[i] = ',') then Inc(i);
  end;

  // Simulate
  outStr := '';
  ip := 0;
  while ip + 1 < lenP do
  begin
    op := prog[ip];
    v  := prog[ip + 1];
    case op of
      0: begin
           // adv: A := A div 2^combo(v)
           A := A div Pow2( Integer(ComboValue(v)) );
           ip := ip + 2;
         end;
      1: begin
           // bxl: B := B xor literal(v)
           B := B xor v;
           ip := ip + 2;
         end;
      2: begin
           // bst: B := combo(v) mod 8
           B := ComboValue(v) mod 8;
           ip := ip + 2;
         end;
      3: begin
           // jnz: if A<>0 then ip:=literal(v) else ip+=2
           if A <> 0 then
             ip := v
           else
             ip := ip + 2;
         end;
      4: begin
           // bxc: B := B xor C
           B := B xor C;
           ip := ip + 2;
         end;
      5: begin
           // out: output combo(v) mod 8
           lst := Integer(ComboValue(v) mod 8);
           if outStr <> '' then
             outStr := outStr + ',';
           outStr := outStr + IntToStr(lst);
           ip := ip + 2;
         end;
      6: begin
           // bdv: B := A div 2^combo(v)
           B := A div Pow2( Integer(ComboValue(v)) );
           ip := ip + 2;
         end;
      7: begin
           // cdv: C := A div 2^combo(v)
           C := A div Pow2( Integer(ComboValue(v)) );
           ip := ip + 2;
         end;
    else
      // invalid opcode: halt
      Break;
    end;
  end;

  // Print the collected output
  WriteLn(outStr);
end.