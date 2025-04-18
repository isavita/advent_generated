
program Day5Asteroids;
{$mode objfpc}{$H+}
uses
  SysUtils;

type
  TIntMem = array of LongInt;

var
  mem: TIntMem;
  ip: LongInt;

// Fetch a parameter according to its mode: 0 = position, 1 = immediate
function GetParam(idx: LongInt; mode: Integer): LongInt;
begin
  if mode = 0 then
    Result := mem[mem[idx]]
  else
    Result := mem[idx];
end;

var
  line: string;
  token: string;
  p, count: Integer;
  instr, opcode, m1, m2, m3: Integer;
  a, b, dest: LongInt;
begin
  // 1) Read and parse input.txt into mem[]
  AssignFile(Input, 'input.txt');
  Reset(Input);
  if not EOF(Input) then
    ReadLn(line);
  CloseFile(Input);

  count := 0;
  while line <> '' do
  begin
    p := Pos(',', line);
    if p > 0 then
      token := Copy(line, 1, p - 1)
    else
      token := line;
    SetLength(mem, count + 1);
    mem[count] := StrToInt(token);
    Inc(count);
    if p > 0 then
      Delete(line, 1, p)
    else
      line := '';
  end;

  // 2) Execute Intcode
  ip := 0;
  while True do
  begin
    instr := mem[ip];
    opcode := instr mod 100;
    m1 := (instr div 100) mod 10;
    m2 := (instr div 1000) mod 10;
    m3 := (instr div 10000) mod 10;

    case opcode of
      1:  // add
        begin
          a := GetParam(ip + 1, m1);
          b := GetParam(ip + 2, m2);
          dest := mem[ip + 3]; // always position mode for writes
          mem[dest] := a + b;
          ip := ip + 4;
        end;
      2:  // multiply
        begin
          a := GetParam(ip + 1, m1);
          b := GetParam(ip + 2, m2);
          dest := mem[ip + 3];
          mem[dest] := a * b;
          ip := ip + 4;
        end;
      3:  // input (we always feed it '1')
        begin
          dest := mem[ip + 1];
          mem[dest] := 1;
          ip := ip + 2;
        end;
      4:  // output
        begin
          a := GetParam(ip + 1, m1);
          Writeln(a);
          ip := ip + 2;
        end;
      5:  // jump-if-true
        begin
          a := GetParam(ip + 1, m1);
          b := GetParam(ip + 2, m2);
          if a <> 0 then
            ip := b
          else
            ip := ip + 3;
        end;
      6:  // jump-if-false
        begin
          a := GetParam(ip + 1, m1);
          b := GetParam(ip + 2, m2);
          if a = 0 then
            ip := b
          else
            ip := ip + 3;
        end;
      7:  // less than
        begin
          a := GetParam(ip + 1, m1);
          b := GetParam(ip + 2, m2);
          dest := mem[ip + 3];
          if a < b then
            mem[dest] := 1
          else
            mem[dest] := 0;
          ip := ip + 4;
        end;
      8:  // equals
        begin
          a := GetParam(ip + 1, m1);
          b := GetParam(ip + 2, m2);
          dest := mem[ip + 3];
          if a = b then
            mem[dest] := 1
          else
            mem[dest] := 0;
          ip := ip + 4;
        end;
      99:  // halt
        Break;
    else
      begin
        Writeln(ErrOutput, 'Error: unknown opcode ', opcode, ' at position ', ip);
        Halt(1);
      end;
    end;
  end;
end.
