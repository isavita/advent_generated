
program SensorBoost;
{$mode objfpc}{$H+}
uses
  SysUtils;

var
  mem: array of Int64;
  pc, rb: Int64;
  inputVal: Int64;
  instruction: Int64;
  m1, m2, m3: Integer;

{ Read a parameter according to its mode (0=position,1=immediate,2=relative) }
function GetParam(mode, offset: Integer): Int64;
var
  addr: Int64;
begin
  case mode of
    0: begin
         addr := mem[pc + offset];
         GetParam := mem[addr];
       end;
    1: GetParam := mem[pc + offset];
    2: begin
         addr := rb + mem[pc + offset];
         GetParam := mem[addr];
       end;
  else
    WriteLn('Invalid read mode ', mode); Halt(1);
  end;
end;

{ Compute the target address for a write parameter (mode 0 or 2) }
function GetAddr(mode, offset: Integer): Int64;
begin
  case mode of
    0: GetAddr := mem[pc + offset];
    2: GetAddr := rb + mem[pc + offset];
  else
    WriteLn('Invalid write mode ', mode); Halt(1);
  end;
end;

var
  f: Text;
  line: string;
  parts: array of string;
  i, j, nProg: Integer;
begin
  {--- Load and parse input.txt ---}
  Assign(f, 'input.txt');
  Reset(f);
  ReadLn(f, line);
  Close(f);

  { split by commas }
  parts := nil;
  i := 1;
  while i <= Length(line) do
  begin
    j := i;
    while (j <= Length(line)) and (line[j] <> ',') do Inc(j);
    SetLength(parts, Length(parts) + 1);
    parts[High(parts)] := Copy(line, i, j - i);
    i := j + 1;
  end;

  { initialize memory to program + extra zeros }
  nProg := Length(parts);
  SetLength(mem, nProg + 10000);
  for i := 0 to nProg - 1 do
    mem[i] := StrToInt64(parts[i]);
  for i := nProg to High(mem) do
    mem[i] := 0;

  {--- Run Intcode with relative base support ---}
  pc := 0;
  rb := 0;
  inputVal := 2;  { sensor-boost mode }

  while True do
  begin
    instruction := mem[pc];
    m1 := (instruction div   100) mod 10;
    m2 := (instruction div  1000) mod 10;
    m3 := (instruction div 10000) mod 10;

    case instruction mod 100 of
      1: { add }
        begin
          mem[GetAddr(m3,3)] := GetParam(m1,1) + GetParam(m2,2);
          pc += 4;
        end;
      2: { multiply }
        begin
          mem[GetAddr(m3,3)] := GetParam(m1,1) * GetParam(m2,2);
          pc += 4;
        end;
      3: { input }
        begin
          mem[GetAddr(m1,1)] := inputVal;
          pc += 2;
        end;
      4: { output }
        begin
          WriteLn(GetParam(m1,1));
          pc += 2;
        end;
      5: { jump-if-true }
        if GetParam(m1,1) <> 0 then
          pc := GetParam(m2,2)
        else
          pc += 3;
      6: { jump-if-false }
        if GetParam(m1,1) = 0 then
          pc := GetParam(m2,2)
        else
          pc += 3;
      7: { less than }
        begin
          if GetParam(m1,1) < GetParam(m2,2) then
            mem[GetAddr(m3,3)] := 1
          else
            mem[GetAddr(m3,3)] := 0;
          pc += 4;
        end;
      8: { equals }
        begin
          if GetParam(m1,1) = GetParam(m2,2) then
            mem[GetAddr(m3,3)] := 1
          else
            mem[GetAddr(m3,3)] := 0;
          pc += 4;
        end;
      9: { adjust relative base }
        begin
          rb += GetParam(m1,1);
          pc += 2;
        end;
      99: { halt }
        Break;
    else
      WriteLn('Unknown opcode at ', pc, ': ', instruction);
      Halt(1);
    end;
  end;
end.
