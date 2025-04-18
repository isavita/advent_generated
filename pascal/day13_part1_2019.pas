
program CarePackage;
{$mode objfpc}{$H+}
uses
  SysUtils;

const
  MAXMEM = 200000;

type
  TIntArr = array[0..MAXMEM-1] of Int64;

var
  mem: TIntArr;
  ip, relBase: Int64;
  blockCount: Int64;

////////////////////////////////////////////////////////////////////////
// low‐level memory access with parameter modes
function GetVal(pos, mode: Int64): Int64;
var
  addr: Int64;
begin
  case mode of
    0: begin
         addr := mem[pos];
         if (addr>=0) and (addr<MAXMEM) then
           Result := mem[addr]
         else
           Result := 0;
       end;
    1: Result := mem[pos];
    2: begin
         addr := relBase + mem[pos];
         if (addr>=0) and (addr<MAXMEM) then
           Result := mem[addr]
         else
           Result := 0;
       end;
  else
    Result := 0;
  end;
end;

procedure SetVal(pos, mode, value: Int64);
var
  addr: Int64;
begin
  case mode of
    0: addr := mem[pos];
    2: addr := relBase + mem[pos];
  else
    Exit;
  end;
  if (addr>=0) and (addr<MAXMEM) then
    mem[addr] := value;
end;

////////////////////////////////////////////////////////////////////////
// run the Intcode computer, process outputs in triples
procedure RunIntcode;
var
  opcode, m1, m2, m3: Int64;
  outPhase: Integer = 0;
  x, y, t: Int64;
  v1, v2: Int64;
begin
  while True do
  begin
    opcode := mem[ip] mod 100;
    m1 := (mem[ip] div 100) mod 10;
    m2 := (mem[ip] div 1000) mod 10;
    m3 := (mem[ip] div 10000) mod 10;

    if opcode = 99 then
      Break;

    case opcode of
      1: begin
           v1 := GetVal(ip+1, m1) + GetVal(ip+2, m2);
           SetVal(ip+3, m3, v1);
           ip := ip + 4;
         end;

      2: begin
           v1 := GetVal(ip+1, m1) * GetVal(ip+2, m2);
           SetVal(ip+3, m3, v1);
           ip := ip + 4;
         end;

      3: begin
           // no joystick input needed for part 1, feed 0
           SetVal(ip+1, m1, 0);
           ip := ip + 2;
         end;

      4: begin
           t := GetVal(ip+1, m1);
           case outPhase of
             0: x := t;
             1: y := t;
             2: begin
                  // t is tile id
                  if t = 2 then
                    Inc(blockCount);
                end;
           end;
           outPhase := (outPhase + 1) mod 3;
           ip := ip + 2;
         end;

      5: begin
           if GetVal(ip+1, m1) <> 0 then
             ip := GetVal(ip+2, m2)
           else
             ip := ip + 3;
         end;

      6: begin
           if GetVal(ip+1, m1) = 0 then
             ip := GetVal(ip+2, m2)
           else
             ip := ip + 3;
         end;

      7: begin
           if GetVal(ip+1, m1) < GetVal(ip+2, m2) then
             v1 := 1
           else
             v1 := 0;
           SetVal(ip+3, m3, v1);
           ip := ip + 4;
         end;

      8: begin
           if GetVal(ip+1, m1) = GetVal(ip+2, m2) then
             v1 := 1
           else
             v1 := 0;
           SetVal(ip+3, m3, v1);
           ip := ip + 4;
         end;

      9: begin
           relBase := relBase + GetVal(ip+1, m1);
           ip := ip + 2;
         end

    else
      // unexpected opcode
      Writeln('Error: unknown opcode ', opcode);
      Break;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////
// entry point
var
  f: TextFile;
  line, token: string;
  posComma: Integer;
  idx: Integer;
  num: Int64;
  code: Integer;
begin
  // load program
  AssignFile(f, 'input.txt');
  Reset(f);
  if not Eof(f) then
  begin
    ReadLn(f, line);
    idx := 0;
    while line <> '' do
    begin
      posComma := Pos(',', line);
      if posComma > 0 then
      begin
        token := Copy(line, 1, posComma-1);
        Delete(line, 1, posComma);
      end
      else
      begin
        token := line;
        line := '';
      end;
      Val(token, mem[idx], code);
      Inc(idx);
    end;
  end;
  CloseFile(f);

  // prepare and run
  ip := 0;
  relBase := 0;
  blockCount := 0;
  RunIntcode;

  // answer: number of block tiles
  Writeln(blockCount);
end.
