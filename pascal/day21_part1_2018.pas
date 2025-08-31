
program OpcodeComputer;

{$mode objfpc}{$H+}
uses
  SysUtils;

type
  TInstruction = record
    OpCode : string[4];
    A,B,C : Integer;
  end;

const
  MaxInstr = 50;

var
  Instructions : array[0..MaxInstr-1] of TInstruction;
  Regs : array[0..5] of Integer;
  IPReg, InstrCount : Integer;

function OpId(const Op : string) : Integer;
begin
  case Op of
    'addr': Result:=0;
    'addi': Result:=1;
    'mulr': Result:=2;
    'muli': Result:=3;
    'banr': Result:=4;
    'bani': Result:=5;
    'borr': Result:=6;
    'bori': Result:=7;
    'setr': Result:=8;
    'seti': Result:=9;
    'gtir': Result:=10;
    'gtri': Result:=11;
    'gtrr': Result:=12;
    'eqir': Result:=13;
    'eqri': Result:=14;
    'eqrr': Result:=15;
  else
    Result:=-1;
  end;
end;

procedure Exec(id : Integer; const A,B,C : Integer);
begin
  case id of
    0: Regs[C] := Regs[A] + Regs[B];
    1: Regs[C] := Regs[A] + B;
    2: Regs[C] := Regs[A] * Regs[B];
    3: Regs[C] := Regs[A] * B;
    4: Regs[C] := Regs[A] and Regs[B];
    5: Regs[C] := Regs[A] and B;
    6: Regs[C] := Regs[A] or Regs[B];
    7: Regs[C] := Regs[A] or B;
    8: Regs[C] := Regs[A];
    9: Regs[C] := A;
   10: Regs[C] := Ord(A > Regs[B]);
   11: Regs[C] := Ord(Regs[A] > B);
   12: Regs[C] := Ord(Regs[A] > Regs[B]);
   13: Regs[C] := Ord(A = Regs[B]);
   14: Regs[C] := Ord(Regs[A] = B);
   15: Regs[C] := Ord(Regs[A] = Regs[B]);
  end;
end;

function Tick : Boolean;
var
  idx : Integer;
  id  : Integer;
begin
  idx := Regs[IPReg];
  if (idx < 0) or (idx >= InstrCount) then
    Exit(True);
  id := OpId(Instructions[idx].OpCode);
  Exec(id, Instructions[idx].A, Instructions[idx].B, Instructions[idx].C);
  Inc(Regs[IPReg]);
  Result := Regs[IPReg] >= InstrCount;
end;

function Solve : Integer;
begin
  while not Tick do
    if Regs[IPReg] = 28 then Break;
  Result := Regs[5];
end;

var
  f : TextFile;
  line : string;
  parts : TStringArray;
  i : Integer;
begin
  AssignFile(f,'input.txt');
  Reset(f);
  ReadLn(f,line);
  IPReg := StrToInt(Copy(line,5,Length(line)-4));
  InstrCount := 0;
  while not Eof(f) do
  begin
    ReadLn(f,line);
    if line='' then Continue;
    parts := line.Split([' ']);
    Instructions[InstrCount].OpCode := Copy(parts[0],1,4);
    Instructions[InstrCount].A := StrToInt(parts[1]);
    Instructions[InstrCount].B := StrToInt(parts[2]);
    Instructions[InstrCount].C := StrToInt(parts[3]);
    Inc(InstrCount);
  end;
  CloseFile(f);
  FillChar(Regs,SizeOf(Regs),0);
  WriteLn(Solve);
end.
