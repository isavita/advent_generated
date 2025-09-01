program Duet;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

const
  MAX_INSTRUCTIONS = 100;
  MAX_LINE_LEN = 256;
  QUEUE_SIZE = 100000;
  NUM_REGISTERS = 26;

type
  TOpCode = (opSND, opSET, opADD, opMUL, opMOD, opRCV, opJGZ);
  TArgument = record
    isReg: Boolean;
    val: Int64;
  end;
  TInstruction = record
    op: TOpCode;
    arg1, arg2: TArgument;
  end;
  TQueue = record
    data: array[0..QUEUE_SIZE-1] of Int64;
    head, tail, cnt: Integer;
  end;

var
  instructions: array[0..MAX_INSTRUCTIONS-1] of TInstruction;
  instrCount: Integer = 0;
  queue0, queue1: TQueue;
  registers: array[0..1, 0..NUM_REGISTERS-1] of Int64;
  pc: array[0..1] of Integer = (0,0);
  waiting, terminated: array[0..1] of Boolean;
  sendCount1: Int64 = 0;

procedure QueueInit(var q: TQueue);
begin
  q.head := 0;
  q.tail := 0;
  q.cnt := 0;
end;

function QueueEmpty(const q: TQueue): Boolean;
begin
  Result := q.cnt = 0;
end;

procedure QueuePush(var q: TQueue; v: Int64);
begin
  q.data[q.tail] := v;
  q.tail := (q.tail + 1) mod QUEUE_SIZE;
  Inc(q.cnt);
end;

function QueuePop(var q: TQueue): Int64;
begin
  Result := q.data[q.head];
  q.head := (q.head + 1) mod QUEUE_SIZE;
  Dec(q.cnt);
end;

function GetValue(arg: TArgument; prog: Integer): Int64;
begin
  if arg.isReg then
    Result := registers[prog, arg.val]
  else
    Result := arg.val;
end;

procedure ParseArgument(const s: string; var arg: TArgument);
var
  i: Integer;
begin
  if (Length(s) = 1) and (s[1] in ['a'..'z']) then
  begin
    arg.isReg := True;
    arg.val := Ord(s[1]) - Ord('a');
  end
  else
  begin
    arg.isReg := False;
    Val(s, i, i);
    arg.val := StrToInt64(s);
  end;
end;

function StrSplit(const line: string; var a,b,c: string): Integer;
var
  p1,p2: Integer;
begin
  p1 := Pos(' ', line);
  if p1 = 0 then
  begin
    a := Trim(line);
    Result := 1;
    Exit;
  end;
  a := Copy(line, 1, p1-1);
  p2 := Pos(' ', line, p1+1);
  if p2 = 0 then
  begin
    b := Trim(Copy(line, p1+1, MaxInt));
    Result := 2;
    Exit;
  end;
  b := Copy(line, p1+1, p2-p1-1);
  c := Trim(Copy(line, p2+1, MaxInt));
  Result := 3;
end;

var
  f: TextFile;
  line, cmd, s1, s2: string;
  i, scanned: Integer;
  prog, otherProg, targetReg: Integer;
  instr: TInstruction;
  recvQ: ^TQueue;
begin
  AssignFile(f, 'input.txt');
  Reset(f);
  while not Eof(f) and (instrCount < MAX_INSTRUCTIONS) do
  begin
    ReadLn(f, line);
    line := Trim(line);
    if line = '' then Continue;
    scanned := StrSplit(line, cmd, s1, s2);
    if scanned < 2 then Continue;
    instr.op := opSND;
    if cmd = 'snd' then instr.op := opSND
    else if cmd = 'set' then instr.op := opSET
    else if cmd = 'add' then instr.op := opADD
    else if cmd = 'mul' then instr.op := opMUL
    else if cmd = 'mod' then instr.op := opMOD
    else if cmd = 'rcv' then instr.op := opRCV
    else if cmd = 'jgz' then instr.op := opJGZ
    else Continue;
    ParseArgument(s1, instr.arg1);
    if scanned = 3 then ParseArgument(s2, instr.arg2)
    else begin instr.arg2.isReg := False; instr.arg2.val := 0; end;
    instructions[instrCount] := instr;
    Inc(instrCount);
  end;
  CloseFile(f);

  FillChar(registers, SizeOf(registers), 0);
  registers[1, Ord('p')-Ord('a')] := 1;
  QueueInit(queue0);
  QueueInit(queue1);
  FillChar(waiting, SizeOf(waiting), 0);
  FillChar(terminated, SizeOf(terminated), 0);

  while not ((terminated[0] and terminated[1]) or (waiting[0] and waiting[1])) do
  begin
    for prog := 0 to 1 do
    begin
      if terminated[prog] or waiting[prog] then Continue;
      while (pc[prog] >= 0) and (pc[prog] < instrCount) do
      begin
        instr := instructions[pc[prog]];
        targetReg := -1;
        if instr.arg1.isReg then targetReg := instr.arg1.val;
        case instr.op of
          opSND:
            begin
              if prog = 0 then QueuePush(queue1, GetValue(instr.arg1, 0))
              else begin QueuePush(queue0, GetValue(instr.arg1, 1)); Inc(sendCount1); end;
            end;
          opSET:
            if targetReg <> -1 then registers[prog, targetReg] := GetValue(instr.arg2, prog);
          opADD:
            if targetReg <> -1 then registers[prog, targetReg] := registers[prog, targetReg] + GetValue(instr.arg2, prog);
          opMUL:
            if targetReg <> -1 then registers[prog, targetReg] := registers[prog, targetReg] * GetValue(instr.arg2, prog);
          opMOD:
            if targetReg <> -1 then
            begin
              i := GetValue(instr.arg2, prog);
              if i <> 0 then registers[prog, targetReg] := registers[prog, targetReg] mod i
              else registers[prog, targetReg] := 0;
            end;
          opRCV:
            begin
              if prog = 0 then recvQ := @queue0 else recvQ := @queue1;
              if QueueEmpty(recvQ^) then
              begin
                waiting[prog] := True;
                Break;
              end
              else
              begin
                if targetReg <> -1 then registers[prog, targetReg] := QueuePop(recvQ^);
                waiting[prog] := False;
              end;
            end;
          opJGZ:
            if GetValue(instr.arg1, prog) > 0 then
            begin
              pc[prog] := pc[prog] + GetValue(instr.arg2, prog);
              Continue;
            end;
        end;
        Inc(pc[prog]);
        if instr.op = opSND then
        begin
          otherProg := 1 - prog;
          if waiting[otherProg] then waiting[otherProg] := False;
        end;
      end;
      if (pc[prog] < 0) or (pc[prog] >= instrCount) then terminated[prog] := True;
    end;
  end;

  WriteLn(sendCount1);
end.