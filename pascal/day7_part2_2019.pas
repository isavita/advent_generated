
program AmplifierFeedback;

{$mode objfpc}{$H+}
uses
  SysUtils;

const
  NUM_AMPLIFIERS = 5;
  MAX_CODE_SIZE  = 4096;

type
  TInt64Array = array[0..MAX_CODE_SIZE-1] of Int64;

  TVM = record
    Code      : TInt64Array;
    IP        : Integer;
    Halted    : Boolean;
    InVals    : array[0..15] of Int64;  // small input buffer
    InCount   : Integer;               // total inputs stored
    InPos     : Integer;               // next to read
  end;

{--------------------  Helper functions for VM  --------------------}
function GetParam(const vm: TVM; offset, mode: Integer): Int64;
var
  addr: Int64;
begin
  addr := vm.Code[vm.IP + offset];
  if mode = 0 then
    Result := vm.Code[addr]
  else
    Result := addr;
end;

procedure VMAddInput(var vm: TVM; val: Int64);
begin
  vm.InVals[vm.InCount mod Length(vm.InVals)] := val;
  Inc(vm.InCount);
end;

function VMHasInput(const vm: TVM): Boolean;
begin
  Result := vm.InPos < vm.InCount;
end;

function VMGetInput(var vm: TVM): Int64;
begin
  Result := vm.InVals[vm.InPos mod Length(vm.InVals)];
  Inc(vm.InPos);
end;

{ Run the VM until it produces an output or halts.
  Returns True when an output was produced (output stored in OutVal),
  False when the VM halted without producing a new output. }
function VMRun(var vm: TVM; out OutVal: Int64): Boolean;
var
  instr, opcode, mode1, mode2, mode3: Int64;
  p1, p2, addr: Int64;
begin
  Result := False;
  while not vm.Halted do
  begin
    instr  := vm.Code[vm.IP];
    opcode := instr mod 100;
    mode1  := (instr div 100) mod 10;
    mode2  := (instr div 1000) mod 10;
    mode3  := (instr div 10000) mod 10; // never used for writes

    case opcode of
      1: begin { add }
           p1 := GetParam(vm, 1, mode1);
           p2 := GetParam(vm, 2, mode2);
           addr := vm.Code[vm.IP + 3];
           vm.Code[addr] := p1 + p2;
           Inc(vm.IP, 4);
         end;
      2: begin { mul }
           p1 := GetParam(vm, 1, mode1);
           p2 := GetParam(vm, 2, mode2);
           addr := vm.Code[vm.IP + 3];
           vm.Code[addr] := p1 * p2;
           Inc(vm.IP, 4);
         end;
      3: begin { input }
           if not VMHasInput(vm) then
             Exit;                     // need more input, pause execution
           addr := vm.Code[vm.IP + 1];
           vm.Code[addr] := VMGetInput(vm);
           Inc(vm.IP, 2);
         end;
      4: begin { output }
           OutVal := GetParam(vm, 1, mode1);
           Inc(vm.IP, 2);
           Result := True;             // one output produced
           Exit;
         end;
      5: begin { jump-if-true }
           p1 := GetParam(vm, 1, mode1);
           p2 := GetParam(vm, 2, mode2);
           if p1 <> 0 then vm.IP := p2 else Inc(vm.IP, 3);
         end;
      6: begin { jump-if-false }
           p1 := GetParam(vm, 1, mode1);
           p2 := GetParam(vm, 2, mode2);
           if p1 = 0 then vm.IP := p2 else Inc(vm.IP, 3);
         end;
      7: begin { less than }
           p1 := GetParam(vm, 1, mode1);
           p2 := GetParam(vm, 2, mode2);
           addr := vm.Code[vm.IP + 3];
           vm.Code[addr] := Ord(p1 < p2);
           Inc(vm.IP, 4);
         end;
      8: begin { equals }
           p1 := GetParam(vm, 1, mode1);
           p2 := GetParam(vm, 2, mode2);
           addr := vm.Code[vm.IP + 3];
           vm.Code[addr] := Ord(p1 = p2);
           Inc(vm.IP, 4);
         end;
      99: begin { halt }
            vm.Halted := True;
            Exit;
          end;
    else
      vm.Halted := True;               // unknown opcode – treat as halt
      Exit;
    end;
  end;
end;

{--------------------  Permutation helper  --------------------}
function NextPermutation(var a: array of Integer): Boolean;
var
  i, j, l, r, tmp: Integer;
begin
  i := High(a) - 1;
  while (i >= Low(a)) and (a[i] >= a[i+1]) do Dec(i);
  if i < Low(a) then
    Exit(False);                     // last permutation reached

  j := High(a);
  while a[j] <= a[i] do Dec(j);
  tmp := a[i]; a[i] := a[j]; a[j] := tmp;

  l := i + 1; r := High(a);
  while l < r do
  begin
    tmp := a[l]; a[l] := a[r]; a[r] := tmp;
    Inc(l); Dec(r);
  end;
  Result := True;
end;

{--------------------  Feedback loop execution  --------------------}
function RunFeedbackLoop(const Phase: array of Integer; const InitCode: TInt64Array; CodeSize: Integer): Int64;
var
  VMs   : array[0..NUM_AMPLIFIERS-1] of TVM;
  i, cur: Integer;
  outVal: Int64;
  lastOutput: Int64;
begin
  { initialise each VM }
  for i := 0 to NUM_AMPLIFIERS-1 do
  begin
    FillChar(VMs[i], SizeOf(VMs[i]), 0);
    Move(InitCode[0], VMs[i].Code[0], CodeSize * SizeOf(Int64));
    VMs[i].IP     := 0;
    VMs[i].Halted := False;
    VMs[i].InCount:= 0;
    VMs[i].InPos  := 0;
    VMAddInput(VMs[i], Phase[i]);      // first input = phase setting
  end;

  VMAddInput(VMs[0], 0);               // initial signal

  cur := 0;
  lastOutput := 0;
  while True do
  begin
    if not VMRun(VMs[cur], outVal) then
    begin
      { VM halted without producing output }
      if VMs[cur].Halted and (cur = NUM_AMPLIFIERS-1) then
        Break;                         // last amplifier finished
      { otherwise just move to next amplifier }
    end
    else
    begin
      lastOutput := outVal;
      VMAddInput(VMs[(cur+1) mod NUM_AMPLIFIERS], outVal);
    end;
    cur := (cur + 1) mod NUM_AMPLIFIERS;
  end;
  Result := lastOutput;
end;

{--------------------  Main program  --------------------}
var
  f          : TextFile;
  line, num  : string;
  code       : TInt64Array;
  codeSize   : Integer = 0;
  phase      : array[0..NUM_AMPLIFIERS-1] of Integer = (5,6,7,8,9);
  maxOutput  : Int64;
  curOutput  : Int64;
begin
  AssignFile(f, 'input.txt');
  {$I-} Reset(f); {$I+}
  if IOResult <> 0 then
  begin
    WriteLn('Cannot open input.txt');
    Halt(1);
  end;

  while not Eof(f) do
  begin
    ReadLn(f, line);
    while Length(line) > 0 do
    begin
      if Pos(',', line) > 0 then
      begin
        num := Copy(line, 1, Pos(',', line)-1);
        Delete(line, 1, Pos(',', line));
      end
      else
      begin
        num := line;
        line := '';
      end;
      if codeSize < MAX_CODE_SIZE then
      begin
        code[codeSize] := StrToInt64(Trim(num));
        Inc(codeSize);
      end;
    end;
  end;
  CloseFile(f);

  if codeSize = 0 then
  begin
    WriteLn('Empty program');
    Halt(1);
  end;

  maxOutput := Low(Int64);
  repeat
    curOutput := RunFeedbackLoop(phase, code, codeSize);
    if curOutput > maxOutput then
      maxOutput := curOutput;
  until not NextPermutation(phase);

  WriteLn(maxOutput);
end.
