
{$MODE OBJFPC}{$H+} // Use Object Pascal mode for features like Trim, Val

program AssembunnySimulator;

uses
  SysUtils; // For Val, Trim

const
  MAX_INSTRUCTIONS = 50; // Adjust if needed

type
  RegisterName = 'a'..'d';
  Registers = array[RegisterName] of Integer;
  Instruction = String;
  InstructionList = array[1..MAX_INSTRUCTIONS] of Instruction;

var
  regs: Registers;
  instructions: InstructionList;
  numInstructions: Integer;
  ip: Integer;
  inputFile: TextFile;
  line: String;
  op, arg1, arg2: String;
  p1, p2, code, valInt, jumpCond, offset: Integer;
  nextIp: Integer;
  targetReg: Char;


function GetValue(const currentRegs: Registers; const s: String): Integer;
var
  localVal: Integer;
  localCode: Integer;
begin
  if (Length(s) = 1) and (s[1] in ['a'..'d']) then
  begin
    Result := currentRegs[s[1]];
  end
  else
  begin
    Val(s, localVal, localCode);
    if localCode <> 0 then
    begin
       // Handle potential error, though problem implies valid input
       // For simplicity, assume valid conversion or return 0
       Result := 0;
    end
    else
    begin
      Result := localVal;
    end;
  end;
end;

procedure ParseLine(const fullLine: String; var outOp, outArg1, outArg2: String);
var
  space1Pos, space2Pos: Integer;
  trimmedLine: String;
begin
  outOp := '';
  outArg1 := '';
  outArg2 := '';
  trimmedLine := Trim(fullLine);
  space1Pos := Pos(' ', trimmedLine);

  if space1Pos > 0 then
  begin
    outOp := Copy(trimmedLine, 1, space1Pos - 1);
    space2Pos := Pos(' ', Copy(trimmedLine, space1Pos + 1, Length(trimmedLine)));

    if space2Pos > 0 then // Three parts (cpy, jnz)
    begin
      outArg1 := Copy(trimmedLine, space1Pos + 1, space2Pos - 1);
      outArg2 := Copy(trimmedLine, space1Pos + space2Pos + 1, Length(trimmedLine));
    end
    else // Two parts (inc, dec)
    begin
      outArg1 := Copy(trimmedLine, space1Pos + 1, Length(trimmedLine));
    end;
  end;
  // Implicitly handles single-word lines as error case (op='', arg1='', arg2='')
end;


begin
  regs['a'] := 0;
  regs['b'] := 0;
  regs['c'] := 0;
  regs['d'] := 0;

  Assign(inputFile, 'input.txt');
  try
    Reset(inputFile);
    numInstructions := 0;
    while not Eof(inputFile) and (numInstructions < MAX_INSTRUCTIONS) do
    begin
      Inc(numInstructions);
      ReadLn(inputFile, instructions[numInstructions]);
    end;
  finally
    Close(inputFile);
  end;

  ip := 1;
  while (ip >= 1) and (ip <= numInstructions) do
  begin
    nextIp := ip + 1; // Assume default next instruction

    ParseLine(instructions[ip], op, arg1, arg2);

    if op = 'cpy' then
    begin
      if (Length(arg2) = 1) and (arg2[1] in ['a'..'d']) then
      begin
         targetReg := arg2[1];
         regs[targetReg] := GetValue(regs, arg1);
      end;
    end
    else if op = 'inc' then
    begin
       if (Length(arg1) = 1) and (arg1[1] in ['a'..'d']) then
       begin
          targetReg := arg1[1];
          Inc(regs[targetReg]);
       end;
    end
    else if op = 'dec' then
    begin
       if (Length(arg1) = 1) and (arg1[1] in ['a'..'d']) then
       begin
          targetReg := arg1[1];
          Dec(regs[targetReg]);
       end;
    end
    else if op = 'jnz' then
    begin
      jumpCond := GetValue(regs, arg1);
      if jumpCond <> 0 then
      begin
         // arg2 for jnz is always a value (offset)
         Val(arg2, offset, code);
         if code = 0 then // Check for valid conversion
           nextIp := ip + offset;
         // Else: handle error or assume valid input leads to code=0
      end;
    end;

    ip := nextIp;
  end;

  WriteLn(regs['a']);
end.
