program ThermalDiagnostics;

{$mode objfpc}{$H+}

uses
  SysUtils;

const
  InputFile = 'input.txt';

type
  TInt64Array = array of Int64;

{ Runs the Intcode program in memory 'mem' with a single input value.
  Returns an array of all outputs produced. }
function RunIntcode(programMem: TInt64Array; inputValue: Int64): TInt64Array;
var
  ip, opcode: Integer;
  instr, pm1, pm2: Integer;
  a, b: Int64;
  outIndex: Integer;
begin
  SetLength(Result, 0);
  ip := 0;
  while True do
  begin
    instr := programMem[ip];
    opcode := instr mod 100;
    pm1 := (instr div 100) mod 10;
    pm2 := (instr div 1000) mod 10;
    case opcode of
      1: begin  { add }
           if pm1 = 0 then a := programMem[programMem[ip+1]]
           else a := programMem[ip+1];
           if pm2 = 0 then b := programMem[programMem[ip+2]]
           else b := programMem[ip+2];
           programMem[programMem[ip+3]] := a + b;
           Inc(ip, 4);
         end;
      2: begin  { multiply }
           if pm1 = 0 then a := programMem[programMem[ip+1]]
           else a := programMem[ip+1];
           if pm2 = 0 then b := programMem[programMem[ip+2]]
           else b := programMem[ip+2];
           programMem[programMem[ip+3]] := a * b;
           Inc(ip, 4);
         end;
      3: begin  { input }
           { always position mode for destination }
           programMem[programMem[ip+1]] := inputValue;
           Inc(ip, 2);
         end;
      4: begin  { output }
           if pm1 = 0 then a := programMem[programMem[ip+1]]
           else a := programMem[ip+1];
           outIndex := Length(Result);
           SetLength(Result, outIndex + 1);
           Result[outIndex] := a;
           Inc(ip, 2);
         end;
      5: begin  { jump-if-true }
           if pm1 = 0 then a := programMem[programMem[ip+1]]
           else a := programMem[ip+1];
           if pm2 = 0 then b := programMem[programMem[ip+2]]
           else b := programMem[ip+2];
           if a <> 0 then ip := b
           else Inc(ip, 3);
         end;
      6: begin  { jump-if-false }
           if pm1 = 0 then a := programMem[programMem[ip+1]]
           else a := programMem[ip+1];
           if pm2 = 0 then b := programMem[programMem[ip+2]]
           else b := programMem[ip+2];
           if a = 0 then ip := b
           else Inc(ip, 3);
         end;
      7: begin  { less than }
           if pm1 = 0 then a := programMem[programMem[ip+1]]
           else a := programMem[ip+1];
           if pm2 = 0 then b := programMem[programMem[ip+2]]
           else b := programMem[ip+2];
           if a < b then programMem[programMem[ip+3]] := 1
           else programMem[programMem[ip+3]] := 0;
           Inc(ip, 4);
         end;
      8: begin  { equals }
           if pm1 = 0 then a := programMem[programMem[ip+1]]
           else a := programMem[ip+1];
           if pm2 = 0 then b := programMem[programMem[ip+2]]
           else b := programMem[ip+2];
           if a = b then programMem[programMem[ip+3]] := 1
           else programMem[programMem[ip+3]] := 0;
           Inc(ip, 4);
         end;
      99: begin  { halt }
           Break;
         end
      else
        begin
          WriteLn('Error: Unknown opcode ', opcode, ' at position ', ip);
          Halt(1);
        end;
    end;
  end;
end;

var
  f: TextFile;
  line, token: string;
  posComma: Integer;
  initialMem: TInt64Array;
  tempMem: TInt64Array;
  outputs: TInt64Array;
  idx: Integer;
  value: Int64;
begin
  { Read the comma-separated Intcode program from input.txt }
  AssignFile(f, InputFile);
  Reset(f);
  if not Eof(f) then
    ReadLn(f, line)
  else
    begin
      WriteLn('Error: input file is empty');
      Halt(1);
    end;
  CloseFile(f);

  { Parse into initialMem }
  SetLength(initialMem, 0);
  while line <> '' do
  begin
    posComma := Pos(',', line);
    if posComma > 0 then
    begin
      token := Copy(line, 1, posComma - 1);
      Delete(line, 1, posComma);
    end
    else
    begin
      token := line;
      line := '';
    end;
    token := Trim(token);
    if token <> '' then
    begin
      if not TryStrToInt64(token, value) then
      begin
        WriteLn('Error parsing integer: ', token);
        Halt(1);
      end;
      idx := Length(initialMem);
      SetLength(initialMem, idx + 1);
      initialMem[idx] := value;
    end;
  end;

  { Run with system ID = 5 (thermal radiator controller) }
  outputs := RunIntcode(initialMem, 5);

  { The diagnostic code is the last output produced }
  if Length(outputs) = 0 then
    WriteLn('No output produced.')
  else
    WriteLn(outputs[High(outputs)]);
end.
