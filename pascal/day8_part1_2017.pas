program Day8;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Generics.Collections;

var
  regs: specialize TDictionary<string, Integer>;
  lines: TStringList;
  parts: TStringList;
  i, amount, condVal, newVal, maxVal: Integer;
  regName, op, condReg, condOp, line: string;

// Retrieve register value, defaulting to 0 if not present
function GetReg(const name: string): Integer;
begin
  if not regs.TryGetValue(name, Result) then
  begin
    regs.Add(name, 0);
    Result := 0;
  end;
end;

// Set register to a new value
procedure SetReg(const name: string; value: Integer);
begin
  regs.AddOrSetValue(name, value);
end;

// Evaluate the condition on a register
function EvalCond(const rName, cOp: string; cVal: Integer): Boolean;
var
  cur: Integer;
begin
  cur := GetReg(rName);
  case cOp of
    '>':  Result := cur > cVal;
    '<':  Result := cur < cVal;
    '>=': Result := cur >= cVal;
    '<=': Result := cur <= cVal;
    '==': Result := cur = cVal;
    '!=': Result := cur <> cVal;
  else
    Result := False;
  end;
end;

begin
  // Initialize dictionary and load input
  regs := specialize TDictionary<string, Integer>.Create;
  lines := TStringList.Create;
  try
    if not FileExists('input.txt') then
      raise Exception.Create('File "input.txt" not found.');
    lines.LoadFromFile('input.txt');

    // Parse and execute each instruction
    for i := 0 to lines.Count - 1 do
    begin
      line := lines[i];
      parts := TStringList.Create;
      try
        // Split on spaces
        parts.Delimiter := ' ';
        parts.StrictDelimiter := True;
        parts.DelimitedText := line;
        // parts[0]=register, [1]=inc/dec, [2]=amount, [3]=if,
        // [4]=condReg, [5]=condOp, [6]=condVal
        regName := parts[0];
        op      := parts[1];
        amount  := StrToInt(parts[2]);
        condReg := parts[4];
        condOp  := parts[5];
        condVal := StrToInt(parts[6]);
        // Evaluate condition
        if EvalCond(condReg, condOp, condVal) then
        begin
          if op = 'inc' then
            newVal := GetReg(regName) + amount
          else
            newVal := GetReg(regName) - amount;
          SetReg(regName, newVal);
        end;
      finally
        parts.Free;
      end;
    end;

    // Find the largest register value
    if regs.Count > 0 then
    begin
      maxVal := Low(Integer);
      for regName in regs.Keys do
        if regs[regName] > maxVal then
          maxVal := regs[regName];
      WriteLn(maxVal);
    end
    else
      WriteLn(0);

  finally
    regs.Free;
    lines.Free;
  end;
end.