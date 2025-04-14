
program AssembunnyInterpreterOptimized;

{$mode delphi}{$H+} // Use Delphi mode for modern features like dynamic arrays and helper functions

uses
  SysUtils, // For StrToIntDef, Trim, File I/O routines
  StrUtils; // For SplitString (optional, implementing manually for clarity)

type
  TStringDynArray = array of string;
  TRegisterName = 'a'..'d';
  TRegisters = array[TRegisterName] of Integer;

// Helper to split a string, similar to Python's split()
procedure SplitStringManual(const S: string; const Delimiter: Char; out Parts: TStringDynArray);
var
  CurrentPart: string;
  i, StartPos: Integer;
begin
  SetLength(Parts, 0);
  if S = '' then Exit;

  StartPos := 1;
  for i := 1 to Length(S) do
  begin
    if S[i] = Delimiter then
    begin
      CurrentPart := Trim(Copy(S, StartPos, i - StartPos));
      if CurrentPart <> '' then // Avoid empty parts if multiple delimiters exist
      begin
         SetLength(Parts, Length(Parts) + 1);
         Parts[High(Parts)] := CurrentPart;
      end;
      StartPos := i + 1;
    end;
  end;

  // Add the last part
  CurrentPart := Trim(Copy(S, StartPos, Length(S) - StartPos + 1));
   if CurrentPart <> '' then
   begin
        SetLength(Parts, Length(Parts) + 1);
        Parts[High(Parts)] := CurrentPart;
   end;

   // If the string was non-empty but resulted in no parts (e.g., just delimiters)
   if (Length(S) > 0) and (Length(Parts) = 0) and (Trim(S) <> '') then
   begin
        SetLength(Parts, 1);
        Parts[0] := Trim(S);
   end;

end;

procedure ReadInput(var instructions: TStringDynArray);
var
  F: TextFile;
  line: string;
  count: Integer;
begin
  Assign(F, 'input.txt');
  try
    {$I-} // Disable I/O error checking
    Reset(F);
    {$I+}
    if IOResult <> 0 then
    begin
      WriteLn('Error: Cannot open input.txt');
      Halt(1);
    end;

    count := 0;
    while not Eof(F) do
    begin
      ReadLn(F, line);
      SetLength(instructions, count + 1); // Dynamic array resizing
      instructions[count] := Trim(line);
      Inc(count);
    end;
  finally
    Close(F);
  end;
end;

function IsRegister(const x: string): Boolean;
begin
  Result := (Length(x) = 1) and (x[1] >= 'a') and (x[1] <= 'd');
end;

function GetValue(const x: string; const registers: TRegisters): Integer;
begin
  if IsRegister(x) then
    Result := registers[x[1]] // Pascal array indexed by char
  else
    Result := StrToIntDef(x, 0); // Convert string to integer, default 0 if invalid
end;

procedure ExecuteProgram(var instructions: TStringDynArray; var registers: TRegisters);
var
  i: Integer;
  parts, patternParts0, patternParts1, patternParts2, patternParts3, patternParts4, patternParts5: TStringDynArray;
  cmd, x, y, cpy_x, cpy_y, inc_a, dec_c, jnz_c, jnz_d, dec_d: string;
  valX, valY, targetIdx, jnz_c_offset, jnz_d_offset : Integer;
  targetCmd : string;
  numInstructions: Integer;
begin
  i := 0;
  numInstructions := Length(instructions); // Cache length for performance

  while (i >= 0) and (i < numInstructions) do // Check bounds explicitly
  begin

    // Optimization Check: Multiplication pattern
    if (i + 5 < numInstructions) then
    begin
      // Avoid repeated splitting if possible, split only when needed
      SplitStringManual(instructions[i], ' ', patternParts0);
      SplitStringManual(instructions[i+1], ' ', patternParts1);
      SplitStringManual(instructions[i+2], ' ', patternParts2);
      SplitStringManual(instructions[i+3], ' ', patternParts3);
      SplitStringManual(instructions[i+4], ' ', patternParts4);
      SplitStringManual(instructions[i+5], ' ', patternParts5);

      if (Length(patternParts0) > 0) and (patternParts0[0] = 'cpy') and
         (Length(patternParts1) > 0) and (patternParts1[0] = 'inc') and
         (Length(patternParts2) > 0) and (patternParts2[0] = 'dec') and
         (Length(patternParts3) > 0) and (patternParts3[0] = 'jnz') and
         (Length(patternParts4) > 0) and (patternParts4[0] = 'dec') and
         (Length(patternParts5) > 0) and (patternParts5[0] = 'jnz') and
         (Length(patternParts0) = 3) and (Length(patternParts1) = 2) and
         (Length(patternParts2) = 2) and (Length(patternParts3) = 3) and
         (Length(patternParts4) = 2) and (Length(patternParts5) = 3) then
      begin
          cpy_x := patternParts0[1]; cpy_y := patternParts0[2];
          inc_a := patternParts1[1];
          dec_c := patternParts2[1];
          jnz_c := patternParts3[1]; jnz_c_offset := StrToIntDef(patternParts3[2], 999); // Use default unlikely value
          dec_d := patternParts4[1];
          jnz_d := patternParts5[1]; jnz_d_offset := StrToIntDef(patternParts5[2], 999); // Use default unlikely value

          // Check if the pattern matches the specific multiplication structure
          if (IsRegister(cpy_y)) and (IsRegister(inc_a)) and (IsRegister(dec_c)) and
             (IsRegister(jnz_c)) and (IsRegister(dec_d)) and (IsRegister(jnz_d)) and
             (inc_a = 'a') and (dec_c = cpy_y) and (jnz_c = cpy_y) and (jnz_c_offset = -2) and
             (dec_d = 'd') and (jnz_d = 'd') and (jnz_d_offset = -5) then
          begin
              // Perform optimized multiplication: registers['a'] += GetValue(cpy_x) * registers['d']
              registers['a'] := registers['a'] + GetValue(cpy_x, registers) * registers['d'];
              // Clear the registers used in the inner loops
              registers[cpy_y[1]] := 0; // cpy_y is known to be a register
              registers['d'] := 0;
              // Jump past the optimized block
              i := i + 6;
              Continue; // Go to next iteration of the while loop
          end;
      end;
    end;

    // If optimization didn't run, proceed with normal execution
    SplitStringManual(instructions[i], ' ', parts);

    // Handle potential empty lines or invalid instructions gracefully
    if Length(parts) = 0 then
    begin
      Inc(i);
      Continue;
    end;

    cmd := parts[0];

    // Handle toggle instruction first as it modifies other instructions
    if cmd = 'tgl' then
    begin
        if Length(parts) <> 2 then
        begin
           Inc(i); // Invalid tgl, skip
           Continue;
        end;
        x := parts[1];
        valX := GetValue(x, registers);
        targetIdx := i + valX;

        if (targetIdx >= 0) and (targetIdx < numInstructions) then
        begin
          SplitStringManual(instructions[targetIdx], ' ', patternParts0); // Re-use patternParts0
          if Length(patternParts0) = 2 then // inc, dec, tgl (1 argument)
          begin
            targetCmd := patternParts0[0];
            if targetCmd = 'inc' then
                patternParts0[0] := 'dec'
            else // dec, tgl, or others become inc
                patternParts0[0] := 'inc';
            instructions[targetIdx] := patternParts0[0] + ' ' + patternParts0[1];
          end
          else if Length(patternParts0) = 3 then // jnz, cpy (2 arguments)
          begin
             targetCmd := patternParts0[0];
             if targetCmd = 'jnz' then
                 patternParts0[0] := 'cpy'
             else // cpy or others become jnz
                 patternParts0[0] := 'jnz';
             instructions[targetIdx] := patternParts0[0] + ' ' + patternParts0[1] + ' ' + patternParts0[2];
          end;
          // else: instruction with different number of arguments - do nothing
        end;
        Inc(i); // Move to next instruction after tgl
        Continue; // Go to next iteration
    end;

    // Handle standard instructions
    if cmd = 'cpy' then
    begin
      if Length(parts) = 3 then
      begin
        x := parts[1];
        y := parts[2];
        if IsRegister(y) then // Can only copy to a register
        begin
          registers[y[1]] := GetValue(x, registers);
        end;
      end;
      Inc(i);
    end
    else if cmd = 'inc' then
    begin
      if Length(parts) = 2 then
      begin
        x := parts[1];
        if IsRegister(x) then
        begin
          Inc(registers[x[1]]);
        end;
      end;
       Inc(i);
    end
    else if cmd = 'dec' then
    begin
       if Length(parts) = 2 then
       begin
         x := parts[1];
         if IsRegister(x) then
         begin
           Dec(registers[x[1]]);
         end;
       end;
        Inc(i);
    end
    else if cmd = 'jnz' then
    begin
      if Length(parts) = 3 then
      begin
        x := parts[1];
        y := parts[2]; // y can be a register or a number
        valX := GetValue(x, registers);
        valY := GetValue(y, registers); // Get jump offset
        if valX <> 0 then
          i := i + valY // Jump relative to current instruction
        else
          Inc(i); // Condition false, move to next instruction
      end
      else // Invalid jnz
         Inc(i);
    end
    else // Unknown instruction
    begin
       Inc(i);
    end;

  end; // while loop
end;

var
  instructions: TStringDynArray;
  registers: TRegisters;

begin
  // Initialize registers
  registers['a'] := 12;
  registers['b'] := 0;
  registers['c'] := 0;
  registers['d'] := 0;

  // Read instructions from file
  ReadInput(instructions);

  // Execute the program
  ExecuteProgram(instructions, registers);

  // Print the final value of register 'a'
  WriteLn(registers['a']);
end.
