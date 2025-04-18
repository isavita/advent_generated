
program SafeCracking;
{$mode objfpc}{$H+}
uses
  SysUtils;

type
  TInst = record
    op, x, y: string;
  end;

var
  inst: array of TInst;
  regs: array['a'..'d'] of Int64;
  pc, n, i: Integer;

// split a line like "cpy 2 a" into words[0]="cpy", words[1]="2", words[2]="a"
procedure SplitLine(const s: string; out words: array of string; var cnt: Integer);
var
  i, len: Integer;
  cur: string;
begin
  cnt := 0;
  cur := '';
  len := Length(s);
  for i := 1 to len do
  begin
    if s[i] = ' ' then
    begin
      if cnt <= High(words) then
        words[cnt] := cur;
      Inc(cnt);
      cur := '';
    end
    else
      cur += s[i];
  end;
  // last word
  if cnt <= High(words) then
    words[cnt] := cur;
  Inc(cnt);
end;

// is this operand a register a..d ?
function IsReg(const s: string): Boolean;
begin
  Result := (Length(s)=1) and (s[1] in ['a'..'d']);
end;

// numeric value of either a register or an integer literal
function GetVal(const s: string): Int64;
var
  v: Int64;
begin
  if IsReg(s) then
    Result := regs[s[1]]
  else if TryStrToInt64(s, v) then
    Result := v
  else
    Result := 0;
end;

// toggle the instruction at index idx if in range
procedure DoToggle(idx: Integer);
begin
  if (idx < 0) or (idx >= n) then Exit;
  with inst[idx] do
  begin
    // one-argument?
    if y = '' then
    begin
      if op = 'inc' then
        op := 'dec'
      else
        op := 'inc';
    end
    else
    begin
      // two-argument
      if op = 'jnz' then
        op := 'cpy'
      else
        op := 'jnz';
    end;
  end;
end;

var
  f: Text;
  line: string;
  parts: array[0..2] of string;
  pcOffset: Int64;
  wc: Integer;
begin
  // read the program from input.txt
  Assign(f, 'input.txt');
  Reset(f);
  n := 0;
  while not Eof(f) do
  begin
    ReadLn(f, line);
    if line = '' then Continue;
    SetLength(inst, n+1);
    SplitLine(line, parts, wc);
    inst[n].op := parts[0];
    inst[n].x  := '';
    inst[n].y  := '';
    if wc > 1 then inst[n].x := parts[1];
    if wc > 2 then inst[n].y := parts[2];
    Inc(n);
  end;
  Close(f);

  // initialize registers
  regs['a'] := 7;  // the puzzle hint: 7 eggs
  regs['b'] := 0;
  regs['c'] := 0;
  regs['d'] := 0;

  pc := 0;
  // interpreter loop
  while (pc >= 0) and (pc < n) do
  begin
    case inst[pc].op of
      'cpy':
        begin
          // cpy x y: if y is register, y:= val(x)
          if IsReg(inst[pc].y) then
            regs[inst[pc].y[1]] := GetVal(inst[pc].x);
          Inc(pc);
        end;

      'inc':
        begin
          // inc x
          if IsReg(inst[pc].x) then
            Inc(regs[inst[pc].x[1]]);
          Inc(pc);
        end;

      'dec':
        begin
          // dec x
          if IsReg(inst[pc].x) then
            Dec(regs[inst[pc].x[1]]);
          Inc(pc);
        end;

      'jnz':
        begin
          // jnz x y
          if GetVal(inst[pc].x) <> 0 then
          begin
            pcOffset := GetVal(inst[pc].y);
            pc += pcOffset;
          end
          else
            Inc(pc);
        end;

      'tgl':
        begin
          // tgl x
          DoToggle(pc + Integer(GetVal(inst[pc].x)));
          Inc(pc);
        end

    else
      // unknown or invalid => skip
      Inc(pc);
    end;
  end;

  // answer is in register a
  WriteLn(regs['a']);
end.
