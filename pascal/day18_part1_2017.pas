
program Duet;
{$mode objfpc}{$H+}
uses
  SysUtils, Classes;

type
  TInstr = record
    op, x, y: string;
  end;

var
  lines: TStringList;
  instr: array of TInstr;
  regs: array['a'..'z'] of Int64;
  lastSound: Int64;
  ip, n, i, p, p2: Integer;
  line, rest: string;

function GetVal(const s: string): Int64;
begin
  if (Length(s) = 1) and (s[1] in ['a'..'z']) then
    Result := regs[s[1]]
  else
    Result := StrToInt64(s);
end;

begin
  // 1) Load and parse all instructions
  lines := TStringList.Create;
  try
    lines.LoadFromFile('input.txt');
    n := lines.Count;
    SetLength(instr, n);
    for i := 0 to n-1 do
    begin
      line := lines[i];
      p := Pos(' ', line);
      instr[i].op := Copy(line, 1, p-1);
      rest := Copy(line, p+1, MaxInt);
      p2 := Pos(' ', rest);
      if p2 > 0 then
      begin
        instr[i].x := Copy(rest, 1, p2-1);
        instr[i].y := Copy(rest, p2+1, MaxInt);
      end
      else
      begin
        instr[i].x := rest;
        instr[i].y := '';
      end;
    end;
  finally
    lines.Free;
  end;

  // 2) Initialize registers and instruction pointer
  for i := Ord('a') to Ord('z') do
    regs[Chr(i)] := 0;
  ip := 0;

  // 3) Execute until the first non‐zero rcv
  while (ip >= 0) and (ip < n) do
  with instr[ip] do
  begin
    case op of
      'snd': begin
        lastSound := GetVal(x);
        Inc(ip);
      end;
      'set': begin
        regs[x[1]] := GetVal(y);
        Inc(ip);
      end;
      'add': begin
        regs[x[1]] := regs[x[1]] + GetVal(y);
        Inc(ip);
      end;
      'mul': begin
        regs[x[1]] := regs[x[1]] * GetVal(y);
        Inc(ip);
      end;
      'mod': begin
        regs[x[1]] := regs[x[1]] mod GetVal(y);
        Inc(ip);
      end;
      'rcv': begin
        if GetVal(x) <> 0 then
        begin
          Writeln(lastSound);
          Break;        // terminate on first non‐zero rcv
        end
        else
          Inc(ip);
      end;
      'jgz': begin
        if GetVal(x) > 0 then
          ip := ip + GetVal(y)
        else
          Inc(ip);
      end
    else
      Inc(ip); // unrecognized op, skip
    end;
  end;
end.
