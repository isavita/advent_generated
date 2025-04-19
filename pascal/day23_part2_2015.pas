program Solve;
uses SysUtils, Classes;
type TOp = (opHLF,opTPL,opINC,opJMP,opJIE,opJIO);
var SL: TStringList;
ops: array of TOp;
regc: array of Char;
offs: array of Integer;
i, cnt: Integer;
a, b: Int64;
s, cmd, rest: string;
p: Integer;
begin
  SL := TStringList.Create;
  SL.LoadFromFile('input.txt');
  cnt := SL.Count;
  SetLength(ops, cnt);
  SetLength(regc, cnt);
  SetLength(offs, cnt);
  for i := 0 to cnt - 1 do begin
    s := SL[i];
    p := Pos(' ', s);
    cmd := Copy(s, 1, p - 1);
    rest := Copy(s, p + 1);
    if cmd = 'hlf' then begin ops[i] := opHLF; regc[i] := rest[1] end else
    if cmd = 'tpl' then begin ops[i] := opTPL; regc[i] := rest[1] end else
    if cmd = 'inc' then begin ops[i] := opINC; regc[i] := rest[1] end else
    if cmd = 'jmp' then begin ops[i] := opJMP; offs[i] := StrToInt(rest) end else
    if cmd = 'jie' then begin ops[i] := opJIE; regc[i] := rest[1]; offs[i] := StrToInt(Copy(rest, Pos(',', rest) + 1)) end else
    if cmd = 'jio' then begin ops[i] := opJIO; regc[i] := rest[1]; offs[i] := StrToInt(Copy(rest, Pos(',', rest) + 1)) end;
  end;
  i := 0; a := 1; b := 0;
  while i < cnt do
    case ops[i] of
      opHLF: begin if regc[i] = 'a' then a := a div 2 else b := b div 2; Inc(i) end;
      opTPL: begin if regc[i] = 'a' then a := a * 3 else b := b * 3; Inc(i) end;
      opINC: begin if regc[i] = 'a' then Inc(a) else Inc(b); Inc(i) end;
      opJMP: i := i + offs[i];
      opJIE: if (((regc[i] = 'a') and (a mod 2 = 0)) or ((regc[i] = 'b') and (b mod 2 = 0))) then i := i + offs[i] else Inc(i);
      opJIO: if (((regc[i] = 'a') and (a = 1)) or ((regc[i] = 'b') and (b = 1))) then i := i + offs[i] else Inc(i);
    end;
  Writeln(b);
end.