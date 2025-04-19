program Solve;
uses SysUtils;
var
  f: TextFile;
  line: string;
  ln, block, b, top: integer;
  l, k, m, p, d, digits, st: array[0..13] of integer;
  delta: integer;
begin
  Assign(f, 'input.txt'); Reset(f);
  ln := 0;
  while not Eof(f) do begin
    ReadLn(f, line);
    block := ln div 18;
    case ln mod 18 of
      4: l[block] := StrToInt(Copy(line, LastDelimiter(' ', line) + 1, MaxInt));
      5: k[block] := StrToInt(Copy(line, LastDelimiter(' ', line) + 1, MaxInt));
      15: m[block] := StrToInt(Copy(line, LastDelimiter(' ', line) + 1, MaxInt));
    end;
    Inc(ln);
  end;
  Close(f);
  top := 0;
  for b := 0 to 13 do
    if l[b] = 1 then begin
      st[top] := b; Inc(top)
    end else begin
      Dec(top);
      p[st[top]] := b;
      d[st[top]] := m[st[top]] + k[b];
    end;
  for b := 0 to 13 do digits[b] := 0;
  for b := 0 to 13 do
    if l[b] = 1 then begin
      delta := d[b];
      if delta >= 0 then begin
        digits[b] := 9 - delta;
        digits[p[b]] := 9
      end else begin
        digits[b] := 9;
        digits[p[b]] := 9 + delta
      end;
    end;
  for b := 0 to 13 do Write(digits[b]);
  WriteLn;
end.