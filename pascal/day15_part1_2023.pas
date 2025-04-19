program p;
var f: text; ch: char; total, cv: longint;
begin
  Assign(f, 'input.txt'); Reset(f);
  total := 0; cv := 0;
  while not Eof(f) do begin
    Read(f, ch);
    if ch = ',' then begin total := total + cv; cv := 0 end
    else if (ch <> #13) and (ch <> #10) then cv := ((cv + Ord(ch)) * 17) mod 256;
  end;
  Close(f);
  total := total + cv;
  Writeln(total);
end.