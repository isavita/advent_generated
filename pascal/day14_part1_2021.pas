program p;
var
  f: Text;
  s, poly, newp: ansistring;
  r: array[0..675] of char;
  cnt: array['A'..'Z'] of longint;
  i, j, idx: integer;
  ch: char;
  mn, mx: longint;
begin
  assign(f, 'input.txt'); reset(f);
  readln(f, poly);
  while not eof(f) do
  begin
    readln(f, s);
    if length(s) >= 7 then
    begin
      idx := (ord(s[1]) - 65) * 26 + (ord(s[2]) - 65);
      r[idx] := s[7];
    end;
  end;
  close(f);
  for j := 1 to 10 do
  begin
    newp := '';
    for i := 1 to length(poly) - 1 do
    begin
      newp := newp + poly[i];
      idx := (ord(poly[i]) - 65) * 26 + (ord(poly[i+1]) - 65);
      ch := r[idx];
      if ch <> #0 then newp := newp + ch;
    end;
    newp := newp + poly[length(poly)];
    poly := newp;
  end;
  for ch := 'A' to 'Z' do cnt[ch] := 0;
  for i := 1 to length(poly) do inc(cnt[poly[i]]);
  mn := High(longint); mx := 0;
  for ch := 'A' to 'Z' do
    if cnt[ch] > 0 then
    begin
      if cnt[ch] < mn then mn := cnt[ch];
      if cnt[ch] > mx then mx := cnt[ch];
    end;
  writeln(mx - mn);
end.