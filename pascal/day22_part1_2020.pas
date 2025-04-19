program Combat;
uses SysUtils;
const MAX = 1000;
var f: Text;
    line: string;
    p1, p2: array[1..MAX] of integer;
    p1h, p1t, p2h, p2t, c1, c2, i, cnt: integer;
    score: Int64;
begin
  Assign(f,'input.txt'); Reset(f);
  ReadLn(f,line);
  while not EOF(f) do begin
    ReadLn(f,line);
    if line='' then Break;
    Inc(p1t); p1[p1t]:=StrToInt(line);
  end;
  ReadLn(f,line);
  while not EOF(f) do begin
    ReadLn(f,line);
    if line='' then Break;
    Inc(p2t); p2[p2t]:=StrToInt(line);
  end;
  Close(f);
  p1h:=1; p2h:=1;
  while (p1h<=p1t) and (p2h<=p2t) do begin
    c1:=p1[p1h]; Inc(p1h);
    c2:=p2[p2h]; Inc(p2h);
    if c1>c2 then begin
      Inc(p1t); p1[p1t]:=c1;
      Inc(p1t); p1[p1t]:=c2;
    end else begin
      Inc(p2t); p2[p2t]:=c2;
      Inc(p2t); p2[p2t]:=c1;
    end;
  end;
  score:=0;
  if p1h<=p1t then begin
    cnt:=p1t-p1h+1;
    for i:=0 to cnt-1 do
      score:=score+(Int64(i+1)*p1[p1t-i]);
  end else begin
    cnt:=p2t-p2h+1;
    for i:=0 to cnt-1 do
      score:=score+(Int64(i+1)*p2[p2t-i]);
  end;
  WriteLn(score);
end.