program main;
var m: array[0..200,0..200] of byte;
    v: array[0..200,0..200] of boolean;
    h,w,i,j,t1,t2,t3,cnt: integer;
    row: string;
    f: text;
procedure flood(x,y: integer; var cnt: integer);
begin
  if (x<0) or (y<0) or (x>=w) or (y>=h) or v[y][x] or (m[y][x]=9) then exit;
  v[y][x]:=true; inc(cnt);
  flood(x-1,y,cnt); flood(x+1,y,cnt);
  flood(x,y-1,cnt); flood(x,y+1,cnt);
end;
function low(x,y: integer): boolean;
var v0: byte;
begin
  v0:=m[y][x];
  low:=((x=0) or (m[y][x-1]>v0)) and ((x=w-1) or (m[y][x+1]>v0))
      and ((y=0) or (m[y-1][x]>v0)) and ((y=h-1) or (m[y+1][x]>v0));
end;
begin
  assign(f,'input.txt'); reset(f);
  h:=0;
  while not eof(f) do begin
    readln(f,row); w:=length(row);
    for j:=1 to w do m[h][j-1]:=ord(row[j])-48;
    inc(h);
  end;
  close(f);
  t1:=0; t2:=0; t3:=0;
  for i:=0 to h-1 do for j:=0 to w-1 do if low(j,i) then begin
    cnt:=0; flood(j,i,cnt);
    if cnt>t1 then begin t3:=t2; t2:=t1; t1:=cnt end
    else if cnt>t2 then begin t3:=t2; t2:=cnt end
    else if cnt>t3 then t3:=cnt;
  end;
  writeln(t1*t2*t3);
end.