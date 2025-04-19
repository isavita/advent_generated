program BossLoot;
uses SysUtils;
type Item=record c,d,a:integer;end;
     CharRec=record hp,d,a:integer;end;
const
  Weapons:array[0..4] of Item=(
    (c:8;d:4;a:0),(c:10;d:5;a:0),(c:25;d:6;a:0),(c:40;d:7;a:0),(c:74;d:8;a:0)
  );
  Armors:array[0..5] of Item=(
    (c:0;d:0;a:0),(c:13;d:0;a:1),(c:31;d:0;a:2),(c:53;d:0;a:3),(c:75;d:0;a:4),(c:102;d:0;a:5)
  );
  Rings:array[0..6] of Item=(
    (c:0;d:0;a:0),(c:25;d:1;a:0),(c:50;d:2;a:0),(c:100;d:3;a:0),
    (c:20;d:0;a:1),(c:40;d:0;a:2),(c:80;d:0;a:3)
  );
var boss:CharRec;
    line:string;
    p,i,j,ri,rj,cost,md,nd,pt,bt,maxCost:integer;
begin
  Assign(input,'input.txt'); Reset(input);
  Readln(line); p:=Pos(':',line); boss.hp:=StrToInt(Copy(line,p+2,255));
  Readln(line); p:=Pos(':',line); boss.d :=StrToInt(Copy(line,p+2,255));
  Readln(line); p:=Pos(':',line); boss.a :=StrToInt(Copy(line,p+2,255));
  maxCost:=0;
  for i:=0 to 4 do
    for j:=0 to 5 do
      for ri:=0 to 6 do
        for rj:=ri+1 to 6 do begin
          cost:=Weapons[i].c+Armors[j].c+Rings[ri].c+Rings[rj].c;
          md:=Weapons[i].d+Rings[ri].d+Rings[rj].d-boss.a; if md<1 then md:=1;
          nd:=boss.d-(Armors[j].a+Rings[ri].a+Rings[rj].a); if nd<1 then nd:=1;
          pt:=(boss.hp+md-1) div md;
          bt:=(100+nd-1) div nd;
          if pt>bt then if cost>maxCost then maxCost:=cost;
        end;
  Writeln(maxCost);
end.