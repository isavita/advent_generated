program Main;
uses SysUtils,StrUtils;
type TPoint=record x,y,z,t:integer end;
var f:text; s:string; pts:array of TPoint; par:array of integer;
    n,i,j,p1,p2,p3,counts:integer;
function Find(x:integer):integer;
begin
  if par[x]<>x then par[x]:=Find(par[x]);
  Find:=par[x];
end;
procedure Union(a,b:integer);
var ra,rb:integer;
begin
  ra:=Find(a); rb:=Find(b);
  if ra<>rb then par[ra]:=rb;
end;
function AbsV(x:integer):integer;
begin
  if x<0 then AbsV:=-x else AbsV:=x;
end;
function Dist(a,b:TPoint):integer;
begin
  Dist:=AbsV(a.x-b.x)+AbsV(a.y-b.y)+AbsV(a.z-b.z)+AbsV(a.t-b.t);
end;
begin
  Assign(f,'input.txt'); Reset(f); n:=0;
  while not Eof(f) do
  begin
    ReadLn(f,s);
    p1:=Pos(',',s); p2:=PosEx(',',s,p1+1); p3:=PosEx(',',s,p2+1);
    SetLength(pts,n+1);
    with pts[n] do
    begin
      x:=StrToInt(Copy(s,1,p1-1));
      y:=StrToInt(Copy(s,p1+1,p2-p1-1));
      z:=StrToInt(Copy(s,p2+1,p3-p2-1));
      t:=StrToInt(Copy(s,p3+1,Length(s)-p3));
    end;
    Inc(n);
  end;
  Close(f);
  SetLength(par,n);
  for i:=0 to n-1 do par[i]:=i;
  for i:=0 to n-1 do
    for j:=i+1 to n-1 do
      if Dist(pts[i],pts[j])<=3 then Union(i,j);
  counts:=0;
  for i:=0 to n-1 do if Find(i)=i then Inc(counts);
  WriteLn(counts);
end.