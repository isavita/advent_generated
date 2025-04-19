program VentLines;
uses SysUtils;
var
  grid: array[0..1000,0..1000] of integer;
  s,s1,s2: string;
  c,x1,y1,x2,y2,p,j,k,err: integer;
function mn(a,b: integer): integer; begin if a<b then mn:=a else mn:=b end;
function mx(a,b: integer): integer; begin if a>b then mx:=a else mx:=b end;
begin
  assign(input,'input.txt'); reset(input);
  c:=0;
  while not eof do
  begin
    readln(s);
    p:=pos(' -> ',s);
    s1:=copy(s,1,p-1);
    s2:=copy(s,p+4,length(s));
    j:=pos(',',s1);
    val(copy(s1,1,j-1),x1,err);
    val(copy(s1,j+1,length(s1)),y1,err);
    j:=pos(',',s2);
    val(copy(s2,1,j-1),x2,err);
    val(copy(s2,j+1,length(s2)),y2,err);
    if x1=x2 then
      for k:=mn(y1,y2) to mx(y1,y2) do
      begin
        if grid[x1,k]=1 then inc(c);
        inc(grid[x1,k]);
      end
    else if y1=y2 then
      for k:=mn(x1,x2) to mx(x1,x2) do
      begin
        if grid[k,y1]=1 then inc(c);
        inc(grid[k,y1]);
      end;
  end;
  writeln(c);
end.