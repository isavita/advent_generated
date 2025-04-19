program FoldPoints;
const MAX = 2000;
type TGrid = array[0..MAX,0..MAX] of boolean;
var grid,newgrid:TGrid;
    s:string;
    x,y,fx,fy,i,j,pc,fc,posval,maxX,maxY,newX,newY,count:integer;
    readingPoints:boolean;
    axisArray:array[0..100] of char;
    posArray:array[0..100] of integer;
begin
  assign(input,'input.txt'); reset(input);
  readingPoints:=true;
  maxX:=0; maxY:=0; fc:=0;
  for y:=0 to MAX do for x:=0 to MAX do grid[x,y]:=false;
  while not eof do
  begin
    readln(s);
    if s='' then readingPoints:=false
    else if readingPoints then
    begin
      pc:=0;
      val(copy(s,1,pos(',',s)-1),x,pc);
      delete(s,1,pos(',',s));
      val(s,y,pc);
      grid[x,y]:=true;
      if x>maxX then maxX:=x;
      if y>maxY then maxY:=y;
    end else
    begin
      pc:=0;
      val(copy(s,pos('=',s)+1),posval,pc);
      axisArray[fc]:=s[pos('=',s)-1];
      posArray[fc]:=posval;
      inc(fc);
    end;
  end;
  for i:=0 to fc-1 do
  begin
    for y:=0 to maxY do for x:=0 to maxX do newgrid[x,y]:=false;
    if axisArray[i]='x' then
    begin
      fx:=posArray[i];
      for y:=0 to maxY do for x:=0 to maxX do if grid[x,y] then
      begin
        if x>fx then newX:=2*fx-x else newX:=x;
        newgrid[newX,y]:=true;
      end;
      maxX:=fx-1;
    end else
    begin
      fy:=posArray[i];
      for y:=0 to maxY do for x:=0 to maxX do if grid[x,y] then
      begin
        if y>fy then newY:=2*fy-y else newY:=y;
        newgrid[x,newY]:=true;
      end;
      maxY:=fy-1;
    end;
    for y:=0 to maxY do for x:=0 to maxX do grid[x,y]:=newgrid[x,y];
    if i=0 then
    begin
      count:=0;
      for y:=0 to maxY do for x:=0 to maxX do if grid[x,y] then inc(count);
      writeln('Number of dots visible after first fold: ',count);
    end;
  end;
  for y:=0 to maxY do
  begin
    for x:=0 to maxX do if grid[x,y] then write('#') else write('.');
    writeln;
  end;
end.