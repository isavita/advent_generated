program solve;
uses SysUtils, Classes;
type TPoint=record x,y,z:integer end;
var sl: TStringList; i,nx,ny,nz,minx,miny,minz,maxx,maxy,maxz,ans,dx,dy,dz,px,py,pz,head,tail:integer;
    s,t: string; p: TPoint;
    pts: array of TPoint;
    grid,vis: array of array of array of byte;
    q: array of TPoint;
begin
  Assign(input,'input.txt'); Reset(input);
  sl:=TStringList.Create; while not eof do begin ReadLn(s); if s<>'' then sl.Add(s) end;
  minx:=MaxInt; miny:=MaxInt; minz:=MaxInt; maxx:=-MaxInt; maxy:=-MaxInt; maxz:=-MaxInt;
  SetLength(pts,sl.Count);
  for i:=0 to sl.Count-1 do begin
    s:=sl[i]; Delete(s,1,0);
    px:=StrToInt(Copy(s,1,Pos(',',s)-1)); Delete(s,1,Pos(',',s));
    py:=StrToInt(Copy(s,1,Pos(',',s)-1)); Delete(s,1,Pos(',',s));
    pz:=StrToInt(s);
    pts[i].x:=px; pts[i].y:=py; pts[i].z:=pz;
    if px<minx then minx:=px; if py<miny then miny:=py; if pz<minz then minz:=pz;
    if px>maxx then maxx:=px; if py>maxy then maxy:=py; if pz>maxz then maxz:=pz;
  end;
  Dec(minx); Dec(miny); Dec(minz);
  Inc(maxx); Inc(maxy); Inc(maxz);
  nx:=maxx-minx+1; ny:=maxy-miny+1; nz:=maxz-minz+1;
  SetLength(grid,nx,ny,nz); SetLength(vis,nx,ny,nz);
  for i:=0 to High(pts) do with pts[i] do grid[x-minx,y-miny,z-minz]:=1;
  SetLength(q,nx*ny*nz);
  head:=0; tail:=1; q[0].x:=0-minx; q[0].y:=0-miny; q[0].z:=0-minz; vis[q[0].x,q[0].y,q[0].z]:=1;
  ans:=0;
  dx:=0; dy:=0; dz:=0;
  while head<tail do begin
    p:=q[head]; Inc(head);
    for i:=1 to 6 do begin
      case i of
        1: begin dx:=-1; dy:=0; dz:=0 end;
        2: begin dx:=1; dy:=0; dz:=0 end;
        3: begin dx:=0; dy:=-1; dz:=0 end;
        4: begin dx:=0; dy:=1; dz:=0 end;
        5: begin dx:=0; dy:=0; dz:=-1 end;
        6: begin dx:=0; dy:=0; dz:=1 end;
      end;
      px:=p.x+dx; py:=p.y+dy; pz:=p.z+dz;
      if (px<0)or(py<0)or(pz<0)or(px>=nx)or(py>=ny)or(pz>=nz) then Continue;
      if grid[px,py,pz]=1 then Inc(ans) else
      if vis[px,py,pz]=0 then begin
        vis[px,py,pz]:=1; q[tail].x:=px; q[tail].y:=py; q[tail].z:=pz; Inc(tail)
      end;
    end;
  end;
  WriteLn(ans);
end.