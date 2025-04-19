program Solve;
uses sysutils;
type
  TCoord = record x,y: Integer end;
  TEdge  = record to_,w: Integer end;
var
  lines: array of string;
  w,h: Integer;
  vindex: array of Integer;
  coords: array of TCoord;
  edges: array of array of TEdge;
  dist,q: array of Integer;
  seen: array of Boolean;
  dx: array[0..3] of Integer = (0,0,-1,1);
  dy: array[0..3] of Integer = (-1,1,0,0);
  vid,startVid,endVid,bestDist: Integer;

procedure ReadInput;
var f: Text; s: string;
begin
  assign(f,'input.txt'); reset(f);
  h:=0;
  while not eof(f) do begin
    readln(f,s);
    setlength(lines,h+1);
    lines[h]:=s;
    inc(h);
  end;
  close(f);
  if h>0 then w:=Length(lines[0]) else w:=0;
end;

function InBounds(x,y: Integer): Boolean;
begin
  InBounds:=(x>=0) and (x<w) and (y>=0) and (y<h);
end;

procedure AddVertex(x,y: Integer);
begin
  vindex[y*w+x]:=vid;
  setlength(coords,vid+1);
  coords[vid].x:=x; coords[vid].y:=y;
  inc(vid);
end;

procedure DFS(cur,dacc: Integer);
var e: TEdge;
begin
  if cur=endVid then begin
    if dacc>bestDist then bestDist:=dacc;
    exit;
  end;
  seen[cur]:=True;
  for e in edges[cur] do
    if not seen[e.to_] then DFS(e.to_,dacc+e.w);
  seen[cur]:=False;
end;

var
  i,j,k,idx,nx,ny,head,tail,x0,y0,nc: Integer;
begin
  ReadInput;
  setlength(vindex,w*h);
  for i:=0 to w*h-1 do vindex[i]:=-1;
  vid:=0;
  for y0:=0 to h-1 do
    for x0:=0 to w-1 do
      if (x0=1) and (y0=0) then begin startVid:=vid; AddVertex(x0,y0) end
      else if (x0=w-2) and (y0=h-1) then begin endVid:=vid; AddVertex(x0,y0) end
      else if lines[y0][x0+1]<>'#' then begin
        nc:=0;
        if InBounds(x0,y0-1) and (lines[y0-1][x0+1]<>'#') then inc(nc);
        if InBounds(x0,y0+1) and (lines[y0+1][x0+1]<>'#') then inc(nc);
        if InBounds(x0-1,y0) and (lines[y0][x0]<>'#')    then inc(nc);
        if InBounds(x0+1,y0) and (lines[y0][x0+2]<>'#')  then inc(nc);
        if nc>2 then AddVertex(x0,y0);
      end;
  setlength(edges,vid);
  setlength(dist,w*h);
  setlength(q,w*h);
  for k:=0 to vid-1 do begin
    for i:=0 to w*h-1 do dist[i]:=-1;
    head:=0; tail:=0;
    x0:=coords[k].x; y0:=coords[k].y;
    dist[y0*w+x0]:=0;
    q[tail]:=y0*w+x0; inc(tail);
    while head<tail do begin
      idx:=q[head]; inc(head);
      y0:=idx div w; x0:=idx mod w;
      if (vindex[idx]>=0) and (vindex[idx]<>k) then begin
        setlength(edges[k],Length(edges[k])+1);
        edges[k][High(edges[k])].to_:=vindex[idx];
        edges[k][High(edges[k])].w:=dist[idx];
        continue;
      end;
      for i:=0 to 3 do begin
        nx:=x0+dx[i]; ny:=y0+dy[i];
        if InBounds(nx,ny) and (lines[ny][nx+1]<>'#') and (dist[ny*w+nx]<0) then begin
          dist[ny*w+nx]:=dist[idx]+1;
          q[tail]:=ny*w+nx; inc(tail);
        end;
      end;
    end;
  end;
  setlength(seen,vid);
  bestDist:=0;
  DFS(startVid,0);
  writeln(bestDist);
end.
