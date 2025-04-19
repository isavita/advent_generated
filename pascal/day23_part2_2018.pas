program Nanobots;
{$mode objfpc}{$H+}
uses sysutils;
type
  TNano = record x,y,z,r:Int64 end;
  TNode = record count,dist,size,x,y,z:Int64 end;
var
  bots: array of TNano;
  heap: array of TNode;
  heapSize: Integer;

function abs64(a:Int64):Int64; begin if a<0 then abs64:=-a else abs64:=a end;

function cmpNode(const a,b:TNode): Boolean;
begin
  if a.count>b.count then Exit(True);
  if a.count<b.count then Exit(False);
  Exit(a.dist<b.dist);
end;

procedure heapPush(n:TNode);
var i,p:Integer;
begin
  Inc(heapSize);
  if heapSize>Length(heap) then SetLength(heap,Length(heap)*2);
  i:=heapSize; heap[i]:=n;
  while i>1 do begin
    p:=i div 2;
    if cmpNode(heap[i],heap[p]) then begin
      heap[0]:=heap[i]; heap[i]:=heap[p]; heap[p]:=heap[0];
      i:=p
    end else Break;
  end;
end;

function heapPop:TNode;
var cur,child:Integer;
begin
  Result:=heap[1];
  heap[1]:=heap[heapSize];
  Dec(heapSize);
  cur:=1;
  while cur*2<=heapSize do begin
    child:=cur*2;
    if (child<heapSize) and cmpNode(heap[child+1],heap[child]) then Inc(child);
    if cmpNode(heap[child],heap[cur]) then begin
      heap[0]:=heap[cur]; heap[cur]:=heap[child]; heap[child]:=heap[0];
      cur:=child
    end else Break;
  end;
end;

function minDistOrigin(x,y,z,size:Int64):Int64;
var dx,dy,dz:Int64;
begin
  if x>0 then dx:=x
  else if x+size-1<0 then dx:=-(x+size-1)
  else dx:=0;
  if y>0 then dy:=y
  else if y+size-1<0 then dy:=-(y+size-1)
  else dy:=0;
  if z>0 then dz:=z
  else if z+size-1<0 then dz:=-(z+size-1)
  else dz:=0;
  Result:=dx+dy+dz;
end;

function countInCube(nx,ny,nz,size:Int64):Int64;
var i:Int32; dx,dy,dz,d:Int64;
begin
  Result:=0;
  for i:=0 to High(bots) do begin
    dx:=0; dy:=0; dz:=0;
    if bots[i].x<nx then dx:=nx-bots[i].x
    else if bots[i].x>nx+size-1 then dx:=bots[i].x-(nx+size-1);
    if bots[i].y<ny then dy:=ny-bots[i].y
    else if bots[i].y>ny+size-1 then dy:=bots[i].y-(ny+size-1);
    if bots[i].z<nz then dz:=nz-bots[i].z
    else if bots[i].z>nz+size-1 then dz:=bots[i].z-(nz+size-1);
    d:=dx+dy+dz;
    if d<=bots[i].r then Inc(Result);
  end;
end;

function manDist(x1,y1,z1,x2,y2,z2:Int64):Int64;
begin Result:=abs64(x1-x2)+abs64(y1-y2)+abs64(z1-z2) end;

var
  f: TextFile;
  s,t:String;
  nums: array of Int64;
  i,j,k:Integer;
  tmp:Int64;
  sx,sy,sz,sr,count1:Int64;
  minx,maxx,miny,maxy,minz,maxz,size,half:Int64;
  node,node2:TNode;
  bestDist:Int64;
begin
  AssignFile(f,'input.txt'); Reset(f);
  SetLength(bots,0);
  while not EOF(f) do begin
    ReadLn(f,s);
    t:=''; SetLength(nums,0);
    for i:=1 to Length(s) do begin
      if s[i] in ['0'..'9','-'] then t+=s[i]
      else if t<>'' then begin
        SetLength(nums,Length(nums)+1);
        nums[High(nums)]:=StrToInt64(t);
        t:='';
      end;
    end;
    if t<>'' then begin
      SetLength(nums,Length(nums)+1);
      nums[High(nums)]:=StrToInt64(t);
    end;
    if Length(nums)=4 then begin
      tmp:=Length(bots);
      SetLength(bots,tmp+1);
      bots[tmp].x:=nums[0]; bots[tmp].y:=nums[1];
      bots[tmp].z:=nums[2]; bots[tmp].r:=nums[3];
    end;
  end;
  CloseFile(f);

  sr:=-1;
  for i:=0 to High(bots) do
    if bots[i].r>sr then begin
      sr:=bots[i].r; sx:=bots[i].x; sy:=bots[i].y; sz:=bots[i].z;
    end;
  count1:=0;
  for i:=0 to High(bots) do
    if manDist(sx,sy,sz,bots[i].x,bots[i].y,bots[i].z)<=sr then Inc(count1);
  WriteLn(count1);

  minx:=bots[0].x; maxx:=bots[0].x;
  miny:=bots[0].y; maxy:=bots[0].y;
  minz:=bots[0].z; maxz:=bots[0].z;
  for i:=1 to High(bots) do begin
    if bots[i].x<minx then minx:=bots[i].x;
    if bots[i].x>maxx then maxx:=bots[i].x;
    if bots[i].y<miny then miny:=bots[i].y;
    if bots[i].y>maxy then maxy:=bots[i].y;
    if bots[i].z<minz then minz:=bots[i].z;
    if bots[i].z>maxz then maxz:=bots[i].z;
  end;
  size:=1;
  while size<maxx-minx do size*=2;
  while size<maxy-miny do size*=2;
  while size<maxz-minz do size*=2;

  SetLength(heap,1024);
  heapSize:=0;
  node.x:=minx; node.y:=miny; node.z:=minz; node.size:=size;
  node.count:=countInCube(minx,miny,minz,size);
  node.dist:=minDistOrigin(minx,miny,minz,size);
  heapPush(node);

  bestDist:=0;
  while heapSize>0 do begin
    node:=heapPop;
    if node.size=1 then begin
      bestDist:=node.dist;
      Break;
    end;
    half:=node.size div 2;
    for i:=0 to 1 do for j:=0 to 1 do for k:=0 to 1 do begin
      node2.x:=node.x + i*half;
      node2.y:=node.y + j*half;
      node2.z:=node.z + k*half;
      node2.size:=half;
      if node2.size<1 then node2.size:=1;
      node2.count:=countInCube(node2.x,node2.y,node2.z,node2.size);
      node2.dist:=minDistOrigin(node2.x,node2.y,node2.z,node2.size);
      heapPush(node2);
    end;
  end;
  WriteLn(bestDist);
end.
