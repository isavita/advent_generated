program PathFind;
type TNode=record x,y,d,k,prio:LongInt end;
var grid:array of array of Byte;
    costArr:array of array of array of array of LongInt;
    heap:array of TNode;
    heapLen,W,H,GoalX,GoalY,i,j,d,k,MaxNodes:LongInt;
    XDir:array[1..4] of Integer=(0,-1,0,1);
    YDir:array[1..4] of Integer=(-1,0,1,0);
    opp:array[0..4] of Integer=(0,3,4,1,2);
    lines:array of string;
    f:Text;
    s:string;
    node,cur:TNode;
    nx,ny,newd,newk,newCost,heu,curCost:LongInt;
procedure Push(n:TNode);
var idx,p:LongInt;
begin
  inc(heapLen); idx:=heapLen;
  heap[idx]:=n;
  while idx>1 do begin
    p:=idx div 2;
    if heap[idx].prio<heap[p].prio then begin
      heap[0]:=heap[idx]; heap[idx]:=heap[p]; heap[p]:=heap[0];
      idx:=p
    end else break;
  end;
end;
function Pop:TNode;
var idx,c,mc:LongInt;
    t:TNode;
begin
  Pop:=heap[1];
  heap[1]:=heap[heapLen]; dec(heapLen);
  idx:=1;
  while True do begin
    c:=idx*2; mc:=idx;
    if (c<=heapLen) and (heap[c].prio<heap[mc].prio) then mc:=c;
    if (c+1<=heapLen) and (heap[c+1].prio<heap[mc].prio) then mc:=c+1;
    if mc=idx then break;
    t:=heap[idx]; heap[idx]:=heap[mc]; heap[mc]:=t;
    idx:=mc;
  end;
end;
begin
  Assign(f,'input.txt'); Reset(f);
  H:=0;
  while not Eof(f) do begin
    ReadLn(f,s);
    if Length(s)>0 then begin
      SetLength(lines,H+1);
      lines[H]:=s;
      inc(H);
    end;
  end;
  Close(f);
  W:=Length(lines[0]);
  SetLength(grid,W);
  for i:=0 to W-1 do begin
    SetLength(grid[i],H);
    for j:=0 to H-1 do grid[i][j]:=Ord(lines[j][i+1])-48;
  end;
  GoalX:=W-1; GoalY:=H-1;
  SetLength(costArr,W);
  for i:=0 to W-1 do begin
    SetLength(costArr[i],H);
    for j:=0 to H-1 do begin
      SetLength(costArr[i][j],5);
      for d:=0 to 4 do begin
        SetLength(costArr[i][j][d],4);
        for k:=0 to 3 do costArr[i][j][d][k]:=MaxLongInt;
      end;
    end;
  end;
  costArr[0][0][0][0]:=0;
  MaxNodes:=W*H*5*4+2;
  SetLength(heap,MaxNodes);
  heapLen:=0;
  node.x:=0; node.y:=0; node.d:=0; node.k:=0;
  node.prio:=Abs(0-GoalX)+Abs(0-GoalY);
  Push(node);
  while heapLen>0 do begin
    cur:=Pop;
    i:=cur.x; j:=cur.y; d:=cur.d; k:=cur.k;
    heu:=Abs(i-GoalX)+Abs(j-GoalY);
    curCost:=costArr[i][j][d][k];
    if (i=GoalX) and (j=GoalY) then begin
      WriteLn(curCost);
      Exit;
    end;
    if cur.prio-heu<>curCost then Continue;
    for newd:=1 to 4 do begin
      nx:=i+XDir[newd]; ny:=j+YDir[newd];
      if (nx<0) or (nx>GoalX) or (ny<0) or (ny>GoalY) then Continue;
      if newd<>d then newk:=1 else newk:=k+1;
      if newk>3 then Continue;
      if (d<>0) and (newd=opp[d]) then Continue;
      newCost:=curCost+grid[nx][ny];
      if newCost<costArr[nx][ny][newd][newk] then begin
        costArr[nx][ny][newd][newk]:=newCost;
        node.x:=nx; node.y:=ny; node.d:=newd; node.k:=newk;
        node.prio:=newCost+Abs(nx-GoalX)+Abs(ny-GoalY);
        Push(node);
      end;
    end;
  end;
end.