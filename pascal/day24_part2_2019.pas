program p;
const OFFSET=210;
var curr,nxt: array[0..420,0..24] of boolean;
    s: string;
    i,j,it,lvlIdx,cell,row,col,neigh,newMin,newMax,minIdx,maxIdx,count: integer;
    f: Text;
begin
  assign(f,'input.txt'); reset(f);
  for i:=0 to 24 do curr[OFFSET,i]:=false;
  for j:=0 to 4 do begin
    readln(f,s);
    for i:=1 to length(s) do curr[OFFSET,(j)*5+(i-1)]:=(s[i]='#');
  end;
  close(f);
  minIdx:=OFFSET; maxIdx:=OFFSET;
  for it:=1 to 200 do begin
    newMin:=999; newMax:=-999;
    for lvlIdx:=minIdx-1 to maxIdx+1 do begin
      for cell:=0 to 24 do nxt[lvlIdx,cell]:=false;
      for cell:=0 to 24 do if cell<>12 then begin
        row:=cell div 5; col:=cell mod 5; neigh:=0;
        if row=0 then if curr[lvlIdx-1,7] then inc(neigh);
        if col=0 then if curr[lvlIdx-1,11] then inc(neigh);
        if col=4 then if curr[lvlIdx-1,13] then inc(neigh);
        if row=4 then if curr[lvlIdx-1,17] then inc(neigh);
        if cell=7 then for i:=0 to 4 do if curr[lvlIdx+1,i] then inc(neigh);
        if cell=11 then for i:=0 to 4 do if curr[lvlIdx+1,5*i] then inc(neigh);
        if cell=13 then for i:=0 to 4 do if curr[lvlIdx+1,5*i+4] then inc(neigh);
        if cell=17 then for i:=0 to 4 do if curr[lvlIdx+1,20+i] then inc(neigh);
        if (row>0) and (cell<>17) then if curr[lvlIdx,cell-5] then inc(neigh);
        if (col>0) and (cell<>13) then if curr[lvlIdx,cell-1] then inc(neigh);
        if (col<4) and (cell<>11) then if curr[lvlIdx,cell+1] then inc(neigh);
        if (row<4) and (cell<>7) then if curr[lvlIdx,cell+5] then inc(neigh);
        if curr[lvlIdx,cell] then begin
          if neigh=1 then nxt[lvlIdx,cell]:=true
        end else if (neigh=1) or (neigh=2) then nxt[lvlIdx,cell]:=true;
      end;
      for cell:=0 to 24 do if nxt[lvlIdx,cell] then begin
        if lvlIdx<newMin then newMin:=lvlIdx;
        if lvlIdx>newMax then newMax:=lvlIdx;
        break;
      end;
    end;
    if newMin>newMax then begin minIdx:=0; maxIdx:=-1 end
    else begin minIdx:=newMin; maxIdx:=newMax end;
    for lvlIdx:=minIdx to maxIdx do
      for cell:=0 to 24 do curr[lvlIdx,cell]:=nxt[lvlIdx,cell];
  end;
  count:=0;
  for lvlIdx:=minIdx to maxIdx do
    for cell:=0 to 24 do if curr[lvlIdx,cell] then inc(count);
  writeln(count);
end.