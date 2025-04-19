program OctopusFlashes;
var
  f: Text;
  s: string;
  grid: array[1..200,1..200] of byte;
  vis: array[1..200,1..200] of boolean;
  r,c,total,i,j,step,di,dj,ni,nj: integer;
  newFlash: boolean;
begin
  assign(f,'input.txt'); reset(f);
  r:=0;
  while not eof(f) do begin
    readln(f,s);
    inc(r); c:=length(s);
    for j:=1 to c do grid[r,j]:=ord(s[j])-48;
  end;
  close(f);
  total:=0;
  for step:=1 to 100 do begin
    for i:=1 to r do
      for j:=1 to c do begin
        inc(grid[i,j]);
        vis[i,j]:=false;
      end;
    repeat
      newFlash:=false;
      for i:=1 to r do
        for j:=1 to c do
          if (grid[i,j]>9) and not vis[i,j] then begin
            vis[i,j]:=true;
            inc(total);
            for di:=-1 to 1 do
              for dj:=-1 to 1 do
                if not((di=0) and (dj=0)) then begin
                  ni:=i+di; nj:=j+dj;
                  if (ni>=1) and (ni<=r) and (nj>=1) and (nj<=c) then
                    inc(grid[ni,nj]);
                end;
            newFlash:=true;
          end;
    until not newFlash;
    for i:=1 to r do
      for j:=1 to c do
        if vis[i,j] then
          grid[i,j]:=0;
  end;
  writeln(total);
end.