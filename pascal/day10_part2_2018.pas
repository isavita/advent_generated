program main;
uses SysUtils;
type TPoint = record x,y,vx,vy: Integer end;
var f: Text;
    s, num: string;
    pts: array of TPoint;
    nums: array[0..3] of Integer;
    i, n, t, idx, code: Integer;
    c: Char;
    minx, maxx, miny, maxy: Integer;
begin
  Assign(f,'input.txt'); Reset(f);
  n:=0;
  while not Eof(f) do begin
    ReadLn(f,s);
    num:=''; idx:=0;
    Inc(n); SetLength(pts,n);
    for i:=1 to Length(s) do begin
      c:=s[i];
      if (c='-') or (c in ['0'..'9']) then num:=num+c
      else if num<>'' then begin Val(num,nums[idx],code); Inc(idx); num:=''; end;
    end;
    if num<>'' then Val(num,nums[idx],code);
    with pts[n-1] do begin x:=nums[0]; y:=nums[1]; vx:=nums[2]; vy:=nums[3]; end;
  end;
  Close(f);
  t:=0;
  while True do begin
    minx:=pts[0].x; maxx:=minx; miny:=pts[0].y; maxy:=miny;
    for i:=1 to n-1 do begin
      if pts[i].x<minx then minx:=pts[i].x else if pts[i].x>maxx then maxx:=pts[i].x;
      if pts[i].y<miny then miny:=pts[i].y else if pts[i].y>maxy then maxy:=pts[i].y;
    end;
    if (maxx-minx<100) and (maxy-miny<10) then Break;
    for i:=0 to n-1 do begin
      pts[i].x+=pts[i].vx;
      pts[i].y+=pts[i].vy;
    end;
    Inc(t);
  end;
  WriteLn(t);
end.