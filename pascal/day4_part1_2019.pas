program solution;
uses SysUtils;
var f: TextFile;
line: string;
startNum,endNum,i,count: LongInt;
good,repeatFound: Boolean;
j: Integer;
begin
  Assign(f,'input.txt'); Reset(f);
  ReadLn(f,line);
  Close(f);
  startNum:=StrToInt(Copy(line,1,Pos('-',line)-1));
  endNum:=StrToInt(Copy(line,Pos('-',line)+1,MaxInt));
  count:=0;
  for i:=startNum to endNum do begin
    line:=IntToStr(i);
    good:=True; repeatFound:=False;
    for j:=1 to Length(line)-1 do begin
      if line[j]>line[j+1] then begin good:=False; Break end;
      if line[j]=line[j+1] then repeatFound:=True;
    end;
    if good and repeatFound then Inc(count);
  end;
  WriteLn(count);
end.