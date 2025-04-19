program GuardSleep;
type TStringArray = array of string;
var
  lines: TStringArray;
  nLines,i,j: integer;
  f: Text;
  line: string;
  currentGuard: integer;
  fallsAsleep,wakesUp: integer;
  p: integer;
  guardIdStr: string;
  guards: array[0..9999,0..59] of integer;
  guardUsed: array[0..9999] of boolean;
  sumSleep: array[0..9999] of integer;
  maxSleep,sleepiestGuard,minuteMax: integer;
  mostFreqVal,mostFreqGuard,mostFreqMinute: integer;

procedure QuickSort(var A: TStringArray; L,R: integer);
var i,j: integer; pivot,tmp: string;
begin
  i:=L; j:=R; pivot:=A[(L+R) shr 1];
  while i<=j do
  begin
    while A[i]<pivot do inc(i);
    while A[j]>pivot do dec(j);
    if i<=j then
    begin
      tmp:=A[i]; A[i]:=A[j]; A[j]:=tmp;
      inc(i); dec(j);
    end;
  end;
  if L<j then QuickSort(A,L,j);
  if i<R then QuickSort(A,i,R);
end;

begin
  assign(f,'input.txt'); reset(f);
  nLines:=0;
  while not eof(f) do
  begin
    readln(f,line);
    inc(nLines);
    setlength(lines,nLines);
    lines[nLines-1]:=line;
  end;
  close(f);
  if nLines>0 then QuickSort(lines,0,nLines-1);
  for i:=0 to 9999 do
  begin
    guardUsed[i]:=false;
    sumSleep[i]:=0;
    for j:=0 to 59 do guards[i,j]:=0;
  end;
  currentGuard:=-1;
  for i:=0 to nLines-1 do
  begin
    line:=lines[i];
    if Pos('#',line)>0 then
    begin
      p:=Pos('#',line)+1;
      guardIdStr:='';
      while (p<=Length(line)) and (line[p] in ['0'..'9']) do
      begin
        guardIdStr:=guardIdStr+line[p];
        inc(p);
      end;
      Val(guardIdStr,currentGuard,p);
      guardUsed[currentGuard]:=true;
    end
    else if Pos('falls asleep',line)>0 then
      Val(Copy(line,16,2),fallsAsleep,p)
    else if Pos('wakes up',line)>0 then
    begin
      Val(Copy(line,16,2),wakesUp,p);
      for j:=fallsAsleep to wakesUp-1 do inc(guards[currentGuard,j]);
    end;
  end;
  maxSleep:=0;
  for i:=0 to 9999 do
    if guardUsed[i] then
    begin
      for j:=0 to 59 do inc(sumSleep[i],guards[i,j]);
      if sumSleep[i]>maxSleep then
      begin
        maxSleep:=sumSleep[i];
        sleepiestGuard:=i;
      end;
    end;
  minuteMax:=0;
  for j:=1 to 59 do
    if guards[sleepiestGuard,j]>guards[sleepiestGuard,minuteMax] then
      minuteMax:=j;
  writeln(sleepiestGuard*minuteMax);
  mostFreqVal:=0;
  for i:=0 to 9999 do
    if guardUsed[i] then
      for j:=0 to 59 do
        if guards[i,j]>mostFreqVal then
        begin
          mostFreqVal:=guards[i,j];
          mostFreqGuard:=i;
          mostFreqMinute:=j;
        end;
  writeln(mostFreqGuard*mostFreqMinute);
end.