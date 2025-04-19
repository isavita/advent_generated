program EvalExpr;
{$mode objfpc}{$H+}
uses SysUtils;
var sum1,sum2:Int64; line:string; p:integer;
function eval1(s:string; var p:integer):Int64; forward;
function factor1(s:string; var p:integer):Int64;
var v:Int64; num:string;
begin
  while (p<=length(s)) and (s[p]=' ') do inc(p);
  if (p<=length(s)) and (s[p]='(') then begin inc(p); v:=eval1(s,p); inc(p) end
  else begin num:=''; while (p<=length(s)) and (s[p] in ['0'..'9']) do begin num:=num+s[p]; inc(p) end; v:=StrToInt64(num) end;
  result:=v;
end;
function eval1(s:string; var p:integer):Int64;
var v,t:Int64; op:char;
begin
  v:=factor1(s,p);
  while true do begin
    while (p<=length(s)) and (s[p]=' ') do inc(p);
    if (p>length(s)) or not(s[p] in ['+','*']) then break;
    op:=s[p]; inc(p); t:=factor1(s,p);
    if op='+' then v:=v+t else v:=v*t;
  end;
  result:=v;
end;
function eval2(s:string; var p:integer):Int64; forward;
function factor2(s:string; var p:integer):Int64;
var v:Int64; num:string;
begin
  while (p<=length(s)) and (s[p]=' ') do inc(p);
  if (p<=length(s)) and (s[p]='(') then begin inc(p); v:=eval2(s,p); inc(p) end
  else begin num:=''; while (p<=length(s)) and (s[p] in ['0'..'9']) do begin num:=num+s[p]; inc(p) end; v:=StrToInt64(num) end;
  result:=v;
end;
function term2(s:string; var p:integer):Int64;
var v,t:Int64;
begin
  v:=factor2(s,p);
  while true do begin
    while (p<=length(s)) and (s[p]=' ') do inc(p);
    if (p>length(s)) or (s[p]<>'+') then break;
    inc(p); t:=factor2(s,p); v:=v+t;
  end;
  result:=v;
end;
function eval2(s:string; var p:integer):Int64;
var v,t:Int64;
begin
  v:=term2(s,p);
  while true do begin
    while (p<=length(s)) and (s[p]=' ') do inc(p);
    if (p>length(s)) or (s[p]<>'*') then break;
    inc(p); t:=term2(s,p); v:=v*t;
  end;
  result:=v;
end;
begin
  assign(input,'input.txt'); reset(input);
  sum1:=0; sum2:=0;
  while not eof do begin
    readln(line);
    if line='' then continue;
    p:=1; sum1:=sum1+eval1(line,p);
    p:=1; sum2:=sum2+eval2(line,p);
  end;
  writeln(sum1);
  writeln(sum2);
end.