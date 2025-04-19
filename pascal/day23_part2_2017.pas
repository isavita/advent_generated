program CountNonPrimes;
uses SysUtils;
function isPrime(n: LongInt): Boolean; var i, m: LongInt; begin
  if n<2 then exit(false);
  m:=Trunc(Sqrt(n));
  for i:=2 to m do if n mod i=0 then exit(false);
  isPrime:=true;
end;
var b,c,x,h: LongInt;
begin
  Assign(input,'input.txt'); Reset(input);
  b:=57*100+100000; c:=b+17000; h:=0; x:=b;
  while x<=c do begin
    if not isPrime(x) then inc(h);
    x+=17;
  end;
  Writeln(h);
end.