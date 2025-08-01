#!/usr/bin/env bash
set -euo pipefail

awk '
function abs(x){return x<0?-x:x}
BEGIN{
  x[0]=0;y[0]=0;n=1
}
{
  split($0,a," ")
  color=a[3]
  dir=substr(color,8,1)
  lenhex=substr(color,3,5)
  len=0
  for(i=1;i<=5;i++){
    c=substr(lenhex,i,1)
    if(c>="0"&&c<="9") v=c+0
    else if(c>="a"&&c<="f") v=10+index("abcdef",c)-1
    else if(c>="A"&&c<="F") v=10+index("ABCDEF",c)-1
    len=len*16+v
  }
  xx=x[n-1]; yy=y[n-1]
  if(dir=="0") xx+=len
  else if(dir=="1") yy+=len
  else if(dir=="2") xx-=len
  else if(dir=="3") yy-=len
  x[n]=xx; y[n]=yy; n++
}
END{
  area2=0; perim=0
  for(i=0;i<n;i++){
    j=(i+1)%n
    area2 += x[i]*y[j] - y[i]*x[j]
    perim += abs(x[i]-x[j]) + abs(y[i]-y[j])
  }
  if(area2<0) area2=-area2
  # area = area2/2 + perim/2 + 1 -> integer
  # compute using integers: (area2 + perim)/2 + 1
  ans = int((area2 + perim)/2) + 1
  print ans
}
' input.txt