#!/usr/bin/env bash
set -euo pipefail

awk '
function abs(x){return x<0?-x:x}
BEGIN{FS=""; OFS=""}
{
  line=$0
  if(NR==1) W=length(line)
  H=NR
  for(i=1;i<=W;i++){
    c=substr(line,i,1)
    if(c!="."){
      has[i]=1
      hasr[NR]=1
      ptsN++
      px[ptsN]=i
      py[ptsN]=NR
    }
  }
}
END{
  EF=1000000
  add=EF-1

  for(i=1;i<=W;i++){
    dx[i]=(i>1?dx[i-1]:0)
    if(!(i in has)) dx[i]++
  }
  for(j=1;j<=H;j++){
    dy[j]=(j>1?dy[j-1]:0)
    if(!(j in hasr)) dy[j]++
  }

  for(k=1;k<=ptsN;k++){
    ex[k]=px[k] + dx[px[k]]*add
    ey[k]=py[k] + dy[py[k]]*add
  }

  n=ptsN
  split("", xs); split("", ys)
  for(i=1;i<=n;i++){ xs[i]=ex[i]; ys[i]=ey[i] }

  # sort xs
  for(i=1;i<=n;i++){
    m=i
    for(j=i+1;j<=n;j++) if(xs[j]<xs[m]) m=j
    if(m!=i){t=xs[i]; xs[i]=xs[m]; xs[m]=t}
  }
  # sort ys
  for(i=1;i<=n;i++){
    m=i
    for(j=i+1;j<=n;j++) if(ys[j]<ys[m]) m=j
    if(m!=i){t=ys[i]; ys[i]=ys[m]; ys[m]=t}
  }

  sx=0; pxsum=0
  for(i=1;i<=n;i++){
    sx += xs[i]*(2*i - n - 1)
  }
  sy=0
  for(i=1;i<=n;i++){
    sy += ys[i]*(2*i - n - 1)
  }
  print sx + sy
}
' input.txt