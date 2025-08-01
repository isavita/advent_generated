#!/usr/bin/env bash
set -euo pipefail

awk -v min=200000000000000 -v max=400000000000000 '
function abs(x){return x<0?-x:x}
BEGIN{FS="[@,]"; OFS=","}
{
  px[NR]=$1+0; py[NR]=$2+0; pz[NR]=$3+0; vx[NR]=$4+0; vy[NR]=$5+0; vz[NR]=$6+0
}
END{
  n=NR
  cnt=0
  for(i=1;i<=n-1;i++){
    for(j=i+1;j<=n;j++){
      vx1=vx[i]; vy1=vy[i]; px1=px[i]; py1=py[i]
      vx2=vx[j]; vy2=vy[j]; px2=px[j]; py2=py[j]
      denom= vx1*vy2 - vy1*vx2
      if(denom==0) continue
      t2 = ((py1 - py2)*vx1 - (px1 - px2)*vy1) / denom
      t1 = (px2 - px1 + vx2 * t2) / vx1
      if(t1<=0 || t2<=0) continue
      x = px1 + vx1 * t1
      y = py1 + vy1 * t1
      if(x>=min && x<=max && y>=min && y<=max) cnt++
    }
  }
  print cnt
}
' input.txt