#!/usr/bin/env bash
set -euo pipefail

awk '
function key(x,y){return x "," y}
function splitkey(k,a){n=split(k,a,","); a[1]+=0; a[2]+=0}
function add(x,y){g[key(x,y)]=1; if(x<minx)minx=x; if(y<miny)miny=y; if(x>maxx)maxx=x; if(y>maxy)maxy=y}
BEGIN{minx=1e18;miny=1e18;maxx=-1e18;maxy=-1e18}
{
  n=split($0, seg, " -> ")
  for(i=1;i<n;i++){
    split(seg[i], a, ","); x1=a[1]+0; y1=a[2]+0
    split(seg[i+1], b, ","); x2=b[1]+0; y2=b[2]+0
    if(x1==x2){
      if(y1>y2){t=y1;y1=y2;y2=t}
      for(y=y1;y<=y2;y++) add(x1,y)
    } else {
      if(x1>x2){t=x1;x1=x2;x2=t}
      for(x=x1;x<=x2;x++) add(x,y1)
    }
  }
}
END{
  floor=maxy+1
  sands=0
  first=0
  while(1){
    sx=500; sy=0
    while(1){
      if(sy==floor-1){
        if(first==0) first=sands
        g[key(sx,sy)]=1
        break
      }
      k1=key(sx,sy+1)
      if(!(k1 in g)){sy++; continue}
      k2=key(sx-1,sy+1)
      if(!(k2 in g)){sx--; sy++; continue}
      k3=key(sx+1,sy+1)
      if(!(k3 in g)){sx++; sy++; continue}
      g[key(sx,sy)]=1
      break
    }
    sands++
    if(sy==0){print first; exit}
  }
}
' input.txt