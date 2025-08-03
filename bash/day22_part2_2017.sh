#!/usr/bin/env bash
set -euo pipefail

awk '
BEGIN{
  MAX=10000
  Clean=0; Weakened=1; Infected=2; Flagged=3
}
{
  gsub(/\r/,"")
  L[NR]=$0
  W=length($0)
}
END{
  H=NR
  sx=int(W/2); sy=int(H/2)
  for(y=0;y<H;y++){
    n=L[y+1]
    for(x=0;x<W;x++){
      c=substr(n,x+1,1)
      if(c=="#"){
        k=(x+MAX)*MAX+(y+MAX)
        G[k]=Infected
      }
    }
  }
  dx[0]=0; dy[0]=-1
  dx[1]=1; dy[1]=0
  dx[2]=0; dy[2]=1
  dx[3]=-1; dy[3]=0

  x=sx; y=sy; dir=0; infected=0
  steps=10000000
  for(i=0;i<steps;i++){
    k=(x+MAX)*MAX+(y+MAX)
    s = (k in G ? G[k] : Clean)
    if(s==Clean){
      dir=(dir+3)%4
      G[k]=Weakened
    } else if(s==Weakened){
      G[k]=Infected
      infected++
    } else if(s==Infected){
      dir=(dir+1)%4
      G[k]=Flagged
    } else {
      dir=(dir+2)%4
      delete G[k]
    }
    x+=dx[dir]; y+=dy[dir]
  }
  print infected
}
' input.txt