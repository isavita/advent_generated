#!/usr/bin/env bash
set -euo pipefail

awk -F'[~,]' '
function abs(x){return x<0?-x:x}
function cmp(a,b){return a<b?-1:(a>b?1:0)}
BEGIN{n=0}
{
  x1[n]=$1+0; y1[n]=$2+0; z1[n]=$3+0
  x2[n]=$4+0; y2[n]=$5+0; z2[n]=$6+0
  n++
}
END{
  for(i=0;i<n;i++){idx[i]=i}
  for(i=0;i<n;i++)for(j=i+1;j<n;j++)if(z2[idx[i]]>z2[idx[j]]){t=idx[i];idx[i]=idx[j];idx[j]=t}

  for(ii=0;ii<n;ii++){
    i=idx[ii]
    supportZ=0
    bcnt=0
    for(jj=ii-1;jj>=0;jj--){
      j=idx[jj]
      ix = ( (x1[i]>x2[j]?x1[i]:x1[j]) <= (x2[i]<x2[j]?x2[i]:x2[j]) )
      iy = ( (y1[i]>y2[j]?y1[i]:y1[j]) <= (y2[i]<y2[j]?y2[i]:y2[j]) )
      if(ix && iy){
        if(z2[j]==supportZ){
          based[i,bcnt]=j; bcnt++
        } else if(z2[j]>supportZ){
          supportZ=z2[j]; bcnt=0; based[i,bcnt]=j; bcnt=1
        }
      }
    }
    basedCnt[i]=bcnt
    for(k=0;k<bcnt;k++){
      b=based[i,k]
      supportCnt[b]++; supportIdx[b,supportCnt[b]-1]=i
    }
    dz=z2[i]-z1[i]
    z1[i]=supportZ+1
    z2[i]=z1[i]+dz
  }

  ans=0
  for(i=0;i<n;i++){
    ok=1
    sc=supportCnt[i]+0
    for(k=0;k<sc;k++){
      s=supportIdx[i,k]
      if((basedCnt[s]+0)<2){ok=0;break}
    }
    if(ok)ans++
  }
  print ans
}
' input.txt