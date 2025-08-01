#!/usr/bin/env bash
set -euo pipefail

awk '
BEGIN{
  SH=6; W=5; H=7
}
function parse_lock(b, res, c,r,cnt){
  for(c=1;c<=W;c++){
    cnt=0
    for(r=2;r<=H;r++){
      if(substr(b[r],c,1)=="#") cnt++
      else break
    }
    res[c]=cnt
  }
}
function parse_key(b, res, c,r,cnt){
  for(c=1;c<=W;c++){
    cnt=0
    for(r=SH;r>=1;r--){
      if(substr(b[r],c,1)=="#") cnt++
      else break
    }
    res[c]=cnt
  }
}
function fits(i,j,k){
  for(k=1;k<=W;k++) if(locks[i,k]+keys[j,k]>SH-1) return 0
  return 1
}
{
  gsub(/[\r ]+$/,"")
  if($0=="") next
  blk[++ln]=$0
  if(length($0)<W) inval=1
  if(ln==H){
    if(!inval){
      is_lock=1
      for(i=1;i<=W;i++) if(substr(blk[1],i,1)!="#"){is_lock=0;break}
      if(is_lock){
        parse_lock(blk, tmp)
        L++
        for(i=1;i<=W;i++) locks[L,i]=tmp[i]
      } else {
        parse_key(blk, tmp)
        K++
        for(i=1;i<=W;i++) keys[K,i]=tmp[i]
      }
    }
    ln=0;inval=0;split("",blk)
  }
  total++
}
END{
  if(total==0 || total%H!=0){print 0;exit}
  c=0
  for(i=1;i<=L;i++) for(j=1;j<=K;j++) if(fits(i,j)) c++
  print c
}
' input.txt