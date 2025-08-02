#!/usr/bin/env bash
awk '
BEGIN{FS="[,|]"}
function is_correct(u,   i,j,x,y,pos,n) {
  delete pos
  n=split(u,a,",")
  for(i=1;i<=n;i++) pos[a[i]]=i
  for(i=1;i<=nr;i++){
    x=rx[i]; y=ry[i]
    if(x in pos && y in pos && pos[x]>pos[y]) return 0
  }
  return 1
}
{
  gsub(/^[ \t]+|[ \t]+$/,"")
  if($0=="") next
  if($0 ~ /\|/){ rx[++nr]=$1; ry[nr]=$2; next }
  up[++nu]=$0
}
END{
  s=0
  for(i=1;i<=nu;i++){
    if(is_correct(up[i])){
      n=split(up[i],a,",")
      s+=a[int(n/2)+1]
    }
  }
  print s
}
' input.txt