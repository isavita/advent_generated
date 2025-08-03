#!/usr/bin/env bash
set -euo pipefail

awk '
BEGIN{
  getline polymer < "input.txt"
  while ((getline line < "input.txt") > 0) {
    if (line=="") continue
    split(line,a," -> ")
    rules[a[1]]=a[2]
  }
}
function apply(poly,   i,pair,newp,c){
  newp=substr(poly,1,1)
  for(i=1;i<length(poly);i++){
    pair=substr(poly,i,2)
    c=rules[pair]
    if(c!="") newp=newp c
    newp=newp substr(poly,i+1,1)
  }
  return newp
}
function count(poly,   i,c,ch){
  delete cnt
  for(i=1;i<=length(poly);i++){
    ch=substr(poly,i,1)
    cnt[ch]++
  }
  min=1e18; max=-1
  for(c in cnt){
    if(cnt[c]<min) min=cnt[c]
    if(cnt[c]>max) max=cnt[c]
  }
  print max-min
}
END{
  for(step=0;step<10;step++) polymer=apply(polymer)
  count(polymer)
}
' /dev/null