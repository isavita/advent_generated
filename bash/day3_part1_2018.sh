#!/usr/bin/env bash
set -euo pipefail

awk '
function max(a,b){return a>b?a:b}
BEGIN{
  FS="[ #@,:x]+"
}
{
  left=$3; top=$4; width=$5; height=$6
  for(i=left;i<left+width;i++)
    for(j=top;j<top+height;j++){
      k=i "," j
      cnt[k]++
    }
}
END{
  for(k in cnt) if(cnt[k]>1) overlap++
  print overlap+0
}
' input.txt