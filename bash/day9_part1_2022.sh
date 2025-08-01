#!/usr/bin/env bash
set -euo pipefail

awk '
function key(x,y){return x "," y}
BEGIN{
  hx=hy=tx=ty=0
  seen[key(tx,ty)]=1
  count=1
}
{
  dir=$1; steps=$2
  for(i=0;i<steps;i++){
    if(dir=="R") hx++
    else if(dir=="L") hx--
    else if(dir=="U") hy++
    else if(dir=="D") hy--

    dx=hx-tx; dy=hy-ty
    if((dx<0?-dx:dx)>1 || (dy<0?-dy:dy)>1){
      if(dx>0) tx++
      else if(dx<0) tx--
      if(dy>0) ty++
      else if(dy<0) ty--
    }
    k=key(tx,ty)
    if(!(k in seen)){ seen[k]=1; count++ }
  }
}
END{ print count }
' input.txt