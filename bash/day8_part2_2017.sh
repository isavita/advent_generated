#!/usr/bin/env bash
set -euo pipefail

awk '
function val(r){return r in reg?reg[r]:0}
BEGIN{maxv=0}
{
  regn=$1; op=$2; amt=$3+0; condreg=$5; cop=$6; cval=$7+0
  cr=val(condreg)
  ok=0
  if(cop==">") ok=(cr>cval)
  else if(cop==">=") ok=(cr>=cval)
  else if(cop=="<") ok=(cr<cval)
  else if(cop=="<=") ok=(cr<=cval)
  else if(cop=="==") ok=(cr==cval)
  else if(cop=="!=") ok=(cr!=cval)
  if(ok){
    if(op=="inc") reg[regn]=val(regn)+amt
    else if(op=="dec") reg[regn]=val(regn)-amt
    if(reg[regn]>maxv) maxv=reg[regn]
  }
}
END{print maxv}
' input.txt