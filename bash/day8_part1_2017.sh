#!/usr/bin/env bash
awk '
function get(r){return (r in reg)?reg[r]:0}
function set(r,v){reg[r]=v}
function cmp(a,op,b){
  if(op==">") return a>b
  if(op==">=") return a>=b
  if(op=="<") return a<b
  if(op=="<=") return a<=b
  if(op=="==") return a==b
  if(op=="!=") return a!=b
  return 0
}
{
  reg1=$1; op=$2; amt=$3; condreg=$5; condop=$6; condval=$7
  if(cmp(get(condreg),condop,condval)){
    v=get(reg1)
    if(op=="inc") v+=amt
    else if(op=="dec") v-=amt
    set(reg1,v)
  }
}
END{
  max=0
  for(r in reg) if(reg[r]>max) max=reg[r]
  print max
}
' input.txt