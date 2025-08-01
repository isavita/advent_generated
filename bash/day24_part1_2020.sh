#!/usr/bin/env bash
set -euo pipefail

awk '
function key(q,r){return q "," r}
BEGIN{
  dirs_q[0]=1;  dirs_r[0]=0;   # e
  dirs_q[1]=0;  dirs_r[1]=1;   # se
  dirs_q[2]=-1; dirs_r[2]=1;   # sw
  dirs_q[3]=-1; dirs_r[3]=0;   # w
  dirs_q[4]=0;  dirs_r[4]=-1;  # nw
  dirs_q[5]=1;  dirs_r[5]=-1;  # ne
}
{
  q=0; r=0; i=1; n=length($0)
  while(i<=n){
    c=substr($0,i,1)
    if(c=="e"){idx=0; i++}
    else if(c=="w"){idx=3; i++}
    else{
      d=substr($0,i,2)
      if(d=="se") idx=1
      else if(d=="sw") idx=2
      else if(d=="nw") idx=4
      else if(d=="ne") idx=5
      i+=2
    }
    q+=dirs_q[idx]; r+=dirs_r[idx]
  }
  k=key(q,r)
  state[k]=1-state[k]
}
END{
  c=0
  for(k in state) if(state[k]==1) c++
  print c
}
' input.txt