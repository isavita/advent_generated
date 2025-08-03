#!/usr/bin/env bash
set -euo pipefail

awk -v CYCLES=1000 '
function idx(name,   i){i=H[name]; return (i==""?-1:i)}
function addmod(name,type,   i){
  i=mc++
  M[i,"name"]=name
  M[i,"type"]=type
  M[i,"nd"]=0
  M[i,"state"]=0
  M[i,"nm"]=0
  H[name]=i
  return i
}
function adddest(mi,dname,   di){
  di=idx(dname)
  D[mi, M[mi,"nd"]++]=di
}
function addmem(mi,src){
  M[mi,"memsrc",M[mi,"nm"]]=src
  M[mi,"mempulse",M[mi,"nm"]]=0
  M[mi,"nm"]++
}
function qpush(val,from,to){
  Qh[qtail]=val; Qf[qtail]=from; Qt[qtail]=to
  qtail=(qtail+1)%QMAX
  qcnt++
}
function qpop(   v){
  v=Qh[qhead]; pv=v SUBSEP Qf[qhead] SUBSEP Qt[qhead]
  qhead=(qhead+1)%QMAX
  qcnt--
  return pv
}
function splitpulse(p,   a){ split(p,a,SUBSEP); return a[1] }
function trim(s){ sub(/^[ \t\r\n]+/,"",s); sub(/[ \t\r\n]+$/,"",s); return s }
BEGIN{
  QMAX=65536
  mc=0
}
{
  line=$0
  if(line=="") next
  split(line,parts," -> ")
  np=parts[1]; dp=parts[2]
  t="o"; name=np
  c=substr(np,1,1)
  if(c=="%" || c=="&"){ t=c; name=substr(np,2) }
  else if(np=="broadcaster"){ t="b"; name=np }
  addmod(name,t)
  rawdest[NR]=dp
  order[NR]=name
}
END{
  for(i=1;i<=NR;i++){
    mi=idx(order[i])
    n=split(rawdest[i],arr,/,/)
    for(j=1;j<=n;j++){
      d=trim(arr[j])
      adddest(mi,d)
      if(idx(d)==-1 && d!="output" && d!="rx"){
        addmod(d,"o")
      }
    }
  }
  for(si=0;si<mc;si++){
    nd=M[si,"nd"]
    for(j=0;j<nd;j++){
      di=D[si,j]
      if(di!=-1 && M[di,"type"]=="&"){
        addmem(di,si)
      }
    }
  }
  low=0; high=0
  bidx=idx("broadcaster")
  for(c=0;c<CYCLES;c++){
    qhead=qtail=qcnt=0
    qpush(0,-1,bidx)
    while(qcnt>0){
      p=qpop(); split(p,a,SUBSEP)
      val=a[1]+0; from=a[2]+0; to=a[3]+0
      if(val==0) low++; else high++
      if(to==-1) continue
      t=M[to,"type"]
      send=1; out=0
      if(t=="%"){
        if(val==0){
          M[to,"state"]=1-M[to,"state"]
          out=(M[to,"state"]?1:0)
        } else send=0
      } else if(t=="&"){
        nm=M[to,"nm"]
        for(i=0;i<nm;i++){
          if(M[to,"memsrc",i]==from){ M[to,"mempulse",i]=val; break }
        }
        allh=1
        for(i=0;i<nm;i++){
          if(M[to,"mempulse",i]==0){ allh=0; break }
        }
        out=(allh?0:1)
      } else if(t=="b"){
        out=val
      } else {
        send=0
      }
      if(send){
        nd=M[to,"nd"]
        for(j=0;j<nd;j++){
          di=D[to,j]+0
          qpush(out,to,di)
        }
      }
    }
  }
  printf "%llu\n", low*high
}
' input.txt