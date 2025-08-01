#!/usr/bin/env bash
set -euo pipefail

awk -F- '
BEGIN{
  n=0
}
function getid(s,  i){
  if(s in id) return id[s]
  i=n++
  id[s]=i
  name[i]=s
  return i
}
{
  gsub(/\r/,"")
  if(NF!=2||$1==""||$2=="") next
  a=$1; b=$2
  i=getid(a); j=getid(b)
  adj[i SUBSEP j]=1
  adj[j SUBSEP i]=1
}
END{
  cnt=0
  for(i=0;i<n;i++){
    for(j=i+1;j<n;j++){
      if(!( (i SUBSEP j) in adj )) continue
      for(k=j+1;k<n;k++){
        if( ((j SUBSEP k) in adj) && ((k SUBSEP i) in adj) ){
          if(substr(name[i],1,1)=="t" || substr(name[j],1,1)=="t" || substr(name[k],1,1)=="t"){
            cnt++
          }
        }
      }
    }
  }
  print cnt
}
' input.txt