#!/usr/bin/env bash
set -euo pipefail

awk '
function push(dir){stack[++sp]=dir}
function pop(){if(sp>1) sp--}
function join(){s="/"; for(i=2;i<=sp;i++){s=s stack[i] (i<sp?"/":"")} return s}
{
  if($1=="$" && $2=="cd"){
    if($3=="/"){sp=0; push("/")}
    else if($3==".."){pop()}
    else{push($3)}
  } else if($1=="dir"){next}
  else if($1 ~ /^[0-9]+$/){
    size=$1
    for(i=1;i<=sp;i++){
      path="/"
      if(i>1){path="/"; for(j=2;j<=i;j++){path=path stack[j] (j<i?"/":"")}}
      sizes[path]+=size
    }
  }
}
END{
  sum=0
  for(p in sizes) if(sizes[p]<=100000) sum+=sizes[p]
  print sum
}
' input.txt