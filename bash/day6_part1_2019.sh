#!/usr/bin/env bash
set -euo pipefail

awk -F '[)]' '
{ parent=$1; child=$2; g[parent]=g[parent] ? g[parent] SUBSEP child : child }
/.*/{nodes[parent]=1; nodes[child]=1}
END{
  split("", vis)
  stack_n=0
  push("COM",0)
  total=0
  while(stack_n>0){
    pop(node,depth)
    total+=depth
    if(node in g){
      n=split(g[node], kids, SUBSEP)
      for(i=1;i<=n;i++) push(kids[i], depth+1)
    }
  }
  print total
}
function push(n,d){stack_n++; stack_node[stack_n]=n; stack_depth[stack_n]=d}
function pop(   n,d){n=stack_node[stack_n]; d=stack_depth[stack_n]; stack_n--; node=n; depth=d}
' input.txt