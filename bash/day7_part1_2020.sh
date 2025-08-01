#!/usr/bin/env bash
set -euo pipefail

awk -F' bags contain ' '
function id(name,  i){
  if(name in idmap) return idmap[name]
  i=++n; idmap[name]=i; nameOf[i]=name; return i
}
function push(a, v){ a[++a[0]]=v }
function can(u,    i,v){
  if(u==gold) return 1
  if(seen[u]) return seen[u]-1
  for(i=1;i<=adj[u,0];i++){
    v=adj[u,i]
    if(can(v)){ seen[u]=2; return 1 }
  }
  seen[u]=1; return 0
}
BEGIN{ n=0 }
{
  gsub(/\r/,"")
  if($0=="") next
  split($0, p, / bags contain /); if(length(p)<2) next
  outer=p[1]; oid=id(outer)
  contents=p[2]
  if(contents ~ /^no other bags\./) next
  gsub(/, /,"|",contents)
  nitems=split(contents, items, /\|/)
  for(i=1;i<=nitems;i++){
    gsub(/\./,"",items[i])
    split(items[i], t, " ")
    if(t[1] !~ /^[0-9]+$/) continue
    inner=t[2]" "t[3]
    iid=id(inner)
    adj[oid, ++adj[oid,0]]=iid
  }
}
END{
  gold=id("shiny gold")
  for(i=1;i<=n;i++) seen[i]=0
  cnt=0
  for(i=1;i<=n;i++){
    if(i==gold) continue
    if(can(i)) cnt++
  }
  print cnt
}
' input.txt