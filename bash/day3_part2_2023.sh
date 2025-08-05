#!/usr/bin/env bash
set -euo pipefail

awk '
function add_part(p) {
  parts_cnt++
  parts_n[parts_cnt]=p["n"]
  for (x=p["xmin"]; x<=p["xmax"]; x++) parts_grid[x SUBSEP p["y"]]=parts_cnt
}
BEGIN{
  FS=""
}
{
  y=NR-1
  curr=0
  for (x=0; x<NF; x++) {
    c=$((x+1))
    grid[x SUBSEP y]=c
    if (c ~ /[0-9]/) {
      if (!curr) {
        xmin=x; xmax=x; n=c+0; curr=1
      } else {
        n = n*10 + c
        xmax=x
      }
    } else if (curr) {
      p["xmin"]=xmin; p["xmax"]=xmax; p["y"]=y; p["n"]=n
      add_part(p)
      curr=0
    }
  }
  if (curr) {
    p["xmin"]=xmin; p["xmax"]=xmax; p["y"]=y; p["n"]=n
    add_part(p)
  }
}
END{
  sum=0
  for (g in grid) {
    split(g, a, SUBSEP); gx=a[1]; gy=a[2]
    if (grid[g]=="*") {
      delete seen
      cnt=0
      for (dy=-1; dy<=1; dy++) for (dx=-1; dx<=1; dx++) if (dx||dy) {
        k=(gx+dx) SUBSEP (gy+dy)
        if (k in parts_grid) {
          id=parts_grid[k]
          if (!(id in seen)) { seen[id]=1; ids[++cnt]=id }
        }
      }
      if (cnt==2) sum += parts_n[ids[1]] * parts_n[ids[2]]
    }
  }
  print sum
}
' input.txt