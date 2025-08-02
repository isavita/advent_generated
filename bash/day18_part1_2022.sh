#!/usr/bin/env bash
set -euo pipefail

awk -F, '
function key(x,y,z){ return x "|" y "|" z }
BEGIN{
  while ((getline line < "input.txt") > 0) {
    if (line ~ /[^0-9,-]/ || line == "") continue
    split(line,a,","); x=a[1]+0; y=a[2]+0; z=a[3]+0
    cubes[key(x,y,z)]=1
  }
  dirs[1]="1|0|0";   dirs[2]="-1|0|0"
  dirs[3]="0|1|0";   dirs[4]="0|-1|0"
  dirs[5]="0|0|1";   dirs[6]="0|0|-1"

  for (c in cubes) {
    split(c,p,"|"); x=p[1]+0; y=p[2]+0; z=p[3]+0
    exposed=6
    for (i=1;i<=6;i++){
      split(dirs[i],d,"|")
      k=key(x+d[1], y+d[2], z+d[3])
      if (k in cubes) exposed--
    }
    surface+=exposed
  }
  print surface+0
}' /dev/null