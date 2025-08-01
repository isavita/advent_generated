#!/usr/bin/env bash
# Advent of Code 2022 - Day 18: Boiling Boulders
# Reads from input.txt and prints:
# Part 1: total surface area (including internal cavities)
# Part 2: exterior surface area (excluding internal cavities)
set -euo pipefail

main() {
  local input="input.txt"
  [[ -f "$input" ]] || { echo "input.txt not found" >&2; exit 1; }

  # Use awk for performance and clarity
  awk -F',' '
  function key(x,y,z){ return x "," y "," z }
  function S(k){ return (k in cubes) }          # cube set membership
  function N(x,y,z, arr,    i){
    # 6-axis neighbors
    arr[0]=key(x+1,y,z); arr[1]=key(x-1,y,z);
    arr[2]=key(x,y+1,z); arr[3]=key(x,y-1,z);
    arr[4]=key(x,y,z+1); arr[5]=key(x,y,z-1);
    return 6
  }
  BEGIN{
    minx=miny=minz=1e9; maxx=maxy=maxz=-1e9
  }
  NF==3{
    x=$1+0; y=$2+0; z=$3+0
    k=key(x,y,z)
    if(!(k in cubes)){
      cubes[k]=1
      if(x<minx)minx=x; if(x>maxx)maxx=x
      if(y<miny)miny=y; if(y>maxy)maxy=y
      if(z<minz)minz=z; if(z>maxz)maxz=z
    }
  }
  END{
    # Part 1: sum exposed faces against neighbors not in cubes
    part1=0
    for(k in cubes){
      split(k,p,","); x=p[1]; y=p[2]; z=p[3]
      ncount=N(x,y,z,nb)
      for(i=0;i<ncount;i++){
        if(!(nb[i] in cubes)) part1++
      }
    }

    # Part 2: flood-fill external air from expanded bounding box
    bx0=minx-1; by0=miny-1; bz0=minz-1
    bx1=maxx+1; by1=maxy+1; bz1=maxz+1

    # Simple queue using indexed arrays
    qh=0; qt=0
    start=key(bx0,by0,bz0)
    seen[start]=1; q[qt++]=start

    ext=0
    while(qh<qt){
      cur=q[qh++]
      split(cur,p,","); x=p[1]; y=p[2]; z=p[3]

      ncount=N(x,y,z,nb)
      for(i=0;i<ncount;i++){
        nk=nb[i]
        split(nk,r,","); nx=r[1]; ny=r[2]; nz=r[3]
        # Stay within expanded bounds
        if(nx<bx0||nx>bx1||ny<by0||ny>by1||nz<bz0||nz>bz1) continue

        if(nk in cubes){
          # Neighbor is lava: contributes one exterior face
          ext++
        } else if(!(nk in seen)){
          seen[nk]=1
          q[qt++]=nk
        }
      }
    }

    print part1
    print ext
  }' "$input"
}

main "$@"