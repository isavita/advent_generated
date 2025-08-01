#!/usr/bin/env bash
# Advent of Code 2023 Day 22: Sand Slabs
# Reads input from input.txt and prints:
# Part 1: number of bricks that can be safely disintegrated
# Part 2: sum of number of bricks that would fall for each single-brick disintegration
# Implementation uses bash + awk for clarity and performance.

set -euo pipefail

main() {
  awk -F'[~,]' '
  function min(a,b){return a<b?a:b}
  function max(a,b){return a>b?a:b}

  # Sort indices by z1 ascending (stable settle order)
  function sort_by_z1(n,   i,j,tmp) {
    # simple stable insertion sort on idx array using z1[]
    for (i=2;i<=n;i++){
      tmp_idx = idx[i]
      tmp_z = z1[tmp_idx]
      j = i-1
      while (j>=1 && z1[idx[j]]>tmp_z){
        idx[j+1]=idx[j]; j--
      }
      idx[j+1]=tmp_idx
    }
  }

  # Compute support relationships after settling.
  # Approach:
  # 1) Normalize bricks, store endpoints and axis.
  # 2) Settle bricks downward by processing in increasing z1.
  #    For each brick, find the highest contact under any (x,y) it occupies:
  #      new_z1 = max over cells (height[x,y] + 1), where height is current top z at (x,y).
  #    Then place brick: update all its covered cells height to z2.
  # 3) Build supports: for each brick, for each covered cell (x,y),
  #      if height at (x,y) equals brick.z1-1 before placing? We already placed.
  #    Instead, determine supporters by checking, for each cell, if there is a brick whose top z == brick.z1-1.
  #    We keep at each (x,y) a map: which brick occupies that top z (top_id[x,y]).
  #
  # To accomplish this, during settling, we also track owner at each (x,y,z) top layer: we only need the top brick id at each (x,y)
  # since stacking is vertical without gaps after settling.

  BEGIN{
    n=0
  }
  {
    # parse line: x1,y1,z1~x2,y2,z2 with FS split by [~,]
    ++n
    rx1=min($1,$4); ry1=min($2,$5); rz1=min($3,$6)
    rx2=max($1,$4); ry2=max($2,$5); rz2=max($3,$6)
    x1[n]=rx1; y1[n]=ry1; z1[n]=rz1
    x2[n]=rx2; y2[n]=ry2; z2[n]=rz2
    if (rx1!=rx2){ axis[n]="x" }
    else if (ry1!=ry2){ axis[n]="y" }
    else { axis[n]="z" }
    idx[n]=n
  }
  END{
    # settle
    # height[x,y] = current top z at that column
    # topid[x,y]  = brick id currently at top for that column
    PROCINFO["sorted_in"]="@ind_str_asc" # for deterministic iteration, optional

    sort_by_z1(n)

    # temp arrays to hold new z after settling
    for(i=1;i<=n;i++){ nz1[i]=z1[i]; nz2[i]=z2[i] }

    # Use associative arrays for height and topid keyed as "x,y"
    delete height
    delete topid

    for (ord=1; ord<=n; ord++){
      b = idx[ord]
      # compute drop
      need = 1
      if (axis[b]=="x"){
        for (xx=x1[b]; xx<=x2[b]; xx++){
          key=xx "," y1[b]
          hz = (key in height)?height[key]:0
          if (hz+1>need) need=hz+1
        }
      } else if (axis[b]=="y"){
        for (yy=y1[b]; yy<=y2[b]; yy++){
          key=x1[b] "," yy
          hz = (key in height)?height[key]:0
          if (hz+1>need) need=hz+1
        }
      } else { # vertical
        key=x1[b] "," y1[b]
        hz = (key in height)?height[key]:0
        if (hz+1>need) need=hz+1
      }
      drop = z1[b]-need
      nz1[b]=z1[b]-drop
      nz2[b]=z2[b]-drop

      # place and update height and topid
      if (axis[b]=="x"){
        for (xx=x1[b]; xx<=x2[b]; xx++){
          key=xx "," y1[b]
          height[key]=nz2[b]
          topid[key]=b
        }
      } else if (axis[b]=="y"){
        for (yy=y1[b]; yy<=y2[b]; yy++){
          key=x1[b] "," yy
          height[key]=nz2[b]
          topid[key]=b
        }
      } else {
        key=x1[b] "," y1[b]
        height[key]=nz2[b]
        topid[key]=b
      }
    }

    # Build supporters and supported-by relations.
    # To find supporters of brick b:
    #  For each covered (x,y), check what brick was at height nz1[b]-1 after settling.
    # We need to know which brick owns that height. We can reconstruct by scanning bricks placed in order again,
    # tracking for each (x,y) a stack of segments or mapping from z range to brick id.
    # Simpler: build a 3D occupancy map per (x,y) column as ranges, but that could be large.
    # Efficient approach: during second pass in settle order, maintain for each (x,y) the current top z and top brick id.
    # Additionally, when placing a brick b, for each covered (x,y), the supporter is whatever brick is currently at top with top z == nz1[b]-1.
    # That works because we place in increasing nz1.

    delete height
    delete topid
    # reset for second pass
    for (ord=1; ord<=n; ord++){
      b = idx[ord]
      supporters[b]=""
      supcount[b]=0
      # find supporters per covered (x,y)
      if (axis[b]=="x"){
        for (xx=x1[b]; xx<=x2[b]; xx++){
          key=xx "," y1[b]
          hz = (key in height)?height[key]:0
          if (hz == nz1[b]-1){
            sid = (key in topid)?topid[key]:0
            if (sid>0 && !seen_sup[b, sid]){ supporters[b]=supporters[b] sid " "; seen_sup[b, sid]=1; supcount[b]++ }
          }
        }
      } else if (axis[b]=="y"){
        for (yy=y1[b]; yy<=y2[b]; yy++){
          key=x1[b] "," yy
          hz = (key in height)?height[key]:0
          if (hz == nz1[b]-1){
            sid = (key in topid)?topid[key]:0
            if (sid>0 && !seen_sup[b, sid]){ supporters[b]=supporters[b] sid " "; seen_sup[b, sid]=1; supcount[b]++ }
          }
        }
      } else {
        key=x1[b] "," y1[b]
        hz = (key in height)?height[key]:0
        if (hz == nz1[b]-1){
          sid = (key in topid)?topid[key]:0
          if (sid>0 && !seen_sup[b, sid]){ supporters[b]=supporters[b] sid " "; seen_sup[b, sid]=1; supcount[b]++ }
        }
      }
      # place b
      if (axis[b]=="x"){
        for (xx=x1[b]; xx<=x2[b]; xx++){
          key=xx "," y1[b]
          height[key]=nz2[b]
          topid[key]=b
        }
      } else if (axis[b]=="y"){
        for (yy=y1[b]; yy<=y2[b]; yy++){
          key=x1[b] "," yy
          height[key]=nz2[b]
          topid[key]=b
        }
      } else {
        key=x1[b] "," y1[b]
        height[key]=nz2[b]
        topid[key]=b
      }
    }

    # Build dependents (who each brick supports)
    for (i=1;i<=n;i++){ dependents[i]="" }
    for (b=1;b<=n;b++){
      split(supporters[b], arr, " ")
      for (k in arr){
        s=arr[k]; if (s=="") continue
        # append b to dependents[s]
        dependents[s]=dependents[s] b " "
      }
    }

    # Part 1: count bricks that can be safely removed.
    # A brick is safe if all of its dependents have at least two supporters.
    safe=0
    for (i=1;i<=n;i++){
      ok=1
      split(dependents[i], ds, " ")
      for (k in ds){
        d=ds[k]; if (d=="") continue
        # if dependent has only one supporter (i), then removing i causes it to fall
        # supporters counted in supcount[d], but could include duplicates; we removed duplicates earlier
        # Need to check if supporters of d excluding i is >=1
        # Since supcount[d] is number of distinct supporters, if supcount[d] == 1 then i is sole supporter.
        if (supcount[d]==1){ ok=0; break }
      }
      if (ok) safe++
    }

    # Part 2: for each brick removed, simulate cascade using DAG over support relations.
    # For removal of r: any brick b falls if all its supporters are in the fallen set.
    # Algorithm: BFS/queue. Start fallen[r]=1. For each brick b, track remaining supporters count rem[b]=supcount[b].
    # When a supporter falls, decrement rem for its dependents; if rem becomes 0, mark b falls and enqueue.
    total_fall_sum=0
    # Precompute adjacency lists as arrays for speed
    for (i=1;i<=n;i++){
      # parse dependents list into an array dep[i][*]
      depN[i]=0
      split(dependents[i], ds, " ")
      for (k in ds){
        d=ds[k]; if (d=="") continue
        dep[i, ++depN[i]] = d+0
      }
      # store supporters list similarly
      supN[i]=0
      split(supporters[i], sp, " ")
      for (k in sp){
        s=sp[k]; if (s=="") continue
        sup[i, ++supN[i]] = s+0
      }
    }

    for (r=1;r<=n;r++){
      # init remaining supporters
      for (i=1;i<=n;i++){ rem[i]=supN[i]; fallen[i]=0 }
      qh=1; qt=0
      # remove r
      fallen[r]=1
      fallen_count=0
      # decrement all dependents of r
      for (j=1;j<=depN[r]; j++){
        d = dep[r,j]
        rem[d]--
        if (rem[d]==0 && !fallen[d]){
          qt++; q[qt]=d; fallen[d]=1
        }
      }
      # process queue
      while (qh<=qt){
        u=q[qh]; qh++
        fallen_count++
        # when u falls, reduce its dependents
        for (j=1;j<=depN[u]; j++){
          v = dep[u,j]
          rem[v]--
          if (rem[v]==0 && !fallen[v]){
            qt++; q[qt]=v; fallen[v]=1
          }
        }
      }
      total_fall_sum += fallen_count
    }

    print safe
    print total_fall_sum
  }
  ' input.txt
}

main "$@"