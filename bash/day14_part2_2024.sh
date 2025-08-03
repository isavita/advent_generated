#!/usr/bin/env bash
set -euo pipefail

sizeX=101
sizeY=103

parse() {
  awk -v sx="$sizeX" -v sy="$sizeY" '
  function mod(a,b){r=a%b; if(r<0) r+=b; return r}
  BEGIN{nr=0}
  /^[[:space:]]*$/ {next}
  {
    gsub(/p=/,""); gsub(/ v=/," ");
    split($0,a,/[, ]+/)
    x[nr]=a[1]; y[nr]=a[2]; vx[nr]=a[3]; vy[nr]=a[4]; nr++
  }
  function step(times,    i){
    for(t=0;t<times;t++){
      for(i=0;i<nr;i++){
        x[i]=mod(x[i]+vx[i],sx)
        y[i]=mod(y[i]+vy[i],sy)
      }
    }
  }
  function quad_score(    cx,cy,i,q0,q1,q2,q3){
    cx=int(sx/2); cy=int(sy/2); q0=q1=q2=q3=0
    for(i=0;i<nr;i++){
      if(x[i]<cx){
        if(y[i]<cy) q0++
        else if(y[i]>cy) q1++
      } else if(x[i]>cx){
        if(y[i]<cy) q2++
        else if(y[i]>cy) q3++
      }
    }
    return q0*q1*q2*q3
  }
  function no_overlap(    i,k,key){
    delete seen
    for(i=0;i<nr;i++){
      key=x[i] "," y[i]
      if(key in seen) return 0
      seen[key]=1
    }
    return 1
  }
  function draw(    i,j,key){
    for(j=0;j<sy;j++){
      line=""
      for(i=0;i<sx;i++){
        key=i "," j
        line = line ((key in seen) ? "#" : ".")
      }
      print line
    }
  }
  END{
    # Part 1
    for(i=0;i<nr;i++){x1[i]=x[i]; y1[i]=y[i]; vx1[i]=vx[i]; vy1[i]=vy[i]}
    for(t=0;t<100;t++){
      for(i=0;i<nr;i++){
        x1[i]=mod(x1[i]+vx1[i],sx)
        y1[i]=mod(y1[i]+vy1[i],sy)
      }
    }
    cx=int(sx/2); cy=int(sy/2); q0=q1=q2=q3=0
    for(i=0;i<nr;i++){
      if(x1[i]<cx){
        if(y1[i]<cy) q0++
        else if(y1[i]>cy) q1++
      } else if(x1[i]>cx){
        if(y1[i]<cy) q2++
        else if(y1[i]>cy) q3++
      }
    }
    sf=q0*q1*q2*q3
    print "Part 1 - Safety Factor after 100 seconds: " sf

    # Part 2
    for(i=0;i<nr;i++){xp[i]=x[i]; yp[i]=y[i]}
    seconds=0
    limit=1000000
    while(1){
      delete seen
      overlap=0
      for(i=0;i<nr;i++){
        key=xp[i] "," yp[i]
        if(key in seen){overlap=1; break}
        seen[key]=1
      }
      if(!overlap) break
      if(seconds>=limit){print "Exceeded maximum iterations without finding a unique position configuration."; exit 1}
      for(i=0;i<nr;i++){
        xp[i]=mod(xp[i]+vx[i],sx)
        yp[i]=mod(yp[i]+vy[i],sy)
      }
      seconds++
    }
    print "Part 2 - Fewest seconds to display Easter egg: " seconds
    print "Final positions of robots:"
    delete seen
    for(i=0;i<nr;i++){seen[xp[i] "," yp[i]]=1}
    for(j=0;j<sy;j++){
      line=""
      for(i=0;i<sx;i++){
        key=i "," j
        line = line ((key in seen) ? "#" : ".")
      }
      print line
    }
  }
  ' input.txt
}

parse "$@"