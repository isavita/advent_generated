
#!/usr/bin/env bash
awk '
BEGIN{FS=""}
{
  for(i=1;i<=NF;i++) g[NR,i]=$i
  cols=NF
}
END{
  rows=NR
  p1=0
  for(r=1;r<=rows;r++) for(c=1;c<=cols;c++) if(g[r,c]=="@"){
    n=0
    for(dr=-1;dr<=1;dr++) for(dc=-1;dc<=1;dc++){
      if(!dr&&!dc) continue
      rr=r+dr; cc=c+dc
      if(rr<1||rr>rows||cc<1||cc>cols) continue
      if(g[rr,cc]=="@") n++
    }
    if(n<4) p1++
  }
  print p1

  for(r=1;r<=rows;r++) for(c=1;c<=cols;c++) h[r,c]=g[r,c]

  total=0
  while(1){
    cnt=0
    for(r=1;r<=rows;r++) for(c=1;c<=cols;c++) if(h[r,c]=="@"){
      n=0
      for(dr=-1;dr<=1;dr++) for(dc=-1;dc<=1;dc++){
        if(!dr&&!dc) continue
        rr=r+dr; cc=c+dc
        if(rr<1||rr>rows||cc<1||cc>cols) continue
        if(h[rr,cc]=="@") n++
      }
      if(n<4) rem[++cnt]=r SUBSEP c
    }
    if(!cnt) break
    for(i=1;i<=cnt;i++){
      split(rem[i],a,SUBSEP)
      h[a[1],a[2]]="."
    }
    total+=cnt
  }
  print total
}
' input.txt
