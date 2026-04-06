
#!/bin/bash
awk '{
  R=NR; C=split($0, a, ""); for(i=1; i<=C; i++) g[R,i]=a[i]
}
END {
  while(1) {
    u=0
    for(r=1; r<=R; r++) {
      for(c=1; c<=C; c++) {
        v=g[r,c]; if(v==".") { n[r,c]="."; continue }
        k=0
        for(i=-1; i<=1; i++) {
          for(j=-1; j<=1; j++) {
            if(i==0 && j==0) continue
            y=r+i; x=c+j
            while(y>0 && y<=R && x>0 && x<=C) {
              if(g[y,x]=="L") break
              if(g[y,x]=="#") { k++; break }
              y+=i; x+=j
            }
          }
        }
        if(v=="L" && k==0) { n[r,c]="#"; u++ }
        else if(v=="#" && k>=5) { n[r,c]="L"; u++ }
        else n[r,c]=v
      }
    }
    if(!u) break
    for(r=1; r<=R; r++) for(c=1; c<=C; c++) g[r,c]=n[r,c]
  }
  for(r=1; r<=R; r++) for(c=1; c<=C; c++) if(g[r,c]=="#") t++
  print t+0
}' input.txt
