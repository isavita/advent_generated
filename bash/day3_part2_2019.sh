
#!/bin/bash
awk -F, '{
  x=y=s=0
  for(i=1;i<=NF;i++){
    d=substr($i,1,1); n=substr($i,2)
    while(n--){
      s++; if(d=="U")y++; else if(d=="D")y--; else if(d=="L")x--; else x++
      if(NR==1){
        if(!((x,y) in p)) p[x,y]=s
      } else {
        if((x,y) in p){
          t=s+p[x,y]
          if(!m || t<m) m=t
        }
      }
    }
  }
} END { print m }' input.txt
