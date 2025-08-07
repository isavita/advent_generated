
#!/usr/bin/env bash
awk '
BEGIN{FS=""}
{
  for(i=1;i<=NF;i++) grid[NR,i]=$i
  rows=NR
  cols=NF
}
END{
  total=0
  for(r=1;r<=rows;r++){
    for(c=1;c<=cols;c++){
      key=r","c
      if(!(key in visited)){
        area=0; perimeter=0
        queue[1]=key; head=1; tail=1
        visited[key]=1
        while(head<=tail){
          split(queue[head],pos,",")
          cr=pos[1]; cc=pos[2]
          area++
          ch=grid[cr,cc]
          # top
          if(cr>1){
            if(grid[cr-1,cc]!=ch) perimeter++
            else if(!( (cr-1)","cc in visited)){
              tail++; queue[tail]=(cr-1)","cc; visited[(cr-1)","cc]=1
            }
          }else perimeter++
          # bottom
          if(cr<rows){
            if(grid[cr+1,cc]!=ch) perimeter++
            else if(!( (cr+1)","cc in visited)){
              tail++; queue[tail]=(cr+1)","cc; visited[(cr+1)","cc]=1
            }
          }else perimeter++
          # left
          if(cc>1){
            if(grid[cr,cc-1]!=ch) perimeter++
            else if(!( cr","(cc-1) in visited)){
              tail++; queue[tail]=cr","(cc-1); visited[cr","(cc-1)]=1
            }
          }else perimeter++
          # right
          if(cc<cols){
            if(grid[cr,cc+1]!=ch) perimeter++
            else if(!( cr","(cc+1) in visited)){
              tail++; queue[tail]=cr","(cc+1); visited[cr","(cc+1)]=1
            }
          }else perimeter++
          head++
        }
        total+=area*perimeter
      }
    }
  }
  print total
}
' input.txt
