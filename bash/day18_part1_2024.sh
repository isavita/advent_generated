
#!/usr/bin/env bash
set -euo pipefail

awk '
BEGIN{
  size=71
  for(i=0;i<size;i++){
    for(j=0;j<size;j++){
      obs[i "," j]=0
      vis[i "," j]=0
    }
  }
}
NR<=1024{
  split($0,a,",")
  x=a[1]+0; y=a[2]+0
  if(x>=0 && x<size && y>=0 && y<size) obs[y "," x]=1
}
END{
  if(obs["0,0"]==1 || obs[(size-1) "," (size-1)]==1){ print "No path"; exit }
  qx[1]=0; qy[1]=0; qs[1]=0; head=1; tail=1
  vis["0,0"]=1
  dx[1]=1; dy[1]=0
  dx[2]=-1; dy[2]=0
  dx[3]=0; dy[3]=1
  dx[4]=0; dy[4]=-1
  while(head<=tail){
    x=qx[head]; y=qy[head]; s=qs[head]; head++
    if(x==size-1 && y==size-1){ print s; exit }
    for(k=1;k<=4;k++){
      nx=x+dx[k]; ny=y+dy[k]
      key=ny "," nx
      if(nx>=0 && ny>=0 && nx<size && ny<size && obs[key]==0 && vis[key]==0){
        vis[key]=1
        tail++
        qx[tail]=nx; qy[tail]=ny; qs[tail]=s+1
      }
    }
  }
  print "No path"
}
' input.txt
