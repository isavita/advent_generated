#!/usr/bin/awk -f
BEGIN{
  h=0; w=0
  while((getline line<"input.txt")>0){
    h++
    len=length(line)
    w=len
    for(i=1;i<=len;i++){
      c=substr(line,i,1)
      grid[h,i]=c
      if(c=="^"){startX=i;startY=h;startDir=0}
      else if(c==">"){startX=i;startY=h;startDir=1}
      else if(c=="v"){startX=i;startY=h;startDir=2}
      else if(c=="<"){startX=i;startY=h;startDir=3}
    }
  }
  close("input.txt")
  grid[startY,startX]="."
}
END{
  count=0
  for(y=1;y<=h;y++){
    for(x=1;x<=w;x++){
      if(grid[y,x]=="." && !(y==startY && x==startX)){
        grid[y,x]="#"
        if(loops(startX,startY,startDir,h,w)) count++
        grid[y,x]="."
      }
    }
  }
  print count
}
function loops(sx,sy,sdir,h,w,   x,y,dir,dx,dy,nx,ny,step,key){
  delete seen
  x=sx; y=sy; dir=sdir
  for(step=0;step<2000000;step++){
    key=y","x","dir
    if(key in seen) return 1
    seen[key]=1
    if(dir==0){dx=0;dy=-1}
    else if(dir==1){dx=1;dy=0}
    else if(dir==2){dx=0;dy=1}
    else {dx=-1;dy=0}
    nx=x+dx; ny=y+dy
    if(nx<1||nx>w||ny<1||ny>h) return 0
    if(grid[ny,nx]=="#"){dir=(dir+1)%4; continue}
    x=nx; y=ny
  }
  return 0
}