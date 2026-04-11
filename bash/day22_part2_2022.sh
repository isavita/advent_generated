
#!/usr/bin/env bash
awk '
BEGIN{height=0;width=0;mode="MAP"}
{
  gsub(/\r$/,"")
  if(mode=="MAP"){
    if(length($0)==0){mode="PATH";next}
    w=length($0)
    if(w>width)width=w
    r=height
    for(i=1;i<=w;i++)map[r","(i-1)]=substr($0,i,1)
    height++
  }else{
    path=$0
    exit
  }
}
END{solve()}
function cell(r,c){k=r","c;v=map[k];return(v==""?" ":v)}
function wrap(r,c,d,   nr,nc,nd){
  if(d==3&&r==0&&c>=50&&c<100){nr=c+100;nc=0;nd=0}
  else if(d==2&&c==50&&r>=0&&r<50){nr=149-r;nc=0;nd=0}
  else if(d==3&&r==0&&c>=100&&c<150){nr=199;nc=c-100;nd=3}
  else if(d==0&&c==149&&r>=0&&r<50){nr=149-r;nc=99;nd=2}
  else if(d==1&&r==49&&c>=100&&c<150){nr=c-50;nc=99;nd=2}
  else if(d==0&&c==99&&r>=50&&r<100){nr=49;nc=r+50;nd=3}
  else if(d==2&&c==50&&r>=50&&r<100){nr=100;nc=r-50;nd=1}
  else if(d==2&&c==0&&r>=100&&r<150){nr=149-r;nc=50;nd=0}
  else if(d==3&&r==100&&c>=0&&c<50){nr=c+50;nc=50;nd=0}
  else if(d==0&&c==99&&r>=100&&r<150){nr=149-r;nc=149;nd=2}
  else if(d==1&&r==149&&c>=50&&c<100){nr=c+100;nc=49;nd=2}
  else if(d==0&&c==49&&r>=150&&r<200){nr=149;nc=r-100;nd=3}
  else if(d==1&&r==199&&c>=0&&c<50){nr=0;nc=c+100;nd=1}
  else if(d==2&&c==0&&r>=150&&r<200){nr=0;nc=r-100;nd=1}
  else{nr=r;nc=c;nd=d}
  return nr" "nc" "nd
}
function solve(   r,c,d,i,ch,n,step,nr,nc,nd,tmp,ans){
  r=0;c=0;d=0
  while(cell(r,c)!=".")c++
  dr[0]=0;dc[0]=1;dr[1]=1;dc[1]=0;dr[2]=0;dc[2]=-1;dr[3]=-1;dc[3]=0
  p=path
  while(p!=""){
    if(p~/^[0-9]/){
      n=0;i=1;ch=substr(p,i,1)
      while(ch~/[0-9]/){n=n*10+(ch+0);i++;ch=substr(p,i,1)}
      p=substr(p,i)
      for(step=1;step<=n;step++){
        nr=r+dr[d];nc=c+dc[d];nd=d
        if(nr<0||nr>=height||nc<0||nc>=width||cell(nr,nc)==" "){
          tmp=wrap(r,c,d);split(tmp,nums," ");nr=nums[1];nc=nums[2];nd=nums[3]
        }
        if(cell(nr,nc)=="#")break
        r=nr;c=nc;d=nd
      }
    }else{
      ch=substr(p,1,1)
      if(ch=="R")d=(d+1)%4
      else if(ch=="L")d=(d+3)%4
      p=substr(p,2)
    }
  }
  ans=1000*(r+1)+4*(c+1)+d
  print ans
}
' input.txt
