
#!/bin/bash

awk '
function rot(s, a,i,j,n,v,r){
  n=split(s,a,"/")
  for(i=1;i<=n;i++){
    v=""
    for(j=n;j>=1;j--)v=v substr(a[j],i,1)
    r=(i==1?"":r"/")v
  }
  return r
}
function flip(s, a,i,j,n,v,r){
  n=split(s,a,"/")
  for(i=1;i<=n;i++){
    v=""
    for(j=n;j>=1;j--)v=v substr(a[i],j,1)
    r=(i==1?"":r"/")v
  }
  return r
}
BEGIN{
  g[0]=".#.";g[1]="..#";g[2]="###";sz=3
  while((getline < "input.txt") > 0){
    split($0,a," => ");k=a[1];v=a[2];n=split(v,p,"/")
    for(i=0;i<4;i++){
      fk=flip(k)
      for(m=1;m<=n;m++){rules_p[k,m]=p[m];rules_p[fk,m]=p[m]}
      k=rot(k)
    }
  }
  for(it=0;it<18;it++){
    step=(sz%2==0?2:3);ns=step+1;nsz=sz/step*ns;delete ng
    for(y=0;y<sz;y+=step){
      br=y/step
      for(x=0;x<sz;x+=step){
        b=substr(g[y],x+1,step)
        for(dy=1;dy<step;dy++)b=b "/" substr(g[y+dy],x+1,step)
        for(dy=0;dy<ns;dy++)ng[br*ns+dy]=ng[br*ns+dy] rules_p[b,dy+1]
      }
    }
    delete g;for(i=0;i<nsz;i++)g[i]=ng[i];sz=nsz
  }
  for(i=0;i<sz;i++)c+=gsub(/#/,"#",g[i]);print c
}'
