
#!/bin/bash
awk '
function r(  i,j,v,d,f){
  while(1){
    f=0
    for(i=1;i<n;i++)
      if(dep[i]>4 && dep[i]==dep[i+1]){
        if(i>1) val[i-1]+=val[i]
        if(i+1<n) val[i+2]+=val[i+1]
        val[i]=0; dep[i]--
        for(j=i+1;j<n;j++){val[j]=val[j+1]; dep[j]=dep[j+1]}
        n--; f=1; break
      }
    if(f) continue
    for(i=1;i<=n;i++)
      if(val[i]>=10){
        v=val[i]; d=dep[i]
        for(j=n;j>i;j--){val[j+1]=val[j]; dep[j+1]=dep[j]}
        n++; val[i]=int(v/2); dep[i]=d+1; val[i+1]=int((v+1)/2); dep[i+1]=d+1
        f=1; break
      }
    if(!f) break
  }
}
function m(  v,d,nn,i,j,x){
  for(i=1;i<=n;i++){v[i]=val[i]; d[i]=dep[i]}
  nn=n
  while(nn>1){
    x=0; for(i=1;i<=nn;i++) if(d[i]>x) x=d[i]
    for(i=1;i<nn;i++)
      if(d[i]==x && d[i]==d[i+1]){
        v[i]=3*v[i]+2*v[i+1]; d[i]--
        for(j=i+1;j<nn;j++){v[j]=v[j+1]; d[j]=d[j+1]}
        nn--; break
      }
  }
  return v[1]
}
{
  s=$0; gsub(/\[/," [ ",s); gsub(/\]/," ] ",s); gsub(/,/," ",s); k=split(s,a)
  if(NR>1) for(i=1;i<=n;i++) dep[i]++
  p=0
  for(i=1;i<=k;i++)
    if(a[i]=="[") p++
    else if(a[i]=="]") p--
    else if(a[i]~/[0-9]/){n++; val[n]=a[i]; dep[n]=p+(NR>1)}
  if(NR>1) r()
}
END {print m()}' input.txt
