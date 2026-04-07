
#!/bin/bash
awk -F, '
function R(id,  i,o,m1,m2,p1,p2,p3,v1,v2){
  while(1){
    i=m[id,p[id]];o=i%100;m1=int(i/100)%10;m2=int(i/1000)%10
    if(o==99){h[id]=1;return "H"}
    p1=m[id,p[id]+1];p2=m[id,p[id]+2];p3=m[id,p[id]+3]
    v1=(m1?p1:m[id,p1]);v2=(m2?p2:m[id,p2])
    if(o==1){m[id,p3]=v1+v2;p[id]+=4}
    else if(o==2){m[id,p3]=v1*v2;p[id]+=4}
    else if(o==3){if(ip[id]>ic[id])return "W";m[id,p1]=iq[id,ip[id]++];p[id]+=2}
    else if(o==4){p[id]+=2;out=v1;return "O"}
    else if(o==5)p[id]=(v1?v2:p[id]+3)
    else if(o==6)p[id]=(!v1?v2:p[id]+3)
    else if(o==7){m[id,p3]=(v1<v2?1:0);p[id]+=4}
    else if(o==8){m[id,p3]=(v1==v2?1:0);p[id]+=4}
  }
}
function S(ps,  i,a,s,c,r,l){
  for(i=0;i<5;i++){p[i]=h[i]=0;ip[i]=1;ic[i]=0;for(a=0;a<n;a++)m[i,a]=g[a];iq[i,++ic[i]]=ps[i]}
  s=c=l=0;while(!h[4]){iq[c,++ic[c]]=s;while(1){r=R(c);if(r=="O"){s=out;if(c==4)l=s;break}if(r=="H"||r=="W")break}c=(c+1)%5}
  return l
}
function P(k,a,  i,v,t){
  if(k==5){v=S(a);if(v>M)M=v}
  else{for(i=k;i<5;i++){t=a[k];a[k]=a[i];a[i]=t;P(k+1,a);t=a[k];a[k]=a[i];a[i]=t}}
}
{for(i=1;i<=NF;i++)g[n++]=$i}
END{
  for(i=0;i<5;i++)a[i]=i;M=0;P(0,a);print M
  for(i=0;i<5;i++)a[i]=i+5;M=0;P(0,a);print M
}' input.txt
